################################################################################
#'fia_gst
#'@name fia_gst
#'@description
#' 
#' This function takes in a FIA SQLite database and writes infromation to a
#' growth sample tree (gst) database with variables described in the 
#' gst-variables.R file. This function is called from function build_fia defined 
#' in gst-functions.R file.
#
#'@param dbin:
#' Character string of file path FIA SQLite database.
#
#'@param dbout:		
#' Character string of file path to SQLite database where growth sample tree 
#' information will be written to.
#
#'@param gst_table: 
#' Character string corresponding to name of growth sample tree database table 
#' written to dbout argument.
#' 
#'@return
#' None
################################################################################

#'@export
fia_gst <- function(dbin = NULL,
                    dbout = NULL,
                    gst_table = NULL,
                    verbose = FALSE)
{

  #=============================================================================
  #Step 1
  #Query for TREE, PLOT, COND, SUBPLOT, REF_SPECIES
  #Query SITETREE
  #=============================================================================
  
  if(verbose) 
    cat("Step 1: Querying FIA data...", "\n")

  #Connect to dbin
  con <- RSQLite::dbConnect(RSQLite::SQLite(),
                            dbin)
  
  on.exit(
    expr = try(if(RSQLite::dbIsValid(con)) RSQLite::dbDisconnect(con),
    silent = TRUE))

  #Query all tables except SITREE
  tree <- RSQLite::dbGetQuery(con,
                                  fia_query)

  #Query SITREE
  site <- RSQLite::dbGetQuery(con,
                                  fia_si_query)

  #Disconnect from dbin
  RSQLite::dbDisconnect(con)

  #=============================================================================
  #Step 2
  #Summarize site index by plot and then join to tree. Not quite sure how
  #to best handle site index in fitting dataset yet.
  #=============================================================================
  
  if(verbose) 
    cat("Step 2: Summarizing site index...", "\n")

  #Calculate site index for each FIA plot (not subplot or condition) by species
  #For now, average of site index observations is taken for each species.
  site_sum <- site |>
    #Drop duplicate site trees that occur by condition
    dplyr::filter(!duplicated(paste(STATECD,
                                    INVYR,
                                    UNITCD,
                                    COUNTYCD,
                                    PLOT,
                                    SUBP,
                                    TREE,
                                    sep = "_"))) |>
    #Group by unique plot and species to calculate mean SI and SIBASE
    dplyr::summarize(SI = round(mean(SITREE[VALIDCD == 1], na.rm = T),0),
                     SIBASE = round(mean(SIBASE[VALIDCD == 1], na.rm = T),0),
                    .by = c(STATECD, INVYR, UNITCD, COUNTYCD, PLOT, SPCD))

  #Join site index summary to site_sum
  tree <- tree |>
    dplyr::left_join(y = site_sum,
                      by = c("STATECD",
                             "INVYR",
                             "UNITCD",
                             "COUNTYCD",
                             "PLOT",
                             "SPCD"))

  #Cleanup sitetree data
  rm(site, site_sum); gc()

  #=============================================================================
  #Step 3
  #Define set of GST variables prior to calling merge_inv function.
  #=============================================================================
  
  if(verbose) 
    cat("Step 3: Preparing variables before inventory remeasurement pairing...", "\n")

  tree <- tree |>
    dplyr::mutate(DATAPROVIDER = 'FIA',
           #Unique plot ID
           UNIQUEPLOTID = paste(STATECD,
                                INVYR,
                                UNITCD,
                                COUNTYCD,
                                PLOT,
                                sep = "_"),
           #Unique Plot ID without INVYR
           PLOTMERGEID = paste(STATECD,
                                UNITCD,
                                COUNTYCD,
                                PLOT,
                                sep = "_"),
           #Unique subplot ID
           UNIQUESUBPID = paste(STATECD,
                                INVYR,
                                UNITCD,
                                COUNTYCD,
                                PLOT,
                                SUBP,
                                sep = "_"),
           #Unique tree ID
           UNIQUETREEID = paste(STATECD,
                                INVYR,
                                UNITCD,
                                COUNTYCD,
                                PLOT,
                                SUBP,
                                TREE,
                                sep = "_"),
           #Create ID that will be used in merge_inv function
           #Unique tree ID without INVYR
           # TREEMERGEID = paste(STATECD,
           #                     UNITCD,
           #                     COUNTYCD,
           #                     PLOT,
           #                     SUBP,
           #                     TREE,
           #                     sep = "_"),
           #Broken top indicator
           BT = dplyr::coalesce(dplyr::if_else(ACTUALHT < HT, 1, 0), 0),
           #Create temporary HTCD for determining HT
           HTCD_TEMP = dplyr::coalesce(HTCD, 1),
           #Get measured height value (only observations that were actually measured)
           HT= dplyr::case_when(is.na(ACTUALHT) & is.na(HT) ~ NA,
                          HTCD_TEMP == 1 & !is.na(ACTUALHT) ~ ACTUALHT,
                          HTCD_TEMP == 1 & is.na(ACTUALHT) ~ HT,
                          HTCD_TEMP ==  2 & !is.na(ACTUALHT) ~ ACTUALHT),
           #HT = mapply(fia_ht, HTCD, ACTUALHT, HT),
           #Grab PREVIA for dead trees if needed
           DIA = dplyr::if_else(is.na(DIA) & STATUSCD == 2, PREVDIA, DIA),
           #Fill in missing DIACHECK values
           DIACHECK = dplyr::coalesce(DIACHECK, 0),
           #Assume 1 for missing DIAHTCD values
           DIAHTCD = dplyr::coalesce(DIAHTCD, 1),
           TPA_UNADJ = dplyr::coalesce(TPA_UNADJ, 0.0)) |>
    dplyr::rename(EXPF = "TPA_UNADJ")
    #Drop rows that are not needed in GST
    #Missing measurement year and cycle
    #Missing DIA
    #Missing EXPF
    #Retain STATUSCD 1 (live) or 2 (dead)
    # dplyr::filter(!is.na(MEASYEAR), 
    #               !is.na(CYCLE), 
    #               !is.na(DIA), 
    #               !is.na(EXPF),
    #               STATUSCD %in% c(1, 2))

  #=============================================================================
  # Step 4
  # 
  # Align time 1 and time 2 variables together and then merge with other
  # attributes using the merge_inv function.
  #
  # Split tree dataframe into two dataframes
  # merge_df - data that will be passed into merge_inv function
  # tree -     data that will be merged to fia_merge after merge_inv function 
  #            has completed processing.
  #
  #Note: There may be some better alternative to merge_inv using native 
  #dplyr/tidyverse functions or those from other packages.
  #=============================================================================

  if(verbose)
    cat("Step 4:", "Pairing remeasurements...", "\n")

  #Obtain variables that will be included in the fia_meas data frame and passed
  #to merge_inv function
  merge_vars <- c(
 "CYCLE", "MEASYEAR", "MEASMON",
 "MEASDAY", "DIA", "HT", "CR", "STATUSCD",
 "AGENTCD", "DIACHECK", "HTDMP", "DESIGNCD")

  #Create list of data frames split by variable that will be used to join 
  #remeasurement data (cycle or year)
  #TREEMERGEID is created to align remeasurement periods
  merge_df <- split(tree |>
    dplyr::mutate(TREEMERGEID = dplyr::cur_group_id(), 
                  .by = c(STATECD, UNITCD, COUNTYCD, PLOT, SUBP, TREE)) |>
    dplyr::select(dplyr::all_of(c("UNIQUETREEID", 
                                  "UNIQUESUBPID",
                                  "TREEMERGEID",
                                  "SPCD",
                                   merge_vars))),
  f = tree$CYCLE)

  #Obtain variables not in merge_df except for UNIQUETREEID and UNIQUESUBPID.
  exclude_vars <- c("SPCD", merge_vars)

  #Isolate tree level variables not needed in merge_inv function
  tree <- tree |>
    dplyr::select(!exclude_vars)

  #Call merge_inv function
  merge_df <- merge_inv(merge_df,
                        interval = "CYCLE",
                        verbose = verbose)

  #Extract appropriate columns and rename columns
  tree <- merge_df |>
    dplyr::select(!c(UNIQUETREEID.y, UNIQUESUBPID.y, UNIQUESUBPID.x)) |>
    dplyr::rename(UNIQUETREEID = "UNIQUETREEID.x") |>
    dplyr::rename_with(~ gsub(".x", "0", .)) |>
    dplyr::rename_with(~ gsub(".y", "1", .))   |>
    dplyr::left_join(y = tree, by = c("UNIQUETREEID"))
  
  #Clean up
  rm(merge_df); gc()
 
  #=============================================================================
  #Step 5
  #
  #Calculate remaining variables that are only possible and/or more convenient
  #to calculate after call to merge_inv functions.
  #=============================================================================
  
  if(verbose)
    cat("Step 5:", "Calculating final variables...", "\n")

  tree <- tree |>
    #Find maximum year by PLOTMERGEID
    dplyr::mutate(MAXYEAR = max(MEASYEAR1, na.rm = TRUE),
                  .by = PLOTMERGEID) |>
    #Sum DIACHECK value by TREEMERGEID. These values will be used to determine
    #if DIA measurement location changed during a remeasurement interval.
    dplyr::mutate(DIASUM0 = sum(DIACHECK0, na.rm = TRUE),
                  DIASUM1 = sum(DIACHECK1, na.rm = TRUE), 
                  .by = TREEMERGEID) |>
    #Remove remeasurement pairings that are equal but do NOT coincide with the
    #latest remeasurement of the plot. 
    dplyr::mutate(VALIDYEAR = dplyr::case_when(
                    MEASYEAR0 < MEASYEAR1 ~ 1,
                    MEASYEAR0 == MEASYEAR1 & 
                      (MEASYEAR0 == MAXYEAR & MEASYEAR1 == MAXYEAR) ~ 1,
                    is.na(MEASYEAR0) | is.na(MEASYEAR1) | is.na(MAXYEAR) ~ 1,
                    .default = 0),
                  MEASLEN = MEASYEAR1 - MEASYEAR0,
                  MORT = dplyr::case_when(
                    STATUSCD0 == 1 & STATUSCD1 == 2 & MEASLEN > 0 ~ 1,
                    STATUSCD0 == 1 & STATUSCD1 == 1 & MEASLEN > 0 ~ 0),
                  IDGRM = dplyr::if_else(
                    DIA1 >= DIA0 & 
                    (STATUSCD0 == 1 & STATUSCD1 == 1) & 
                    MEASLEN > 0, 1, 0),
                  DI = DIA1 - DIA0,
                  IHGRM = dplyr::if_else(
                    HT1 >= HT0 & 
                    (STATUSCD0 == 1 & STATUSCD1 == 1) & 
                    MEASLEN > 0, 1, 0),
                  HI = HT1 - HT0,
                  #Attempt to use future or past crown ratios when needed
                  #We assume that crown ratio does not change substantially
                  #between remeasurement periods
                  CR0 = dplyr::coalesce(CR0, CR1),
                  CR1 = dplyr::if_else(is.na(CR1) & STATUSCD1 == 1 & !is.na(CR0),
                   CR0, CR1),
                  HCB0 = HT0 - (HT0 * CR0/100),
                  HCB1 = HT1 - (HT1 * CR1/100),
                  CW0 = NA,
                  CW1 = NA,
                  #Zero out values when NA. Helpful for plot variable calculations
                  EXPF = dplyr::coalesce(EXPF, 0.0),
                  DIA0 = dplyr::coalesce(DIA0, 0.0),
                  DIA1 = dplyr::coalesce(DIA1, 0.0),
                  HT0 = dplyr::coalesce(HT0, 0.0),
                  HT1 = dplyr::coalesce(HT1, 0.0)) |>
                  dplyr::filter(VALIDYEAR > 0) |>
    dplyr::rename_with(toupper) |>
    dplyr::select(any_of(names(gst_vars)))

  #=============================================================================
  # Step 6
  # Write GST dataframe to output database
  #=============================================================================
  
  if(verbose)
    cat("Step 6:", "Writing GST...", "\n")

  #Write GST to dbout
  write_gst(gst = tree, dbout = dbout, gst_table = gst_table)

  invisible()
}

# ################################################################################
# # 'Implementation seems to be nearly equivalent to dplyr in terms of speed.
# #' Not sure if I am missing out on something key with syntax...
# #'@name fia_gst_dt
# #'@description
# #' 
# #' This function takes in a FIA SQLite database and writes infromation to a
# #' growth sample tree (gst) database with variables described in the 
# #' gst-variables.R file. This function is called from function build_fia defined 
# #' in gst-functions.R file.
# #
# #'@param dbin:
# #' Character string of file path FIA SQLite database.
# #
# #'@param dbout:		
# #' Character string of file path to SQLite database where growth sample tree 
# #' information will be written to.
# #
# #'@param gst_table: 
# #' Character string corresponding to name of growth sample tree database table 
# #' written to dbout argument.
# #' 
# #'@return
# #' None
# ################################################################################

# #'@export
# fia_gst_dt <- function(dbin = NULL,
#                     dbout = NULL,
#                     gst_table = NULL,
#                     verbose = FALSE)
# {
  
#   #=============================================================================
#   #Step 1
#   #Query for TREE, PLOT, COND, SUBPLOT, REF_SPECIES
#   #Query SITETREE
#   #=============================================================================
  
#   if(verbose) 
#     cat("Step 1: Querying FIA data...", "\n")
  
#   #Connect to dbin
#   con <- RSQLite::dbConnect(RSQLite::SQLite(),
#                             dbin)
  
#   on.exit(
#     expr = try(if(RSQLite::dbIsValid(con)) RSQLite::dbDisconnect(con),
#                silent = TRUE))
  
#   #Query all tables except SITREE
#   tree <- data.table::setDT(x = RSQLite::dbGetQuery(con, fia_query))
  
#   #Query SITREE
#   site <- data.table::setDT(x = RSQLite::dbGetQuery(con, fia_si_query))
  
#   #Disconnect from dbin
#   RSQLite::dbDisconnect(con)
  
#   #=============================================================================
#   #Step 2
#   #Summarize site index by plot and then join to tree. Not quite sure how
#   #to best handle site index in fitting dataset yet.
#   #=============================================================================
  
#   if(verbose) 
#     cat("Step 2: Summarizing site index...", "\n")
  
#   #Calculate site index for each FIA plot (not subplot or condition) by species
#   #For now, average of site index observations is taken for each species.
#   site_sum <- site |>
#     #Drop duplicate site trees that occur by condition
#     _[!duplicated(paste(STATECD,
#                         INVYR,
#                         UNITCD,
#                         COUNTYCD,
#                         PLOT,
#                         SUBP,
#                         TREE,
#                         sep = "_"))] |>
#     #Group by unique plot and species to calculate mean SI and SIBASE
#     _[, .(SI = round(mean(SITREE[VALIDCD == 1], na.rm = T),0),
#           SIBASE = round(mean(SIBASE[VALIDCD == 1], na.rm = T),0)),
#       by = .(STATECD, INVYR, UNITCD, COUNTYCD, PLOT, SPCD)]

#   #Join site index summary to site_sum
#   merge(x = tree,
#         y = site_sum,
#         by = c("STATECD",
#                        "INVYR",
#                        "UNITCD",
#                        "COUNTYCD",
#                        "PLOT",
#                        "SPCD"),
#         all.x = TRUE)
  
#   #Cleanup sitetree data
#   rm(site, site_sum); gc()
  
#   #=============================================================================
#   #Step 3
#   #Define set of GST variables prior to calling merge_inv function.
#   #=============================================================================
  
#   if(verbose) 
#     cat("Step 3: Preparing variables before inventory remeasurement pairing...", "\n")
  
#   tree |>
#     _[, HTCD_TEMP := data.table::fcoalesce(HTCD, as.integer(1))] |>
#     _[, ':=' (DATAPROVIDER = 'FIA',
#                   #Unique plot ID
#                   UNIQUEPLOTID = paste(STATECD,
#                                        INVYR,
#                                        UNITCD,
#                                        COUNTYCD,
#                                        PLOT,
#                                        sep = "_"),
#                   #Unique Plot ID without INVYR
#                   PLOTMERGEID = paste(STATECD,
#                                       UNITCD,
#                                       COUNTYCD,
#                                       PLOT,
#                                       sep = "_"),
#                   #Unique subplot ID
#                   UNIQUESUBPID = paste(STATECD,
#                                        INVYR,
#                                        UNITCD,
#                                        COUNTYCD,
#                                        PLOT,
#                                        SUBP,
#                                        sep = "_"),
#                   #Unique tree ID
#                   UNIQUETREEID = paste(STATECD,
#                                        INVYR,
#                                        UNITCD,
#                                        COUNTYCD,
#                                        PLOT,
#                                        SUBP,
#                                        TREE,
#                                        sep = "_"),
#                   #Create ID that will be used in merge_inv function
#                   #Unique tree ID without INVYR
#                   TREEMERGEID = paste(STATECD,
#                                       UNITCD,
#                                       COUNTYCD,
#                                       PLOT,
#                                       SUBP,
#                                       TREE,
#                                       sep = "_"),
#                   #Broken top indicator
#                   BT = data.table::fcoalesce(data.table::fifelse(ACTUALHT < HT, 1, 0), 0),
#                   #Create temporary HTCD for determining HT
#                   #HTCD_TEMP = data.table::fcoalesce(HTCD, as.integer(1)),
#                   #Get measured height value (only observations that were actually measured)
#                   HT= data.table::fcase(is.na(ACTUALHT) & is.na(HT), as.integer(0),
#                                        HTCD_TEMP == 1 & !is.na(ACTUALHT), ACTUALHT,
#                                        HTCD_TEMP == 1 & is.na(ACTUALHT), HT,
#                                        HTCD_TEMP ==  2 & !is.na(ACTUALHT), ACTUALHT),
#                   #HT = mapply(fia_ht, HTCD, ACTUALHT, HT),
#                   #Grab PREVIA for dead trees if needed
#                   DIA = data.table::fifelse(is.na(DIA) & STATUSCD == 2, PREVDIA, DIA),
#                   #Fill in missing DIACHECK values
#                   DIACHECK = data.table::fcoalesce(DIACHECK, as.integer(0)),
#                   #Assume 1 for missing DIAHTCD values
#                   DIAHTCD = data.table::fcoalesce(DIAHTCD, as.integer(1)),
#                   TPA_UNADJ = data.table::fcoalesce(TPA_UNADJ, 0.0))] |>
#     data.table::setnames(x = _, old = c("TPA_UNADJ"), new = c("EXPF")) |>
#     #Drop rows that are not needed in GST
#     #Missing measurement year and cycle
#     #Missing DIA
#     #Missing EXPF
#     #Retain STATUSCD 1 (live) or 2 (dead)
#     _[(!is.na(MEASYEAR) & 
#          !is.na(CYCLE) &
#          !is.na(DIA) &
#          !is.na(EXPF) &
#          STATUSCD %in% c(1, 2))]
  
#   #=============================================================================
#   # Step 4
#   # 
#   # Align time 1 and time 2 variables together and then merge with other
#   # attributes using the merge_inv function.
#   #
#   # Split tree dataframe into two dataframes
#   # merge_df - data that will be passed into merge_inv function
#   # tree -     data that will be merged to fia_merge after merge_inv function 
#   #            has completed processing.
#   #
#   #Note: There may be some better alternative to merge_inv using native 
#   #dplyr/tidyverse functions or those from other packages.
#   #=============================================================================
  
#   if(verbose)
#     cat("Step 4:", "Pairing remeasurements...", "\n")
  
#   #Obtain variables that will be included in the fia_meas data frame and passed
#   #to merge_inv function
#   merge_vars <- c(
#     "TREEMERGEID", "SPCD", "CYCLE", "MEASYEAR", "MEASMON",
#     "MEASDAY", "DIA", "HT", "CR", "STATUSCD",
#     "AGENTCD", "DIACHECK", "HTDMP", "DESIGNCD")
  
#   #Merge dataframe
#   merge_df <- split(tree[, c("UNIQUETREEID", "UNIQUESUBPID", merge_vars), with = FALSE],
#                     by = "CYCLE")
  
#   #Obtain variables not in merge_df except for UNIQUETREEID and UNIQUESUBPID.
#   exclude_vars <- c("UNIQUETREEID", 
#                     "UNIQUESUBPID",
#                     colnames(tree)[!colnames(tree) %in% merge_vars])
  
#   #Isolate tree level variables not needed in merge_inv function
#   tree[, !merge_vars, with = FALSE]
  
#   #Call merge_inv function
#   merge_df <- merge_inv_dt(merge_df,
#                         interval = "CYCLE",
#                         verbose = verbose)
  
#   return(merge_df)
  
#   #Extract appropriate columns and rename columns
#   tree <- merge_df |>
#     _[, !c(UNIQUETREEID.y, UNIQUESUBPID.y, UNIQUESUBPID.x)] |>
#     data.table::setnames(x = _, 
#                          old = c("UNIQUETREEID.x"), 
#                          new = c("UNIQUETREEID")) |>
#     dplyr::rename_with(~ gsub(".x", "0", .)) |>
#     dplyr::rename_with(~ gsub(".y", "1", .))   |>
#     dplyr::left_join(y = tree, by = c("UNIQUETREEID"))
  
#   #Clean up
#   rm(merge_df); gc()
  
#   return(tree)
  
#   #=============================================================================
#   #Step 5
#   #
#   #Calculate remaining variables that are only possible and/or more convenient
#   #to calculate after call to merge_inv functions.
#   #=============================================================================
  
#   if(verbose)
#     cat("Step 5:", "Calculating final variables...", "\n")
  
#   tree <- tree |>
#     #Find maximum year by PLOTMERGEID
#     dplyr::mutate(MAXYEAR = max(MEASYEAR1, na.rm = TRUE),
#                   .by = PLOTMERGEID) |>
#     #Sum DIACHECK value by TREEMERGEID. These values will be used to determine
#     #if DIA measurement location changed during a remeasurement interval.
#     dplyr::mutate(DIASUM0 = sum(DIACHECK0, na.rm = TRUE),
#                   DIASUM1 = sum(DIACHECK1, na.rm = TRUE), 
#                   .by = TREEMERGEID) |>
#     #Remove remeasurement pairings that are equal but do NOT coincide with the
#     #latest remeasurement of the plot. 
#     dplyr::mutate(VALIDYEAR = dplyr::case_when(
#       MEASYEAR0 < MEASYEAR1 ~ 1,
#       MEASYEAR0 == MEASYEAR1 & 
#         (MEASYEAR0 == MAXYEAR & MEASYEAR1 == MAXYEAR) ~ 1,
#       is.na(MEASYEAR0) | is.na(MEASYEAR1) | is.na(MAXYEAR) ~ 1,
#       .default = 0),
#       MEASLEN = MEASYEAR1 - MEASYEAR0,
#       MORT = dplyr::case_when(
#         STATUSCD0 == 1 & STATUSCD1 == 2 & MEASLEN > 0 ~ 1,
#         STATUSCD0 == 1 & STATUSCD1 == 1 & MEASLEN > 0 ~ 0),
#       IDGRM = dplyr::if_else(
#         DIA1 >= DIA0 & 
#           (STATUSCD0 == 1 & STATUSCD1 == 1) & 
#           MEASLEN > 0, 1, 0),
#       DI = DIA1 - DIA0,
#       IHGRM = dplyr::if_else(
#         HT1 >= HT0 & 
#           (STATUSCD0 == 1 & STATUSCD1 == 1) & 
#           MEASLEN > 0, 1, 0),
#       HI = HT1 - HT0,
#       #Attempt to use future or past crown ratios when needed
#       #We assume that crown ratio does not change substantially
#       #between remeasurement periods
#       CR0 = dplyr::coalesce(CR0, CR1),
#       CR1 = dplyr::if_else(is.na(CR1) & STATUSCD1 == 1 & !is.na(CR0),
#                            CR0, CR1),
#       HCB0 = HT0 - (HT0 * CR0/100),
#       HCB1 = HT1 - (HT1 * CR1/100),
#       CW0 = NA,
#       CW1 = NA,
#       #Zero out values when NA. Helpful for plot variable calculations
#       EXPF = dplyr::coalesce(EXPF, 0.0),
#       DIA0 = dplyr::coalesce(DIA0, 0.0),
#       DIA1 = dplyr::coalesce(DIA1, 0.0),
#       HT0 = dplyr::coalesce(HT0, 0.0),
#       HT1 = dplyr::coalesce(HT1, 0.0)) |>
#     dplyr::filter(VALIDYEAR > 0)
  
#   #=============================================================================
#   # Step 6
#   # Write GST dataframe to output database
#   #=============================================================================
  
#   if(verbose)
#     cat("Step 6:", "Writing GST...", "\n")
  
#   #Capitalize field names and select those only in gst_vars
#   tree <- tree |>
#     dplyr::rename_with(toupper) |>
#     dplyr::select(any_of(names(gst_vars)))
  
#   #Write GST to dbout
#   write_gst(gst = tree, dbout = dbout, gst_table = gst_table)
  
#   invisible()
# }
