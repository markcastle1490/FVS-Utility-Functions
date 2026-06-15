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
                    gst_table = "GST",
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
           #Broken top indicator
           BT = dplyr::coalesce(dplyr::if_else(ACTUALHT < HT, 1, 0), 0),
           #Create temporary HTCD for determining HT
           #Get measured height value (only observations that were actually measured)
           HT = {HTCD_TEMP = dplyr::coalesce(HTCD, 1)
             dplyr::case_when(is.na(ACTUALHT) & is.na(HT) ~ NA_real_,
                              HTCD_TEMP == 1 & !is.na(ACTUALHT) ~ ACTUALHT,
                              HTCD_TEMP == 1 & is.na(ACTUALHT) ~ HT,
                              HTCD_TEMP ==  2 & !is.na(ACTUALHT) ~ ACTUALHT,
                             TRUE ~ HT)},
           #Grab PREVIA for dead trees if needed
           DIA = dplyr::if_else(is.na(DIA) & STATUSCD == 2, PREVDIA, DIA),
           #Fill in missing DIACHECK values
           DIACHECK = dplyr::coalesce(DIACHECK, 0),
           #Assume 1 for missing DIAHTCD values
           DIAHTCD = dplyr::coalesce(DIAHTCD, 1)) |>
    #Create tree ID that is not unique by INVYR
    dplyr::mutate(TREEMERGEID = dplyr::cur_group_id(), 
                  .by = c(STATECD, UNITCD, COUNTYCD, PLOT, SUBP, TREE)) |>
    dplyr::mutate(PLOTMERGEID = dplyr::cur_group_id(), 
                  .by = c(STATECD, UNITCD, COUNTYCD, PLOT)) |>
    dplyr::rename(EXPF = "TPA_UNADJ")

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

  #Obtain variables that will have time 1 (start of remeasurment period) and 
  #time 2 (end of remeasurement period) values
  time_vars <- c(
 "CYCLE", "MEASYEAR", "MEASMON", "MEASDAY", "DIA", "HT", "CR", "STATUSCD",
 "AGENTCD", "DIACHECK", "HTDMP", "DESIGNCD")

  #Create list of data frames split by variable that will be used to join 
  #remeasurement data (cycle or year)
  merge_df <- split(tree |>
    dplyr::select(dplyr::all_of(c("UNIQUETREEID", 
                                  "PLOTMERGEID",
                                  "TREEMERGEID",
                                   time_vars))),
  f = tree$CYCLE)

  #Get variables that will not be included in merge_inv function
  tree <- tree |>
    dplyr::select(!c("TREEMERGEID", time_vars))

  #Call merge_inv function
  merge_df <- merge_inv(merge_df,
                        interval = "CYCLE",
                        verbose = verbose)

  #Extract drop columns, rename columns, and join to tree
  tree <- merge_df |>
    dplyr::select(!c(UNIQUETREEID.y, PLOTMERGEID.y, PLOTMERGEID.x)) |>
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
    #Sum DIACHECK value by TREEMERGEID. These values will be used to determine
    #if DIA measurement location changed during a remeasurement interval.
    dplyr::mutate(DIASUM0 = sum(DIACHECK0, na.rm = TRUE),
                  DIASUM1 = sum(DIACHECK1, na.rm = TRUE), 
                  .by = TREEMERGEID) |>
    dplyr::mutate(#Measurement interval length
                  MEASLEN = MEASYEAR1 - MEASYEAR0,
                  #Mortality observation indicator
                  MORT = dplyr::case_when(
                    STATUSCD0 == 1 & STATUSCD1 == 2 & MEASLEN > 0 ~ 1,
                    STATUSCD0 == 1 & STATUSCD1 == 1 & MEASLEN > 0 ~ 0,
                  .default = NA_integer_),
                  #Diameter growth observation indicator
                  IDGRM = dplyr::if_else(
                    DIA1 >= DIA0 & 
                    (STATUSCD0 == 1 & STATUSCD1 == 1) & 
                    MEASLEN > 0, 1L, NA_integer_),
                  #Diameter increment
                  DI = DIA1 - DIA0,
                  #Height growth observation indicator
                  IHGRM = dplyr::if_else(
                    HT1 >= HT0 & 
                    (STATUSCD0 == 1 & STATUSCD1 == 1) & 
                    MEASLEN > 0, 1L, NA_integer_),
                  #Height increment
                  HI = HT1 - HT0,
                  #Attempt to use future or past crown ratios when needed
                  #We assume that crown ratio does not change substantially
                  #between remeasurement periods
                  CR0 = dplyr::coalesce(CR0, CR1),
                  CR1 = dplyr::if_else(is.na(CR1) & STATUSCD1 == 1 & !is.na(CR0),
                   CR0, CR1),
                  #Height to crown base
                  HCB0 = HT0 - (HT0 * CR0/100),
                  HCB1 = HT1 - (HT1 * CR1/100),
                  #Placeholders for crown width
                  CW0 = NA_real_,
                  CW1 = NA_real_,
                  #Zero out values when NA. Helpful for plot variable calculations
                  EXPF = dplyr::coalesce(EXPF, 0.0),
                  DIA0 = dplyr::coalesce(DIA0, 0.0),
                  DIA1 = dplyr::coalesce(DIA1, 0.0),
                  HT0 = dplyr::coalesce(HT0, 0.0),
                  HT1 = dplyr::coalesce(HT1, 0.0)) |>
                  #dplyr::filter(VALIDYEAR > 0) |>
    dplyr::rename_with(toupper) |>
    dplyr::select(any_of(names(gst_vars)))

  #=============================================================================
  #Step 6
  #Calculate density and competition metrics
  #=============================================================================

  #=============================================================================
  # Step 7
  # Write GST dataframe to output database
  #=============================================================================
  
  if(verbose)
    cat("Step 6:", "Writing GST...", "\n")

  #Write GST to dbout
  write_gst(gst = tree, dbout = dbout, gst_table = gst_table)

  invisible()
}

#Archive this logic in case it is needed again
# VALIDYEAR = dplyr::case_when(
#                     MEASYEAR0 < MEASYEAR1 ~ 1,
#                     MEASYEAR0 == MEASYEAR1 & 
#                       (MEASYEAR0 == MAXYEAR & MEASYEAR1 == MAXYEAR) ~ 1,
#                     is.na(MEASYEAR0) | is.na(MEASYEAR1) | is.na(MAXYEAR) ~ 1,
#                     .default = 0),

################################################################################
# 'Implementation seems to be nearly equivalent to dplyr in terms of speed.
#' Not sure if I am missing out on something key with syntax...
#'@name fia_gst_dt
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

.datatable.aware <- TRUE

#'@export
fia_gst_dt <- function(dbin = NULL,
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
  tree <- data.table::setDT(x = RSQLite::dbGetQuery(con, fia_query))

  #Query SITREE
  site <- data.table::setDT(x = RSQLite::dbGetQuery(con, fia_si_query))

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
      #Drop duplicate site trees that occur by condition
    #Group by unique plot and species to calculate mean SI and SIBASE
  site_sum <- unique(site, 
                     by = c("STATECD", "INVYR", "UNITCD", "COUNTYCD", "PLOT",
                            "SUBP", "TREE"))[
    , 
    list( 
      SI     = round(mean(SITREE[VALIDCD == 1], na.rm = TRUE), 0),
      SIBASE = round(mean(SIBASE[VALIDCD == 1], na.rm = TRUE), 0)
    ),
    by = list(STATECD, INVYR, UNITCD, COUNTYCD, PLOT, SPCD)
  ]

  #Join site index summary to site_sum
  tree <- merge(x = tree,
        y = site_sum,
        by = c("STATECD",
                       "INVYR",
                       "UNITCD",
                       "COUNTYCD",
                       "PLOT",
                       "SPCD"),
        all.x = TRUE)

  #Cleanup sitetree data
  rm(site, site_sum); gc()

  #=============================================================================
  #Step 3
  #Define set of GST variables prior to calling merge_inv function.
  #=============================================================================

  if(verbose)
    cat("Step 3: Preparing variables before inventory remeasurement pairing...", "\n")

  tree[, HTCD_TEMP := data.table::fcoalesce(HTCD, 1L)
  ][, ':=' (DATAPROVIDER = 'FIA',
                  #Unique plot ID
                  UNIQUEPLOTID = paste(STATECD,
                                       INVYR,
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
                  #Broken top indicator
                  BT = data.table::fcoalesce(data.table::fifelse(ACTUALHT < HT, 1L, 0L), 0L),
                  #Create temporary HTCD for determining HT
                  #HTCD_TEMP = data.table::fcoalesce(HTCD, as.integer(1)),
                  #Get measured height value (only observations that were actually measured)
                  HT= data.table::fcase(is.na(ACTUALHT) & is.na(HT), 0L,
                                       HTCD_TEMP == 1 & !is.na(ACTUALHT), ACTUALHT,
                                       HTCD_TEMP == 1 & is.na(ACTUALHT), HT,
                                       HTCD_TEMP ==  2 & !is.na(ACTUALHT), ACTUALHT),
                  #HT = mapply(fia_ht, HTCD, ACTUALHT, HT),
                  #Grab PREVIA for dead trees if needed
                  DIA = data.table::fifelse(is.na(DIA) & STATUSCD == 2, PREVDIA, DIA),
                  #Fill in missing DIACHECK values
                  DIACHECK = data.table::fcoalesce(DIACHECK, 0L),
                  #Assume 1 for missing DIAHTCD values
                  DIAHTCD = data.table::fcoalesce(DIAHTCD, 1L),
                  TPA_UNADJ = data.table::fcoalesce(TPA_UNADJ, 0.0))
  ][, TREEMERGEID := .GRP, by = .(paste(STATECD,
                                        UNITCD,
                                        COUNTYCD,
                                        PLOT,
                                        SUBP,
                                        TREE))
  ][, PLOTMERGEID := .GRP, by = .(paste(STATECD,
                                        UNITCD,
                                        COUNTYCD,
                                        PLOT))]
  
  data.table::setnames(x = tree, old = c("TPA_UNADJ"), new = c("EXPF"))

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
    "CYCLE", "MEASYEAR", "MEASMON", "MEASDAY", "DIA", "HT", "CR", "STATUSCD",
    "AGENTCD", "DIACHECK", "HTDMP", "DESIGNCD")

  #Merge dataframe
  merge_df <- split(tree[, c("UNIQUETREEID",
                             "PLOTMERGEID",
                              "TREEMERGEID",
                              merge_vars), with = FALSE],
                    by = "CYCLE")

  #Isolate tree level variables not needed in merge_inv function
  tree = tree[, !c("TREEMERGEID", "PLOTMERGEID", merge_vars), with = FALSE]

  #Call merge_inv function
  merge_df <- merge_inv_dt(merge_df,
                           interval = "CYCLE",
                           verbose = verbose)

  #Extract appropriate columns
  merge_df <- merge_df[, c("UNIQUETREEID.y", "PLOTMERGEID.y", "PLOTMERGEID.x") := NULL]
  
  #Rename columns
  data.table::setnames(x = merge_df, old = c("UNIQUETREEID.x"), new = c("UNIQUETREEID"))
  data.table::setnames(x = merge_df, old = names(merge_df), gsub(".x", "0", names(merge_df)))
  data.table::setnames(x = merge_df, old = names(merge_df), gsub(".y", "1", names(merge_df)))
  
  #join merge_df to tree and then remove
  tree = merge(x = merge_df,
               y = tree, 
               by = c("UNIQUETREEID"),
               all.x = TRUE)

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
  
  #Sum DIACHECK value by TREEMERGEID. These values will be used to determine
  #if DIA measurement location changed during a remeasurement interval.
  tree[, ':=' (DIASUM0 = sum(DIACHECK0, na.rm = TRUE),
                 DIASUM1 = sum(DIACHECK1, na.rm = TRUE)),
                 by = TREEMERGEID]
  
  #Measurement interval length
  tree[, MEASLEN := MEASYEAR1 - MEASYEAR0]
  
  tree[, ':=' (#Mortality observation indicator
                  MORT = data.table::fcase(
                    STATUSCD0 == 1 & STATUSCD1 == 2 & MEASLEN > 0, 1L,
                    STATUSCD0 == 1 & STATUSCD1 == 1 & MEASLEN > 0, 0L,
                    default = NA_integer_),
                  #Diameter growth observation indicator
                  IDGRM = data.table::fifelse(
                    DIA1 >= DIA0 & 
                    (STATUSCD0 == 1 & STATUSCD1 == 1) & 
                    MEASLEN > 0, 1L, NA_integer_),
                  #Diameter increment
                  DI = DIA1 - DIA0,
                  #Height growth observation indicator
                  IHGRM = data.table::fifelse(
                    HT1 >= HT0 & 
                    (STATUSCD0 == 1 & STATUSCD1 == 1) & 
                    MEASLEN > 0, 1L, NA_integer_),
                  #Height increment
                  HI = HT1 - HT0,
                  #Attempt to use future or past crown ratios when needed
                  #We assume that crown ratio does not change substantially
                  #between remeasurement periods
                  CR0 = data.table::fcoalesce(CR0, CR1),
                  CR1 = data.table::fifelse(
                    is.na(CR1) & STATUSCD1 == 1 & !is.na(CR0),
                    CR0, CR1),
                  #Height to crown base
                  HCB0 = HT0 - (HT0 * CR0/100),
                  HCB1 = HT1 - (HT1 * CR1/100),
                  #Placeholders for crown width
                  CW0 = NA_real_,
                  CW1 = NA_real_,
                  #Zero out values when NA. Helpful for plot variable calculations
                  EXPF = data.table::fcoalesce(EXPF, 0.0),
                  DIA0 = data.table::fcoalesce(DIA0, 0.0),
                  DIA1 = data.table::fcoalesce(DIA1, 0.0),
                  HT0 = data.table::fcoalesce(as.double(HT0), 0.0),
                  HT1 = data.table::fcoalesce(as.double(HT1), 0.0))]
  
  #Upper case column names and get gst variables
  data.table::setnames(x = tree, toupper)
  tree = tree[, .SD, .SDcols = names(gst_vars)]

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
