################################################################################
#' fia_fitdb
#' @name fia_fitdb
#' @description This function takes in a FIA SQLite database and writes 
#' information to a database that is used for fitting equations for the Forest 
#' Vegetation Simulator (FVS) fitdb-variables.R file. This function is called 
#' from function build_fitdb defined in fitdb-functions.R file.
#' 
#' @param dbin
#' Character string of file path FIA SQLite database. Defaults to NULL.
#' 
#' @param dbout
#' Character string of file path to SQLite database where equation fitting 
#' information will be written to. Defaults to NULL.
#' 
#' @param fitdb_name
#' Character string corresponding to name of database table written to dbout 
#' argument. Defaults to NULL.
#' 
#' @param merge_type
#' Integer value corresponding to merging strategy to use for aligning
#' #remeasurements
#' 1 - uses unique_id
#' 2 - uses merge_id and interval_id
#' 
#' @param verbose
#' Logical variable where if TRUE, progress messages will be output to the 
#' console. Defaults to FALSE.
#' 
#' @return
#' None
#' @export
################################################################################

fia_fitdb <- function(dbin = NULL,
                      dbout = NULL,
                      fitdb_name = NULL,
                      merge_type = 1,
                      verbose = FALSE)
 {

  #=============================================================================
  #Query for TREE, PLOT, PLOTGEOM, COND, SUBPLOT, REF_SPECIES
  #Query SITETREE
  #=============================================================================

  if(verbose)
    cat("Querying FIA data...", "\n")

  #Connect to dbin
  con <- RSQLite::dbConnect(RSQLite::SQLite(),
                             dbin)

  on.exit(
     expr = try(if(RSQLite::dbIsValid(con)) RSQLite::dbDisconnect(con),
                silent = TRUE))

  #Query all tables except SITREE
  tree <- data.table::setDT(x = RSQLite::dbGetQuery(con, fia_tree_query()))

  #Query SITREE
  site <- data.table::setDT(x = RSQLite::dbGetQuery(con, fia_si_query()))

  #Disconnect from dbin
  RSQLite::dbDisconnect(con)

  #=============================================================================
  #Summarize site index by plot and then join to tree. Not quite sure how
  #to best handle site index in fitting dataset yet.
  #=============================================================================

  if(verbose)
    cat("Summarizing site index...", "\n")

  #Calculate site index and base age by PLT_CN and SPCD
  site_sum <- site[
    , 
    .( 
      SI_FIA = round(mean(SITREE[VALIDCD == 1], na.rm = TRUE), 0),
      SIBASE_FIA = round(mean(SIBASE[VALIDCD == 1], na.rm = TRUE), 0),
      SI_FVS     = round(mean(SITREE_FVS[VALIDCD == 1], na.rm = TRUE), 0),
      SIBASE_FVS = round(mean(SIBASE_FVS[VALIDCD == 1], na.rm = TRUE), 0)),
    by = .(PLT_CN, SPCD)
  ]

  #Join site index summary to site_sum
  tree <- merge(x = tree,
                y = site_sum,
                by = c("PLT_CN",
                       "SPCD"),
                all.x = TRUE)

  #Cleanup sitetree data
  rm(site, site_sum); gc()

  #=============================================================================
  #Define set of GST variables prior to calling merge_inv function.
  #=============================================================================

  if(verbose)
    cat("Preparing variables before inventory remeasurement pairing...", "\n")

  #Create temporary HTCD with null values filled in (1)
  tree[, HTCD_TEMP := data.table::fcoalesce(HTCD, 1L)
  ][, ':=' (DATASOURCE = 'FIA',
                  #Create broken top indicator
                  #Need to verify if DAMAGE_AGENT_CD1 or ABNORMAL termination
                  #needs to be considered
                  BT = data.table::fcase(ACTUALHT < HT, 1L, default = 0L),
                  #Get measured height value (only observations that were actually measured)
                  #HT= data.table::fifelse(!is.na(ACTUALHT), ACTUALHT, HT),
                  #Grab PREVIA for dead trees if needed
                  DIA = data.table::fifelse(is.na(DIA) & STATUSCD == 2, PREVDIA, DIA),
                  #Set EXPF
                  EXPF = data.table::fcoalesce(TPA_UNADJ, 0.0),
                  #Fill in missing HTDMP values and allow for tolerance of
                  #values (0.0 = valid HTDMP)
                  #Add this in if code gets used.
                  # HTDMP = data.table::fcase(is.na(HTDMP), 0.0,
                  #               HTDMP >= 4 & HTDMP < 5, 0.0,
                  #               default = HTDMP),
                  #Assume 1 for missing DIAHTCD values
                  #Add this in if code gets used.
                  DIAHTCD = data.table::fcoalesce(DIAHTCD, 1L),
                  #Assume 0 for missing DIACHECK values
                  #Add this in if this code gets used. 
                  #DIACHECK = data.table::fcoalesce(DIACHECK, 0L),
                  CRTYPE = 0L,
                  #Create date for REMPER calculation
                  DATE = as.Date(sprintf("%04d-%02d-%02d", MEASYEAR, MEASMON, MEASDAY),
                  format = "%Y-%m-%d",
                  na.strings = "NA-NA-NA"))
    #TREEMERGEID could be used for aligning remeasurements for Alaska
  ][, TREEMERGEID := .GRP, by = .(paste(STATECD,
                                         UNITCD,
                                         COUNTYCD,
                                         PLOT,
                                         SUBP,
                                         TREE))
  ]
  
  #=============================================================================
  #Calculate competition and density metrics prior to merging
  #=============================================================================

  if(verbose)
  cat("Calculating competition and density measures...", "\n")

  #Calculate temporary variables for plot calculations
  tree[, ':=' (TEXPF = data.table::fifelse(STATUSCD == 2, 0.0, EXPF),
               TDIA = data.table::fifelse(STATUSCD == 2, 0.0, DIA))]   
  
  #Calculate plot level values
  tree[, ':=' (BA = ba(dbh = TDIA, expf = TEXPF),
               TPA = tpa(expf = TEXPF, dbh = TDIA),
               QMD = qmd(dbh = TDIA, expf = TEXPF),
               ZSDI = zsdi(dbh = TDIA, expf = TEXPF),
               BAL = bal(dbh = TDIA, expf = TEXPF)),
        by = .(PLT_CN)]
  
  #Calculate temporary variables for subplot calculations
  #Not yet 100% sure 4 can be globally applied here
  tree[, TEXPF := TEXPF * 4]   
  
  #Calculate subplot level values
  tree[, ':=' (PBA = ba(dbh = TDIA, expf = TEXPF),
               PTPA = tpa(expf = TEXPF, dbh = TDIA),
               PQMD = qmd(dbh = TDIA, expf = TEXPF),
               PZSDI = zsdi(dbh = TDIA, expf = TEXPF),
               PBAL = bal(dbh = TDIA, expf = TEXPF)),
       by = .(SUBP_CN)]
  
  #=============================================================================
  # Align time 1 and time 2 variables together and then merge with other
  # attributes using the merge_inv function.
  #=============================================================================

  if(verbose)
    cat("Pairing remeasurements...", "\n")

  #Obtain variables that will be included in the merge_inv function
  merge_vars <- c(
    "CYCLE", "MEASYEAR", "MEASMON", "MEASDAY", "DATE", "SPCD", "DIA", "ACTUALHT",
    "HT", "BT", "CR", "STATUSCD", "AGENTCD", "DIACHECK", "HTDMP", "DESIGNCD",
    "HTCD_TEMP", "HTCD", "TPA", "QMD", "BA", "ZSDI", "BAL", "PTPA", "PQMD",
    "PBA", "PZSDI", "PBAL")
  
  #Merge measurements. May need to use different merge for Alaska (can be set
  #by merge_type)
  merge_df <- merge_inv(data = tree[, c("TREE_CN",
                                        "PREV_TRE_CN",
                                        "TREEMERGEID",
                                        merge_vars), with = FALSE],
                        unique_id = "TREE_CN",
                        prev_unique_id = "PREV_TRE_CN",
                        merge_id = "TREEMERGEID",
                        interval_id = "CYCLE",
                        merge_type = merge_type)

  #Isolate tree level variables not needed in merge_inv function
  tree <- tree[, !c("TREEMERGEID", merge_vars), with = FALSE]
  
  #May need different merge for Alaska data
  tree <- merge(x = merge_df,
                y = tree,
                by.x = "TREE_CN1",
                by.y = "TREE_CN",
                all.x = TRUE)
  
  #Clean up
  rm(merge_df); gc()

  #=============================================================================
  #Calculate remaining variables that are only possible and/or more convenient
  #to calculate after call to merge_inv functions.
  #=============================================================================

  if(verbose)
    cat("Calculating variables post remeasurement pairing...", "\n")
  
  #Compute REMPER
  tree[, REMPER := round(((DATE2 - DATE1)/365.25), 1)]

  #Calculate fitting indicator variables, MORT, and HCB
  tree[, ':=' (#Height observation
               IHTM = data.table::fcase(
                 HTCD_TEMP1 == 1 & 
                   !is.na(HT1) &
                   STATUSCD1 == 1 &
                   BT1 != 1, 1L,
                 default = 0L),
               #Crown size observation
               ICRM = data.table::fcase(
                   !is.na(CR1) &
                   STATUSCD1 == 1 &
                   BT1 != 1, 1L, 
                   default = 0L),
               #Diameter growth observation indicator
               IDGRM = data.table::fcase(
                 DIA2 >= DIA1 & 
                   STATUSCD1 == 1 & 
                   STATUSCD2 == 1 & 
                   HTDMP1 == 0 & 
                   HTDMP2 == 0 & 
                   DIACHECK1 == 0 &
                   DIACHECK2 == 0 &
                   REMPER > 0, 1L, 
                   default = 0L),
               #Height growth observation indicator
               IHGRM = data.table::fcase(
                 HT2 >= HT1 & 
                   STATUSCD1 == 1 & 
                   STATUSCD2 == 1 & 
                   HTCD_TEMP1 == 1 &
                   HTCD_TEMP2 == 1 &
                   REMPER > 0, 1L, 
                   default = 0L),
               #Mortality response variable (0 or 1)
               MORT = data.table::fcase(
                 STATUSCD1 == 1 & STATUSCD2 == 2 & REMPER > 0, 1L,
                 STATUSCD1 == 1 & STATUSCD2 == 1 & REMPER > 0, 0L,
                 default = NA_integer_),
               #Height to crown base
               HCB1 = HT1 - (HT1 * CR1/100),
               HCB2 = HT2 - (HT2 * CR2/100))]
  
  #Mortality observation indicator
  tree[, IMRT := data.table::fcase(MORT %in% c(0, 1), 1L, 
                                   default = 0L)]

  #Upper case column names and get fitdb variables
  data.table::setnames(x = tree, toupper)

  #Eliminate columns not in fitdb_vars
  keep_cols <- names(fitdb_vars())
  tree <- tree[, ..keep_cols]  

  #=============================================================================
  # Write tree (fitdb) data.table to output database
  #=============================================================================

  if(verbose)
    cat("Writing", paste0(fitdb_name, "..."), "\n")
  
  #Write GST to dbout
  write_fitdb(fitdb = tree, dbout = dbout, fitdb_name = fitdb_name)

  invisible()
}

### Archived fia_fitdb version that is based on dplyr

# ################################################################################
# #'fia_fitdb
# #'@name fia_fitdb
# #'@description
# #' 
# #' This function takes in a FIA SQLite database and writes information to a
# #' database that is used for fitting equations for the Forest Vegetation
# #' Simulator (FVS) fitdb-variables.R file. This function is called from function
# #' build_fitdb defined in fitdb-functions.R file.
# #
# #'@param dbin:
# #' Character string of file path FIA SQLite database.
# #
# #'@param dbout:		
# #' Character string of file path to SQLite database where equation fitting
# #' information will be written to.
# #
# #'@param fitdb_name: 
# #' Character string corresponding to name of database table written to dbout 
# #' argument.
# #' 
# #'@param verbose:
# #' Logical variable where if TRUE, progress messages will be output to the
# #' console.
# #' 
# #'@return
# #' None
# ################################################################################

# #'@export
# fia_fitdb <- function(dbin = NULL,
#                       dbout = NULL,
#                       fitdb_name = "FITDB",
#                       verbose = FALSE)
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
#     silent = TRUE))

#   #Query all tables except SITREE
#   tree <- RSQLite::dbGetQuery(con,
#                               fia_tree_query())

#   #Query SITREE
#   site <- RSQLite::dbGetQuery(con,
#                               fia_si_query())

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
#     dplyr::filter(!duplicated(paste(STATECD,
#                                     INVYR,
#                                     UNITCD,
#                                     COUNTYCD,
#                                     PLOT,
#                                     SUBP,
#                                     TREE,
#                                     sep = "_"))) |>
#     #Group by unique plot and species to calculate mean SI and SIBASE
#     dplyr::summarize(SI = round(mean(SITREE[VALIDCD == 1], na.rm = T),0),
#                      SIBASE = round(mean(SIBASE[VALIDCD == 1], na.rm = T),0),
#                     .by = c(STATECD, INVYR, UNITCD, COUNTYCD, PLOT, SPCD))

#   #Join site index summary to site_sum
#   tree <- tree |>
#     dplyr::left_join(y = site_sum,
#                       by = c("STATECD",
#                              "INVYR",
#                              "UNITCD",
#                              "COUNTYCD",
#                              "PLOT",
#                              "SPCD"))

#   #Cleanup sitetree data
#   rm(site, site_sum); gc()

#   #=============================================================================
#   #Step 3
#   #Define set of GST variables prior to calling merge_inv function.
#   #=============================================================================
  
#   if(verbose) 
#     cat("Step 3: Preparing variables before inventory remeasurement pairing...", "\n")

#   tree <- tree |>
#     dplyr::mutate(DATAPROVIDER = 'FIA',
#            #Unique plot ID
#            UNIQUEPLOTID = paste(STATECD,
#                                 INVYR,
#                                 UNITCD,
#                                 COUNTYCD,
#                                 PLOT,
#                                 sep = "_"),
#            #Unique subplot ID
#            UNIQUESUBPID = paste(STATECD,
#                                 INVYR,
#                                 UNITCD,
#                                 COUNTYCD,
#                                 PLOT,
#                                 SUBP,
#                                 sep = "_"),
#            #Unique tree ID
#            UNIQUETREEID = paste(STATECD,
#                                 INVYR,
#                                 UNITCD,
#                                 COUNTYCD,
#                                 PLOT,
#                                 SUBP,
#                                 TREE,
#                                 sep = "_"),
#            #Broken top indicator
#            BT = dplyr::coalesce(dplyr::if_else(ACTUALHT < HT, 1, 0), 0),
#            #Create temporary HTCD for determining HT
#            #Get measured height value (only observations that were actually measured)
#            HT = {HTCD_TEMP = dplyr::coalesce(HTCD, 1)
#              dplyr::case_when(is.na(ACTUALHT) & is.na(HT) ~ NA_real_,
#                               HTCD_TEMP == 1 & !is.na(ACTUALHT) ~ ACTUALHT,
#                               HTCD_TEMP == 1 & is.na(ACTUALHT) ~ HT,
#                               HTCD_TEMP ==  2 & !is.na(ACTUALHT) ~ ACTUALHT,
#                              TRUE ~ HT)},
#            #Grab PREVIA for dead trees if needed
#            DIA = dplyr::if_else(is.na(DIA) & STATUSCD == 2, PREVDIA, DIA),
#            #Fill in missing DIACHECK values
#            DIACHECK = dplyr::coalesce(DIACHECK, 0),
#            #Assume 1 for missing DIAHTCD values
#            DIAHTCD = dplyr::coalesce(DIAHTCD, 1)) |>
#     #Create tree ID that is not unique by INVYR
#     dplyr::mutate(TREEMERGEID = dplyr::cur_group_id(), 
#                   .by = c(STATECD, UNITCD, COUNTYCD, PLOT, SUBP, TREE)) |>
#     dplyr::mutate(PLOTMERGEID = dplyr::cur_group_id(), 
#                   .by = c(STATECD, UNITCD, COUNTYCD, PLOT)) |>
#     dplyr::rename(EXPF = "TPA_UNADJ")

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

#   #Obtain variables that will have time 1 (start of remeasurment period) and 
#   #time 2 (end of remeasurement period) values
#   time_vars <- c(
#  "CYCLE", "MEASYEAR", "MEASMON", "MEASDAY", "DIA", "HT", "CR", "STATUSCD",
#  "AGENTCD", "DIACHECK", "HTDMP", "DESIGNCD")

#   #Create list of data frames split by variable that will be used to join 
#   #remeasurement data (cycle or year)
#   merge_df <- split(tree |>
#     dplyr::select(dplyr::all_of(c("UNIQUETREEID", 
#                                   "PLOTMERGEID",
#                                   "TREEMERGEID",
#                                    time_vars))),
#   f = tree$CYCLE)

#   #Get variables that will not be included in merge_inv function
#   tree <- tree |>
#     dplyr::select(!c("TREEMERGEID", time_vars))

#   #Call merge_inv function
#   merge_df <- merge_inv(merge_df,
#                         interval = "CYCLE",
#                         verbose = verbose)

#   #Extract drop columns, rename columns, and join to tree
#   tree <- merge_df |>
#     dplyr::select(!c(UNIQUETREEID.y, PLOTMERGEID.y, PLOTMERGEID.x)) |>
#     dplyr::rename(UNIQUETREEID = "UNIQUETREEID.x") |>
#     dplyr::rename_with(~ gsub(".x", "0", .)) |>
#     dplyr::rename_with(~ gsub(".y", "1", .))   |>
#     dplyr::left_join(y = tree, by = c("UNIQUETREEID"))
  
#   #Clean up
#   rm(merge_df); gc()
 
#   #=============================================================================
#   #Step 5
#   #
#   #Calculate remaining variables that are only possible and/or more convenient
#   #to calculate after call to merge_inv functions.
#   #=============================================================================
  
#   if(verbose)
#     cat("Step 5:", "Calculating final variables...", "\n")

#   tree <- tree |>
#     #Sum DIACHECK value by TREEMERGEID. These values will be used to determine
#     #if DIA measurement location changed during a remeasurement interval.
#     dplyr::mutate(DIASUM1 = sum(DIACHECK1, na.rm = TRUE),
#                   DIASUM2 = sum(DIACHECK2, na.rm = TRUE), 
#                   .by = TREEMERGEID) |>
#     dplyr::mutate(#Measurement interval length
#                   MEASLEN = MEASYEAR2 - MEASYEAR1,
#                   #Mortality observation indicator
#                   MORT = dplyr::case_when(
#                     STATUSCD1 == 1 & STATUSCD2 == 2 & MEASLEN > 0 ~ 1,
#                     STATUSCD1 == 1 & STATUSCD2 == 1 & MEASLEN > 0 ~ 0,
#                   .default = NA_integer_),
#                   #Diameter growth observation indicator
#                   IDGRM = dplyr::if_else(
#                     DIA2 >= DIA1 & 
#                     (STATUSCD1 == 1 & STATUSCD2 == 1) & 
#                     MEASLEN > 0, 1L, NA_integer_),
#                   #Diameter increment
#                   DI = DIA2 - DIA1,
#                   #Height growth observation indicator
#                   IHGRM = dplyr::if_else(
#                     HT2 >= HT1 & 
#                     (STATUSCD1 == 1 & STATUSCD2 == 1) & 
#                     MEASLEN > 0, 1L, NA_integer_),
#                   #Height increment
#                   HI = HT2 - HT1,
#                   #Attempt to use future or past crown ratios when needed
#                   #We assume that crown ratio does not change substantially
#                   #between remeasurement periods
#                   CR1 = dplyr::coalesce(CR1, CR2),
#                   CR2 = dplyr::if_else(is.na(CR2) & STATUSCD2 == 1 & !is.na(CR1),
#                    CR1, CR2),
#                   #Height to crown base
#                   HCB1 = HT1 - (HT1 * CR1/100),
#                   HCB2 = HT2 - (HT2 * CR2/100),
#                   #Placeholders for crown width
#                   CW1 = NA_real_,
#                   CW2 = NA_real_,
#                   #Zero out values when NA. Helpful for plot variable calculations
#                   EXPF = dplyr::coalesce(EXPF, 0.0),
#                   DIA1 = dplyr::coalesce(DIA1, 0.0),
#                   DIA2 = dplyr::coalesce(DIA2, 0.0),
#                   HT1 = dplyr::coalesce(HT1, 0.0),
#                   HT2 = dplyr::coalesce(HT2, 0.0)) |>
#                   #dplyr::filter(VALIDYEAR > 0) |>
#     dplyr::rename_with(toupper) |>
#     dplyr::select(any_of(names(fitdb_vars())))

#   #=============================================================================
#   # Step 7
#   # Write GST dataframe to output database
#   #=============================================================================
  
#   if(verbose)
#     cat("Step 6:", "Writing GST...", "\n")

#   #Write GST to dbout
#   write_fitdb(fitdb = tree, dbout = dbout, fitdb_name = fitdb_name)

#   invisible()
# }
