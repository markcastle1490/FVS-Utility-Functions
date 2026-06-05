################################################################################
#fia_gst
#
#Description
#
#This function takes in a FIA SQLite database and creates a growth sample tree
#database with the following variables described in
#fvsRefit_R_Documentation.docx (variables included in the database can be found
#in the getGSTVars function definition as well). The created growth sample tree
#database is sent to a specified output database. This function is called from
#function build_fia defined in FIA_GST_Functions.R file.
#
#Source Code
#
#Function fia_gst is currently located in the FIA_GST_Functions.R file.
#
#Arguments
#
#db_in:     Directory path and file name to FIA SQLite database. Currently an
#           FIA database must include the following database tables: TREE, PLOT,
#           SUBP, COND, and SITETREE.
#
#db_out:		Directory path and file name to SQLite database where GST will be
#           created.
#
#Value
#
#Integer value of 0 if function returns from the end of the function definition
#or a value of 1 if function returns before end of function definition.
################################################################################

fia_gst <- function(db_in = NA,
                    db_out = NA,
                    verbose = FALSE)
{

  #=============================================================================
  #Step 1
  #Query for TREE, PLOT, COND, SUBPLOT, REF_SPECIES
  #Query SITETREE
  #=============================================================================
  
  if(verbose) cat("Step 1: Querying FIA data...", "\n")

  #Connect to db_in
  con <- RSQLite::dbConnect(RSQLite::SQLite(),
                            db_in)

  #add on exit statement to disconnect database in case of error
  on.exit(
    expr = try(if(RSQLite::dbIsValid(con)) RSQLite::dbDisconnect(con),
    silent = TRUE))

  #Query all tables except SITREE
  fia_tree <- RSQLite::dbGetQuery(con,
                                  fia_query)

  #Query SITREE
  fia_site <- RSQLite::dbGetQuery(con,
                                  fia_si_query)

  #Disconnect from db_in
  RSQLite::dbDisconnect(con)

  #=============================================================================
  #Step 2
  #Summarize site index by plot and then join to fia_tree. Not quite sure how
  #to best handle site index in fitting dataset yet.
  #=============================================================================
  
  if(verbose) cat("Step 2: Summarizing site index...", "\n")

  #Calculate site index for each FIA plot (not subplot or condition) by species
  #For now, average of site index observations is taken for each species.
  fia_site_sum <- fia_site |>
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
    dplyr::group_by(STATECD,
                    INVYR,
                    UNITCD,
                    COUNTYCD,
                    PLOT,
                    SPCD) |>
    dplyr::summarize(SI = round(mean(SITREE[VALIDCD == 1], na.rm = T),0),
                     SIBASE = round(mean(SIBASE[VALIDCD == 1], na.rm = T),0)) |>
    dplyr::ungroup()

  #Join site index summary to fia_site_sum
  fia_tree <- fia_tree |>
    dplyr::left_join(y = fia_site_sum,
                      by = c("STATECD",
                             "INVYR",
                             "UNITCD",
                             "COUNTYCD",
                             "PLOT",
                             "SPCD"))

  #Cleanup sitetree data
  rm(fia_site, fia_site_sum)

  #=============================================================================
  #Step 3
  #Define set of GST variables prior to calling merge_inv function.
  #=============================================================================
  
  if(verbose) cat("Step 3: Preparing variables before inventory remeasurement pairing...", "\n")

  fia_tree <- fia_tree |>
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
           TREEMERGEID = paste(STATECD,
                               UNITCD,
                               COUNTYCD,
                               PLOT,
                               SUBP,
                               TREE,
                               sep = "_"),
           #Broken top indicator
           BT = dplyr::coalesce(dplyr::if_else(ACTUALHT < HT, 1, 0), 0),
           #Measured height value (only observations that were actually measured)
           HT = mapply(fia_ht, HTCD, ACTUALHT, HT),
           #Grab PREVIA for dead trees if needed
           DIA = dplyr::if_else(is.na(DIA) & STATUSCD == 2, PREVDIA, DIA),
           #Fill in missing DIACHECK values
           DIACHECK = dplyr::if_else(is.na(DIACHECK), 0, DIACHECK)) |>
    rename(EXPF = "TPA_UNADJ") |>
    #Drop ACTUALHT
    dplyr::select(!ACTUALHT) |>
    #Drop rows that are not needed in GST
    #Missing measurement year
    #Missing CYCLE
    #Missing DIA
    #Missing EXPF
    #Retain STATUSCD 1 (live) or 2 (dead)
    dplyr::filter(!is.na(MEASYEAR), 
                  !is.na(CYCLE), 
                  !is.na(DIA), 
                  !is.na(EXPF),
                  STATUSCD %in% c(1, 2))

  #=============================================================================
  # Step 4
  # 
  # Align time 1 and time 2 variables together and then merge with other
  # attributes using the merge_inv function.
  #
  # Split tree dataframe into two dataframes
  # fia_merge - data that will be passed into merge_inv function
  # fia_tree -  data that will be merged to fia_merge after merge_inv function has
  #             completed processing.
  #
  #Note: merge_inv is brute force. If not all possible measure periods are
  #needed, the use of lead/lag functions could be used.
  #=============================================================================

  #Obtain variables that will be included in the fia_meas data frame and passed
  #to merge_inv function
  #Deprecate get_gpvars function and just explicitly list variables
  merge_vars <- c(
 "TREEMERGEID", "SPCD", "CYCLE", "MEASYEAR", "MEASMON",
 "MEASDAY", "DIA", "HT", "CR", "STATUSCD",
 "AGENTCD", "DIACHECK", "HTDMP", "DESIGNCD")

  #Merge dataframe
  merge_df <- fia_tree |>
    dplyr::select(dplyr::all_of(c("UNIQUETREEID", "UNIQUESUBPID", merge_vars)))

  #Obtain variables not in fia_meas except for UNIQUETREEID and UNIQUESUBPID.
  #These will be merged to fia_meas after call to merge_inv.
  exclude_vars <- c("UNIQUETREEID", 
                    "UNIQUESUBPID",
                    colnames(fia_tree)[!colnames(fia_tree) %in% merge_vars])

  #Isolate tree level variables not needed in merge_inv function
  fia_tree <- fia_tree |>
    dplyr::select(!dplyr::all_of(merge_vars))
  
  cat("Step 4:", "Pairing remeasurements...")

  #Call merge_inv function
  merge_df <- merge_inv(merge_df,
                        interval = "CYCLE")

  #Extract appropriate columns
  fia_tree <- merge_df |>
    select(!c(UNIQUETREEID.y, UNIQUESUBPID.y, UNIQUESUBPID.x)) |>
    rename(UNIQUETREEID = "UNIQUETREEID.x") |>
    rename_with(~ gsub(".x", "0", .)) |>
    rename_with(~ gsub(".y", "1", .))   |>
    left_join(y = fia_tree,
              by = c("UNIQUETREEID"))
  
  #Remove fia_meas
  rm(merge_df); gc()

  #=============================================================================
  #Step 5
  #
  #Calculate remaining variables that are only possible and/or more convenient
  #to calculate after call to merge_inv functions.
  #=============================================================================
  
  cat("Step 5:", "Calculating final variables...", "\n")

  #Find most recent measurement year for each PLOTQUERYID
  #Determine valid years to retain in data frame
  #All records where MEASYEAR0 < MEASYEAR1 are retained and records from the
  #most recent re-measurement period. The purpose of this function is to keep
  #all possible re-measurements or plots that were only measured once.
  fia_tree <- fia_tree |>
    dplyr::group_by(PLOTMERGEID) |>
    dplyr::mutate(MAXYEAR = max(MEASYEAR1)) |>
    dplyr::group_by(TREEMERGEID) |>
    dplyr::mutate(DIASUM0 = sum(DIACHECK0),
                  DIASUM1 = sum(DIACHECK1)) |>
    dplyr::ungroup() |>
    dplyr::mutate(VALIDYEAR = mapply(valid_year, 
                                     MEASYEAR0, 
                                     MEASYEAR1,
                                     MAXYEAR),
                  MEASLEN = MEASYEAR1 - MEASYEAR0,
                  MORT = mapply(valid_mort, STATUSCD0, STATUSCD1, DIA0, MEASLEN),
                  IDGRM = mapply(valid_incr, DIA0, DIA1, STATUSCD0, STATUSCD1,
                                 MEASLEN),
                  DI = DIA1 - DIA0,
                  IHGRM = mapply(valid_incr, HT0, HT1, STATUSCD0, STATUSCD1,
                                 MEASLEN),
                  HI = HT1 - HT0,
                  HCB0 = HT0 - (HT0 * CR0/100),
                  HCB1 = HT1 - (HT1 * CR1/100),
                  CW0 = NA,
                  CW1 = NA) |>
                  dplyr::filter(VALIDYEAR > 0) |>
    dplyr::select(dplyr::all_of(names(gst_vars)))
    
    return(fia_tree)

  #=============================================================================
  #Rename and reorder column headings and then check if any columns are missing.
  #=============================================================================

  #Capitalize all column headers
  names(fia_tree) <- toupper(names(fia_tree))

  #Check for missing columns in GST definition

  #If there are missing columns stop with an error.
  if(length(missing) > 0)
  {
    stop(paste("The following columns are missing:",
               missing,
               "\n"))
  }

  #=============================================================================
  #Write GST dataframe to output database
  #=============================================================================

  #Get columns defined in GST
  fia_tree <- fia_tree[names(getGSTVars())]

  #Write GST to db_out
  write_gst(gst = fia_tree,
            db_out = db_out)

  invisible()
}

################################################################################
#fia_ht
#
#Description
#
#This function takes in a height code (HTCD), actual height (ACTUALHT), and
#height value (HT) and returns a height value to be used in growth sample tree
#database. HTCD, ACTUALHT, and HT correspond the variables referenced in the
#TREE table of an FIA database.
#
#Source Code
#
#Function fia_ht is currently located in the FIA_GST_Functions.R file.
#
#Argument
#
#htcd:     Value corresponding to height code.
#
#actual_ht: Actual height (length) measurement.
#
#ht:       Total height (length) measurement.
#
#Value
#
#Height value to be used in growth sample tree dataset for FIA data.
################################################################################

#'@export
fia_ht <- function(htcd = NA,
                   actual_ht = NA,
                   ht = NA)
{
  #Initialize height
  height <- NA

  #Set htcd to 1 if htcd is NA...This assumes that height code was not recorded
  #in a given inventory.
  if(is.na(htcd)) htcd <- 1

  #If both actual_ht and ht are NA, return NA
  if((is.na(actual_ht) & is.na(ht))) return(height)

  #If height is field measured and there is an actual height value, use actual
  #height
  if(htcd == 1 & !is.na(actual_ht)) height <- actual_ht

  #If height is field measured and there is a height value but actual height is
  #missing, use height value.
  else if(htcd == 1 & is.na(actual_ht)) height <- ht

  #If ht is visually estimated but there is an actual height measurement
  #(htcd == 2) use actual height value
  else if(htcd ==  2 & !is.na(actual_ht)) height <- actual_ht

  #Everything else should be NA
  else height <- NA

  return(height)
}
