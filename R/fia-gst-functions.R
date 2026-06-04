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
  #Get data from the input FIA database.
  #More tables could be included here as necessary. Probably would be best to
  #process all data at once for state. Thinking we will only want to grab all
  #data from annual inventory and periodic for AK.
  #
  #Have query for TREE, PLOT, COND, SUBPLOT, REF_SPECIES
  #Then query SITETREE
  #Query for database should be stored and used. use of db_select_query function
  #can be deprecated.
  #=============================================================================

  #Connect to db_in
  con <- RSQLite::dbConnect(RSQLite::SQLite(),
                            db_in)

  #Add on exit statement to disconnect database in case of error

  #Query all tables except SITREE
  fia_tree <- RSQLite::dbGetQuery(con,
                                 db_query)

  #Query SITREE
  fia_site <- RSQLite::dbGetQuery(con,
                                 db_query)

  #Disconnect from db_in
  RSQLite::dbDisconnect(con)

  #=============================================================================
  #Summarize site index by plot and then join to fia_tree. Not quite sure how
  #to best handle site index in fitting dataset yet.
  #=============================================================================

  #Calculate site index for each FIA plot (not subplot or condition) by species
  #For now, average of site index observations is taken for each species.
  fia_site_sum <- fia_site |>
    #Drop duplicate site trees that occur by condition
    dplyr::filter(!duplicated(paste(fia_site$STATECD,
                                    fia_site$INVYR,
                                    fia_site$UNITCD,
                                    fia_site$COUNTYCD,
                                    fia_site$PLOT,
                                    fia_site$SUBP,
                                    fia_site$TREE,
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

  #Drop duplicate site trees that occur by condition
  fia_site <- fia_site[!duplicated(paste(fia_site$STATECD,
                                       fia_site$INVYR,
                                       fia_site$UNITCD,
                                       fia_site$COUNTYCD,
                                       fia_site$PLOT,
                                       fia_site$SUBP,
                                       fia_site$TREE,
                                       sep = "_")),]

  fia_siteSum <- fia_site |>
    dplyr::group_by(STATECD,
                    INVYR,
                    UNITCD,
                    COUNTYCD,
                    PLOT,
                    SPCD) |>
    dplyr::summarize(SI = round(mean(SITREE[VALIDCD == 1], na.rm = T),0),
                     SIBASE = round(mean(SIBASE[VALIDCD == 1], na.rm = T),0))

  #Merge site index and site index base age to fia_tree (use the dplyr way)
  fia_tree <- merge(fia_tree,
                   fia_siteSum,
                   by = c("STATECD",
                          "INVYR",
                          "UNITCD",
                          "COUNTYCD",
                          "PLOT",
                          "SPCD"),
                   all.x = T)

  #Cleanup
  rm(fia_site, fia_siteSum)

  #=============================================================================
  #Define set of GST variables prior to merge_inv function.
  #Rename relevant variables in source data to align with variable names in
  #GST_Variables.R
  #Change this code to be in one tidyr block.
  #=============================================================================

  fia_tree <- fia_tree |>
    mutate(DATAPROVIDER = 'FIA',
           UNIQUEPLOTID = paste(fia_tree$STATECD,
                                fia_tree$INVYR,
                                fia_tree$UNITCD,
                                fia_tree$COUNTYCD,
                                fia_tree$PLOT,
                                sep = "_"),
           UNIQUESUBPID <- paste(fia_tree$STATECD,
                                 fia_tree$INVYR,
                                 fia_tree$UNITCD,
                                 fia_tree$COUNTYCD,
                                 fia_tree$PLOT,
                                 fia_tree$SUBP,
                                 sep = "_"),
           UNIQUETREEID <- paste(fia_tree$STATECD,
                                 fia_tree$INVYR,
                                 fia_tree$UNITCD,
                                 fia_tree$COUNTYCD,
                                 fia_tree$PLOT,
                                 fia_tree$SUBP,
                                 fia_tree$TREE,
                                 sep = "_"))

  #=============================================================================
  #Create time one tree attributes variables before merge_inv and drop
  #invalid tree records.
  #=============================================================================

  #Height determination
  fia_tree$MEAS_HT <- mapply(fia_ht,
                            fia_tree$HTCD,
                            fia_tree$ACTUALHT,
                            fia_tree$HT)

  #BT - Broken top indicator
  fia_tree$BT <- ifelse(fia_tree$ACTUALHT < fia_tree$HT, 1,0)

  #If BT is NA assign 0
  fia_tree$BT <- ifelse(is.na(fia_tree$BT), 0, fia_tree$BT)

  #Remove HT, HTCD, and ACTUALHT
  fia_tree <- fia_tree[! colnames(fia_tree) %in% c("HTCD", "HT", "ACTUALHT")]

  #Rename MEAS_HT to HT
  colnames(fia_tree)[names(fia_tree) == 'MEAS_HT'] <- "HT"

  #Fill in missing DIA values with PREVDIA values for dead trees
  #Not sure if this is really needed but doing it for now.
  fia_tree$DIA <- ifelse(is.na(fia_tree$DIA) & fia_tree$STATUSCD == 2,
                        fia_tree$PREVDIA,
                        fia_tree$DIA)

  #If DIACHECK is NA assume it is valid (0). NA DIACHECK values occur in
  #situations where this variable was not recorded in the field.
  fia_tree$DIACHECK <- ifelse(is.na(fia_tree$DIACHECK), 0, fia_tree$DIACHECK)

  #=============================================================================
  #Drop trees that do not have measurement year or cycle number.
  #=============================================================================

  fia_tree <- fia_tree[!is.na(fia_tree$MEASYEAR) &
                     !is.na(fia_tree$CYCLE),]

  #Get trees greater than or equal to start_year and less than or equal to
  #current system year.
  sys_year <- as.integer(sub("-.*", "", Sys.Date()))
  fia_tree <- fia_tree[fia_tree$MEASYEAR >= start_year &
                       fia_tree$MEASYEAR <= sys_year, ]

  #=============================================================================
  #Separate data into the following dataframes
  #
  #fia_meas -  data that will be passed into merge_inv function
  #fia_tree -  data that will be merged to fia_meas after merge_inv
  #           function has completed processing.
  #=============================================================================

  #Create Merge ID for growth periods. This is essentially UNIQUETREEID but
  #without MEASYEAR0 (MEASYEAR) included.
  # fia_tree$MERGE_ID <- mapply(mergeID,
  #                             treeID = fia_tree$UNIQUETREEID,
  #                             dataSource = fia_tree$DATAPROVIDER)
  #
  # cat("MERGE IDs obtained.", "\n", "\n")

  #Create TREEMERGEID - UNIQUETREEID but without INVYR
  fia_tree$TREEMERGEID <- paste(fia_tree$STATECD,
                               fia_tree$UNITCD,
                               fia_tree$COUNTYCD,
                               fia_tree$PLOT,
                               fia_tree$SUBP,
                               fia_tree$TREE,
                               sep = "_")

  #Obtain variables that will be included in the fia_meas data frame and passed
  #to merge_inv function
  #Deprecate get_gpvars function and just explicitly list variables
  growPeriodVars <- get_gpvars()
  fia_meas <- fia_tree[, c(growPeriodVars)]

  #Obtain variables not in fia_meas except for UNIQUETREEID and UNIQUESUBPID.
  #These will be merged to fia_meas after call to merge_inv.
  notInMeas <- c("UNIQUETREEID",
                 "UNIQUESUBPID",
                 colnames(fia_tree)[!colnames(fia_tree) %in%
                                     get_gpvars()])

  #Isolate tree level variables not needed in merge_inv function
  fia_tree <- fia_tree[notInMeas]

  #=============================================================================
  #Call merge_inv function
  #
  #merge_inv function is called to pair beginning and end of measurement
  #interval values defined in fia_meas.
  #=============================================================================

  #Deprecate get_gpvars function and just explicitly list variables

  #Call merge_inv
  fia_meas <- merge_inv(fia_meas)

  #Extract appropriate columns
  fia_meas <- fia_meas[,get_gpvars(2)]

  #Rename columns as appropriate
  names(fia_meas) <- c(get_gpvars(3))

  #Merge fia_tree back to fia_meas
  fia_tree <- merge(fia_meas,
                    fia_tree,
                    by = "UNIQUETREEID",
                    all.x = T)

  #Remove fia_meas
  rm(fia_meas)

  #=============================================================================
  #Determine valid years to retain in data frame
  #All records where MEASYEAR0 < MEASYEAR1 are retained and records from the
  #most recent re-measurement period. The purpose of this function is to keep
  #all possible re-measurements or plots that were only measured once.
  #=============================================================================

  #Find most recent measurement year for each PLOTQUERYID
  fia_tree <- fia_tree |>
    dplyr::group_by(PLOTQUERYID) |>
    dplyr::mutate(MAXYEAR = max(MEASYEAR1)) |>
    #apply validYear with pmap
    dplyr::mutate(VALIDYEAR = validYear(MEASYEAR0, MEASYEAR1, MAXYEAR)) |>
    dplyr::filter(VALIDYEAR > 0) |>
    ungroup()

  #Determine records that have valid years
  # fia_tree$VALIDYEAR <- mapply(validYear,
  #                             fia_tree$MEASYEAR0,
  #                             fia_tree$MEASYEAR1,
  #                             fia_tree$MAXYEAR)

  #Remove records that have invalid measurement year pairings
  # fia_tree <- fia_tree[fia_tree$VALIDYEAR > 0, ]

  #=============================================================================
  #Calculate remaining variables that are only possible and/or more convenient
  #to calculate after call to merge_inv functions.
  #Build this into one tidyr statement.
  #=============================================================================

  #MEASLEN
  fia_tree$MEASLEN <- fia_tree$MEASYEAR1 - fia_tree$MEASYEAR0

  #DIACHG
  #If the sum of DIACHECK0 or DIACHECK1 values is greater than 0 then
  #tree record had its diameter measurement location changed or diameter was
  #estimated during at least one measurement period (DIACHG == 1)
  fia_tree <- fia_tree |>
    dplyr::group_by(TREEMERGEID) |>
    dplyr::mutate(DIASUM0 = sum(DIACHECK0),
                  DIASUM1 = sum(DIACHECK1))

  fia_tree$DIACHG <- ifelse(fia_tree$DIASUM0 > 0 | fia_tree$DIASUM1 > 0, 1, 0)

  #MRTCD
  fia_tree$MRTCD <- mapply(mrtcd,
                          fia_tree$STATUSCD0)

  #MORT
  fia_tree$MORT <- mapply(validMort,
                         fia_tree$MRTCD,
                         fia_tree$STATUSCD0,
                         fia_tree$STATUSCD1,
                         fia_tree$DIA0,
                         fia_tree$MEASLEN)

  #INGROW
  # fia_tree$INGROW <- mapply(validIngrow,
  #                          fia_tree$DIA0,
  #                          fia_tree$DIA1,
  #                          fia_tree$STATUSCD1)

  #IDGRM
  fia_tree$IDGRM <- mapply(validIncr,
                          fia_tree$DIA0,
                          fia_tree$DIA1,
                          fia_tree$STATUSCD0,
                          fia_tree$STATUSCD1,
                          fia_tree$MEASLEN)

  #DI
  fia_tree$DI <- ifelse(fia_tree$IDGRM == 1, fia_tree$DIA1 - fia_tree$DIA0, NA)

  #IHGRM
  fia_tree$IHGRM <- mapply(validIncr,
                          fia_tree$HT0,
                          fia_tree$HT1,
                          fia_tree$STATUSCD0,
                          fia_tree$STATUSCD1,
                          fia_tree$MEASLEN)

  #HI
  fia_tree$HI <- ifelse(fia_tree$IHGRM == 1, fia_tree$HT1 - fia_tree$HT0, NA)

  #HCB0
  fia_tree$HCB0 <- fia_tree$HT0 - (fia_tree$HT0 * fia_tree$CR0/100)

  #HCB1
  fia_tree$HCB1 <- fia_tree$HT1 - (fia_tree$HT1 * fia_tree$CR1/100)

  #CW0
  fia_tree$CW0 <- NA

  #CW1
  fia_tree$CW1 <- NA

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
