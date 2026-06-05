################################################################################
#build_gst
#
#Description
#
#This function processes a set of sqlite databases and creates a standardized
#output growth sample tree database than be used for fitting equation
#development (GST). Currently this function is only equipped to build GST
#databases from FIA data. This function calls build_fia which subsequently calls
#fiaGST.
#
#Source Code
#
#Function build_gst is currently located in the GST_Prep_Functions.R file.
#
#Arguments
#
#db_in:	    Character vector of directory paths and file name to sqlite database
#           (.db or .sqlite).
#
#dbout:	    Character vector of directory path and file name to output sqlite
#           GST database (.db or .sqlite).
#
#append:    Logical variable used to determine if new information should be
#           appended to existing dbout file. By default, this argument is set to
#           FALSE (F). Both append and overwrite cannot be set to either TRUE or
#           FALSE when file specified in dbout exists.
#
#overwrite: Logical variable used to determine if currently existing dbout file
#           should be deleted. By default this argument is set to FALSE (F).
#           Both append and overwrite cannot be set to either TRUE or FALSE when
#           file specified in dbout exists.
#
#           NOTE: Arguments append and overwrite are only checked once per call
#           to build_gst function.
#
#start_year: Calendar year used to specify what data will be included in output
#           GST. Trees inventoried before this year will not be included in
#           output GST. By default, this value is set to 0 (all data will be
#           included).
#
#by_plot:    Logical variable indicating if data from db_in should be processed
#           one plot at a time. If this argument is not TRUE, then an entire
#           database from db_in will be processed at one time. This can use a LOT
#           of memory in R. By default, this argument is set to TRUE (T).
#
#Value
#
#Integer value of 0 returned invisibly.
################################################################################

#'@export
build_gst <- function(db_in = c(),
                      dbout = NA,
                      append = F,
                      overwrite = F,
                      start_year = 0,
                      by_plot = T,
                      n_process = 1)
{

  #If db_in or dbout are missing, stop with error message
  if(length(db_in) <= 0 | is.na(dbout))
  {
    stop("No files specified in db_in or dbout.")
  }

  #=============================================================================
  #Check if db_in files exist before starting processing
  #=============================================================================

  missing_file <- checkFiles(db_in)
  if(missing_file > 0)
    stop(paste(db_in[missing_file], "does not exist.", "\n"))

  #=============================================================================
  #Check if all file paths in db_in are valid
  #=============================================================================

  invalid_ext <- checkFileExts(files = db_in,
                               exts = c("db", "sqlite"))

  if(invalid_ext > 0)
    stop(paste(db_in[invalid_ext], "is not a sqlite database.", "\n"))

  #=============================================================================
  #Do checks on dbout argument
  #=============================================================================

  #Change \\ to / in dbout argument
  dbout <- gsub("\\\\", "/", dbout)

  #Extract path to dbout by extracting all characters before the last / in
  #dbout.
  out_path <- gsub("/[^/]+$", "", dbout)

  #Test existence of dbout path and if it does not exist report error.
  if (!(file.exists(out_path))){
    stop(paste("Path to dbout:", out_path, "was not found.",
               "Make sure directory path to dbout is spelled correctly."))
  }

  #Extract file extension for dbout argument.
  fileext_out <- getFileExt(dbout)

  #Test if dbout file extension is valid (.db or .sqlite).
  if(!fileext_out %in% c("db", "sqlite"))
  {
    stop(paste("dbout argument does not have a valid file extension. File",
               "extension must be .db or .sqlite."))
  }

  #=============================================================================
  #Do checks on append and overwrite arguments
  #=============================================================================

  #If dbout already exists and append and overwrite are F, stop with error
  #message
  if(file.exists(dbout) & (!append & !overwrite))
  {
    stop(paste("dbout exists and both append and overwrite are FALSE. One of",
               "these must be set to TRUE."))
  }

  #If dbout already exists and append and overwrite are T, stop with error
  #message
  if(file.exists(dbout) & (append & overwrite))
  {
    stop(paste("dbout exists and both append and overwrite are TRUE. One of",
               "these must be set to FALSE."))
  }

  #If overwrite is TRUE, delete dbout
  if(overwrite)
  {
    unlink(dbout)
  }

  #=============================================================================
  #Start loop across databases in db_in
  #=============================================================================

  for(i in 1:length(db_in))
  {
    db <- db_in[i]
    cat("Processing:", db, "\n", "\n")

    #===========================================================================
    #Call build function specific to data provider. Currently only FIA data is
    #supported but this section can be updated to accommodate other data sources
    #and providers.
    #===========================================================================

    cat("Calling build_fia", "\n", "\n")

    build_fia(db_in = db,
              dbout = dbout,
              start_year = start_year,
              by_plot = by_plot,
              n_process = n_process)

    cat("Finished processing:", db, "\n", "\n")
  }

  invisible(0)
}

################################################################################
#merge_inv:
#
#Description
#
#This function accepts a data frame containing the variables from
#getGrowthPeriodVars(type = 1) function or the competition and density variables
#written to a GST database by the plotVars function (type != 1). The function
#will pair tree level attributes or competition and density variables from
#remeasurement periods defined for a plot_id in the input data frame.
#
#Source Code
#
#Function merge_inv is currently located in the GST_Prep_Functions.R file.
#
#Arguments
#
#data:     Data frame containing columns determined by getGrowthPeriodVars
#          function.
#
#plot_id:   Character string of column name used to represent a unique plot ID.
#          By default, UNIQUESUBPID is the default value.
#
#interval: Character string of column name used to specify a measurement year.
#          This variable is used to pair re-measurement observations. "DTCIN"
#          is the default value and corresponds to measurement year.
#
#merge_id:  Character string of column name in data argument used to merge
#          re-measurement periods together. By default this argument is set to
#          "TREEMERGEID".
#
#species:  Character string of column name used to represent FIA species code
#          in data argument. By default, this argument is set to "SPECIES".
#
#tree_id:  Character string of column name used to represent a unique tree ID.
#          By default, "UNIQUETREEID" is the default value.
#
#          NOTE: The primary purpose of the plot_id, interval, merge_id, dbh,
#          species, tree_id columns is to avoid hardcoding of variable names in
#          the merge_inv function. If the variable names from a GST used
#          in this function change, the references to the new variable names can
#          be changed more easily by altering the values in these arguments.
#
#Value
#
#Data frame with paired remeasurement periods.
################################################################################

merge_inv<-function(data,
                    plot_id = "UNIQUESUBPID",
                    tree_id = "UNIQUETREEID",
                    merge_id = "TREEMERGEID",
                    interval = "MEASYEAR",
                    species = "SPECIES")
{
  #Upper case interval name
  interval <- toupper(interval)

  #Change name of interval variable to INTERVAL in data
  names(data)[names(data) == interval] <- "INTERVAL"

  #Get unique INTERVAL values and sort
  intervals <- sort(unique(data$INTERVAL))

  #Define list used to store merged dataframes
  df_list<-vector(mode = "list", length = length(intervals)^2)

  #Intialize variable that will be used to track number of insertions into
  #df_list
  list_insert<-1

  cat("ID to be used for merging:",
      merge_id,
      "\n",
      "\n")

  #Start outer loop across intervals. Each interval in the outer loop will be
  #merged with the interval in inner loop if it is less than or equal to
  #interval in the inner loop. In other words, this is used to test if initial
  #inventory year is less than or equal to subsequent inventory year.
  for(i in 1:length(intervals))
  {
    for(j in 1:length(intervals))
    {
      #Extract dataframes from index i and j
      interval1 <- intervals[i]
      interval2 <- intervals[j]

      #If interval1 is greater than interval2 then data will not be merged.
      if(interval1 > interval2) next

      #Else data from interval2  will be merged to data from interval1
      else
      {
        #Extract data corresponding to interval1 and interval2
        df1 <- data[data$INTERVAL == interval1,]
        df2 <- data[data$INTERVAL == interval2,]

        #Merge dataframes df1 and df2 by merge_id and SPCD
        df <- merge(df1,
                     df2,
                     by = c(merge_id, species),
                     all = T)

        #Delete temporary dataframe
        rm(df1, df2)

        #Identify plots where there are no re-measurements for a given
        #interval1 - interval2 combination. This is done by summing the
        #INTERVAL.y by UNIQUESUBPID.x. Plots with no re-measurements will have
        #a value of 0 for the sum of INTERVAL.y (SUM_Y).
        df <- df |>
          dplyr::group_by(.data[[paste0(plot_id, ".x")]]) |>
          dplyr::mutate(SUM_Y = sum(INTERVAL.y, na.rm = T))

        #Identify plots where there are no initial measurements for a given
        #interval1 - interval2 combination. This is done by summing the
        #INTERVAL.x by UNIQUESUBPID.y. Plots with no initial measurements will
        #have a value of 0 for the sum of INTERVAL.x (SUM_X).
        df <- df |>
          dplyr::group_by(.data[[paste0(plot_id, ".y")]]) |>
          dplyr::mutate(SUM_X = sum(INTERVAL.x, na.rm = T)) |>
          dplyr::ungroup()

        #Extract observations that have COUNT_X and COUNT_Y > 0
        df <- df |>
          dplyr::filter(df$SUM_Y > 0, df$SUM_X > 0) |>
          dplyr::select(!any_of(SUM_Y, SUM_X))

        #If tree_id.x is NA, use the value from tree_id.y. These are ingrowth,
        #missed trees, or those not accounted for due to change in DESIGNCD
        #between initial (interval1) and subsequent inventory (interval2). This
        #is only done when type == 1.
        #When UNIQUETREEID.x is missing use UNIQUETREEID.y
        df[[paste0(tree_id, ".x")]] <- 
          ifelse(is.na(df[[paste0(tree_id, ".x")]]),
        df[[paste0(tree_id, ".y")]],
        df[[paste0(tree_id, ".x")]])

        #Fill in any NA values for INTERVAL.x and INTERVAL.y
        df$INTERVAL.x <- ifelse(is.na(df$INTERVAL.x), interval1,
                                 df$INTERVAL.x)

        df$INTERVAL.y <- ifelse(is.na(df$INTERVAL.y), interval2,
                                 df$INTERVAL.y)

        #Add dataframe to list
        df_list[[list_insert]] <- df

        #Increment list_insert
        list_insert<- list_insert + 1

        #Clear df
        rm(df); gc()
      }
    }
  }

  #Bind all items in df_list into a single dataframe (all_dat)
  all_dat <- do.call("rbind", df_list)

  #Rename INTERVAL.x and INTETVAL.y to value specified in interval argument if
  #all_dat is not NULL.
  if(!is.null(all_dat))
  {
    #Reset INTERVAL.x and .y column names
    names(all_dat)[names(all_dat) == 'INTERVAL.x'] <- paste0(interval, ".x")
    names(all_dat)[names(all_dat) == 'INTERVAL.y'] <- paste0(interval, ".y")

    cat("Columns in all_dat:", names(all_dat), "\n")
    cat("Number of rows in all_dat dataframe:", nrow(all_dat), "\n")
  }

  cat("Leaving merge_inv function.", "\n", "\n")
  return(all_dat)
}

################################################################################
#valid_mort
#
#Description
#
#This function takes in a mortality code, status code corresponding to beginning
#of inventory period, and status code corresponding to end of inventory period
#for a tree record and returns an indicator specify whether a tree is a valid
#survival (0) or mortality observation (1). Function currently assumes that
#status code 1 means alive and 2 means dead.
#
#Source Code
#
#Function valid_mort is currently located in the GST_Prep_Functions.R file.
#
#Arguments
#
#mrtcd:  Mortality code as determined by mrtcd function.
#
#stat0:  Status code (live, dead, cut) for an initial inventory period.
#
#stat1:  Status code (live, dead, cut) for a subsequent inventory period.
#
#cyclen: Integer variable corresponding to the length of measurement interval
#        in years.
#
#Value
#
#Integer value or NA value specifying whether tree is survival or mortality
#observation.
#0 = survival
#1 = mortality
################################################################################

#'@export
valid_mort <- function(mrtcd = NA,
                       stat0 = NA,
                       stat1 = NA,
                       dbh0 = NA,
                       cyclen = 0)
{
  #Initialize mort
  mort <- NA

  #If any of the input arguments are NA or mrtcd == 1, return NA
  if(NA %in% c(mrtcd, stat0, stat1, dbh0, cyclen) | mrtcd == 1)
    return(NA)

  #If tree is live at beginning of inventory and dead at end, this is a
  #mortality observation
  else if((stat0 == 1 & stat1 == 2) & cyclen > 0) mort = 1

  #If tree is live at beginning and end of inventory and dead at end, this is a
  #survival observation
  else if((stat0 == 1 & stat1 == 1) & cyclen > 0) mort = 0

  #Everything else is NA
  else mort <- NA

  return(mort)
}

################################################################################
#validIngrow
#
#FUNCTION IS NOT CURRENTLY IN USE.
#
#Description
#
#This function takes in a DBH value corresponding to beginning of inventory
#period, DBH value corresponding to end of inventory period, and status code at
#end of period for a tree record and returns an indicator specifying whether a
#tree is a valid ingrowth observation. Function currently assumes that status
#code 1 means alive and 2 means dead.
#
#Source Code
#
#Function validIngrow is currently located in the GST_Prep_Functions.R file.
#
#Arguments
#
#dbh0:  DBH at beginning of inventory period
#
#dbh1:  DBH at end of inventory period
#
#stat1: Status code at end of inventory period
#
#Value
#
#Integer value or NA value specifying whether tree is ingrowth observation.
#0 = not ingrowth
#1 = ingrowth
################################################################################

# validIngrow <- function(dbh0 = NA,
#                         dbh1 = NA,
#                         stat1 = NA)
# {
#   validIng <- 0
#
#   #Get mortality code
#   mort <- mrtcd(stat1)
#
#   #If dbh0 is NA, dbh1 is not NA, and mort is 0 this is ingrowth
#   if(is.na(dbh0) & !is.na(dbh1) & mort == 0)
#   {
#     validIng <- 1
#   }
#
#   else
#   {
#     validIng <- 0
#   }
#
#   return(validIng)
# }

################################################################################
#mrtcd
#
#Description
#
#This function takes in a status code corresponding to the beginning of an
#inventory period for a tree record and returns a mortality code. This code is
#used to determine if tree record is a valid survival or mortality observation
#(see valid_mort function) and to filter out initially dead or cut trees in
#derivation of plot level variables. This variable may seem redundant but it is
#helpful for data sources that have a wide variety of status codes and need to
#be preserved in GST. Function currently assumes that a status code of 2 and 3
#means dead or cut respectively.
#
#Argument:
#
#stat0:    Status code (live, dead, cut) at beginning of inventory period
#
#Value
#
#Mortality code
#0 = live at initial inventory.
#1 = dead or cut at initial inventory.
################################################################################

#'@export
mrtcd <- function(stat0 = NA)
{
  mrtcd <- NA

  #if stat0 is NA, return mrtcd
  if(is.na(stat0)) mrtcd <- NA

  #If tree is dead or cut, set mrtcd to 1. This condition can be adjusted to
  #accommodate dead or cut tree codes from other data providers.
  else if(stat0 %in% c(2, 3)) mrtcd <- 1

  #Else mrtcd is 0
  else mrtcd <- 0

  return(mrtcd)
}

################################################################################
#valid_incr
#
#Description
#
#This function takes a DBH/HT value at an initial inventory period, DBH/Height
#value at a subsequent inventory period, status code at an initial inventory
#period, and status code at a subsequent inventory period for a tree record
#and returns an indicator variable signifying if diameter/height growth
#observation is valid. Function currently assumes that a status code of 1 means
#tree is alive.
#
#Source Code
#
#Function valid_incr is currently located in the GST_Prep_Functions.R file.
#
#
#Arguments
#
#meas0:  Diameter/height for an initial inventory period.
#
#meas1:  Diameter/height for a subsequent inventory period.
#
#stat0:  Status code (live, dead, cut) for an initial inventory period.
#
#stat1:  Status code (live, dead, cut) for a subsequent inventory period.
#
#cyclen: Integer variable corresponding to the length of measurement interval
#        in years.
#Value
#
#Indicator variable indicating if increment observation is valid.
#0 = valid
#1 = invalid
################################################################################

#'@export
valid_incr <- function(meas0 = NA,
                      meas1 = NA,
                      stat0 = NA,
                      stat1 = NA,
                      cyclen = 0)
{
  #Initialize return value
  valid <- 0

  #If any arguments are NA, valid is 0
  if(NA %in% c(meas0, meas1, stat0, stat1, cyclen)) valid <- 0

  #If end of period value is greater than or equal to beginning of year value
  #and tree is alive at both points, this is a valid increment observation
  else if(meas1 >= meas0 & (stat0 == 1 & stat1 == 1) & cyclen > 0) valid <- 1

  else valid <- 0

  return(valid)
}

################################################################################
#write_gst
#
#Description
#
#This function is used to write data to a database table within a GST database.
#This function is currently called from fiaGST and plotVars.
#
#Source Code
#
#Function write_gst is currently located in the GST_Prep_Functions.R file.
#
#Arguments
#
#gst:        Data being sent to GST database specified in dbout argument. This
#            could be data from build_gst or plotVars function.
#
#dbout:      Directory path and file name to output SQLite database.
#
#dbout_table: Name of database table from value in dbout argument where data in
#            gst argument will be stored.
#
#type:       Integer variable indicating what type of data is being sent to
#            dbout.
#            1 = GST variables defined in getGSTVar function.
#            2 = Plot level variables defined in plotVars function.
#
#Value
#
#Integer value of 0 returned invisibly.
################################################################################

write_gst <- function(gst,
                      dbout,
                      dbout_table = "GST",
                      type = 1)
{
  #Get variable names and data types based on type argument (GST versus plot)
  if(type == 1)
  {
    var_types <- getGSTVars()
  }
  else if(type == 2)
  {
    var_types <- getPlotVars(type = 1)
  }
  else
  {
    var_types <- getPlotVars(type = 2)
  }

  #Connect to the database
  con <- RSQLite::dbConnect(RSQLite::SQLite(),
                            dbout)

  #Check if number of columns are the same in gst and dbout provided dbout_table
  #exists.
  if(dbout_table %in% RSQLite::dbListTables(con))
  {
    gst_fields <- names(gst)
    db_fields <- RSQLite::dbListFields(conn = con,
                                      name = dbout_table)

    if(length(gst_fields) < length(db_fields))
    {
      missing <- db_fields[! db_fields %in% gst_fields]
      RSQLite::dbDisconnect(con)
      stop(paste("The following columns are missing from GST dataframe:",
                 paste(missing, collapse = ", "),
                 "\n"))
      RSQLite::dbDisconnect(con)
    }

    if(length(gst_fields) > length(db_fields))
    {
      missing <- gst_fields[! gst_fields %in% db_fields]
      RSQLite::dbDisconnect(con)
      stop(paste("The following columns are missing from GST database:",
                 dbout,
                 "\n",
                 paste(missing, collapse = ", "),
                 "\n"))
    }
  }

  #If dbout_table does exist in database, append data to table
  if(RSQLite::dbExistsTable(con,
                            dbout_table))
  {
    RSQLite::dbWriteTable(conn = con,
                          name = dbout_table,
                          value = gst,
                          append = T)
  }

  #Otherwise write data.
  else
  {
    RSQLite::dbWriteTable(conn = con,
                          name = dbout_table,
                          value = gst,
                          field.types = var_types)
  }

  RSQLite::dbDisconnect(con)
  cat("GST data written to:", dbout, "\n")

  invisible(0)
}

################################################################################
#valid_year
#
#Description
#
#This function is a helper function for selectYear. This function flags
#measurement year combinations as being valid or invalid.

#0 = Invalid
#    Records where year1 == year2 & (year1 < max_year | year2 < max_year)
#
#1 = valid
#    Records where year1 == year2 & (year1 == max_year & year2 == max_year)
#    Records where year1 < year2

#Arguments
#
#year1:   Numeric value corresponding to current measurement year.
#
#year2:   Numeric value corresponding to subsequent measurement year.
#
#max_year: Numeric value corresponding to most recent measurement year.
#
#Value
#
#0 or 1 integer value.
################################################################################

#'@export
valid_year <- function(year1, year2, max_year)
{
  #If year1 < year 2, valid
  if(year1 < year2) valid <- 1

  #If year1 == year2 & year1 & year2 == max_year valid
  else if(year1 == year2 & (year1 == max_year & year2 == max_year)) valid <- 1

  #Invalid
  else valid <- 0

  return(valid)
}
