################################################################################
#' @name build_fitdb
#' @title Build standardized equation fitting database (FITDB)
#' @description This function processes a set of sqlite databases and creates a
#' standardized output database than be used for fitting equation development 
#' (FITDB). Currently this function is only equipped to build FITDB databases 
#' from FIA data.
#' 
#' @param dbin
#' Character vector of file paths to sqlite database (.db or .sqlite). 
#' Defaults to NULL.
#' 
#' @param dbout
#' Character string corresponding to output sqlite FITDB database (.db or .sqlite). 
#' Defaults to NULL.
#' 
#' @param fitdb_name
#' Character string corresponding to name of database table written to dbout 
#' argument. Defaults to "FITDB".
#' 
#' @param fitdb_type
#' Numeric value corresponding to type of GST database to create.
#' 
#' 1 = FITDB built from FIA data
#' 
#' Defaults to 1.
#' 
#' @param overwrite
#' Logical variable used to determine if currently existing dbout file should be 
#' deleted. If this argument is left as FALSE, data will be appended to existing 
#' file specified in dbout. Defaults to FALSE.
#' 
#' @param verbose
#' Logical variable used to determine if progress milestones and logging messages 
#' are printed to the console. Defaults to FALSE.
#' 
#' @return
#' None
#' @export
################################################################################

build_fitdb <- function(dbin = NULL,
                        dbout = NULL,
                        fitdb_name = "FITDB",
                        fitdb_type = 1,
                        overwrite = FALSE,
                        verbose = FALSE)
{

  #=============================================================================
  #Check if values were specified for dbin and dbout
  #=============================================================================

  #If dbin or dbout are missing, stop with error message
  if(is.null(dbin))
    stop("No files specified for dbin argument.")

  if(is.null(dbout))
    stop("No files specified for dbout argument.")

  #=============================================================================
  #Checks on dbin arguments.
  #=============================================================================

  #Change \\ to / in dbout argument
  dbin <- chartr("\\", "/", dbin)

  #Check if files do not exist
  if(any(!file.exists(dbin), na.rm = TRUE))
    stop("One or more files in dbin do not exist.")

  #Check if any files do not have valid extensions
  if(any(! tools::file_ext(dbin) %in% c("db", "sqlite"), na.rm = TRUE))
    stop("One or more files in dbin is not a valid database type.")

  #=============================================================================
  #Do checks on dbout argument
  #=============================================================================

  #Change \\ to / in dbout argument
  dbout <- chartr("\\", "/", dbout)[1]

  #Extract path to dbout by extracting all characters before the last / in
  #dbout.
  dbout_path <- gsub("/[^/]+$", "", dbout)

  #Test existence of dbout path and if it does not exist report error.
  if (!(file.exists(dbout_path)))
    stop(paste("Path to dbout:", dbout_path, "does not exist.",
               "Make sure value in dbout is spelled correctly."))

  #Test if dbout file extension is valid
  if(!tools::file_ext(dbout) %in% c("db", "sqlite"))
    stop("dbout argument is not a valid database type.")

  #=============================================================================
  #Do checks on other arugments
  #=============================================================================

  #Catch bad overwrite values
  if(! overwrite %in% c(TRUE, FALSE)) overwrite <- FALSE
  
  #Catch bad fitdb values
  if(is.na(fitdb_name) || is.null(fitdb_name) || !is.character(fitdb_name)) 
    fitdb_name <- "FITDB"
  
  #Catch bad fitdb type values
  if(! fitdb_type %in% 1) fitdb_type <- 1

  #=============================================================================
  #Process values in dbin
  #=============================================================================

  #If overwrite is TRUE, delete dbout
  if(overwrite)
    unlink(dbout)

  for(i in 1:length(dbin))
  {
    db <- dbin[i]
    if(verbose) cat("Processing:", db, "\n", "\n")

    #===========================================================================
    #Call function for building FITDB database. Currently only FIA data is
    #supported but this section can be updated to accommodate other data
    #sources
    #===========================================================================

    if(fitdb_type == 1)
    {
      if(verbose) cat("Calling fia_fitdb", "\n", "\n")
      
      fia_fitdb(dbin = db,
              dbout = dbout,
              fitdb_name = fitdb_name,
              verbose = verbose)
    }

    if(verbose)
      cat("Finished processing:", db, "\n", "\n")
  }

  invisible()
}

################################################################################
#' merge_inv
#' @name merge_inv
#' @description This function accepts a list of dataframes containing information 
#' that will be paired by a measurement interval (typically a cycle or year). 
#' This function is called from the code that prepares a fitting database 
#' specific to a data source (e.g. fia_fitdb).
#' 
#' @param data
#' Data table (from data.table package) containing variables that should be 
#' paired based on a measurement interval.
#' 
#' @param unique_id
#' Character string of column name used to represent a unique tree ID. 
#' Defaults to "UNIQUETREEID".
#' 
#' @param plot_id
#' Character string of column name used to represent a unique plot ID. 
#' Defaults to "PLOTMERGEID".
#' 
#' @param merge_id
#' Character string of column name used to merge re-measurement periods together 
#' at the tree level. Defaults to "TREEMERGEID".
#' 
#' @param interval_id
#' Character string of column name used to specify a measurement year or cycle. 
#' This variable is used to pair re-measurement observations. 
#' Defaults to "CYCLE".
#' 
#' @param verbose
#' Logical variable used to determine if tracking milestones and processing logs 
#' are printed to the console. Defaults to TRUE.
#' 
#' @return
#' Data table with paired remeasurement data.
#' @export
################################################################################

merge_inv <- function(data, 
                      unique_id = "UNIQUETREEID",
                      plot_id = "PLOTMERGEID", 
                      merge_id = "TREEMERGEID", 
                      interval_id = "CYCLE", 
                      verbose = TRUE)
{

  #Sort the data
  data.table::setorderv(x = data, cols = c(plot_id, merge_id, interval_id))
  
  #Create column for next consecutive cycle
  #data[, next_cycle_match := get(interval_id) + 1]
  
  # Create column for next consecutive cycle using shift logic
  data[, next_cycle_match := data.table::shift(get(interval_id), type = "lead"), 
     by = c(plot_id, merge_id)]
  
  #Do join
  on_cols <- c(plot_id, merge_id, paste0(interval_id, " == next_cycle_match"))
  
  df <- data[data, 
             on = on_cols,
             nomatch = NA]
  
  #Correct cycle 2 values
  df[is.na(next_cycle_match), (interval_id) := NA]
  
  #Drop columns
  cols_to_drop <- intersect(c(unique_id, 
                              plot_id, 
                              "next_cycle_match", 
                              "i.next_cycle_match"), names(df))
  df[, (cols_to_drop) := NULL]
  
  #Rename time 1 unique id (.i)
  data.table::setnames(df, old = paste0("i.", unique_id), new = unique_id)
  
  #Create time 1 columns. Strip "i." prefix and append "1".
  i_cols <- names(df)[grepl("^i\\.", names(df))]
  t1_names <- paste0(sub("^i\\.", "", i_cols), "1")
  data.table::setnames(df, i_cols, t1_names)
  
  #Create time 2 columns
  cols_to_rename <- names(df)[!names(df) %in% 
                                c(plot_id, merge_id, unique_id, t1_names)]
  t2_names <- paste0(cols_to_rename, "2")
  data.table::setnames(df, cols_to_rename, t2_names)
  
  #Drop the temporary interval column
  data[, next_cycle_match := NULL]
  
  return(df)
}

# merge_inv <- function(data, 
#                       unique_id = "UNIQUETREEID",
#                       plot_id = "PLOTMERGEID", 
#                       merge_id = "TREEMERGEID", 
#                       interval_id = "CYCLE", 
#                       verbose = TRUE)
# {
#   
#   #Add copy of interval_id to data. This is done because data.table does not 
#   #create time1 and time 2 interval_id variables in non-equi join.
#   tmp_interval <- "_interval_id_"
#   data[, (tmp_interval) := get(interval_id)]
#   
#   #Sort the data
#   data.table::setorderv(x = data, cols = c(plot_id, merge_id, interval_id))
#   
#   # Define the non-equi join conditions dynamically
#   # This will act as a left join
#   on_cols <- c(plot_id, merge_id, paste0(interval_id, " > ", interval_id))
#   
#   # Perform the self-join
#   df <- data[data, 
#              on = on_cols, 
#              nomatch = NA, 
#              allow.cartesian = TRUE]
#   
#   # Clean up missing values for trees that existed at time 1 but not time 2
#   int_lab_y <- paste0(interval_id)
#   int_lab_x <- paste0("i.", interval_id)
#   
#   df[is.na(get(int_lab_y)), (int_lab_y) := get(int_lab_x)] 
#   
#   #Drop columns that are no longer needed
#   df[, c(unique_id, plot_id, interval_id) := NULL]
#   
#   #Rename time 1 unique id (.i)
#   data.table::setnames(df, old = paste0("i.", unique_id), new = unique_id)
#   
#   # Rename i. columns (time 1): strip "i." prefix and append "1"
#   i_cols <- names(df)[grepl("^i\\.", names(df))]
#   t1_names <- paste0(sub("^i\\.", "", i_cols), "1")
#   data.table::setnames(df, i_cols, t1_names)
#   
#   # Rename other columns (except plot_id, merge_id, unique_id, AND the renamed i_cols): append "2"
#   cols_to_rename <- names(df)[!names(df) %in% c(plot_id, merge_id, unique_id, t1_names)]
#   t2_names <- paste0(cols_to_rename, "2")
#   data.table::setnames(df, cols_to_rename, t2_names)
#   
#   #Rename the temp intervals to what was carried in interval_id initially
#   data.table::setnames(df, paste0(tmp_interval, "1"), paste0(interval_id, "1"))
#   data.table::setnames(df, paste0(tmp_interval, "2"), paste0(interval_id, "2"))
#   
#   return(df)
# }

################################################################################
#' @name write_fitdb
#' @title Write Fitting Dataset to SQLite Database
#' @description This function is used to write a data table or data.frame 
#' containing a fitting dataset to a specified output SQLite database.
#' 
#' @param fitdb
#' Dataframe that will be written to fitdb database specified in dbout argument.
#' 
#' @param dbout
#' Character string corresponding to file path of output SQLite database.
#' 
#' @param fitdb_name
#' Name of database table that will contain the fitting dataset information in 
#' dbout argument. Defaults to "FITDB".
#' 
#' @return
#' None
#' @export
################################################################################

write_fitdb <- function(fitdb,
                        dbout,
                        fitdb_name = "FITDB")
{

  #Connect to the database
  con <- RSQLite::dbConnect(RSQLite::SQLite(),
                            dbout)
  
  on.exit(
  expr = try(if(RSQLite::dbIsValid(con)) RSQLite::dbDisconnect(con),
  silent = TRUE))

  #Get GST fields and data types
  fitdb_fields <- names(fitdb_vars())
  fitdb_field_types <- fitdb_vars()

  #Handling for when output database table already exists
  if(RSQLite::dbExistsTable(con, name = fitdb_name))
  {
    db_fields <- RSQLite::dbListFields(conn = con,
                                       name = fitdb_name)

    if(length(fitdb_fields) < length(db_fields))
    {
      missing <- db_fields[! db_fields %in% fitdb_fields]
      stop(paste("The following columns are missing from GST dataframe:",
                 paste(missing, collapse = ", "),
                 "\n"))
    }

    if(length(fitdb_fields) > length(db_fields))
    {
      missing <- fitdb_fields[! fitdb_fields %in% db_fields]
      stop(paste("The following columns are missing from GST database:",
                 dbout,
                 "\n",
                 paste(missing, collapse = ", "),
                 "\n"))
    }

    #Append data to existing table
    RSQLite::dbWriteTable(conn = con,
                          name = fitdb_name,
                          value = fitdb,
                          append = T)
  }

  #Otherwise create output database table
  else
  {
    RSQLite::dbWriteTable(conn = con,
                          name = fitdb_name,
                          value = fitdb,
                          field.types = fitdb_field_types)
  }

  RSQLite::dbDisconnect(con)
  invisible()
}

### Archived dplyr version

# ################################################################################
# #'merge_inv
# #'@name merge_inv
# #'@description
# #' 
# #'This function accepts a list of dataframes containing containing information
# #'that will be paired by a measurement interval (cycle or year). This function 
# #'is called from the code that prepares a growth sample tree database (e.g.
# #'fia_fitdb).
# #
# #'@param data:
# #'List of dataframes for each unique value of interval argument
# #
# #'@param plot_id:   
# #'Character string of column name used to represent a unique plot ID.
# #
# #'@param tree_id:  
# #' Character string of column name used to represent a unique tree ID.
# #
# #'@param interval:  
# #'Character string of column name used to specify a measurement year. This 
# #'variable is used to pair re-measurement observations. 
# #
# #'@param merge_id:
# #'Character string of column name used to merge re-measurement periods together.
# #
# #'@param species:
# #'Character string of column name that contains species codes in dataframes 
# #'within data argument. This argument is used in the merging of remeasurement
# #'data.
# #
# #'@return
# #'Data frame with paired remeasurement data.
# ################################################################################

######################################
#Dplyr Join version of merge_inv
######################################

# merge_inv <- function(data,
#                       plot_id = "PLOTMERGEID",
#                       tree_id = "UNIQUETREEID",
#                       merge_id = "TREEMERGEID",
#                       interval = "CYCLE",
#                       #species = "SPCD",
#                       verbose = TRUE)
# {
#   if(verbose) cat("\n", "Entering merge_inv function.", "\n", "\n")

#   #Get unique INTERVAL values and sort
#   intervals <- sort(as.numeric(unique(names(data))))
#   if(verbose) cat("Intervals considered: ", paste(intervals, collapse = ", "), "\n")

#   #Define list used to store merged dataframes
#   df_list <-vector(mode = "list", length = choose(length(intervals), 2))

#   #Setup labels
#   tree_lab_x = paste0(tree_id, ".x")
#   tree_lab_y = paste0(tree_id, ".y")
#   int_lab_x = paste0(interval, ".x")
#   int_lab_y = paste0(interval, ".y")

#   #Intialize variable that will be used to track number of insertions into
#   #df_list
#   n_insert <- 1

#   #Start outer loop across intervals. Combine remeasurements when criteria is 
#   #met
#   for(interval1 in intervals)
#   {
#     for(interval2 in intervals)
#     {
#       #Criteria for merging is met
#       if(interval2 > interval1)
#       {
#         if(verbose)
#         {
#           cat("Interval 1:", interval1, "\n")
#           cat("Interval 2:", interval2, "\n")
#           cat("Merging remeasurements", "\n", "\n")
#         }

#         #Get dataframes associated with interval
#         x = data[[as.character(interval1)]]
#         y = data[[as.character(interval2)]]

#         #Find plots that exist in both x and y
#         match_plot = intersect(x[[plot_id]], y[[plot_id]])

#         #Get only plots that exist at both points in time
#         x = x |> dplyr::filter(.data[[plot_id]] %in% match_plot)
#         y = y |> dplyr::filter(.data[[plot_id]] %in% match_plot) 

#         #Join the tree level information
#         #Full join is used to capture tree records that may not have a matched
#         #record between remeasurement periods
#         df <- dplyr::full_join(x = x, y = y, by = merge_id)

#         #Now do the following:
#         #   If tree_id.x is NA, use the value from tree_id.y. These are ingrowth,
#         #   missed trees, or those not accounted for due to change in DESIGNCD
#         #   between initial (interval1) and subsequent inventory (interval2).
#         #   Records with NA interval values have a value filled in.

#         df <- df |>
#           dplyr::mutate("{tree_lab_x}" := 
#                           dplyr::coalesce(.data[[tree_lab_x]], 
#                                           .data[[tree_lab_y]]),
#                         "{int_lab_x}" := 
#                           dplyr::coalesce(.data[[int_lab_x]], interval1),
#                         "{int_lab_y}" := 
#                           dplyr::coalesce(.data[[int_lab_y]], interval2))

#         #Add dataframe to list
#         df_list[[n_insert]] <- df

#         #Increment n_insert
#         n_insert<- n_insert + 1
#       }
#     }
#   }

#   #Bind all items in df_list into a single dataframe and return
#   df <- dplyr::bind_rows(df_list)

#   if(verbose) cat("Leaving merge_inv function.", "\n", "\n")

#   return(df)
# }

######################################
#Non Equi Join version of merge_inv
######################################

# merge_inv <- function(data,
#                       plot_id = "PLOTMERGEID",
#                       merge_id = "TREEMERGEID",
#                       interval_id = "CYCLE",
#                       verbose = TRUE)
# {
#   if(verbose) cat("\n", "Entering merge_inv function.", "\n", "\n")
  
#   intervals <- sort(as.numeric(unique(names(data))))
#   if(verbose) cat("Intervals considered: ", paste(intervals, collapse = ", "), "\n")
  
#   #Set keys here
#   for (i in seq_along(data)) {
#     data.table::setkeyv(data[[i]], merge_id)
#   }
  
#   #Setup labels needed for column headers after joins
#   int_lab_x = paste0(interval_id, ".x")
#   int_lab_y = paste0(interval_id, ".y")
  
#   #Get order pairs of measurement periods
#   pairs_list <- combn(intervals, m = 2, simplify = FALSE)
  
#   #Define list used to store merged dataframes
#   df_list <- vector(mode = "list", length = length(pairs_list))
  
#   #Loop over pairs_dt and combine information for each interval pairing
#   for(i in seq_along(pairs_list)) 
#   {
#     interval1 <- as.character(pairs_list[[i]][1])
#     interval2 <- as.character(pairs_list[[i]][2])
    
#     if(verbose) {
#       cat("Interval 1:", interval1, "\n")
#       cat("Interval 2:", interval2, "\n")
#       cat("Merging remeasurements", "\n", "\n")
#     }
    
#     #Get the data for each interval
#     time1 = data[[as.character(interval1)]]
#     time2 = data[[as.character(interval2)]]

#     #Find plots that match from time 1 and time 2
#     match_plot <- intersect(
#       time1[[plot_id]],
#       time2[[plot_id]]
#     )

#     #Get plots that match between time periods
#     time1 <- time1[time1[[plot_id]] %in% match_plot]
#     time2 <- time2[time2[[plot_id]] %in% match_plot]
    
#     #Join the tree level information
#     df <- merge(x = time1,
#                 y = time2,
#                 by = c(merge_id), 
#                 all.x = TRUE)
    
#     #Records with NA interval values have a value filled in.
#     df[is.na(get(int_lab_y)), (int_lab_y) := as.integer(interval2)]
    
#     #Insert into list
#     df_list[[i]] <- df
#   }
  
#   #Bind all items in df_list into a single dataframe and return
#   df <- data.table::rbindlist(df_list, fill = TRUE)
  
#   if(verbose) cat("Leaving merge_inv function.", "\n", "\n")
  
#   return(df)
# }

#Renaming part
#  #Remove unnecessary columns
#   merge_df <- merge_df[, c("UNIQUETREEID.y",
#                            "PLOTMERGEID.y",
#                            "PLOTMERGEID.x") := NULL]
  
#   #Rename columns
#   data.table::setnames(x = merge_df, 
#                        old = c("UNIQUETREEID.x"), 
#                        new = c("UNIQUETREEID"))
  
#   data.table::setnames(x = merge_df, 
#                        old = names(merge_df), 
#                        new = gsub(".x", "1", names(merge_df)))
  
#   data.table::setnames(x = merge_df, 
#                        old = names(merge_df), 
#                        new = gsub(".y", "2", names(merge_df)))

