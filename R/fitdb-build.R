################################################################################
#'build_fitdb
#'@name build_fitdb
#'@description
#' 
#'This function processes a set of sqlite databases and creates a standardized
#'output growth sample tree database than be used for fitting equation
#'development (GST). Currently this function is only equipped to build GST
#'databases from FIA data.
#
#'@param dbin:	    
#'Character vector of file paths to sqlite database (.db or .sqlite).
#
#'@param dbout:	    
#'Character string corresponding to output sqlite GST database (.db or .sqlite).
#' 
#'@param fitdb_name: 
#'Character string corresponding to name of database table written to dbout 
#'argument.
#' 
#'@param fitdb_type: 
#'Numeric value corresponding to type of GST database to create. 
#'1 = GST built from FIA data
#
#'@param overwrite:	
#' Logical variable used to determine if currently existing dbout file should be
#' deleted. If this argument is left as FALSE, data will be appended to existing
#' file specified in dbout.
#
#'@return
#' None
################################################################################

#'@export
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
  dbin = chartr("\\", "/", dbin)

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
  dbout = chartr("\\", "/", dbout)[1]

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
  if(! overwrite %in% c(TRUE, FALSE)) overwrite = FALSE
  
  #Catch bad fitdb values
  if(is.na(fitdb_name) || is.null(fitdb_name) || !is.character(fitdb_name)) 
    fitdb_name = "FITDB"
  
  #Catch bad fitdb type values
  if(! fitdb_type %in% 1) fitdb_type = 1

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
    #Call function for building GST datbase. Currently only FIA data is
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

################################################################################
#'merge_inv
#'@name merge_inv
#'@description
#' 
#'This function accepts a list of dataframes containing containing information
#'that will be paired by a measurement interval (cycle or year). This function 
#'is called from the code that prepares a growth sample tree database (e.g.
#'fia_fitdb).
#
#'@param data:
#'List of data tables for each unique value of interval argument
#
#'@param plot_id:   
#'Character string of column name used to represent a unique plot ID.
#
#'@param tree_id:  
#' Character string of column name used to represent a unique tree ID.
#' 
#'@param merge_id:
#'Character string of column name used to merge re-measurement periods together
#'at the tree level.
#
#'@param interval:  
#'Character string of column name used to specify a measurement year or cycle. 
#'This variable is used to pair re-measurement observations.
#
#'@return
#'Data frame with paired remeasurement data.
################################################################################

merge_inv_dt <- function(data,
                         plot_id = "PLOTMERGEID",
                         tree_id = "UNIQUETREEID",
                         merge_id = "TREEMERGEID",
                         interval_id = "CYCLE",
                         verbose = TRUE)
{
  if(verbose) cat("\n", "Entering merge_inv function.", "\n", "\n")
  
  intervals <- sort(as.numeric(unique(names(data))))
  if(verbose) cat("Intervals considered: ", paste(intervals, collapse = ", "), "\n")
  
  #Get data.table of ordered pairs
  pairs <- combn(intervals, m = 2)
  pairs_dt <- data.table::data.table(T1 = pairs[1, ],
                                     T2 = pairs[2, ])
  
  #Define list used to store merged dataframes
  df_list <-vector(mode = "list", length = nrow(pairs_dt))

  #Setup labels needed for column headers after joins
  tree_lab_x = paste0(tree_id, ".x")
  tree_lab_y = paste0(tree_id, ".y")
  int_lab_x = paste0(interval_id, ".x")
  int_lab_y = paste0(interval_id, ".y")
  
  #Loop over pairs_dt and combine information for each interval pairing
  for(i in 1:nrow(pairs_dt)) {

    interval1 <- pairs_dt[[i, 1]]
    interval2 <- pairs_dt[[i, 2]]
   
    if(verbose) {
          cat("Interval 1:", interval1, "\n")
          cat("Interval 2:", interval2, "\n")
          cat("Merging remeasurements", "\n", "\n")
    }
   
    #Get the data for each interval
    time1 = data[[as.character(interval1)]]
    time2 = data[[as.character(interval2)]]
   
    #Find plots that match from time 1 and time 2
    match_plot <- intersect(
      time1[, env = list(p = plot_id), (p)],
      time2[, env = list(p = plot_id), (p)]
    )
   
    #Get plots that match between time periods
    time1 <- time1[env = list(p = plot_id), p %in% match_plot]
    time2 <- time2[env = list(p = plot_id), p %in% match_plot]
   
    data.table::setkeyv(time1, merge_id)
    data.table::setkeyv(time2, merge_id)
   
    #Join the tree level information
    #Full join is used to capture tree records that may not have a matched
    #record between remeasurement periods
    df <- merge(x = time1, y = time2, by = c(merge_id), all = TRUE)

    #Now do the following:
    #If tree_id.x is NA, use the value from tree_id.y. These are ingrowth,
    #missed trees, or those not accounted for due to change in DESIGNCD
    #between initial (interval1) and subsequent inventory (interval2).
    #Records with NA interval values have a value filled in.
    df[, env = list(
      tx = tree_lab_x, ty = tree_lab_y,
      ix = int_lab_x,  iy = int_lab_y,
      v1 = as.integer(interval1), v2 = as.integer(interval2)
    ), `:=`(
      tx = data.table::fcoalesce(tx, ty),
      ix = data.table::fcoalesce(ix, v1),
      iy = data.table::fcoalesce(iy, v2)
    )]
   
    df_list[[i]] <- df
  }
  
  #Bind all items in df_list into a single dataframe and return
  df <- data.table::rbindlist(df_list)
  
  if(verbose) cat("Leaving merge_inv function.", "\n", "\n")
  
  return(df)
}

################################################################################
#'write_fitdb
#'@name write_fitdb
#'@description
#' This function is used to write a dataframe containing growth sample tree data
#' to a specified output SQLite database.
#
#'@param fitdb:
#'Dataframe that will be written to fitdb database specified in dbout argument.
#
#'@param dbout:       
#'Character string corresponding to file path of output SQLite database.
#
#'@param fitdb_name
#'Name of database table that will contain the growth sample tree information in
#'dbout argument.
#
#'@return
#' None
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
