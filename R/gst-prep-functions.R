################################################################################
#'build_gst
#'@name build_gst
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
#'@param gst_table: 
#'Character string corresponding to name of growth sample tree database table 
#'written to dbout argument.
#' 
#'@param gst_type: 
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
build_gst <- function(dbin = NULL,
                      dbout = NULL,
                      gst_table = "GST",
                      gst_type = 1,
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
  
  #Catch bad gst values
  if(is.na(gst_table) || is.null(gst_table) || !is.character(gst_table)) 
    gst_table = "GST"
  
  #Catch bad gst type values
  if(! gst_type %in% 1) gst_type = 1

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

    if(gst_type == 1)
    {
      if(verbose) cat("Calling fia_gst", "\n", "\n")
      
      fia_gst(dbin = db,
              dbout = dbout,
              gst_table = gst_table,
              verbose = verbose)
    }

    if(verbose)
      cat("Finished processing:", db, "\n", "\n")
  }

  invisible()
}

################################################################################
#'merge_inv
#'@name merge_inv
#'@description
#' 
#'This function accepts a list of dataframes containing containing information
#'that will be paired by a measurement interval (cycle or year). This function 
#'is called from the code that prepares a growth sample tree database (e.g.
#'fia_gst).
#
#'@param data:
#'List of dataframes for each unique value of interval argument
#
#'@param plot_id:   
#'Character string of column name used to represent a unique plot ID.
#
#'@param tree_id:  
#' Character string of column name used to represent a unique tree ID.
#
#'@param interval:  
#'Character string of column name used to specify a measurement year. This 
#'variable is used to pair re-measurement observations. 
#
#'@param merge_id:
#'Character string of column name used to merge re-measurement periods together.
#
#'@param species:
#'Character string of column name that contains species codes in dataframes 
#'within data argument. This argument is used in the merging of remeasurement
#'data.
#
#'@return
#'Data frame with paired remeasurement data.
################################################################################

merge_inv <- function(data,
                    plot_id = "PLOTMERGEID",
                    tree_id = "UNIQUETREEID",
                    merge_id = "TREEMERGEID",
                    interval = "CYCLE",
                    #species = "SPCD",
                    verbose = TRUE)
{
  if(verbose) cat("\n", "Entering merge_inv function.", "\n", "\n")

  #Get unique INTERVAL values and sort
  intervals <- sort(as.numeric(unique(names(data))))
  if(verbose) cat("Intervals considered: ", paste(intervals, collapse = ", "), "\n")

  #Define list used to store merged dataframes
  df_list <-vector(mode = "list", length = choose(length(intervals), 2))

  #Intialize variable that will be used to track number of insertions into
  #df_list
  n_insert <- 1
  
  #Start outer loop across intervals. Combine remeasurements when criteria is 
  #met
  for(interval1 in intervals)
  {
    for(interval2 in intervals)
    {
      if(verbose)
      {
        cat("Interval 1:", interval1, "\n")
        cat("Interval 2:", interval2, "\n", "\n")
      }

      #Criteria for merging is met
      if(interval2 > interval1)
      {
        if(verbose) cat("Merging remeasurements", "\n", "\n")
        
        #Get dataframes associated with interval
        x = data[[as.character(interval1)]]
        y = data[[as.character(interval2)]]
        
        #Find plots that exist in both x and y
        match_plot = intersect(x[[plot_id]], y[[plot_id]])
        
        #Get only plots that exist at both points in time
        x = x |> dplyr::filter(.data[[plot_id]] %in% match_plot)
        y = y |> dplyr::filter(.data[[plot_id]] %in% match_plot) 

        #Join the tree level information
        #Full join is used to capture tree records that may not have a matched
        #record between remeasurement periods
        df <- dplyr::full_join(x = x, y = y, by = merge_id)
        
        #Now do the following:
        #   If tree_id.x is NA, use the value from tree_id.y. These are ingrowth,
        #   missed trees, or those not accounted for due to change in DESIGNCD
        #   between initial (interval1) and subsequent inventory (interval2).
        #   Records with NA interval values have a value filled in.

        tree_lab_x = paste0(tree_id, ".x")
        tree_lab_y = paste0(tree_id, ".y")
        int_lab_x = paste0(interval, ".x")
        int_lab_y = paste0(interval, ".y")
        
        df <- df |>
          dplyr::mutate("{tree_lab_x}" := 
                          dplyr::coalesce(.data[[tree_lab_x]], 
                                          .data[[tree_lab_y]]),
                        "{int_lab_x}" := 
                          dplyr::coalesce(.data[[int_lab_x]], interval1),
                        "{int_lab_y}" := 
                          dplyr::coalesce(.data[[int_lab_y]], interval2))

        #Add dataframe to list
        df_list[[n_insert]] <- df

        #Increment n_insert
        n_insert<- n_insert + 1

        #Clean up
        #rm(df); gc()
      }
    }
  }

  #Bind all items in df_list into a single dataframe and return
  df <- dplyr::bind_rows(df_list)

  if(verbose) cat("Leaving merge_inv function.", "\n", "\n")
  
  return(df)
}

################################################################################
#'merge_inv_dt (data.table implementation). May explore at a later date. There
#'is a bug on linux that is causing problems with data.table.
#'@name merge_inv_dt
#'@description
#' 
#'This function accepts a list of dataframes containing containing information
#'that will be paired by a measurement interval (cycle or year). This function 
#'is called from the code that prepares a growth sample tree database (e.g.
#'fia_gst).
#
#'@param data:
#'List of dataframes for each unique value of interval argument
#
#'@param plot_id:   
#'Character string of column name used to represent a unique plot ID.
#
#'@param tree_id:  
#' Character string of column name used to represent a unique tree ID.
#
#'@param interval:  
#'Character string of column name used to specify a measurement year. This 
#'variable is used to pair re-measurement observations. 
#
#'@param merge_id:
#'Character string of column name used to merge re-measurement periods together.
#
#'@param species:
#'Character string of column name that contains species codes in dataframes 
#'within data argument. This argument is used in the merging of remeasurement
#'data.
#
#'@return
#'Data frame with paired remeasurement data.
################################################################################

merge_inv_dt <- function(data,
                      plot_id = "PLOTMERGEID",
                      tree_id = "UNIQUETREEID",
                      merge_id = "TREEMERGEID",
                      interval = "CYCLE",
                      #species = "SPCD",
                      verbose = TRUE)
{
  if(verbose) cat("\n", "Entering merge_inv function.", "\n", "\n")
  
  #Get unique INTERVAL values and sort
  intervals <- sort(as.numeric(unique(names(data))))
  if(verbose) cat("Intervals considered: ", paste(intervals, collapse = ", "), "\n")
  
  #Define list used to store merged dataframes
  df_list <-vector(mode = "list", length = length(intervals)^2)

  #Setup labels needed for column headers after joins
  tree_lab_x = paste0(tree_id, ".x")
  tree_lab_y = paste0(tree_id, ".y")
  int_lab_x = paste0(interval, ".x")
  int_lab_y = paste0(interval, ".y")
  
  #Intialize variable that will be used to track number of insertions into
  #df_list
  n_insert <- 1
  
  #Start outer loop across intervals. Each interval in the outer loop will be
  #merged with the interval in inner loop if it is less than or equal to
  #interval in the inner loop. In other words, this is used to test if initial
  #inventory year is less than or equal to subsequent inventory year.
  for(interval1 in intervals)
  {
    for(interval2 in intervals)
    {
      if(verbose)
      {
        cat("Interval 1:", interval1, "\n")
        cat("Interval 2:", interval2, "\n")
      }
      
      #Criteria for merging is met
      if(interval2 > interval1) 
      {
        if(verbose) cat("Merging remeasurements", "\n", "\n")
        
        #Get data.tables associated with interval
        time1 = data[[as.character(interval1)]]
        time2 = data[[as.character(interval2)]]
        
        #Find plots that exist in both time1 and time 2
        match_plot = intersect(time1[[plot_id]], time2[[plot_id]])
        
        #Get only plots that exist at both points in time
        time1 = time1[(get(plot_id) %in% match_plot)]
        time2 = time2[(get(plot_id) %in% match_plot)] 

        data.table::setkeyv(time1, merge_id)
        data.table::setkeyv(time2, merge_id)

        #Join the tree level information
        #Full join is used to capture tree records that may not have a matched
        #record between remeasurement periods
        df <- merge(x = time1,
                    y = time2,
                    by = c(merge_id),
                    all = TRUE)
        
        #Now do the following:
        #If tree_id.x is NA, use the value from tree_id.y. These are ingrowth,
        #missed trees, or those not accounted for due to change in DESIGNCD
        #between initial (interval1) and subsequent inventory (interval2).
        #Records with NA interval values have a value filled in.
        
        new_cols = c(tree_lab_x, int_lab_x, int_lab_y)
        
        df[, (new_cols) := list(data.table::fcoalesce(get(tree_lab_x), get(tree_lab_y)),
                                 data.table::fcoalesce(get(int_lab_x), as.integer(interval1)),
                                 data.table::fcoalesce(get(int_lab_y), as.integer(interval2)))]
        
      #Add data.table to list
        df_list[[n_insert]] <- df
        
        #Increment n_insert
        n_insert<- n_insert + 1
        
        #Clean up
        #rm(df)
      }
    }
  }
  
  #Bind all items in df_list into a single dataframe and return
  df <- data.table::rbindlist(df_list)
  
  if(verbose) cat("Leaving merge_inv function.", "\n", "\n")
  
  return(df)
}

################################################################################
#'write_gst
#'@name write_gst
#'@description
#' This function is used to write a dataframe containing growth sample tree data
#' to a specified output SQLite database.
#
#'@param gst:
#'Dataframe that will be written to GST database specified in dbout argument.
#
#'@param dbout:       
#'Character string corresponding to file path of output SQLite database.
#
#'@param gst_table
#'Name of database table that will contain the growth sample tree information in
#'dbout argument.
#
#'@return
#' None
################################################################################

write_gst <- function(gst,
                      dbout,
                      gst_table = "GST")
{

  #Connect to the database
  con <- RSQLite::dbConnect(RSQLite::SQLite(),
                            dbout)
  
  on.exit(
  expr = try(if(RSQLite::dbIsValid(con)) RSQLite::dbDisconnect(con),
  silent = TRUE))

  #Get GST fields and data types
  gst_fields <- names(gst_vars)
  gst_field_types <- gst_vars

  #Handling for when output database table already exists
  if(RSQLite::dbExistsTable(con, name = gst_table))
  {
    db_fields <- RSQLite::dbListFields(conn = con,
                                       name = gst_table)

    if(length(gst_fields) < length(db_fields))
    {
      missing <- db_fields[! db_fields %in% gst_fields]
      stop(paste("The following columns are missing from GST dataframe:",
                 paste(missing, collapse = ", "),
                 "\n"))
    }

    if(length(gst_fields) > length(db_fields))
    {
      missing <- gst_fields[! gst_fields %in% db_fields]
      stop(paste("The following columns are missing from GST database:",
                 dbout,
                 "\n",
                 paste(missing, collapse = ", "),
                 "\n"))
    }

    #Append data to existing table
    RSQLite::dbWriteTable(conn = con,
                          name = gst_table,
                          value = gst,
                          append = T)
  }

  #Otherwise create output database table
  else
  {
    RSQLite::dbWriteTable(conn = con,
                          name = gst_table,
                          value = gst,
                          field.types = gst_field_types)
  }

  RSQLite::dbDisconnect(con)
  invisible()
}
