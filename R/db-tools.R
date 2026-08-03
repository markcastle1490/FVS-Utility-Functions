################################################################################
#' fvs_gaak
#' @name fvs_gaak
#' @description This function returns a FVS_GroupAddFilesAndKeyword (GAAK) table 
#' in a dataframe format with the appropriate FVS keywords and SQL statements for 
#' reading stand and tree level data from a specified input FVS database. This 
#' function can accommodate a GAAK table that can read from standard 
#' FVS_TreeInit, FVS_StandInit, and FVS_PlotInit tables and FIA-centric tables 
#' that include FVS_TreeInit_Plot, FVS_TreeInit_Cond, FVS_StandInit_Plot, 
#' FVS_StandInit_Cond, and FVS_PlotInit_Plot.
#' 
#' @param dbin
#' Character string corresponding to filepath or name of database to read from. 
#' File extension (.db, .sqlite) should be included for this value 
#' (e.g. "FVS_Data.db").
#' 
#' @param stand_type
#' Integer value that determine what stand ID will be used to read data from for 
#' STANDSQL and TREESQL statements included in GAAK table.
#' 
#' 0 = Stand_CN (e.g. "WHERE Stand_CN = '%Stand_CN%'")
#' 
#' 1 = Stand_ID (e.g. "WHERE Stand_ID = '%StandID%'")
#' 
#' @param gaak_type
#' Variable to determine what grouping codes are included in GAAK table.
#' 
#' 1 = Standard FVS grouping codes (All_Stands, All_Plots)
#' 
#' 2 = FIA grouping codes (All_FIA_Conditions, All_FIA_Plots, All_FIA_Subplots)
#' 
#' 3 = Both standard FVS grouping codes and FIA grouping codes
#' 
#' @return
#' Dataframe containing FVS GAAK table.
#' @export
################################################################################

fvs_gaak<-function(dbin ="FVS_Data.db",
                   stand_type = 1,
                   gaak_type = 3)
{
  #Catch bad stand_type values
  if(!stand_type %in% c(0, 1)) stand_type <- 1
  
  #Capture bad gaak_type values
  if(!gaak_type %in% c(1, 2, 3)) gaak_type <- 3
  
  #Set the stand column to read data from
  if(stand_type == 1) 
  {
    stand_read = "WHERE Stand_ID = '%StandID%'"
    plot_read = "WHERE StandPlot_ID = '%StandID%'"
  }
  
  else 
  {
    stand_read = "WHERE Stand_CN = '%Stand_CN%'"
    plot_read = "WHERE StandPlot_CN = '%Stand_CN%'"
  }
  
  #Create dataframe containing FVS_GroupAddfilesAndKeywords table
  gaak <- data.frame(GROUPS = c("All_Stands","All_Plots","All_FIA_Conditions",
                               "All_FIA_Plots", "All_FIA_Subplots"),
                    ADDFILES = c("","","","",""),
                    FVSKEYWORDS = c(paste("DATABASE", 
                                          "DSNIN",
                                          dbin,
                                          "STANDSQL",
                                          "SELECT *", 
                                          "FROM FVS_StandInit",
                                          stand_read,
                                          "ENDSQL",
                                          "TREESQL", 
                                          "SELECT *",
                                          "FROM FVS_TreeInit",
                                          stand_read,
                                          "ENDSQL", 
                                          "END", sep = "\n"),
                                    paste("DATABASE", 
                                          "DSNIN",
                                          dbin,
                                          "STANDSQL",
                                          "SELECT *",
                                          "FROM FVS_PlotInit",
                                          plot_read,
                                          "ENDSQL",
                                          "TREESQL",
                                          "SELECT *",
                                          "FROM FVS_TreeInit",
                                          plot_read,
                                          "ENDSQL",
                                          "END", sep = "\n"),
                                    paste("DATABASE", 
                                          "DSNIN",
                                          dbin, 
                                          "STANDSQL",
                                          "SELECT *", 
                                          "FROM FVS_StandInit_Cond",
                                          stand_read,
                                          "ENDSQL",
                                          "TREESQL",
                                          "SELECT *",
                                          "FROM FVS_TreeInit_Cond",
                                          stand_read,
                                          "ENDSQL",
                                          "END", sep = "\n"),
                                    paste("DATABASE", 
                                          "DSNIN",
                                          dbin,
                                          "STANDSQL", 
                                          "SELECT *",
                                          "FROM FVS_StandInit_Plot",
                                          stand_read,
                                          "ENDSQL",
                                          "TREESQL",
                                          "SELECT *",
                                          "FROM FVS_TreeInit_Plot",
                                          stand_read,
                                          "ENDSQL", 
                                          "END", sep = "\n"),
                                    paste("DATABASE", 
                                          "DSNIN",
                                          dbin,
                                          "STANDSQL",
                                          "SELECT *", 
                                          "FROM FVS_PlotInit_Plot",
                                          plot_read,
                                          "ENDSQL",
                                          "TREESQL",
                                          "SELECT *",
                                          "FROM FVS_TreeInit_Plot",
                                          plot_read,
                                          "ENDSQL", 
                                          "END", sep = "\n")))
  
  #GAAK with just FVS group codes
  if(gaak_type == 1)
    gaak <- gaak[1:2,]
  
  #GAAK with just FIA group codes
  if(gaak_type == 2)
    gaak <- gaak[3:5,]
  
  return(gaak)
}

################################################################################
#' db_tbl_schema
#' @name db_tbl_schema
#' @description
#' This function queries an open SQLite database connection using system PRAGMA 
#' metadata to extract the column layout of a specified table. It returns a 
#' named character vector where the names represent column headers and the 
#' values represent their corresponding SQL data types (e.g., TEXT, INTEGER, REAL). 
#' If the specified table does not exist, it safely returns an empty vector.
#' 
#' @param con
#' An active RSQLite connection object to a SQLite database.
#' 
#' @param db_table
#' A character string specifying the target database table name. Defaults to 
#' "TREE".
#' 
#' @return
#' A named character vector containing field names and data types for all fields 
#' in db_table, or character(0) if the table is missing.
#' @export
################################################################################

db_tbl_schema <- function(con,
                          db_table = "TREE")
{
  #Initialize empty vector
  data_types <- character(0)
  
  #If db_table does not exist in db, return empty vector
  if(RSQLite::dbExistsTable(conn = con,
                             name = db_table))
  {
    #Build query
    query <- paste0("PRAGMA table_info(", db_table, ");")
    
    #Get table info
    table_defs <- RSQLite::dbGetQuery(con,query) 
    
    #Make named vector from variables and data types
    data_types <- table_defs$type
    names(data_types) <- table_defs$name
  }
  
  return(data_types)
}

################################################################################
#' db_collect_paths
#' @name db_collect_paths
#' @description
#' This function inspects an input character vector of file paths, flags any 
#' compressed .zip archives, and extracts them into isolated subfolders inside 
#' a dedicated system temporary directory. It then searches both the original 
#' standalone file paths and the newly unzipped directories to return all valid 
#' SQLite database targets (.db, .sqlite, .sqlite3).
#' 
#' @param dbin
#' A character vector containing directory paths and file names to 
#' standalone SQLite databases or compressed .zip folders.
#' 
#' @param verbose
#' Logical. If TRUE, logs extraction steps and path maps to the console. 
#' Defaults to FALSE.
#' 
#' @return
#' A list containing two elements:
#' \item{paths}{A character vector of fully normalized file paths to all 
#' discovered and extracted SQLite databases.}
#' \item{unzip_dir}{A character string specifying the root temporary directory
#'  where zipped archives were expanded.}
################################################################################

db_collect_paths <- function(dbin = character(0),
                             verbose = FALSE)
{
  #Setup unzip directory
  unzip_root <- file.path(tempdir(), "xxxfvstoolsdb_compileUnzipxxx")

  #Ensure the base directory exists cleanly without wiping target data prematurely
  if (!dir.exists(unzip_root)) {
    dir.create(unzip_root, recursive = TRUE, showWarnings = FALSE)
  }
  
  #Loop through dbin and check if any are zip files
  for(i in seq_along(dbin))
  {
    db <- dbin[i]
    fileext_in <- tolower(tools::file_ext(db))
    
    #If the file extension is not zip, skip
    if(!fileext_in %in% c("zip")) next
    
    #Unzip archives into isolated folder
    unique_subfolder <- file.path(unzip_root, paste0("zip_extract_", i))
    dir.create(unique_subfolder, recursive = TRUE, showWarnings = FALSE)
      
    if(verbose) cat("Unzipping:", db, "to", unique_subfolder, "\n\n")
    unzip(zipfile = db, exdir = unique_subfolder)
  }
  
  #Filter out DBs from the original input vector
  db_files <- dbin[tolower(tools::file_ext(dbin)) %in%
                     c("db", "sqlite", "sqlite3")]
  
  #Scan the unzip directory for extracted DB files
  extracted_db_files <- list.files(
    path = unzip_root, 
    pattern = "\\.(db|sqlite|sqlite3)$", 
    full.names = TRUE, 
    recursive = TRUE, 
    ignore.case = TRUE
  )
  
  #Combine all paths
  final_paths <- chartr(old = "\\", 
                        new = "/", 
                        x = c(db_files, extracted_db_files))
  
  #Return paths and unzipdir as list
  return(list(final_paths, unzip_root))
}

################################################################################
#' create_tbl_query
#' @name create_tbl_query
#' @description
#' This function constructs a formatted SQL "CREATE TABLE IF NOT EXISTS" 
#' statement using a schema definition from a named vector.
#' 
#' @param db_fields
#' A named character vector where the names represent table column headers and 
#' the character values represent their respective SQL data types (e.g., TEXT, 
#' INTEGER, REAL).
#' 
#' @param db_table
#' A character string specifying the name of the database table to be initialized.
#' 
#' @param alias
#' An optional character string specifying an insert alias moniker. Defaults 
#' to "dbinsert".
#' 
#' @return
#' A character string containing the complete SQL query to initialize the table 
#' structure, or an empty string "" if arguments are missing.
#' @export
################################################################################

create_tbl_query <- function(db_table = NULL,
                             db_fields = NULL)
{
  
  query <- ""
  
  if(!is.null(db_table) && !is.null(db_fields))
  {
    column_string <- paste(names(db_fields), 
                           db_fields, 
                           collapse = ",\n")
    
    query <- paste0("CREATE TABLE IF NOT EXISTS ",
                    db_table,
                    " (\n  ", 
                    column_string,
                    "\n);")
  }
    
  return(query)
}

################################################################################
#' insert_tbl_query
#' @name insert_tbl_query
#' @description
#' This function constructs a formatted SQL "CREATE TABLE IF NOT EXISTS" 
#' statement using a dynamic schema definition from a named vector.
#' 
#' @param db_fields
#' A named character vector where the names represent table column headers and 
#' the character values represent their respective SQL data types (e.g., TEXT, 
#' INTEGER, REAL).
#' 
#' @param db_table
#' A character string specifying the name of the database table to be initialized.
#' 
#' @param alias
#' An optional character string specifying an insert alias moniker. Defaults 
#' to "dbinsert".
#' 
#' @return
#' A character string containing the complete SQL query to initialize the table 
#' structure safely, or an empty string "" if arguments are missing.
#' @export
################################################################################

insert_tbl_query <- function(db_fields = NULL,
                             db_table = NULL,
                             alias = "dbinsert")
{
  query <- ""
  
  if (!is.null(db_fields) && !is.null(db_table)) {
    
    #Format columns as a comma-separated list
    column_string <- paste(db_fields, collapse = ", ")
    
    #Assemble standard SQLite syntax
    query <- paste0("INSERT INTO ",
                    db_table,
                    " (", column_string, ")\n",
                    "SELECT ", column_string, "\n",
                    "FROM ",
                    alias, ".", db_table, ";")
  }
  
  return(query)
}

################################################################################
#' db_insert_tbl
#' @name db_insert_tbl
#' @description
#' This function coordinates a cross-database data migration. It attaches a 
#' source SQLite database to a target destination database via an ATTACH clause, 
#' audits schemas, initializes missing tables, synchronizes any mismatched column 
#' fields, and appends records smoothly using standard SQL operations. This 
#' function is designed to be called internally by the db_compile function.
#' 
#' @param dbout
#' A character string specifying the file path to the destination master SQLite 
#' database file being populated.
#' 
#' @param dbinsert
#' A character string specifying the file path to the source SQLite database file 
#' containing records to be imported.
#' 
#' @param db_tables
#' A character vector of table names to check, synchronize, and migrate from the 
#' source database into the destination database.
#' 
#' @param keep_casing
#' Logical. If FALSE, automatically forces all column headers to uppercase to 
#' prevent case-mismatch schema splitting. Defaults to TRUE.
#' 
#' @param verbose
#' Logical. If TRUE, prings active table migrations to the console. Defaults to 
#' FALSE.
#' 
#' @return
#' An invisible NULL value.
################################################################################

db_insert_tbl <- function(dbout,
                          dbinsert,
                          db_tables = c(),
                          keep_casing = TRUE,
                          verbose = FALSE)
{
  #Connect to dbout (database information is being sent to)
  con_out <- RSQLite::dbConnect(RSQLite::SQLite(),
                                dbout)
  
  #Disconnect and detach on exit
  on.exit(expr = {
    if (RSQLite::dbIsValid(con_out)) {
      try(RSQLite::dbExecute(con_out, "DETACH DATABASE dbinsert;"), silent = TRUE)
      try(RSQLite::dbDisconnect(con_out), silent = TRUE)
    }
  }, add = TRUE)
  
  #Attach dbinsert to con_out
  RSQLite::dbExecute(con_out,
                     paste0("ATTACH DATABASE '", 
                            dbinsert,
                           "' as dbinsert;"))
  
  #Begin loop across db_tables
  for(table in db_tables)
  {
    if(verbose) cat("Processing table:", table, "\n")
    
    #Connect to dbinsert
    con_in <- RSQLite::dbConnect(RSQLite::SQLite(),
                                 dbinsert)
    
    #Check table existence
    table_found <- RSQLite::dbExistsTable(con = con_in, name = table)
    
    #Get schema if it exists
    if (table_found) {
      insert_fields <- db_tbl_schema(con = con_in, db_table = table)
    }
    
    #Disconnect from dbinsert
    RSQLite::dbDisconnect(con_in)
    
    #If insert_fields is empty, skip
    if(length(insert_fields) <= 0) next
    
    #Capitalize fields if keep_casing is off
    if(!keep_casing) names(insert_fields) <- toupper(names(insert_fields)) 
    
    #if db_table does not exist in dbout, create the table
    if(!RSQLite::dbExistsTable(conn = con_out,
                               name = table))
    {
      query <- create_tbl_query(db_table = table,
                                db_fields = insert_fields)
      
      if(query == "") 
      {
        warning("Invalid table creation query.")
        next
      } else{ 
        RSQLite::dbExecute(conn = con_out, query)
      }
    }
    
    #Check for fields that DO NOT exist in table within dbout
    db_fields <- RSQLite::dbListFields(con_out,
                                       name = table)
    
    missing_fields <- names(insert_fields)[! names(insert_fields) %in% db_fields]
    missing_fields <- insert_fields[missing_fields]
    
    #Loop through missing_fields and add to database table in con_out
    if(length(missing_fields) > 0)
    {
      db_add_fields(conn = con_out,
                    table_name = table,
                    db_fields = missing_fields,
                    verbose = verbose)
    }
    
    #Generate insert query
    query <- insert_tbl_query(db_fields = names(insert_fields),
                              db_table = table)
    
    #If query is invalid move to next iteration
    if(query == "") 
    { 
      warning("Invalid insertion query created.")
      next
    }
    
    RSQLite::dbExecute(con_out,
                       query)
  }
  
  #Detach dbinsert and then disconnect from dbout
  RSQLite::dbExecute(con_out,
                     "DETACH DATABASE dbinsert;")
  RSQLite::dbDisconnect(con_out)
  
  invisible()
}

################################################################################
#' db_add_fields
#' @name db_add_fields
#' @description This function coordinates dynamic database alterations by 
#' identifying column discrepancies between source and target datasets. It loops
#' through a character vector of missing schema parameters, generating and 
#' running ALTER TABLE ADD COLUMN routines sequentially.
#' 
#' @param conn 
#' An active RSQLite connection object to the destination master SQLite database.
#' 
#' @param table_name 
#' Character string specifying the database table name being modified.
#' 
#' @param db_fields 
#' A named character vector where names are missing column headers and values 
#' are SQL data types.
#' 
#' @param verbose 
#' Logical. If TRUE, logs missing fields and alter table progress
#' to the console. Defaults to FALSE.
#'  
#' @return 
#' An invisible NULL value.
################################################################################

db_add_fields <- function(conn,
                          table_name = NULL,
                          db_fields = NULL,
                          verbose = FALSE)
{
  #If db_fields or data_types is empty
  if(is.null(db_fields) || is.null(table_name)) return()
  
  if(verbose)
  {
    cat("\n",
        "Fields missing from", table_name, "\n", names(db_fields), "\n", "\n")
  }

  for(i in seq_along(db_fields))
  {
    #Extract field
    field <- names(db_fields)[[i]]
    
    #Extract data type of field
    data_type <- db_fields[[i]]
    
    if(verbose)
    {
      cat("Adding field:", field, paste0("(", data_type, ")"), "to table:",
          table_name,
          "\n")
    }

    #Create query to alter table and add field in con_out
    query <- add_col_query(db_table = table_name, 
                           db_field = field,
                           data_type = data_type)
    
    #Add field to con_out
    RSQLite::dbExecute(conn = conn, 
                       statement = query)
    
    if(verbose)
    {
      cat("Field:", field, "added to table:", table_name, "\n", "\n")
    }
  }
  
  return()
}

################################################################################
#' db_compile
#' @name db_compile
#' @description This function is used to combine the contents of multiple sqlite
#' databases into a single sqlite database. SQLite databases (.db, .sqlite, 
#' .sqlite3) are the only compatible input database type that can be processed
#' in this function. The primary purpose of this function is to combine input
#' FVS databases into a single database or extract FVS database tables from a 
#' larger database such as those on the FIA datamart.
#' 
#' @param dbin
#' Character vector of directory paths and file names for SQLite databases to 
#' process. Files can either be a SQLite database (.db) or zipped folder (.zip) 
#' which contains a SQLite database(s). 
#' 
#' NOTE: .zip files will be unzipped to an isolated temporary folder called 
#' xxxfvstoolsdb_compileUnzipxxx inside the system temporary directory. This 
#' temporary folder will be deleted automatically after db_compile has finished 
#' processing, even if the script execution encounters a runtime error.
#' 
#' Examples of valid dbin formats: 
#' "C:/FIA2FVS_Databases/SQLite_FIADB_AZ/FIADB_AZ.db" 
#' "C:\\FIA2FVS_Databases\\SQLite_FIADB_AZ\\FIADB_AZ.zip"
#' 
#' @param dbout
#' Character string corresponding to SQLite database to write out to. 
#' Examples of valid dbout formats: 
#' "C:/FIA2FVS_Databases/SQLite_FIADB_AZ/FVS_Data.db"
#' 
#' @param db_tables
#' Character vector of database tables to process from argument dbin. If this 
#' argument is left as NULL, then function will use all tables from the first 
#' database specified in the dbin argument.
#' 
#' @param delete_input
#' Logical variable used to determine if values in dbin should be deleted after 
#' db_compile has been called successfully. The primary purpose of this argument
#' is to conserve hard disk space for users who do not want to retain the input 
#' databases specified in dbin. Defaults to FALSE.
#' 
#' @param keep_casing
#' Logical variable used to determine if the database table names and fields in 
#' dbin should retain original casing. When FALSE, the database table names and 
#' fields in each table written to dbout will be capitalized. Defaults to TRUE.
#' 
#' @param overwrite
#' Logical variable used to determine if currently existing dbout file should be 
#' deleted. If this argument is left as FALSE, data will be appended to existing 
#' file specified in dbout. Defaults to FALSE.
#' 
#' @param verbose
#' Logical variable used to determine if compilation milestones, folder extraction 
#' paths, and table progress updates are printed to the console. Defaults to 
#' FALSE.
#' 
#' @return
#' An invisible NULL value.
#' @export
################################################################################

db_compile <- function(dbin = NULL,
                       dbout = NULL,
                       db_tables = NULL,
                       delete_input = FALSE,
                       keep_casing = TRUE,
                       overwrite = TRUE,
                       verbose = FALSE)
{
  
  #Test if no values have been specified for dbin
  if(is.null(dbin)) stop(paste("No files were specified for dbin."))
  
  #Test if no values have been specified for dbout
  if(is.null(dbout)) stop(paste("No file was specified for dbout."))
  
  #Get first entry in dbout
  dbout <- dbout[1]
  
  #Replace \\ with / in dbin and dbout
  dbin <- chartr(old = "\\", new = "/", x = dbin)
  dbout <- chartr(old = "\\", new = "/", x = dbout)
  
  #Check files in dbin.
  if(!all(file.exists(dbin)))
    stop("One or more files in dbin does not exist.")

  #Test if dbout file path is valid.
  outpath <- gsub("/[^/]+$", "", dbout)
  if (outpath != dbout && !(file.exists(outpath))){
    stop(paste("Path to output:", outpath, "was not found.",
               "Make sure directory path to output is spelled correctly."))
  }
  
  #Test if output file is a SQLite database. If the file is not a SQLite
  #database then error message is reported.
  fileext_out <- tolower(tools::file_ext(dbout))
  if(!fileext_out %in% c("db", "sqlite", "sqlite3"))
  {
    stop(paste("Output database:",
               dbout,
               "is not a SQLite database.",
               "\n"))
  }
  
  #Get database paths and unzip dir
  results <- db_collect_paths(dbin = dbin, verbose = verbose)
  dbin_update <- results[[1]]
  unzipdir <- results[[2]]

  #Delete unzipdir
  on.exit(expr = {
    if (!is.null(unzipdir) && nzchar(unzipdir)) {
      unlink(x = unzipdir, recursive = TRUE)
    }
  }, add = TRUE)

  #If dbin_update does not have any databases, then stop with error message and
  #delete unzip directory if it exists.
  if(length(dbin_update) <= 0)
    stop("No valid database files are available for processing.")
  
  #Check if any of the items in dbin are the same as dbout
  if(any(dbin_update %in% dbout))
    stop("At least one element in dbin is the same as dbout.")
  
  #If dbout already exists, delete it
  if(file.exists(dbout) && overwrite)
  {
    if(verbose) cat("Deleting preexisting dbout", "\n")
    ret <- unlink(dbout)
    if(ret == 1) stop(paste("Failed to delete:", dbout))
  }
  
  if(verbose) cat("Output database:", dbout, "\n","\n")
  
  #Remove duplicate values in dbin_update and print database file paths
  dbin_update <- unique(dbin_update)
  if(verbose) 
  {
    cat("List of db files to process:", "\n")
    cat(paste(dbin_update, collapse = "\n"), "\n", "\n")
  }
  
  #If db_tables is NULL, then grab database tables from first database in 
  #dbin_update and use those for processing
  if(is.null(db_tables))
  {
    con <- RSQLite::dbConnect(RSQLite::SQLite(),
                              dbin_update[1])
    
    db_tables <- RSQLite::dbListTables(conn = con)
    
    RSQLite::dbDisconnect(con)
  }
  
  #If there are no values in db_tables stop with error
  if(length(db_tables) <= 0)
    stop("No valid database tables available for processing.")

  #Capitalize db_tables if keep_casing is off
  if(!keep_casing) db_tables <- toupper(db_tables)
  
  if(verbose)
  {
    cat("Database table names to consider:", "\n")
    cat(paste(db_tables, collapse = "\n"), "\n", "\n")
  }

  #Begin processing databases in dbin_update
  for(i in seq_along(dbin_update))
  {
    
    db <- dbin_update[i]
    
    if(verbose) cat("Processing db:", db, "\n")
    
    db_insert_tbl(dbout = dbout,
                  dbinsert = db,
                  db_tables = db_tables,
                  keep_casing = keep_casing,
                  verbose = verbose)
    
    #Print message indicating which db has been processed.
    if(verbose) cat("Finished processing db:", db, "\n", "\n")
  }
  
  #If delete_input is TRUE, delete files in dbin argument.
  if (delete_input) {
    if(verbose)
    {
      cat("Argument delete_input is TRUE. Removing source input paths from disk...\n")
    }

    unlink(x = dbin, recursive = FALSE)
    
    #Verify if files were dropped
    remaining_files <- dbin[file.exists(dbin)]
    if (length(remaining_files) > 0) {
      warning("Failed to delete one or more source databases from dbin when delete_input is TRUE.\n")
    }
  }
  
  invisible()
}

################################################################################
#' db_indices
#' @name db_indices
#' @description This function queries the SQLite master schema table to retrieve
#' the names of all indices that currently exist within the connected database. 
#' If no indices are present in the database, it safely returns NULL.
#' 
#' @param con
#' An active RSQLite connection object to a SQLite database (.db, .sqlite).
#' 
#' @return
#' Character vector of index names that exist in the connected database, or NULL 
#' if no indices are found.
#' @export
################################################################################

db_indices <- function(con)
{
  #Build query
  query <- "SELECT name FROM sqlite_master where type = 'index'"
  
  #Get name column
  res <- RSQLite::dbGetQuery(con, query)
  
  #Extract index values from type column
  index_names <- res$name
  
  return(index_names)
}
