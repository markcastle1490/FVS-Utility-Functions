################################################################################
#'fvs_gaak
#'@name fvs_gaak
#'@description
#
#'This function returns a FVS_GroupAddFilesAndKeyword (GAAK) table in a 
#'dataframe format with the appropriate FVS keywords and SQL statements for 
#'reading stand and tree level data from a specified input FVS database. This 
#'function can accommodate a GAAK table that can read from standard 
#'FVS_TreeInit, FVS_StandInit, and FVS_PlotInit tables and FIA-centric tables 
#'that include FVS_TreeInit_Plot, FVS_TreeInit_Cond, FVS_StandInit_Plot, 
#'FVS_StandInit_Cond, and FVS_PlotInit_Plot.
#
#'@param dbin:
#'Character string corresponding to filepath or name of database to read from.
#'File extension (.db, .sqlite) should be included for this value (e.g.
#'FVS_Data.db).
#'
#'@param stand_type:
#'Integer value that determine what stand ID will be used to read data from for
#'STANDSQL and TREESQL statements included in GAAK table.
#'
#'0 = Stand_CN (e.g. "WHERE Stand_CN = '%Stand_CN%'")
#'
#'1 = Stand_ID (e.g. "WHERE Stand_ID = '%StandID%'")
#
#'@param gaak_type:
#'Variable to determine what grouping codes are included in GAAK table.
#'
#'1 = Standard FVS grouping codes (All_Stands, All_Plots)
#'
#'2 = FIA grouping codes (All_FIA_Conditions, All_FIA_Plots, All_FIA_Subplots)
#'
#'3 = Both standard FVS grouping codes and FIA grouping codes
#
#'@return 
#'Dataframe containing FVS GAAK table.
################################################################################

#'@export
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
#db_tbl_schema
#
#This function takes in directory path to sqlite database and database table
#name an returns a named list of fields and associated data types for the
#specified database table.
#
#con:      Connection to SQLite database.
#
#db_table:  Character string pertaining to name of database table in db argument.
#
#Named list containing field names and associated data types for all fields in
#db_table.
################################################################################

db_tbl_schema <- function(con,
                          db_table = "")
{
  #Initialize empty vector
  data_types <- c()
  
  #If db_table does not exist in db, return empty vector
  if(RSQLite::dbExistsTable(conn = con,
                             name = db_table))
  {
    #Get name of fields and data_types
    table_defs <- RSQLite::dbGetQuery(con,
                                     paste0("PRAGMA table_info('",
                                            db_table,
                                            "')"))[,c(c("name", "type"))]
    
    #Make named vector from variables and data types
    data_types <- table_defs$type
    names(data_types) <- table_defs$name
  }
  
  return(data_types)
}

################################################################################
#db_collect_paths
#
#This function takes in a character vector of directory paths and file names to
#SQLite databases or zipped folder and returns an updated character vector of
#directory paths and file names to SQLite databases. The updated character 
#vector can contain additional .db paths.
#
#dbin:     Character vector containing directory paths and file names to SQLite 
#          database or zipped folders.
#
#unzipdir: Directory path to folder where contents of zipped folders will be 
#          stored.
#
#Character vector of directory paths and file names to SQLite databases.
################################################################################

db_collect_paths <- function(dbin = c(),
                             unzipdir = "")
{
  #If length of dbin is 0, return dbin
  if(length(dbin) <= 0) return(dbin)

  #If unzipdir exists, delete it
  if(file.exists(unzipdir)) unlink(unzipdir)
  
  #Create directory to unzip files too
  unzipdir <- paste(tempdir(),
                    "xxxfvstoolsdb_compileUnzipxxx",
                    sep = "/")
  
  #Initialize dbin_update. This is a vector that will be used to store input
  #directory paths.
  dbin_update <- vector(mode = "character")
  
  #Loop through dbin and check if files are not .db or .zip. If a file is a .zip
  #then unzip it to unzipdir. All db files will be added to dbin_update.
  for(i in 1:length(dbin))
  {
    db <- dbin[i]
    
    #Grab file extension for db
    fileext_in <- tools::file_ext(db)
    
    #If the file extension of db is not .db or .zip then stop with error message.
    if(!fileext_in %in% c("db", "zip", "sqlite"))
    {
      cat(db, "is not a zipped folder or sqlite database.", "\n")
      next
    }
    
    #If the file is a zip file, then it will be unzipped into xxxdb_compilexxx
    if(fileext_in == "zip")
    {

      cat("Unzipping:", db, "to", unzipdir, "\n", "\n")
      
      unzip(zipfile = db,
            exdir = unzipdir)
      
      #Now list all the files that contain .db or .sqlite in the name.
      #Recursive argument is set to true so any sub directories are checked for
      #db files as well.
      db_list <- c(list.files(unzipdir,
                           pattern = "\\.db",
                           full.names = T,
                           recursive = T),
                  list.files(unzipdir,
                             pattern = "\\.sqlite",
                             full.names = T,
                             recursive = T))
      
      #If db_list is empty move to next iteration of loop
      if(length(db_list) <= 0)
      {
        cat("No .db or .sqlite files found in", db, "\n")
        next
      }
      
      #If db_list has at least one value then append the values in db_list to
      #dbin_update.
      else
      {
        dbin_update <- c(dbin_update, db_list)
      }
    }
    
    #Dealing with .db file. This file will be appended to dbin_update.
    else
    {
      dbin_update <- c(dbin_update, db)
    }
  }
  
  #Delete unzipdir before returning
  unlink(unzipdir)
  
  return(dbin_update)
}

################################################################################
#create_tbl_query
#
#This function takes in a database connection to an existing SQLite database and
#database table name and returns a SQL query for creating the input database
#table.
#
#con:       Connection to SQLite database.
#
#db_table:  Character string corresponding to name of database table.
#
#db_fields: Named character vector where names of vector are field names and the 
#           value in each index is a data type.
#
#Character string of SQL query used to create database table.
################################################################################

create_tbl_query <- function(db_table = NULL,
                             db_fields = NULL)
{
  
  query <- ""
  
  if(!is.null(db_table) && !is.null(db_fields))
  {
    query <- paste(paste("CREATE TABLE", db_table),
                   paste0("(", paste(names(db_fields), 
                                     db_fields, 
                                     collapse = ",\n") ,")", ";"),
                   sep = "\n")
  }
    
  return(query)
}

################################################################################
#insert_tbl_query
#
#This function generates a SQLite query that is used to insert the specified
#fields from the database table of an existing SQLite database into the table of
#another SQLite databse.
#
#db_fields:  Character vector of field names to include in insert query.
#
#db_table:   Character string corresponding to name of database table.
#
#alias:     Character string corresponding to name of database alias.
#
#Character string of SQL query used insert data from one database table to
#another.
################################################################################

insert_tbl_query <- function(db_fields = NULL,
                             db_table = NULL,
                             alias = "dbinsert")
{
  query <- ""
  
  if(!is.null(db_fields) && !is.null(db_table))
  {
    #Create query
    query <- paste(paste("INSERT INTO", db_table),
                   paste0("(", paste(db_fields, collapse = ", "), ")"),
                   paste("SELECT", paste(db_fields, collapse = ", ")),
                   "FROM", paste0(alias, ".", db_table),
                   sep = "\n")
  }
  
  return(query)
}

################################################################################
#db_insert_tbl
#
#This function will insert the contents from specified databases tables of one 
#SQLite database into another SQLite database. This function is called from 
#db_compile function.
#
#dbout:        
#Character string of file path to output database who will be populated by 
#tables included in database of dbinsert argument.
#
#dbinsert:  
#File path to database whose contents will be inserted into dbout argument.
#
#db_tables:  
#Character vector of database table names for those that will be inserted from 
#dbinsert to dbout.
#
#Return
#None
################################################################################

db_insert_tbl <- function(dbout,
                          dbinsert,
                          db_tables = c(),
                          keep_casing = TRUE)
{
  #Connect to dbout (database information is being sent to)
  con_out <- RSQLite::dbConnect(RSQLite::SQLite(),
                                dbout)
  
  #Attach dbinsert to con_out
  RSQLite::dbExecute(con_out,
                     paste("attach database", 
                           paste0("'", dbinsert,"'"), 
                           "as dbinsert"))
  
  #Begin loop across db_tables
  for(table in db_tables)
  {
    
    cat("Processing table:", table, "\n")
    
    #Connect to dbinsert
    con_in <- RSQLite::dbConnect(RSQLite::SQLite(),
                                 dbinsert)
    
    #Skip to next database table if table does not exist in dbinsert
    if(!RSQLite::dbExistsTable(con = con_in,
                               name = table))
    {
      cat("Table:", table, "not found in:", dbinsert, "\n", "\n")
      RSQLite::dbDisconnect(con_in)
      next
    }
    
    #Get database fields and types for dbinsert
    insert_fields <- db_tbl_schema(con = con_in,
                                   db_table = table)
    
    #Disconnect from dbinsert
    RSQLite::dbDisconnect(con_in)
    
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
        cat("Invalid table creation query.", "\n")
        next
      }
      
      else 
      {
        RSQLite::dbExecute(conn = con_out, query)
        #cat("Created table:", table, "in:", dbout, "\n", "\n")
      }
    }
    
    #Check for fields that DO NOT exist in table within dbout
    db_fields <- RSQLite::dbListFields(con_out,
                                       name = table)
    
    missing_fields <- names(insert_fields)[! names(insert_fields) %in% db_fields]
    missing_fields <- insert_fields[missing_fields]
    
    #Loop through missing_fields and add to database table in 
    if(length(missing_fields) > 0)
    {
      db_add_fields(conn = con_out,
                    table_name = table,
                    db_fields = missing_fields)
    }
    
    #Generate insert query
    query <- insert_tbl_query(db_fields = names(insert_fields),
                              db_table = table)
    
    #If query is invalid move to next iteration
    if(query == "") 
    { 
      cat("Invalid insertion query created.")
      next
    }
    
    RSQLite::dbExecute(con_out,
                       query)
  }
  
  #Detach dbinsert and then disconnect from dbout
  RSQLite::dbExecute(con_out,
                     "DETACH DATABASE dbinsert;")
  RSQLite::dbDisconnect(con_out)
  
  invisible(0)
}

################################################################################
#'db_add_fields
#'@name db_add_fields
#'@description
#'
#'This function adds specified fields with corresponding data types to a 
#'database table in open SQLite database connection.
#
#'@param conn:       
#'Connection to SQLite database.
#
#'@param table_name:  
#'Character string corresponding to name of table where fields will added.
#
#'@param db_fields:    
#'Named character vector or list where names are the names of the fields and 
#'the items in the vector or list are the data types of the fields.
#
#'@return 
#'None
################################################################################

#'@export
db_add_fields <- function(conn,
                          table_name,
                          db_fields = c())
{
  #If db_fields or data_types is empty, stop
  if(length(db_fields) <= 0)
  {
    stop("No fields and data types provided.")
  }
  
  cat("\n",
      "Fields missing from", table_name, "\n", names(db_fields), "\n", "\n")
  
  for(i in 1:length(db_fields))
  {
    #Extract field
    field <- names(db_fields)[[i]]
    
    #Extract data type of field
    data_type <- db_fields[[i]]
    
    #cat("Field:", field, "data_type:", data_type, "\n")
    
    cat("Adding field:", field, paste0("(", data_type, ")"), "to table:",
        table_name,
        "\n")
    
    #Create query to alter table and add field in con_out
    add_field <- paste("ALTER TABLE", table_name, "ADD COLUMN", field, data_type)
    
    #Add field to con_out
    RSQLite::dbExecute(conn = conn, 
                       statement = add_field)
    
    cat("Field:", field, "added to table:", table_name, "\n", "\n")
  }
}

################################################################################
#'db_compile
#'@name db_compile
#'@description
#'This function is used to combine the contents of multiple sqlite databases 
#'into a single sqlite database. SQLite databases (.db, .sqlite) are the only
#'compatible input database type that can be processed in this function. The 
#'primary purpose of this function is to combine input FVS databases into a 
#'single database or extract FVS database tables from a larger database such as
#'those on the FIA datamart.
#
#'@param dbin:         
#'Character vector of directory paths and file names for SQLite databases to 
#'process. Files can either be a SQLite database (.db) or zipped folder (.zip) 
#'which contains a SQLite database(s).
#'
#'NOTE: .zip files will be unzipped to a temporary folder called 
#'xxxfvstoolsdb_compileUnzipxxx in current working directory. Temporary folder will
#'be deleted after db_compile has finished writing data to output database.
#'
#'Examples of valid dbin formats:
#'"C:/FIA2FVS_Databases/SQLite_FIADB_AZ/FIADB_AZ.db"'
#'"C:/FIA2FVS_Databases/SQLite_FIADB_AZ/ FIADB_AZ.zip"
#
#'@param dbout:
#'Character string corresponding to SQLite database to write out to.
#'Examples of valid dbout formats:
#'"C:/FIA2FVS_Databases/SQLite_FIADB_AZ/FVS_Data.db"
#
#'@param db_tables:    
#'Character vector of database tables to process from argument dbin. If this
#'argument is left as NULL, then function will use all tables from the first
#'database specified in the dbin argument.
#
#'@param delete_input: 
#'Logical variable used to determine if values in dbin should be deleted after
#'db_compile has been called. The primary purpose of this argument is to 
#'conserve hard disk space for users who do not want to retain the input 
#'databases specified in dbin.
#'
#'@param keep_casing: 
#'Logical variable used to determine if the database table names and fields in
#'dbin should retain original casing. When FALSE, the database table names and
#'fields in each table written to dbout will be capitalized.
#'
#'@return 
#'None
################################################################################

#'@export
db_compile <- function(dbin = NULL,
                       dbout = NULL,
                       db_tables = NULL,
                       delete_input = FALSE,
                       keep_casing = TRUE)
{
  
  #Test if no values have been specified for dbin
  if(is.null(dbin)) stop(paste("No files were specified for dbin."))
  
  #Test if no values have been specified for dbout
  if(is.null(dbout)) stop(paste("No file was specified for dbout."))
  
  #Test if db_tables is null and return with error message.
  #if(is.null(db_tables)) stop(paste("No table names were provided for db_tables."))
  
  #Replace \\ with / in dbin and dbout
  dbin <- chartr(old = "\\", new = "/", x = dbin)
  dbout <- chartr(old = "\\", new = "/", x = dbout)
  
  #Loop through dbin and test if any of the files don't exist. If a file does
  #not exist then error message is reported.
  for(i in 1:length(dbin))
  {
    if(!file.exists(dbin[i])) stop(paste("File:", dbin[i], "does not exist."))
    #else cat("Database", i, dbin[i], "\n")
  }
  
  #If there is more than one value specified in dbout, stop with error message.
  if(length(dbout) > 1)
  {
    stop(paste("Only one output file can be specified for dbout."))
  }
  
  #Test if dbout file path is valid.
  #Extract path to dbout by extracting all characters before the last / in
  #output argument.
  outpath <- gsub("/[^/]+$", "", dbout)
  
  #Test existence of output path and if it does not exist report error.
  if (!(file.exists(outpath))){
    stop(paste("Path to output:", outpath, "was not found.",
               "Make sure directory path to output is spelled correctly."))
  }
  
  #Test if output file is a SQLite database. If the file is not a SQLite
  #database then error message is reported.
  fileext_out <- tools::file_ext(dbout)
  if(!fileext_out %in% c("db", "sqlite"))
  {
    stop(paste("Output database:",
               dbout,
               "is not a SQLite database.",
               "\n"))
  }
  
  #If dbout already exists, delete it
  if(file.exists(dbout))
  {
    cat("Deleting preexisting dbout", "\n")
    ret <- unlink(dbout)
    if(ret == 1) stop(paste("Failed to delete:", dbout))
  }
  
  cat("Output database:", dbout, "\n","\n")
  
  #Get updated directory paths and file names
  dbin_update <- db_collect_paths(dbin = dbin)
  
  #If dbin_update does not have any databases, then stop with error message and
  #delete unzip directory if it exists.
  if(length(dbin_update) <= 0)
  {
    stop("No valid database files (.db, .sqlite) are available for processing.")
  }
  
  #Remove duplicate values in dbin_update and print database file paths
  dbin_update <- unique(dbin_update)
  cat("List of db files to process:", "\n")
  cat(paste(dbin_update, collapse = "\n"), "\n", "\n")
  
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
  {
    stop("No valid database tables available for processing.")
  }
  
  #Capitalize db_tables if keep_casing is off
  if(!keep_casing) db_tables <- toupper(db_tables)
  
  cat("Database table names to consider:", "\n")
  cat(paste(db_tables, collapse = "\n"), "\n", "\n")
  
  #Begin processing databases in dbin_update
  for(i in 1:length(dbin_update))
  {
    
    db <- dbin_update[i]
    
    cat("Processing db:", db, "\n")
    
    db_insert_tbl(dbout = dbout,
                   dbinsert = db,
                   db_tables = db_tables,
                   keep_casing = keep_casing)
    
    #Print message indicating which db has been processed.
    cat("Finished processing db:", db, "\n", "\n")
  }
  
  #If delete_input is TRUE, delete files in dbin argument.
  if(delete_input)
  {
    
    cat(paste("Argument delete_input is TRUE.",
              "Deleting input databases.", "\n"))
    
    ret <- unlink(x = dbin,
                  recursive = FALSE)
    
    if(ret == 1) 
      cat("Failed to delete one or more databases from dbin.")
  }
  
  invisible()
}

################################################################################
#'db_indices
#'@name db_indices
#'@description
#'This function returns the names of indices that exist in input database
#'argument.
#
#'@param input
#'Connection to a SQLite database (.db, .sqlite)
#
#'@return
#'Character vector of index names that exist in input argument.
################################################################################

#'@export
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
