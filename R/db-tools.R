################################################################################
#'fvs_gaak
#'@name fvs_gaak
#'@description
#
#'This function creates a FVS_GroupAddFilesAndKeyword (GAAK) table with the
#'appropriate SQL statements for a variety of grouping codes.
#
#'@param dbin:
#'Character string corresponding to filepath or name of database to read from.
#'File extension (.db, .sqlite) should be included for this value (e.g.
#'FVS_Data.db).
#'
#'@param standType:
#'Integer value that determine what stand ID will be used to read data from.
#'
#'0 = Stand_CN
#'
#'1 = Stand_ID
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
#'Dataframe containing FVS GAAK table
################################################################################

fvs_gaak<-function(dbin ="FVS_Data.db",
                   standType = 1,
                   gaak_type = 2)
{
  #Capture invalid gaak_type argument values
  if(!gaak_type %in% c(1, 2, 3)) gaak_type = 2
  
  #Catch bad standType values
  if(!standType %in% c(0, 1)) standType <- 1
  
  #Set the stand column to read data from
  if(standType == 1) 
  {
    standRead <- "WHERE Stand_ID = '%StandID%'"
  }
  
  else 
  {
    standRead <- "WHERE Stand_CN = '%Stand_CN%'"
  }
  
  #Create dataframe containing FVS_GroupAddfilesAndKeywords table
  gaak<-data.frame(GROUPS = c("All_Stands","All_Plots","All_FIA_Conditions",
                              "All_FIA_Plots", "All_FIA_Subplots"),
                   ADDFILES = c("","","","",""),
                   FVSKEYWORDS = c(paste("DATABASE", 
                                         "DSNIN",
                                         dbin,
                                         "STANDSQL",
                                         "SELECT *", 
                                         "FROM FVS_StandInit",
                                         standRead,
                                         "ENDSQL",
                                         "TREESQL", 
                                         "SELECT *",
                                         "FROM FVS_TreeInit",
                                         standRead,
                                         "ENDSQL", 
                                         "END", sep = "\n"),
                                   paste("DATABASE", 
                                         "DSNIN",
                                         dbin,
                                         "STANDSQL",
                                         "SELECT *",
                                         "FROM FVS_PlotInit",
                                         standRead,
                                         "ENDSQL",
                                         "TREESQL",
                                         "SELECT *",
                                         "FROM FVS_TreeInit",
                                         standRead,
                                         "ENDSQL",
                                         "END", sep = "\n"),
                                   paste("DATABASE", 
                                         "DSNIN",
                                         dbin, 
                                         "STANDSQL",
                                         "SELECT *", 
                                         "FROM FVS_StandInit_Cond",
                                         standRead,
                                         "ENDSQL",
                                         "TREESQL",
                                         "SELECT *",
                                         "FROM FVS_TreeInit_Cond",
                                         standRead,
                                         "ENDSQL",
                                         "END", sep = "\n"),
                                   paste("DATABASE", 
                                         "DSNIN",
                                         dbin,
                                         "STANDSQL", 
                                         "SELECT *",
                                         "FROM FVS_StandInit_Plot",
                                         standRead,
                                         "ENDSQL",
                                         "TREESQL",
                                         "SELECT *",
                                         "FROM FVS_TreeInit_Plot",
                                         standRead,
                                         "ENDSQL", 
                                         "END", sep = "\n"),
                                   paste("DATABASE", 
                                         "DSNIN",
                                         dbin,
                                         "STANDSQL",
                                         "SELECT *", 
                                         "FROM FVS_PlotInit_Plot",
                                         standRead,
                                         "ENDSQL",
                                         "TREESQL",
                                         "SELECT *",
                                         "FROM FVS_TreeInit_Plot",
                                         standRead,
                                         "ENDSQL", 
                                         "END", sep = "\n")))
  
  #GAAK with just FVS grouping codes
  if(gaak_type == 1)
  {
    gaak <- gaak[1:2,]
  }
  
  #GAAK with just FIA grouping codes
  if(gaak_type == 2)
  {
    gaak <- gaak[3:5,]
  }
  
  return(gaak)
}

################################################################################
#'delete_files
#'@name delete_files
#'@description
#'
#'This function takes in a character vector containing directory paths and file
#'names and deletes each file if they exist.
#
#'@param files: 
#'Character vector of directory paths and file names that will be deleted if 
#'they exist.
#
#'@param recur: 
#'Logical variable used to signal if recursive deletion should occur if TRUE. 
#'By default, this argument is set to FALSE.
#
#'@return
#'Integer value of 0.
################################################################################

#'@export
delete_files <- function(files = c(),
                         recur = FALSE)
{
  #Ensure files is character
  files <- as.character(files)
  
  #Delete files if values are provided
  if(length(files) > 0)
  {
    #Loop across files and delete
    for(file in files)
    {
      if(file.exists(file))
      {
        retcode <- unlink(x = file,
                          recursive = recur)
        
        if(retcode == 0)
        {
          cat(file, "was sucessfully deleted.", "\n")
        }
        
        else
        {
          cat(file, "was not sucessfully deleted.", "\n")
        }
      }
    }
  }
  
  return()
}

################################################################################
#db_get_data_types
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

db_get_data_types <- function(con,
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
                    "xxxfvsUtildb_compileUnzipxxx",
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
    fileext_in<-sub("(.*)\\.","",db)
    
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
#db_create_table_query
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
##alias:    Character string corresponding to name of database alias.
#
#Character string of SQL query used to create database table.
################################################################################

db_create_table_query <- function(db_table,
                                  db_fields = c(),
                                  alias = "dbinsert")
{
  
  query <- ""
  
  if(length(db_fields) > 0 )
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
#db_insert_query
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

db_insert_query <- function(db_fields,
                            db_table,
                            alias = "dbinsert")
{
  query <- ""
  
  if(length(db_fields) > 0)
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
#db_insert_tables
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

db_insert_tables <- function(dbout,
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
    insert_types <- db_get_data_types(con = con_in,
                                      db_table = table)
    
    #Disconnect from dbinsert
    RSQLite::dbDisconnect(con_in)
    
    #Capitalize fields if keep_casing is off
    if(!keep_casing) names(insert_types) <- toupper(names(insert_types)) 
    
    #if db_table does not exist in dbout, create the table
    if(!RSQLite::dbExistsTable(conn = con_out,
                               name = table))
    {
      query <- db_create_table_query(db_table = table,
                                  db_fields = insert_types)
      
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
    
    missing_fields <- names(insert_types)[! names(insert_types) %in% db_fields]
    missing_fields <- insert_types[missing_fields]
    
    #Loop through missing_fields and add to database table in 
    if(length(missing_fields) > 0)
    {
      db_add_fields(conn = con_out,
                    table_name = table,
                    db_fields = missing_fields)
    }
    
    #Now Check for fields that do exist in table in dbout but DO NOT exist in
    #dbinsert
    
    #Get database fields and types for dbout
    out_types <- db_get_data_types(con = con_out,
                                db_table = table)
     
    missing_fields <- names(out_types)[! names(out_types) %in% 
                                         names(insert_types)]
    missing_fields <- out_types[missing_fields]
    
    #Loop through missing_fields and add to database table in dbinsert
    if(length(missing_fields) > 0)
    {
      con_in <- RSQLite::dbConnect(RSQLite::SQLite(),
                                dbinsert)
      
      db_add_fields(conn = con_in,
                  table_name = table,
                  db_fields = missing_fields)
      
      RSQLite::dbDisconnect(con_in)
    }
    
    #Get updated field names
    db_fields <- RSQLite::dbListFields(conn = con_out,
                                        name = table)
    
    #Generate insert query
    query <- db_insert_query(db_fields = db_fields,
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
#'xxxfvsUtildb_compileUnzipxxx in current working directory. Temporary folder will
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
  dbin <- gsub("\\\\", "/", dbin)
  dbout <- gsub("\\\\", "/", dbout)
  
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
  fileext_out<-sub("(.*)\\.","",dbout)
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
    
    db_insert_tables(dbout = dbout,
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
    delete_files(files = dbin,
                recur = FALSE)
  }
  
  invisible()
}

################################################################################
#'db_compile_v2
#'@name db_compile_v2
#'@description
#'This function is used to read in database tables from input FVS-ready data 
#'sets and write the database tables from each of these to a single output 
#'SQLite database. SQLite databases (.db, .sqlite) are the only compatible input 
#'database type that can be processed in this function. The primary purpose of 
#'this function is to combine input FVS databases into a single database or 
#'extract FVS database tables from a larger database such as those on the FIA
#'datamart.
#
#'@param dbin:         
#'Character vector of directory paths and file names for SQLite databases to 
#'process. Files can either be a SQLite database or zipped folder (.zip) 
#'which contains a SQLite database(s).
#'
#'NOTE: .zip files will be unzipped to a temporary folder called 
#'xxxfvsUtildb_compileUnzipxxx in a temporary directory. The temporary folder 
#'will be deleted after db_compile has finished writing data to output database.
#'
#'Examples of valid dbin formats:
#'"C:/FIA2FVS_Databases/SQLite_FIADB_AZ/FIADB_AZ.db"'
#'"C:/FIA2FVS_Databases/SQLite_FIADB_AZ/ FIADB_AZ.zip"
#
#'@param dbout:
#'Character string corresponding to SQLite database to write out to.
#'Examples of valid dbout formats:
#'"C:/FIA2FVS_Databases/SQLite_FIADB_AZ/FVS_Data.db",
#
#'@param db_tables:    
#'Character vector of database tables to process from argument dbin.
#'By default this argument contains the following values:
#'
#'"FVS_STANDINIT"
#'
#'"FVS_TREEINIT"
#'
#'"FVS_PLOTINIT"
#'
#'"FVS_STANDINIT_PLOT"
#'
#'"FVS_STANDINIT_COND"
#'
#'"FVS_PLOTINIT_PLOT"
#'
#'"FVS_TREEINIT_PLOT"
#'
#'"FVS_TREEINIT_COND"
#
#'@param build_gaak:   
#'Logical variable used to determine if FVS_GROUPADDFILESANDKEYWORDS will be 
#'#written to dbout. If TRUE, this table will be written to dbout. By default,
#'this argument is set to TRUE.
#
#'@param gaak_type:    
#'Integer value from 1 - 3 used to determine what kind of GAAK table will be 
#'written to dbout if build_gaak is TRUE.
#'
#'1 = GAAK table with All_Stands and All_Plots grouping codes.
#'
#'2 = GAAK table with All_FIA_Conditions, All_FIA_Plots, All_FIA_Subplots 
#'    grouping codes.
#'      
#'3 = GAAK table with All_Stands, All_Plots, All_FIA_Conditions, 
#'    All_FIA_Plots, All_FIA_Subplots grouping codes. For more information 
#'    refer to fvs_gaak function.
#
#'@param delete_input: 
#'Logical variable used to determine if values in dbin should be deleted after
#'db_compile has been called. By default this argument is set to FALSE. Be 
#'careful with this argument. The primary purpose of this argument is to 
#'conserve hard disk space for users who do not want the input databases 
#'specified in dbin.
#
#'@param read_chunks:  
#'Logical variable used to determine if data from database table should be read
#'in chunks. In general, processing time of db_compile increases but less RAM is
#'used at one time in R session if this argument is TRUE.
#
#'@param rows_to_read:  
#'Integer value corresponding to number of rows to read from a database table if
#'read_chunks is TRUE.
#
#'@return 
#'Character string indicating that database has been created.
################################################################################

#'@export
db_compile_v2 <- function(dbin = NULL,
                          dbout = NULL,
                          db_tables = c("FVS_STANDINIT",
                                     "FVS_TREEINIT",
                                     "FVS_PLOTINIT",
                                     "FVS_STANDINIT_PLOT",
                                     "FVS_STANDINIT_COND",
                                     "FVS_PLOTINIT_PLOT",
                                     "FVS_TREEINIT_PLOT",
                                     "FVS_TREEINIT_COND"),
                          build_gaak = T,
                          gaak_type = 2,
                          delete_input = F,
                          read_chunks = F,
                          rows_to_read = 5000)
{
  
  #Test if no values have been specified for dbin
  if(is.null(dbin))
  {
    stop(paste("No files were specified for dbin."))
  }
  
  #Test if no values have been specified for dbout
  if(is.null(dbout))
  {
    stop(paste("No file was specified for dbout."))
  }
  
  #Test if db_tables is null and return with error message.
  if(is.null(db_tables))
  {
    stop(paste("No table names were provided for db_tables."))
  }
  
  #Print database tables to consider
  else
  {
    #Capitalize db_tables
    db_tables <- toupper(db_tables)
    
    cat("Database table names to consider:",
        db_tables,
        "\n")
  }
  
  #Catch erroneous gaak_type values
  if(gaak_type < 1 | gaak_type > 3) gaak_type = 2
  
  #Report error message if rows_to_read is less than or equal to 0
  if(read_chunks)
  {
    rows_to_read <- as.integer(rows_to_read)
    if(rows_to_read <= 0)
    {
      stop(paste("Value for rows_to_read needs to be integer value greater than",
                 "zero."))
    }
  }
  
  #Replace \\ with / in dbin and dbout
  dbin <- gsub("\\\\", "/", dbin)
  dbout <- gsub("\\\\", "/", dbout)
  
  #Loop through dbin and test if any of the files don't exist. If a file does
  #not exist then error message is reported.
  for(i in 1:length(dbin))
  {
    if(!file.exists(dbin[i]))
    {
      stop(paste("File:",
                 dbin[i],
                 "does not exist."))
    }
    
    else
    {
      cat("Database", i, dbin[i], "\n")
    }
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
  fileext_out<-sub("(.*)\\.","",dbout)
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
    cat(paste0("\n","Deleting preexisting dbout"), "\n")
    unlink(dbout,
           force = T)
  }
  
  cat("Output database:", dbout, "\n","\n")
  
  #Get updated directory paths and file names
  dbin_update <- db_collect_paths(dbin = dbin)
  
  #If dbin_update does not have any databases, then stop with error message and
  #delete unzip directory if it exists.
  if(length(dbin_update) <= 0)
  {
    #Check if unzipdir exists. If it does, delete it.
    if(file.exists(unzipdir))
    {
      
      delete_files(files = unzipdir,
                  recur = TRUE)
    }
    
    stop("No valid database files (.db, .sqlite) are available for processing.")
  }
  
  #Remove duplicate values in dbin_update and print database file paths
  dbin_update <- unique(dbin_update)
  cat("List of db files to process:", "\n")
  cat(paste(dbin_update, collapse = "\n"))
  
  #Begin processing databases in dbin_update
  for(i in 1:length(dbin_update))
  {
    
    db <- dbin_update[i]
    
    cat("\n")
    cat("Processing db:", db, "\n", "\n")
    
    #Begin processing db_tables in db
    for(j in 1:length(db_tables))
    {
      #Extract table name
      table_name <- db_tables[j]
      cat("Processing table:",
          table_name,
          "\n")
      
      #Connect to db
      con_in <- RSQLite::dbConnect(RSQLite::SQLite(), db)
      
      #Test if table does not exist in db. if this is the case move to next
      #iteration of loop.
      if(!table_name %in% toupper(RSQLite::dbListTables(con_in)))
      {
        cat("Table:",
            table_name,
            "was not found in database.",
            "\n", "\n")
        #Disconnect from con_in
        RSQLite::dbDisconnect(con_in)
        next
      }
      
      #Determine number of rows in table_name
      query <- paste("SELECT COUNT(*) FROM", table_name)
      num_rows <- RSQLite::dbGetQuery(con_in,
                                     query)[[1]]
      
      #If there are no rows (i.e. no data) in db_table, skip to next iteration
      #of loop.
      if(num_rows <= 0)
      {
        cat("No data found in",
            table_name,
            "\n", "\n")
        
        #Disconnect from con_in
        RSQLite::dbDisconnect(con_in)
        next
      }
      
      #Disconnect from con_in
      RSQLite::dbDisconnect(con_in)
      
      #If read_chunks is FALSE, call add_db_table, otherwise call add_db_rows.
      if(!read_chunks)
      {
        add_db_table(db,
                   dbout,
                   table_name)
      }
      else
      {
        add_db_rows(db,
                  dbout,
                  table_name,
                  rows_to_read,
                  num_rows)
      }
    }
    
    #Print message indicating which db has been processed.
    cat("Finished processing db:",
        db,
        "\n")
  }
  
  #Determine if GAAK table should be written to dbout.
  if(build_gaak)
  {
    con_out <- RSQLite::dbConnect(RSQLite::SQLite(),
                                 dbout)
    
    cat("Writing fvs_gaak table to",
        dbout,
        "\n",
        "\n")
    
    RSQLite::dbWriteTable(conn = con_out,
                          name = "FVS_GROUPADDFILESANDKEYWORDS",
                          value = fvs_gaak(type = gaak_type),
                          overwrite = T)
    
    #Disconnect from con_out
    RSQLite::dbDisconnect(con_out)
  }
  
  #If delete_input is TRUE, delete files in dbin argument.
  if(delete_input)
  {
    
    cat(paste("Argument delete_input is TRUE.",
              "Deleting input databases.", "\n"))
    delete_files(files = dbin,
                recur = FALSE)
  }
  
  invisible(0)
}

################################################################################
#add_db_table
#
#This function sends an entire database table (expressed as dataframe) to
#output SQLite database.
#
#
#db:         File path to input database.
#
#dbout:      File path to output SQLite database.
#
#table_name:  Name of database table in db being sent to dbout.
#
#Return value
#
#None
################################################################################

add_db_table<-function(db,
                       dbout,
                       table_name)
{
  
  #Connect to input database (db)
  con_in <- RSQLite::dbConnect(RSQLite::SQLite(),
                              db)
  
  #Read in the db_table table (table_name)
  db_table <- RSQLite::dbReadTable(con_in,
                                  name = table_name)
  
  #Capitalize column headers
  colnames(db_table) <- toupper(colnames(db_table))
  
  #Get column data types from table_name
  table_types <-db_get_data_types(con_in,
                              table_name)
  
  #Disconnect from con_in
  RSQLite::dbDisconnect(con_in)
  
  #Connect to dbout
  con_out <- RSQLite::dbConnect(RSQLite::SQLite(),
                               dbout)
  
  #Test if table_name exists in con_out. If it does, this db_table will be
  #appended to the existing table in output (con_out).
  if(table_name %in% toupper(RSQLite::dbListTables(con_out)))
  {
    #Identify any fields in db_table that are missing from the same data table
    #in con_out.
    db_fields <- RSQLite::dbListFields(con_out,
                                      name = table_name)
    
    missing_fields <- names(db_table)[! names(db_table) %in% db_fields]
    
    #Loop through missing_fields and add to database table in con_out
    if(length(missing_fields) > 0)
    {
      cat("\n",
          "Fields missing from",
          table_name,
          "in",
          dbout,
          "\n",
          missing_fields, "\n", "\n")
      
      for(i in 1:length(missing_fields))
      {
        #Extract field
        field <- missing_fields[i]
        
        #Extract data_type of field
        data_type <- table_types[names(table_types) == field]
        cat("Field:", field, "data_type:", data_type, "\n")
        
        cat("Adding field:",
            field,
            paste0("(", data_type, ")"),
            "to table:",
            table_name,
            "\n")
        
        #Create query to alter table and add field in con_out
        add_field <-paste("ALTER TABLE",
                         table_name,
                         "ADD COLUMN",
                         field,
                         data_type)
        
        #Add field to con_out
        RSQLite::dbExecute(con_out, add_field)
        
        cat("Field:",
            field,
            "added to table:",
            table_name,
            "\n", "\n")
      }
    }
    
    cat("Appending",
        table_name,
        "to",
        dbout,
        "\n")
    
    #Append data to con_out
    RSQLite::dbWriteTable(conn = con_out,
                          name = table_name,
                          value = db_table,
                          append = T)
    
    cat(table_name,
        "appended to",
        dbout,
        "\n",
        "\n")
  }
  
  #Table will be created in con_out and data will then be written to the table.
  else
  {
    cat("Writing",
        table_name,
        "to",
        dbout,
        "\n")
    
    #Create the db_table in con_out and write information from db_table to it.
    RSQLite::dbWriteTable(conn = con_out,
                          name = table_name,
                          value = db_table,
                          overwrite = T,
                          field.types = table_types)
    
    cat(table_name,
        "written to",
        dbout,
        "\n",
        "\n")
  }
  
  #Delete db_table
  rm(db_table)
  
  #Disconnect from con_out
  RSQLite::dbDisconnect(con_out)
  
  invisible(0)
}

################################################################################
#add_db_rows
#
#This function incrementally sends portions of a database table (expressed as
#dataframe) to output SQLite database.
#
#
#db:         Directory path to input SQLite database.
#
#dbout:      Directory path to output SQLite database.
#
#table_name:  Name of database table being sent from db to dbout.
#
#num_to_read:  Number of rows to read in from database table at a time.
#
#num_rows:    Number of rows in argument table_name.
#
#Return value
#
#None
################################################################################

add_db_rows<-function(db,
                      dbout,
                      table_name,
                      num_to_read,
                      num_rows)
{
  
  #Variable to signify when read of data from table_name in db is complete
  done_reading <- F
  
  #Lower value of rows to read from
  lower <- 0
  
  #Upper value of rows to read from. Upper value is only used in messages sent
  #to console.
  upper <- 0
  
  #Variable used to keep track of number of rows that have been processed
  rows_done <- 0
  
  #Variable to indicate whether first pass is complete.
  first_pass <- T
  
  while(!done_reading)
  {
    # If this is the first pass, set lower to 1 and upper to num_to_read. Then
    #set first_pass to F.
    if(first_pass)
    {
      upper <- num_to_read
      first_pass <- F
    }
    
    #If this is not the first pass then set lower to lower + num_to_read and
    #upper to upper + num_to_read
    else
    {
      lower <- lower + num_to_read
      upper <- upper + num_to_read
    }
    
    #If upper is greater than or equal to num_rows, set num_to_read to
    #num_rows - rows_done and set done_reading to T. This will signify that
    #function is about to make the last read from db.
    if(upper >= num_rows)
    {
      num_to_read <- num_rows - rows_done
      upper <- num_rows
      done_reading <- T
    }
    
    #Setup query for reading data
    query <- paste("SELECT * FROM",
                   table_name,
                   "LIMIT",
                   paste0(lower,",", num_to_read))
    
    cat("Row query:",
        query,
        "\n")
    
    #Display what rows are being read from database table.
    cat("Reading rows:",
        lower + 1,
        "through",
        upper,
        "from",
        table_name, "\n")
    
    #Connect to db
    con_in <- RSQLite::dbConnect(RSQLite::SQLite(),
                                db)
    
    #Read the data
    db_table <- RSQLite::dbGetQuery(con_in,
                                   query)
    
    #Get column data types from table_name
    table_types <-db_get_data_types(con_in,
                                table_name)
    
    #Disconnect from db
    RSQLite::dbDisconnect(con_in)
    
    #Determine number of rows read in current pass
    rows_read <- nrow(db_table)
    
    #Print number of rows in db_table
    cat("Number of rows in read from database:",
        rows_read,
        "\n")
    
    #Capitalize column headers
    colnames(db_table) <- toupper(colnames(db_table))
    
    #Connect to dbout
    con_out <- RSQLite::dbConnect(RSQLite::SQLite(),
                                 dbout)
    
    #Test if table_name exists in con_out. If it does, this db_table will be
    #appended to the existing table in output (con_out).
    if(table_name %in% toupper(RSQLite::dbListTables(con_out)))
    {
      #Identify any fields in db_table that are missing from the same data table
      #in con_out.
      db_fields <- RSQLite::dbListFields(con_out,
                                          name = table_name)
      
      missing_fields <- names(db_table)[! names(db_table) %in% db_fields]
      
      #Loop through missing_fields and add to database table in con_out
      if(length(missing_fields) > 0)
      {
        cat("\n",
            "Fields missing from",
            table_name,
            "in",
            dbout,
            "\n",
            missing_fields, "\n", "\n")
        
        for(i in 1:length(missing_fields))
        {
          #Extract field
          field <- missing_fields[i]
          
          #Extract data_type of field
          data_type <- table_types[names(table_types) == field]
          cat("Field:", field, "data_type:", data_type, "\n")
          
          cat("Adding field:",
              field,
              paste0("(", data_type, ")"),
              "to table:",
              table_name,
              "\n")
          
          #Create query to alter table and add field in con_out
          add_field <-paste("ALTER TABLE",
                           table_name,
                           "ADD COLUMN",
                           field,
                           data_type)
          
          #Add field to con_out
          RSQLite::dbExecute(con_out, add_field)
          
          cat("Field:",
              field,
              "added to table:",
              table_name,
              "\n", "\n")
        }
      }
      
      cat("Appending rows", lower + 1, "through", upper, "from",
          table_name,
          "to",
          dbout,
          "\n")
      
      #Append data to con_out
      RSQLite::dbWriteTable(conn = con_out,
                            name = table_name,
                            value = db_table,
                            append = T)
      
      cat("Rows", lower + 1, "through", upper, "from", table_name,
          "appended to",
          dbout,
          "\n")
    }
    
    #Table will be created in con_out and data will then be written to the
    #table.
    else
    {
      
      cat("Writing rows", lower + 1, "through", upper, "from",
          table_name,
          "to",
          dbout,
          "\n")
      
      #Create the db_table in con_out and write information from db_table to it.
      RSQLite::dbWriteTable(conn = con_out,
                            name = table_name,
                            value = db_table,
                            overwrite = T,
                            field.types = table_types)
      
      cat("Rows", lower + 1, "through", upper, "from", table_name,
          "written to",
          dbout,
          "\n")
    }
    
    #Update rows_done
    rows_done <- rows_done + rows_read
    
    #Print number of rows processed
    cat("Number of rows processed:",
        rows_done,
        "\n",
        "\n")
    
    #Delete db_table
    rm(db_table)
    
    #Disconnect from dbout
    RSQLite::dbDisconnect(con_out)
  }
  
  invisible(0)
}