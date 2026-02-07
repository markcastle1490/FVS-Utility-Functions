################################################################################
#'fvs_gaak
#'@name fvs_gaak
#'@description
#
#'This function creates a FVS_GroupAddFilesAndKeyword (GAAK) table with the
#'appropriate SQL statements for a variety of grouping codes.
#
#'@param dbIn:
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
#'@param gaakType:
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

fvs_gaak<-function(dbIn ="FVS_Data.db",
                   standType = 1,
                   gaakType = 2)
{
  #Capture invalid gaakType argument values
  if(!gaakType %in% c(1, 2, 3)) gaakType = 2
  
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
                                         dbIn,
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
                                         dbIn,
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
                                         dbIn, 
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
                                         dbIn,
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
                                         dbIn,
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
  if(gaakType == 1)
  {
    gaak <- gaak[1:2,]
  }
  
  #GAAK with just FIA grouping codes
  if(gaakType == 2)
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
        retCode <- unlink(x = file,
                          recursive = recur)
        
        if(retCode == 0)
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
#dbTable:  Character string pertaining to name of database table in db argument.
#
#Named list containing field names and associated data types for all fields in
#dbTable.
################################################################################

db_get_data_types <- function(con,
                           dbTable = "")
{
  #Initialize empty vector
  dataTypes <- c()
  
  #If dbTable does not exist in db, return empty vector
  if(RSQLite::dbExistsTable(conn = con,
                             name = dbTable))
  {
    #Get name of fields and datatypes
    tableDefs <- RSQLite::dbGetQuery(con,
                                     paste0("PRAGMA table_info('",
                                            dbTable,
                                            "')"))[,c(c("name", "type"))]
    
    #Make named vector from variables and data types
    dataTypes <- tableDefs$type
    names(dataTypes) <- tableDefs$name
  }
  
  return(dataTypes)
}

################################################################################
#db_collect_paths
#
#This function takes in a character vector of directory paths and file names to
#SQLite databases or zipped folder and returns an updated character vector of
#directory paths and file names to SQLite databases. The updated character 
#vector can contain additional .db paths.
#
#dbIn:     Character vector containing directory paths and file names to SQLite 
#          database or zipped folders.
#
#unzipDir: Directory path to folder where contents of zipped folders will be 
#          stored.
#
#Character vector of directory paths and file names to SQLite databases.
################################################################################

db_collect_paths <- function(dbIn = c(),
                           unzipDir = "")
{
  #If length of dbIn is 0, return dbIn
  if(length(dbIn) <= 0) return(dbIn)

  #If unzipDir exists, delete it
  if(file.exists(unzipDir)) unlink(unzipDir)
  
  #Create directory to unzip files too
  unzipDir <- paste(tempdir(),
                    "xxxfvsUtildb_compileUnzipxxx",
                    sep = "/")
  
  #Initialize dbInUpdate. This is a vector that will be used to store input
  #directory paths.
  dbInUpdate <- vector(mode = "character")
  
  #Loop through dbIn and check if files are not .db or .zip. If a file is a .zip
  #then unzip it to unzipDir. All db files will be added to dbInUpdate.
  for(i in 1:length(dbIn))
  {
    db <- dbIn[i]
    
    #Grab file extension for db
    fileExtIn<-sub("(.*)\\.","",db)
    
    #If the file extension of db is not .db or .zip then stop with error message.
    if(!fileExtIn %in% c("db", "zip", "sqlite"))
    {
      cat(db, "is not a zipped folder or sqlite database.", "\n")
      next
    }
    
    #If the file is a zip file, then it will be unzipped into xxxdb_compilexxx
    if(fileExtIn == "zip")
    {

      cat("Unzipping:", db, "to", unzipDir, "\n", "\n")
      
      unzip(zipfile = db,
            exdir = unzipDir)
      
      #Now list all the files that contain .db or .sqlite in the name.
      #Recursive argument is set to true so any sub directories are checked for
      #db files as well.
      dbList <- c(list.files(unzipDir,
                           pattern = "\\.db",
                           full.names = T,
                           recursive = T),
                  list.files(unzipDir,
                             pattern = "\\.sqlite",
                             full.names = T,
                             recursive = T))
      
      #If dbList is empty move to next iteration of loop
      if(length(dbList) <= 0)
      {
        cat("No .db or .sqlite files found in", db, "\n")
        next
      }
      
      #If dbList has at least one value then append the values in dbList to
      #dbInUpdate.
      else
      {
        dbInUpdate <- c(dbInUpdate, dbList)
      }
    }
    
    #Dealing with .db file. This file will be appended to dbInUpdate.
    else
    {
      dbInUpdate <- c(dbInUpdate, db)
    }
  }
  
  #Delete unzipDir before returning
  unlink(unzipDir)
  
  return(dbInUpdate)
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
#dbTable:   Character string corresponding to name of database table.
#
#dbFields:  Named character vector where names of vector are field names and the 
#           value in each index is a data type.
#
##alias:    Character string corresponding to name of database alias.
#
#Character string of SQL query used to create database table.
################################################################################

db_create_table_query <- function(dbTable,
                               dbFields = c(),
                               alias = "dbInsert")
{
  
  #Define empty query
  query <- ""
  
  #If dbFields is empty, build empty query
  if(length(dbFields) > 0 )
  {
    query <- paste(paste("CREATE TABLE", dbTable),
                   paste0("(", paste(names(dbFields), 
                                     dbFields, 
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
#dbFields:  Character vector of field names to include in insert query.
#
#dbTable:   Character string corresponding to name of database table.
#
#alias:     Character string corresponding to name of database alias.
#
#Character string of SQL query used insert data from one database table to
#another.
################################################################################

db_insert_query <- function(dbFields,
                          dbTable,
                          alias = "dbInsert")
{
  query <- ""
  
  if(length(dbFields) > 0)
  {
    #Create query
    query <- paste(paste("INSERT INTO", dbTable),
                   paste0("(", paste(dbFields, collapse = ", "), ")"),
                   paste("SELECT", paste(dbFields, collapse = ", ")),
                   "FROM", paste0(alias, ".", dbTable),
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
#dbOut:        
#Character string of file path to output database who will be populated by 
#tables included in database of dbInsert argument.
#
#dbInsert:  
#File path to database whose contents will be inserted into dbOut argument.
#
#dbTables:  
#Character vector of database table names for those that will be inserted from 
#dbInsert to dbOut.
#
#Return
#None
################################################################################

db_insert_tables <- function(dbOut,
                           dbInsert,
                           dbTables = c(),
                           keepCasing = TRUE)
{
  #Connect to dbOut (database information is being sent to)
  con_out <- RSQLite::dbConnect(RSQLite::SQLite(),
                                dbOut)
  
  #Attach dbInsert to con_out
  RSQLite::dbExecute(con_out,
                     paste("attach database", 
                           paste0("'", dbInsert,"'"), 
                           "as dbInsert"))
  
  #Begin loop across dbTables
  for(table in dbTables)
  {
    
    cat("Processing table:", table, "\n")
    
    #Connect to dbInsert
    con_in <- RSQLite::dbConnect(RSQLite::SQLite(),
                              dbInsert)
    
    #Skip to next database table if table does not exist in dbInsert
    if(!RSQLite::dbExistsTable(con = con_in,
                               name = table))
    {
      cat("Table:", table, "not found in:", dbInsert, "\n", "\n")
      RSQLite::dbDisconnect(con_in)
      next
    }
    
    #Get database fields and types for dbInsert
    insert_types <- db_get_data_types(con = con_in,
                                   dbTable = table)
    
    #Disconnect from dbInsert
    RSQLite::dbDisconnect(con_in)
    
    #Capitalize fields if keepCasing is off
    if(!keepCasing) names(insert_types) <- toupper(names(insert_types)) 
    
    #if dbTable does not exist in dbOut, create the table
    if(!RSQLite::dbExistsTable(conn = con_out,
                               name = table))
    {
      query <- db_create_table_query(dbTable = table,
                                  dbFields = insert_types)
      
      if(query == "") 
      {
        cat("Invalid table creation query.", "\n")
        next
      }
      
      else 
      {
        RSQLite::dbExecute(conn = con_out, query)
        #cat("Created table:", table, "in:", dbOut, "\n", "\n")
      }
    }
    
    #Check for fields that DO NOT exist in table within dbOut
    db_fields <- RSQLite::dbListFields(con_out,
                                       name = table)
    
    missing_fields <- names(insert_types)[! names(insert_types) %in% db_fields]
    missing_fields <- insert_types[missing_fields]
    
    #Loop through missing_fields and add to database table in 
    if(length(missing_fields) > 0)
    {
      db_add_fields(conn = con_out,
                  tableName = table,
                  dbFields = missing_fields)
    }
    
    #Now Check for fields that do exist in table in dbOut but DO NOT exist in
    #dbInsert
    
    #Get database fields and types for dbOut
    out_types <- db_get_data_types(con = con_out,
                                dbTable = table)
     
    missing_fields <- names(out_types)[! names(out_types) %in% 
                                         names(insert_types)]
    missing_fields <- out_types[missing_fields]
    
    #Loop through missing_fields and add to database table in dbInsert
    if(length(missing_fields) > 0)
    {
      con_in <- RSQLite::dbConnect(RSQLite::SQLite(),
                                dbInsert)
      
      db_add_fields(conn = con_in,
                  tableName = table,
                  dbFields = missing_fields)
      
      RSQLite::dbDisconnect(con_in)
    }
    
    #Get updated field names
    db_fields <- RSQLite::dbListFields(conn = con_out,
                                       name = table)
    
    #Generate insert query
    query <- db_insert_query(dbFields = db_fields,
                           dbTable = table)
    
    #If query is invalid move to next iteration
    if(query == "") 
    { 
      cat("Invalid insertion query created.")
      next
    }
    
    RSQLite::dbExecute(con_out,
                       query)
  }
  
  #Detach dbInsert and then disconnect from dbOut
  RSQLite::dbExecute(con_out,
                     "DETACH DATABASE dbInsert;")
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
#'@param tableName:  
#'Character string corresponding to name of table where fields will added.
#
#'@param dbFields:    
#'Named character vector or list where names are the names of the fields and 
#'the items in the vector or list are the data types of the fields.
#
#'@return 
#'None
################################################################################

#'@export
db_add_fields <- function(conn,
                        tableName,
                        dbFields = c())
{
  #If dbFields or dataTypes is empty, stop
  if(length(dbFields) <= 0)
  {
    stop("No fields and data types provided.")
  }
  
  cat("\n",
      "Fields missing from", tableName, "\n", names(dbFields), "\n", "\n")
  
  for(i in 1:length(dbFields))
  {
    #Extract field
    field <- names(dbFields)[[i]]
    
    #Extract data type of field
    data_type <- dbFields[[i]]
    
    #cat("Field:", field, "data_type:", data_type, "\n")
    
    cat("Adding field:", field, paste0("(", data_type, ")"), "to table:",
        tableName,
        "\n")
    
    #Create query to alter table and add field in conout
    addField <- paste("ALTER TABLE", tableName, "ADD COLUMN", field, data_type)
    
    #Add field to con_out
    RSQLite::dbExecute(conn = conn, 
                       statement = addField)
    
    cat("Field:", field, "added to table:", tableName, "\n", "\n")
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
#'@param dbIn:         
#'Character vector of directory paths and file names for SQLite databases to 
#'process. Files can either be a SQLite database (.db) or zipped folder (.zip) 
#'which contains a SQLite database(s).
#'
#'NOTE: .zip files will be unzipped to a temporary folder called 
#'xxxfvsUtildb_compileUnzipxxx in current working directory. Temporary folder will
#'be deleted after db_compile has finished writing data to output database.
#'
#'Examples of valid dbIn formats:
#'"C:/FIA2FVS_Databases/SQLite_FIADB_AZ/FIADB_AZ.db"'
#'"C:/FIA2FVS_Databases/SQLite_FIADB_AZ/ FIADB_AZ.zip"
#
#'@param dbOut:
#'Character string corresponding to SQLite database to write out to.
#'Examples of valid dbOut formats:
#'"C:/FIA2FVS_Databases/SQLite_FIADB_AZ/FVS_Data.db"
#
#'@param dbTables:    
#'Character vector of database tables to process from argument dbIn. If this
#'argument is left as NULL, then function will use all tables from the first
#'database specified in the dbIn argument.
#
#'@param deleteInput: 
#'Logical variable used to determine if values in dbIn should be deleted after
#'db_compile has been called. The primary purpose of this argument is to 
#'conserve hard disk space for users who do not want to retain the input 
#'databases specified in dbIn.
#'
#'@param keepCasing: 
#'Logical variable used to determine if the database table names and fields in
#'dbIn should retain original casing. When FALSE, the database table names and
#'fields in each table written to dbOut will be capitalized.
#'
#'@return 
#'None
################################################################################

#'@export
db_compile <- function(dbIn = NULL,
                       dbOut = NULL,
                       dbTables = NULL,
                       deleteInput = FALSE,
                       keepCasing = TRUE)
{
  
  #Test if no values have been specified for dbIn
  if(is.null(dbIn)) stop(paste("No files were specified for dbIn."))
  
  #Test if no values have been specified for dbOut
  if(is.null(dbOut)) stop(paste("No file was specified for dbOut."))
  
  #Test if dbTables is null and return with error message.
  #if(is.null(dbTables)) stop(paste("No table names were provided for dbTables."))
  
  #Replace \\ with / in dbIn and dbOut
  dbIn <- gsub("\\\\", "/", dbIn)
  dbOut <- gsub("\\\\", "/", dbOut)
  
  #Loop through dbIn and test if any of the files don't exist. If a file does
  #not exist then error message is reported.
  for(i in 1:length(dbIn))
  {
    if(!file.exists(dbIn[i])) stop(paste("File:", dbIn[i], "does not exist."))
    #else cat("Database", i, dbIn[i], "\n")
  }
  
  #If there is more than one value specified in dbOut, stop with error message.
  if(length(dbOut) > 1)
  {
    stop(paste("Only one output file can be specified for dbOut."))
  }
  
  #Test if dbOut file path is valid.
  #Extract path to dbOut by extracting all characters before the last / in
  #output argument.
  outPath <- gsub("/[^/]+$", "", dbOut)
  
  #Test existence of output path and if it does not exist report error.
  if (!(file.exists(outPath))){
    stop(paste("Path to output:", outPath, "was not found.",
               "Make sure directory path to output is spelled correctly."))
  }
  
  #Test if output file is a SQLite database. If the file is not a SQLite
  #database then error message is reported.
  fileExtOut<-sub("(.*)\\.","",dbOut)
  if(!fileExtOut %in% c("db", "sqlite"))
  {
    stop(paste("Output database:",
               dbOut,
               "is not a SQLite database.",
               "\n"))
  }
  
  #If dbOut already exists, delete it
  if(file.exists(dbOut))
  {
    cat("Deleting preexisting dbOut", "\n")
    ret <- unlink(dbOut)
    if(ret == 1) stop(paste("Failed to delete:", dbOut))
  }
  
  cat("Output database:", dbOut, "\n","\n")
  
  #Get updated directory paths and file names
  dbInUpdate <- db_collect_paths(dbIn = dbIn)
  
  #If dbInUpdate does not have any databases, then stop with error message and
  #delete unzip directory if it exists.
  if(length(dbInUpdate) <= 0)
  {
    stop("No valid database files (.db, .sqlite) are available for processing.")
  }
  
  #Remove duplicate values in dbInUpdate and print database file paths
  dbInUpdate <- unique(dbInUpdate)
  cat("List of db files to process:", "\n")
  cat(paste(dbInUpdate, collapse = "\n"), "\n", "\n")
  
  #If dbTables is NULL, then grab database tables from first database in 
  #dbInUpdate and use those for processing
  if(is.null(dbTables))
  {
    con <- RSQLite::dbConnect(RSQLite::SQLite(),
                              dbInUpdate[1])
    
    dbTables <- RSQLite::dbListTables(conn = con)
    
    RSQLite::dbDisconnect(con)
  }
  
  #If there are no values in dbTables stop with error
  if(length(dbTables) <= 0)
  {
    stop("No valid database tables available for processing.")
  }
  
  #Capitalize dbTables if keepCasing is off
  if(!keepCasing) dbTables <- toupper(dbTables)
  
  cat("Database table names to consider:", "\n")
  cat(paste(dbTables, collapse = "\n"), "\n", "\n")
  
  #Begin processing databases in dbInUpdate
  for(i in 1:length(dbInUpdate))
  {
    
    db <- dbInUpdate[i]
    
    cat("Processing db:", db, "\n")
    
    db_insert_tables(dbOut = dbOut,
                   dbInsert = db,
                   dbTables = dbTables,
                   keepCasing = keepCasing)
    
    #Print message indicating which db has been processed.
    cat("Finished processing db:", db, "\n", "\n")
  }
  
  #If deleteInput is TRUE, delete files in dbIN argument.
  if(deleteInput)
  {
    
    cat(paste("Argument deleteInput is TRUE.",
              "Deleting input databases.", "\n"))
    delete_files(files = dbIn,
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
#'@param dbIn:         
#'Character vector of directory paths and file names for SQLite databases to 
#'process. Files can either be a SQLite database or zipped folder (.zip) 
#'which contains a SQLite database(s).
#'
#'NOTE: .zip files will be unzipped to a temporary folder called 
#'xxxfvsUtildb_compileUnzipxxx in a temporary directory. The temporary folder 
#'will be deleted after db_compile has finished writing data to output database.
#'
#'Examples of valid dbIn formats:
#'"C:/FIA2FVS_Databases/SQLite_FIADB_AZ/FIADB_AZ.db"'
#'"C:/FIA2FVS_Databases/SQLite_FIADB_AZ/ FIADB_AZ.zip"
#
#'@param dbOut:
#'Character string corresponding to SQLite database to write out to.
#'Examples of valid dbOut formats:
#'"C:/FIA2FVS_Databases/SQLite_FIADB_AZ/FVS_Data.db",
#
#'@param dbTables:    
#'Character vector of database tables to process from argument dbIn.
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
#'@param buildGaak:   
#'Logical variable used to determine if FVS_GROUPADDFILESANDKEYWORDS will be 
#'#written to dbOut. If TRUE, this table will be written to dbOut. By default,
#'this argument is set to TRUE.
#
#'@param gaakType:    
#'Integer value from 1 - 3 used to determine what kind of GAAK table will be 
#'written to dbOut if buildGaak is TRUE.
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
#'@param deleteInput: 
#'Logical variable used to determine if values in dbIn should be deleted after
#'db_compile has been called. By default this argument is set to FALSE. Be 
#'careful with this argument. The primary purpose of this argument is to 
#'conserve hard disk space for users who do not want the input databases 
#'specified in dbIn.
#
#'@param readChunks:  
#'Logical variable used to determine if data from database table should be read
#'in chunks. In general, processing time of db_compile increases but less RAM is
#'used at one time in R session if this argument is TRUE.
#
#'@param rowsToRead:  
#'Integer value corresponding to number of rows to read from a database table if
#'readChunks is TRUE.
#
#'@return 
#'Character string indicating that database has been created.
################################################################################

#'@export
db_compile_v2 <- function(dbIn = NULL,
                        dbOut = NULL,
                        dbTables = c("FVS_STANDINIT",
                                     "FVS_TREEINIT",
                                     "FVS_PLOTINIT",
                                     "FVS_STANDINIT_PLOT",
                                     "FVS_STANDINIT_COND",
                                     "FVS_PLOTINIT_PLOT",
                                     "FVS_TREEINIT_PLOT",
                                     "FVS_TREEINIT_COND"),
                        buildGaak = T,
                        gaakType = 2,
                        deleteInput = F,
                        readChunks = F,
                        rowsToRead = 5000)
{
  
  #Test if no values have been specified for dbIn
  if(is.null(dbIn))
  {
    stop(paste("No files were specified for dbIn."))
  }
  
  #Test if no values have been specified for dbOut
  if(is.null(dbOut))
  {
    stop(paste("No file was specified for dbOut."))
  }
  
  #Test if dbTables is null and return with error message.
  if(is.null(dbTables))
  {
    stop(paste("No table names were provided for dbTables."))
  }
  
  #Print database tables to consider
  else
  {
    #Capitalize dbTables
    dbTables <- toupper(dbTables)
    
    cat("Database table names to consider:",
        dbTables,
        "\n")
  }
  
  #Catch erroneous gaakType values
  if(gaakType < 1 | gaakType > 3) gaakType = 2
  
  #Report error message if rowsToRead is less than or equal to 0
  if(readChunks)
  {
    rowsToRead <- as.integer(rowsToRead)
    if(rowsToRead <= 0)
    {
      stop(paste("Value for rowsToRead needs to be integer value greater than",
                 "zero."))
    }
  }
  
  #Replace \\ with / in dbIn and dbOut
  dbIn <- gsub("\\\\", "/", dbIn)
  dbOut <- gsub("\\\\", "/", dbOut)
  
  #Loop through dbIn and test if any of the files don't exist. If a file does
  #not exist then error message is reported.
  for(i in 1:length(dbIn))
  {
    if(!file.exists(dbIn[i]))
    {
      stop(paste("File:",
                 dbIn[i],
                 "does not exist."))
    }
    
    else
    {
      cat("Database", i, dbIn[i], "\n")
    }
  }
  
  #If there is more than one value specified in dbOut, stop with error message.
  if(length(dbOut) > 1)
  {
    stop(paste("Only one output file can be specified for dbOut."))
  }
  
  #Test if dbOut file path is valid.
  #Extract path to dbOut by extracting all characters before the last / in
  #output argument.
  outPath <- gsub("/[^/]+$", "", dbOut)
  
  #Test existence of output path and if it does not exist report error.
  if (!(file.exists(outPath))){
    stop(paste("Path to output:", outPath, "was not found.",
               "Make sure directory path to output is spelled correctly."))
  }
  
  #Test if output file is a SQLite database. If the file is not a SQLite
  #database then error message is reported.
  fileExtOut<-sub("(.*)\\.","",dbOut)
  if(!fileExtOut %in% c("db", "sqlite"))
  {
    stop(paste("Output database:",
               dbOut,
               "is not a SQLite database.",
               "\n"))
  }
  
  #If dbOut already exists, delete it
  if(file.exists(dbOut))
  {
    cat(paste0("\n","Deleting preexisting dbOut"), "\n")
    unlink(dbOut,
           force = T)
  }
  
  cat("Output database:", dbOut, "\n","\n")
  
  #Get updated directory paths and file names
  dbInUpdate <- db_collect_paths(dbIn = dbIn)
  
  #If dbInUpdate does not have any databases, then stop with error message and
  #delete unzip directory if it exists.
  if(length(dbInUpdate) <= 0)
  {
    #Check if unzipDir exists. If it does, delete it.
    if(file.exists(unzipDir))
    {
      
      delete_files(files = unzipDir,
                  recur = TRUE)
    }
    
    stop("No valid database files (.db, .sqlite) are available for processing.")
  }
  
  #Remove duplicate values in dbInUpdate and print database file paths
  dbInUpdate <- unique(dbInUpdate)
  cat("List of db files to process:", "\n")
  cat(paste(dbInUpdate, collapse = "\n"))
  
  #Begin processing databases in dbInUpdate
  for(i in 1:length(dbInUpdate))
  {
    
    db <- dbInUpdate[i]
    
    cat("\n")
    cat("Processing db:", db, "\n", "\n")
    
    #Begin processing dbTables in db
    for(j in 1:length(dbTables))
    {
      #Extract table name
      tableName <- dbTables[j]
      cat("Processing table:",
          tableName,
          "\n")
      
      #Connect to db
      conIn <- RSQLite::dbConnect(RSQLite::SQLite(), db)
      
      #Test if table does not exist in db. if this is the case move to next
      #iteration of loop.
      if(!tableName %in% toupper(RSQLite::dbListTables(conIn)))
      {
        cat("Table:",
            tableName,
            "was not found in database.",
            "\n", "\n")
        #Disconnect from conIn
        RSQLite::dbDisconnect(conIn)
        next
      }
      
      #Determine number of rows in tableName
      query <- paste("SELECT COUNT(*) FROM", tableName)
      numRows <- RSQLite::dbGetQuery(conIn,
                                     query)[[1]]
      
      #If there are no rows (i.e. no data) in dbTable, skip to next iteration
      #of loop.
      if(numRows <= 0)
      {
        cat("No data found in",
            tableName,
            "\n", "\n")
        
        #Disconnect from conIn
        RSQLite::dbDisconnect(conIn)
        next
      }
      
      #Disconnect from conIn
      RSQLite::dbDisconnect(conIn)
      
      #If readChunks is FALSE, call add_db_table, otherwise call add_db_rows.
      if(!readChunks)
      {
        add_db_table(db,
                   dbOut,
                   tableName)
      }
      else
      {
        add_db_rows(db,
                  dbOut,
                  tableName,
                  rowsToRead,
                  numRows)
      }
    }
    
    #Print message indicating which db has been processed.
    cat("Finished processing db:",
        db,
        "\n")
  }
  
  #Determine if GAAK table should be written to dbOut.
  if(buildGaak)
  {
    conOut <- RSQLite::dbConnect(RSQLite::SQLite(),
                                 dbOut)
    
    cat("Writing fvs_gaak table to",
        dbOut,
        "\n",
        "\n")
    
    RSQLite::dbWriteTable(conn = conOut,
                          name = "FVS_GROUPADDFILESANDKEYWORDS",
                          value = fvs_gaak(type = gaakType),
                          overwrite = T)
    
    #Disconnect from conOut
    RSQLite::dbDisconnect(conOut)
  }
  
  #If deleteInput is TRUE, delete files in dbIN argument.
  if(deleteInput)
  {
    
    cat(paste("Argument deleteInput is TRUE.",
              "Deleting input databases.", "\n"))
    delete_files(files = dbIn,
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
#dbOut:      File path to output SQLite database.
#
#tableName:  Name of database table in db being sent to dbOut.
#
#Return value
#
#None
################################################################################

add_db_table<-function(db,
                     dbOut,
                     tableName)
{
  
  #Connect to input database (db)
  conIn <- RSQLite::dbConnect(RSQLite::SQLite(),
                              db)
  
  #Read in the dbTable table (tableName)
  dbTable <- RSQLite::dbReadTable(conIn,
                                  name = tableName)
  
  #Capitalize column headers
  colnames(dbTable) <- toupper(colnames(dbTable))
  
  #Get column data types from tableName
  tableTypes <-db_get_data_types(conIn,
                              tableName)
  
  #Disconnect from conIn
  RSQLite::dbDisconnect(conIn)
  
  #Connect to dbOut
  conOut <- RSQLite::dbConnect(RSQLite::SQLite(),
                               dbOut)
  
  #Test if tableName exists in conOut. If it does, this dbTable will be
  #appended to the existing table in output (conOut).
  if(tableName %in% toupper(RSQLite::dbListTables(conOut)))
  {
    #Identify any fields in dbTable that are missing from the same data table
    #in conOut.
    dbFields <- RSQLite::dbListFields(conOut,
                                      name = tableName)
    
    missingFields <- names(dbTable)[! names(dbTable) %in% dbFields]
    
    #Loop through missingFields and add to database table in conOut
    if(length(missingFields) > 0)
    {
      cat("\n",
          "Fields missing from",
          tableName,
          "in",
          dbOut,
          "\n",
          missingFields, "\n", "\n")
      
      for(i in 1:length(missingFields))
      {
        #Extract field
        field <- missingFields[i]
        
        #Extract datatype of field
        dataType <- tableTypes[names(tableTypes) == field]
        cat("Field:", field, "dataType:", dataType, "\n")
        
        cat("Adding field:",
            field,
            paste0("(", dataType, ")"),
            "to table:",
            tableName,
            "\n")
        
        #Create query to alter table and add field in conout
        addField <-paste("ALTER TABLE",
                         tableName,
                         "ADD COLUMN",
                         field,
                         dataType)
        
        #Add field to conOut
        RSQLite::dbExecute(conOut, addField)
        
        cat("Field:",
            field,
            "added to table:",
            tableName,
            "\n", "\n")
      }
    }
    
    cat("Appending",
        tableName,
        "to",
        dbOut,
        "\n")
    
    #Append data to conOut
    RSQLite::dbWriteTable(conn = conOut,
                          name = tableName,
                          value = dbTable,
                          append = T)
    
    cat(tableName,
        "appended to",
        dbOut,
        "\n",
        "\n")
  }
  
  #Table will be created in conOut and data will then be written to the table.
  else
  {
    cat("Writing",
        tableName,
        "to",
        dbOut,
        "\n")
    
    #Create the dbTable in conOut and write information from dbTable to it.
    RSQLite::dbWriteTable(conn = conOut,
                          name = tableName,
                          value = dbTable,
                          overwrite = T,
                          field.types = tableTypes)
    
    cat(tableName,
        "written to",
        dbOut,
        "\n",
        "\n")
  }
  
  #Delete dbTable
  rm(dbTable)
  
  #Disconnect from conOut
  RSQLite::dbDisconnect(conOut)
  
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
#dbOut:      Directory path to output SQLite database.
#
#tableName:  Name of database table being sent from db to dbOut.
#
#numToRead:  Number of rows to read in from database table at a time.
#
#numRows:    Number of rows in argument tableName.
#
#Return value
#
#None
################################################################################

add_db_rows<-function(db,
                    dbOut,
                    tableName,
                    numToRead,
                    numRows)
{
  
  #Variable to signify when read of data from tableName in db is complete
  doneReading <- F
  
  #Lower value of rows to read from
  lower <- 0
  
  #Upper value of rows to read from. Upper value is only used in messages sent
  #to console.
  upper <- 0
  
  #Variable used to keep track of number of rows that have been processed
  rowsDone <- 0
  
  #Variable to indicate whether first pass is complete.
  firstPass <- T
  
  while(!doneReading)
  {
    # If this is the first pass, set lower to 1 and upper to numToRead. Then
    #set firstPass to F.
    if(firstPass)
    {
      upper <- numToRead
      firstPass <- F
    }
    
    #If this is not the first pass then set lower to lower + numToRead and
    #upper to upper + NumToRead
    else
    {
      lower <- lower + numToRead
      upper <- upper + numToRead
    }
    
    #If upper is greater than or equal to numRows, set numToRead to
    #numRows - rowsDone and set doneReading to T. This will signify that
    #function is about to make the last read from db.
    if(upper >= numRows)
    {
      numToRead <- numRows - rowsDone
      upper <- numRows
      doneReading <- T
    }
    
    #Setup query for reading data
    query <- paste("SELECT * FROM",
                   tableName,
                   "LIMIT",
                   paste0(lower,",", numToRead))
    
    cat("Row query:",
        query,
        "\n")
    
    #Display what rows are being read from database table.
    cat("Reading rows:",
        lower + 1,
        "through",
        upper,
        "from",
        tableName, "\n")
    
    #Connect to db
    conIn <- RSQLite::dbConnect(RSQLite::SQLite(),
                                db)
    
    #Read the data
    dbTable <- RSQLite::dbGetQuery(conIn,
                                   query)
    
    #Get column data types from tableName
    tableTypes <-db_get_data_types(conIn,
                                tableName)
    
    #Disconnect from db
    RSQLite::dbDisconnect(conIn)
    
    #Determine number of rows read in current pass
    rowsRead <- nrow(dbTable)
    
    #Print number of rows in dbTable
    cat("Number of rows in read from database:",
        rowsRead,
        "\n")
    
    #Capitalize column headers
    colnames(dbTable) <- toupper(colnames(dbTable))
    
    #Connect to dbOut
    conOut <- RSQLite::dbConnect(RSQLite::SQLite(),
                                 dbOut)
    
    #Test if tableName exists in conOut. If it does, this dbTable will be
    #appended to the existing table in output (conOut).
    if(tableName %in% toupper(RSQLite::dbListTables(conOut)))
    {
      #Identify any fields in dbTable that are missing from the same data table
      #in conOut.
      dbFields <- RSQLite::dbListFields(conOut,
                                        name = tableName)
      
      missing_fields <- names(dbTable)[! names(dbTable) %in% dbFields]
      
      #Loop through missing_fields and add to database table in conOut
      if(length(missing_fields) > 0)
      {
        cat("\n",
            "Fields missing from",
            tableName,
            "in",
            dbOut,
            "\n",
            missing_fields, "\n", "\n")
        
        for(i in 1:length(missing_fields))
        {
          #Extract field
          field <- missing_fields[i]
          
          #Extract datatype of field
          dataType <- tableTypes[names(tableTypes) == field]
          cat("Field:", field, "dataType:", dataType, "\n")
          
          cat("Adding field:",
              field,
              paste0("(", dataType, ")"),
              "to table:",
              tableName,
              "\n")
          
          #Create query to alter table and add field in conout
          addField <-paste("ALTER TABLE",
                           tableName,
                           "ADD COLUMN",
                           field,
                           dataType)
          
          #Add field to conOut
          RSQLite::dbExecute(conOut, addField)
          
          cat("Field:",
              field,
              "added to table:",
              tableName,
              "\n", "\n")
        }
      }
      
      cat("Appending rows", lower + 1, "through", upper, "from",
          tableName,
          "to",
          dbOut,
          "\n")
      
      #Append data to conOut
      RSQLite::dbWriteTable(conn = conOut,
                            name = tableName,
                            value = dbTable,
                            append = T)
      
      cat("Rows", lower + 1, "through", upper, "from", tableName,
          "appended to",
          dbOut,
          "\n")
    }
    
    #Table will be created in conOut and data will then be written to the
    #table.
    else
    {
      
      cat("Writing rows", lower + 1, "through", upper, "from",
          tableName,
          "to",
          dbOut,
          "\n")
      
      #Create the dbTable in conOut and write information from dbTable to it.
      RSQLite::dbWriteTable(conn = conOut,
                            name = tableName,
                            value = dbTable,
                            overwrite = T,
                            field.types = tableTypes)
      
      cat("Rows", lower + 1, "through", upper, "from", tableName,
          "written to",
          dbOut,
          "\n")
    }
    
    #Update rowsDone
    rowsDone <- rowsDone + rowsRead
    
    #Print number of rows processed
    cat("Number of rows processed:",
        rowsDone,
        "\n",
        "\n")
    
    #Delete dbTable
    rm(dbTable)
    
    #Disconnect from dbOut
    RSQLite::dbDisconnect(conOut)
  }
  
  invisible(0)
}