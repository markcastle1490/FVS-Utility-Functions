################################################################################
#'set_index_query
#'@name set_index_query
#'@description
#'
#'This function takes in a database table name and field name and returns a 
#'query for setting an index with the specified table and field name.
#
#'@param db_table: 
#'Character string corresponding to name of database table.
#
#'@param db_field:
#'Character String corresponding to name of database field name.
#'
#'@param idx_name: 
#'Character string corresponding to name of index that will be created in
#'db_table. If argument is left as NULL, the index name will be a concatenation
#'of 'idx', db_table, and idx_name.
#
#'@return
#'Character string used to set index for specified database table and field name.
################################################################################

#'@export
set_index_query <- function(db_table = "FVS_STANDINIT",
                            db_field = "STAND_ID",
                            idx_name = NULL)
{
  #Set index name if not entered
  if(is.null(idx_name)) idx_name = paste("idx", db_table, db_field, sep = "_")
  
  query <- paste("CREATE INDEX IF NOT EXISTS",
                 idx_name,
                 "ON", paste0(db_table, "(", db_field, ");"))

  return(query)
}

################################################################################
#'add_col_query
#'@name add_col_query
#'@description
#'
#'This function takes in a database table name, database field name, and data
#'type associated with database field name and returns a query which creates a
#'column in the specified database table.
#
#'@param db_table:
#'Character string corresponding to name of database table.
#
#'@param db_field:
#'Character string corresponding to name of database field name.
#
#'@param dataType: 
#'Character string corresponding to data type of db_field.
#
#'@return
#'Character string of query used to create new column in specified database
#'table.
################################################################################

#'@export
add_col_query <- function(db_table = "TREE",
                          db_field = "PLOTQUERYID",
                          dataType = "TEXT")
{
  query <- paste("ALTER TABLE",
                 db_table,
                 "ADD COLUMN",
                 db_field,
                 dataType)

  return(query)
}

################################################################################
#'drop_index_query
#'@name drop_index_query
#'@description
#'
#'This function takes in a SQLite index name and returns a query that will drop
#'the index if it exists.
#
#'@param index: 
#'Character string of index name in database table to drop.
#
#'@return
#'Character string of query that will be used to remove database table index if
#'it exists.
################################################################################

#'@export
drop_index_query <- function(idx_name = "TREE_PLOTQUERYID")
{
  query <- paste0("DROP INDEX IF EXISTS ", idx_name)
  return(query)
}

################################################################################
#'drop_col_query
#'@name drop_col_query
#'@description
#'
#'This function takes in a database table name and column/field and returns a
#'query that will have the column dropped from database table.
#
#'@param db_table: 
#'Character string corresponding to name of database table.
#
#'@param db_field:
#' string corresponding to name of database field name.
#
#'@return
#'Character string of query used to drop column from database table.
################################################################################

#'@export
drop_col_query <- function(db_table = "TREE",
                           db_field = "PLOTQUERYID")
{
  query <- paste("ALTER TABLE",
                 db_table,
                 "DROP COLUMN",
                 paste0(db_field))
  return(query)
}

################################################################################
#'collect_id
#'@name collect_id
#'@description
#'
#'This function takes in a vector of elements typically used for querying a 
#'database and  collapses them into a single string where elements are 
#'surrounded by parentheses and separated by commas.
#
#'@param ids:    
#'Character vector of elements
#
#'@return
#'Character string of elements surrounded by parentheses and separated by 
#'commas.
################################################################################

#'@export
collect_id <- function(ids)
{
  #Add quotes arounds ids and separate with commas
  idString<-paste0("'",ids,"'", collapse = ",")
  
  #Add parentheses around idString
  idString<-paste0("(",idString, ")")
  
  return(idString)
}

################################################################################
#'placeholder_id
#'@name placeholder_id
#'@description
#'
#'This function takes in a vector of elements and creates a character string 
#'with place holder values (?) surrounded by parentheses.The number of place
#'holder values in the string corresponds to the length of the input vector.
#'
#'@param ids:    
#'Vector of elements
#
#'@return
#'Character string of placeholder values surrounded by parentheses and 
#'separated by commas.
################################################################################

#'@export
placeholder_id <- function(ids = NULL)
{
  #Determine n (number of reps)
  if(is.null(ids)) n = 0
  else n = length(ids)
  
  #Build placeholder string
  idString <- paste0("(", 
                     paste(rep("?", times = n), 
                           collapse = ", "),
                     ")")
  return(idString)
}

################################################################################
#Function: db_get_indices
#
#This function returns the names of indices that exist in input database
#argument.
#
#Arguments
#
#input: Connection to a SQLite database (.db, .sqlite)
#
#Value
#
#Character vector of index names that exist in input argument.
################################################################################

#'@export
db_get_indices <- function(con)
{
  #Initialize character vector of length zero
  index_names <- vector(mode = "character")
  
  #Get type and name column
  tables = RSQLite::dbGetQuery(con, "select * from sqlite_master")[,1:2]
  
  #Extract index values from type column
  tables <- tables[tables$type == 'index',]
  
  #If there are indexes in tables, retrieve them
  if(nrow(tables) > 0)
  {
    index_names <- tables$name
  }
  
  #If there are no indexes in tables, set index_names to "No index names found in
  #database"
  if(length(index_names) <= 0)
  {
    index_names <- "No index names found in database"
  }
  
  return(index_names)
}

################################################################################
#add_plotq_id
#
#Description
#
#This function is used to insert a temporary plot ID variable into specified
#database tables of an FIA sqlite database. This idea is then used to query data
#in the buildFIA function. This function could be expanded to encompass other
#data sources.
#
#Source Code
#
#Function add_plotq_id is currently located in the FIA_GST_Functions.R file.
#
#Arguments
#
#dbIn:     	Directory path and file name to FIA sqlite database (.db, .sqlite).
#
#db_tables: 	Character vector of database table names in dbIn where PLOTQUERYID
#           variable will be created.
#
#Value
#
#Integer value of 0 returned invisibly.
################################################################################

add_plotq_id <- function(dbIn,
                       db_tables= c("TREE",
                                   "PLOT",
                                   "SUBPLOT",
                                   "COND",
                                   "SITETREE"))
{
  #Connect to database dbIn
  con <- RSQLite::dbConnect(RSQLite::SQLite(),
                            dbIn)
  
  #Start loop across db_tables
  for(table in db_tables)
  {
    
    #Check if TEMP_PLOT is in table
    if(! "PLOTQUERYID" %in% RSQLite::dbListFields(con,
                                                  table))
    {
      #Create PLOTQUERYID column
      RSQLite::dbExecute(con,
                         create_field_query(db_table = table,
                                          dbField = "PLOTQUERYID"))
      cat("PLOTQUERYID created in", table, "\n")
      
      #Set values
      RSQLite::dbExecute(con,
                         set_plotq_id(db_table = table))
      cat("Values set in PLOTQUERYID column.", "\n")
      
      #Remove index if it already exists
      index_name <- paste(table, "PLOTQUERYID", sep = "_")
      RSQLite::dbExecute(con,
                         drop_index_query(index = index_name))
      
      #Create index
      RSQLite::dbExecute(con,
                         set_index_query(db_table = table,
                                       dbField = "PLOTQUERYID"))
      cat("Index created in", table, "on PLOTQUERYID", "\n", "\n")
    }
    
    else
    {
      cat("PLOTQUERYID already exists in", table, "\n", "\n")
    }
  }
  
  #Disconnect from database dbIn
  RSQLite::dbDisconnect(con)
  
  invisible(0)
}

################################################################################
#Function: set_plotq_id
#
#Description
#
#This function takes in a FIA database table name and creates a query used to
#add values to a PLOTQUERYID column based on STATECD, UNITCD, COUNTYCD, and PLOT
#code. This function is called from add_plotq_id. Logic in function could be
#expanded to account for other data sources.
#
#Source Code
#
#Function set_plotq_id is currently located in the GST_Prep_Functions.R file.
#
#Arguments
#
#db_table:  	Character string corresponding to name of database table.
#
#Value
#
#Character string of query used to add PLOTQUERYID values to PLOTQUERYID column
#in specified database table.
################################################################################

set_plotq_id <- function(db_table = "TREE")
{
  query <- paste("UPDATE",
                 db_table,
                 "SET PLOTQUERYID =",
                 "STATECD || '_' ||",
                 "UNITCD || '_' ||",
                 "COUNTYCD || '_' ||",
                 "PLOT")
  
  return(query)
}
