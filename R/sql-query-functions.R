################################################################################
#'set_index_query
#'@name set_index_query
#'@description
#'
#'This function takes in a database table name and field name and returns a 
#'query for setting an index with the specified table and field name.
#
#'@param dbTable: 
#'Character string corresponding to name of database table.
#
#'@param dbField:
#'Character String corresponding to name of database field name.
#'
#'@param indxName: 
#'Character string corresponding to name of index that will be created in
#'dbTable.
#
#'@return
#'Character string used to set index for specified database table and field name.
################################################################################

#'@export
set_index_query <- function(dbTable = "TREE",
                            dbField = "PLOTQUERYID",
                            indxName = "")
{
  query <- paste("create index",
                 indxName,
                 " on ", paste0(dbTable, "(", dbField, ");"))

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
#'@param dbTable:
#'Character string corresponding to name of database table.
#
#'@param dbField:
#'Character string corresponding to name of database field name.
#
#'@param dataType: 
#'Character string corresponding to data type of dbField.
#
#'@return
#'Character string of query used to create new column in specified database
#'table.
################################################################################

#'@export
add_col_query <- function(dbTable = "TREE",
                          dbField = "PLOTQUERYID",
                          dataType = "TEXT")
{
  query <- paste("ALTER TABLE",
                 dbTable,
                 "ADD COLUMN",
                 dbField,
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
drop_index_query <- function(index = "TREE_PLOTQUERYID")
{
  query <- paste0("DROP INDEX IF EXISTS ",index)
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
#'@param dbTable: 
#'Character string corresponding to name of database table.
#
#'@param dbField:
#' string corresponding to name of database field name.
#
#'@return
#'Character string of query used to drop column from database table.
################################################################################

#'@export
drop_col_query <- function(dbTable = "TREE",
                         dbField = "PLOTQUERYID")
{
  query <- paste("ALTER TABLE",
                 dbTable,
                 "DROP COLUMN",
                 paste0(dbField))
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
#Function: db_get_index_names
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
db_get_index_names <- function(con)
{
  #Initialize character vector of length zero
  indexNames <- vector(mode = "character")
  
  #Get type and name column
  tables = RSQLite::dbGetQuery(con, "select * from sqlite_master")[,1:2]
  
  #Extract index values from type column
  tables <- tables[tables$type == 'index',]
  
  #If there are indexes in tables, retrieve them
  if(nrow(tables) > 0)
  {
    indexNames <- tables$name
  }
  
  #If there are no indexes in tables, set indexNames to "No index names found in
  #database"
  if(length(indexNames) <= 0)
  {
    indexNames <- "No index names found in database"
  }
  
  return(indexNames)
}