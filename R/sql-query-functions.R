################################################################################
#'create_idx_query
#'@name create_idx_query
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
create_idx_query <- function(db_table = "FVS_STANDINIT",
                             db_field = "STAND_ID",
                             idx_name = NULL)
{
  #Set index name if not entered
  if(is.null(idx_name)) idx_name <- paste("idx", db_table, db_field, sep = "_")
  
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
                          data_type = "TEXT")
{
  query <- paste("ALTER TABLE",
                 db_table,
                 "ADD COLUMN",
                 db_field,
                 dataType)

  return(query)
}

################################################################################
#'drop_idx_query
#'@name drop_idx_query
#'@description
#'
#'This function takes in a SQLite index name and returns a query that will drop
#'the index if it exists.
#
#'@param idx_name: 
#'Character string of index name in database table to drop.
#
#'@return
#'Character string of query that will be used to remove database table index if
#'it exists.
################################################################################

#'@export
drop_idx_query <- function(idx_name = "TREE_PLOTQUERYID")
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
  if(is.null(ids)) n <- 0
  else n <- length(ids)
  
  #Build placeholder string
  idString <- paste0("(", 
                     paste(rep("?", times = n), 
                           collapse = ", "),
                     ")")
  return(idString)
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
  #Initialize character vector of length zero
  index_names <- vector(mode = "character")
  
  #Get type and name column
  tables <- RSQLite::dbGetQuery(con, "select * from sqlite_master")[,1:2]
  
  #Extract index values from type column
  tables <- tables[tables$type == 'index',]
  
  #If there are indexes in tables, retrieve them
  if(nrow(tables) > 0)
    index_names <- tables$name
  
  return(index_names)
}
