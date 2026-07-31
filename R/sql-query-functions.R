################################################################################
#' create_idx_query
#' @name create_idx_query
#' @description This function takes in a database table name and field name and
#' returns a query for setting an index with the specified table and field name.
#' 
#' @param db_table
#' Character string corresponding to name of database table. 
#' Defaults to "FVS_STANDINIT".
#' 
#' @param db_fields
#' Character vector corresponding to the name or names of the database fields 
#' to include in the index. Defaults to c("STAND_ID").
#' 
#' @param idx_name
#' Character string corresponding to name of index that will be created in 
#' db_table. If argument is left as NULL, the index name will be a concatenation 
#' of 'idx_', db_table, and the target field name. Defaults to NULL.
#' 
#' @return
#' Character string used to set index for specified database table and field name.
#' @export
################################################################################

create_idx_query <- function(db_table = "FVS_STANDINIT",
                             db_fields = c("STAND_ID"),
                             idx_name = NULL)
{
  #Set index name if not entered
  if(is.null(idx_name)) 
  {
    fields_comb <- paste(db_fields, collapse = "_")
    idx_name <- paste("idx", db_table, fields_comb, sep = "_")
  }

  #Format fields into a comma-separated string: "field1, field2"
  fields_csv <- paste(db_fields, collapse = ", ")

  #Build query
  query <- paste0("CREATE INDEX IF NOT EXISTS ",
                 idx_name,
                 " ON ", 
                 db_table,
                 " (", fields_csv, ");")

  return(query)
}

################################################################################
#' add_col_query
#' @name add_col_query
#' @description This function takes in a database table name, database field 
#' name, and data type associated with database field name and returns a query 
#' which creates a column in the specified database table.
#' 
#' @param db_table
#' Character string corresponding to name of database table. 
#' Defaults to "TREE".
#' 
#' @param db_field
#' Character string corresponding to name of database field name. 
#' Defaults to "PLOTQUERYID".
#' 
#' @param data_type
#' Character string corresponding to data type of db_field. 
#' Defaults to "TEXT".
#' 
#' @return
#' Character string of query used to create new column in specified database 
#' table.
#' @export
################################################################################

add_col_query <- function(db_table = "TREE",
                          db_field = "PLOTQUERYID",
                          data_type = "TEXT")
{
  query <- paste0("ALTER TABLE ",
                 db_table,
                 " ADD COLUMN ",
                 db_field,
                 " ",
                 data_type,
                 ";")

  return(query)
}

################################################################################
#' drop_idx_query
#' @name drop_idx_query
#' @description This function takes in a SQLite index name and returns a query 
#' that will drop the index if it exists.
#' 
#' @param idx_name
#' Character string of index name in database table to drop. 
#' Defaults to "TREE_PLOTQUERYID".
#' 
#' @return
#' Character string of query that will be used to remove database table index if 
#' it exists.
#' @export
################################################################################

drop_idx_query <- function(idx_name = "TREE_PLOTQUERYID")
{
  query <- paste0("DROP INDEX IF EXISTS ", idx_name, ";")
  return(query)
}

################################################################################
#' Generate Drop Column Query
#' @name drop_col_query
#' @description This function takes in a database table name and column/field 
#' and returns a query that will have the column dropped from database table.
#' 
#' @param db_table
#' Character string corresponding to name of database table. 
#' Defaults to "TREE".
#' 
#' @param db_field
#' Character string corresponding to name of database field name. 
#' Defaults to "PLOTQUERYID".
#' 
#' @return
#' Character string of query used to drop column from database table.
#' @export
################################################################################

drop_col_query <- function(db_table = "TREE",
                           db_field = "PLOTQUERYID")
{
  query <- paste0("ALTER TABLE ",
                 db_table,
                 " DROP COLUMN ",
                 db_field,
                 ";")
  return(query)
}

################################################################################
#' placeholder_id
#' @name placeholder_id
#' @description This function takes in a vector of elements and creates a 
#' character string with place holder values (?) surrounded by parentheses. The
#' number of place holder values in the string corresponds to the length of the 
#' input vector.
#' 
#' @param ids
#' Vector of elements. Defaults to NULL.
#' 
#' @return
#' Character string of placeholder values surrounded by parentheses and 
#' separated by commas.
#' @export
################################################################################

placeholder_id <- function(ids = NULL)
{
  #Determine n (number of reps)
  n <- length(ids)
  
  #Build placeholder string
  if(n > 0)
  {
    id_string <- paste0("(", 
                       paste(rep("?", times = n), 
                             collapse = ", "),
                       ")")
  }
  
  #Blank string
  else
  {
    id_string <- ""
  }
  
  return(id_string)
}
