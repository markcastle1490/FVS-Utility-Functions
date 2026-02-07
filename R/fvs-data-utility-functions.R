################################################################################
#'fvsStandInit
#'@name fvsStandInit
#'@description
#'
#'Creates a blank dataframe with column headers found in standard FVS_StandInit
#'table
##
#'@return
#'Empty data frame containing columsn from FVS_StandInit table.
################################################################################

#'@export
fvsStandInit<-function()
{
  stand = data.frame(STAND_CN = character(),
                     STAND_ID = character(),
                     VARIANT = character(),
                     INV_YEAR = numeric(),
                     GROUPS = character(),
                     ADDFILES = character(),
                     FVSKEYWORDS = character(),
                     LATITUDE = numeric(),
                     LONGITUDE = numeric(),
                     REGION = numeric(),
                     FOREST = numeric(),
                     DISTRICT = numeric(),
                     COMPARTMENT = numeric(),
                     LOCATION = numeric(),
                     ECOREGION = character(),
                     PV_CODE = character(),
                     PV_REF_CODE = numeric(),
                     AGE = numeric(),
                     ASPECT = numeric(),
                     SLOPE = numeric(),
                     ELEVATION = numeric(),
                     ELEVFT = numeric(),
                     BASAL_AREA_FACTOR = numeric(),
                     INV_PLOT_SIZE = numeric(),
                     BRK_DBH = numeric(),
                     NUM_PLOTS = numeric(),
                     NONSTK_PLOTS = numeric(),
                     SAM_WT = numeric(),
                     STK_PCNT = numeric(),
                     DG_TRANS = numeric(),
                     DG_MEASURE = numeric(),
                     HTG_TRANS = numeric(),
                     HTG_MEASURE = numeric(),
                     MORT_MEASURE = numeric(),
                     MAX_BA = numeric(),
                     MAX_SDI = numeric(),
                     SITE_SPECIES = character(),
                     SITE_INDEX = numeric(),
                     MODEL_TYPE = numeric(),
                     PHYSIO_REGION = numeric(),
                     FOREST_TYPE = numeric(),
                     STATE = numeric(),
                     COUNTY = numeric(),
                     FUEL_MODEL = numeric(),
                     FUEL_0_25 = numeric(),
                     FUEL_25_1 = numeric(),
                     FUEL_1_3 = numeric(),
                     FUEL_3_6_H = numeric(),
                     FUEL_6_12_H = numeric(),
                     FUEL_12_20_H = numeric(),
                     FUEL_20_35_H = numeric(),
                     FUEL_35_50_H = numeric(),
                     FUEL_GT_50_H = numeric(),
                     FUEL_3_6_S = numeric(),
                     FUEL_6_12_S = numeric(),
                     FUEL_12_20_S = numeric(),
                     FUEL_20_35_S = numeric(),
                     FUEL_35_50_S = numeric(),
                     FUEL_GT_50_S = numeric(),
                     FUEL_LITTER = numeric(),
                     FUEL_DUFF = numeric(),
                     PHOTO_REF = numeric(),
                     PHOTO_CODE = character())
  return(stand)
}
  
################################################################################
#'fvsTreeInit
#'@name fvsTreeInit
#'@description 
#'
#'Creates a blank dataframe with column headers found in standard FVS_TreeInit
#'table
##
#'@return
#'Empty data frame containing columsn from FVS_TreeInit table.
################################################################################

#'@export
fvsTreeInit<-function()
{
  tree<-data.frame(STAND_CN = character(),
                   STAND_ID = character(),
                   PLOT_CN = character(),
                   PLOT_ID = numeric(),
                   STANDPLOT_CN = character(),
                   STANDPLOT_ID = character(),
                   TREE_ID = numeric(),
                   TREE_COUNT = numeric(),
                   HISTORY = numeric(),
                   SPECIES = character(),
                   DIAMETER = numeric(),
                   DIAMETER_HT = numeric(),
                   DG = numeric(),
                   HT = numeric(),
                   HTG = numeric(),
                   HTTOPK = numeric(),
                   HT_TO_LIVE_CROWN = numeric(),
                   CRCLASS = numeric(),
                   CRRATIO = numeric(),
                   DAMAGE1 = numeric(),
                   SEVERITY1 = numeric(),
                   DAMAGE2 = numeric(),
                   SEVERITY2 = numeric(),
                   DAMAGE3 = numeric(),
                   SEVERITY3 = numeric(),
                   DEFECT_CUBIC = numeric(),
                   DEFECT_BOARD = numeric(),
                   TREEVALUE = numeric(),
                   PRESCRIPTION = numeric(),
                   AGE = numeric(),
                   SLOPE = numeric(),
                   ASPECT = numeric(),
                   PV_CODE = character(),
                   TOPOCODE = numeric(),
                   SITEPREP = numeric())
  return(tree)
}

################################################################################
#fvsGetCols
#
#This function returns a vector of field names corresponding to all columns
#which can be included in a FVS input database.
#
#
#none
#
#Return value
#Character vector of FVS column names
################################################################################

fvsGetCols <- function()
{
  #FVS variables from blank database templates
  fvsVars =c(
    "STAND_ID",      "VARIANT",      "INV_YEAR",     "GROUPS",
    "ADDFILES",      "FVSKEYWORDS",  "GIS_LINK",     "PROJECT_NAME",
    "LATITUDE",      "LONGITUDE",    "REGION",       "FOREST",
    "DISTRICT",      "COMPARTMENT",  "LOCATION",     "ECOREGION",
    "PV_CODE",       "PV_REF_CODE",  "AGE",          "ASPECT",
    "SLOPE",         "ELEVATION",    "ELEVFT",       "BASAL_AREA_FACTOR",
    "INV_PLOT_SIZE", "BRK_DBH",      "NUM_PLOTS",    "NONSTK_PLOTS",
    "SAM_WT",        "STK_PCNT",     "DG_TRANS",     "DG_MEASURE",
    "HTG_TRANS",     "HTG_MEASURE",  "MORT_MEASURE", "MAX_BA",
    "MAX_SDI",       "SITE_SPECIES", "SITE_INDEX",   "MODEL_TYPE",
    "PHYSIO_REGION", "FOREST_TYPE",  "STATE",        "COUNTY",
    "FUEL_MODEL",    "FUEL_0_25_H",  "FUEL_25_1_H",  "FUEL_1_3_H",
    "FUEL_3_6_H",    "FUEL_6_12_H",  "FUEL_12_20_H", "FUEL_20_35_H",
    "FUEL_35_50_H",  "FUEL_GT_50_H",  "FUEL_0_25_S", "FUEL_25_1_S",
    "FUEL_1_3_S",    "FUEL_3_6_S",   "FUEL_6_12_S",  "FUEL_12_20_S",
    "FUEL_20_35_S",  "FUEL_35_50_S", "FUEL_GT_50_S", "FUEL_LITTER",
    "FUEL_DUFF",     "PHOTO_REF",    "PHOTO_CODE",   "PLOT_ID",
    "STANDPLOT_ID",  "TREE_ID",      "TREE_COUNT",   "HISTORY",
    "SPECIES",       "DIAMETER",     "DG",           "HT",
    "HTG",           "HTTOPK",       "CRRATIO",      "DAMAGE1",
    "SEVERITY1",     "DAMAGE2",      "SEVERITY2",    "DAMAGE3",
    "SEVERITY3",     "TREEVALUE",    "PRESCRIPTION", "TOPOCODE",
    "SITEPREP",      "DBH",          "STAND_CN",     "STANDPLOT_CN")

  return(fvsVars)
}

################################################################################
#fvsGetTypes
#
#This function returns a character vector containing data types associated with
#values returned from fvsGetCols function.
#
#
#none
#
#Return value
#Character vector of FVS field data types.
################################################################################

fvsGetTypes <- function()
{
  #Datatypes for FVS variables
  fvsTypes = c(
    "character", "character", "integer",   "character",
    "character", "character", "character", "character",
    "double",    "double",    "integer",   "integer",
    "integer",   "integer",   "integer",  "character",
    "character", "integer",   "integer",   "double",
    "double",    "double",    "double",    "double",
    "double",    "double",    "integer",   "integer",
    "double",    "double",    "integer",   "integer",
    "integer",   "integer",   "integer",   "double",
    "double",    "character", "double",    "integer",
    "integer",   "integer",   "integer",   "integer",
    "integer",   "double",    "double",    "double",
    "double",    "double",    "double",    "double",
    "double",    "double",    "double",    "double",
    "double",    "double",    "double",    "double",
    "double",    "double",    "double",    "double",
    "double",    "integer",   "character", "double",
    "character", "double",    "double",    "double",
    "character", "double",    "double",    "double",
    "double",    "double",    "double",    "double",
    "double",    "double",    "double",    "double",
    "double",    "double",    "double",    "double",
    "double",    "double",    "character", "character")

  return(fvsTypes)
}

################################################################################
#setDataTypes
#
#This function accepts a dataframe and checks if all columns in the data frame
#match a specified datatype. If a column does not match a specified data type,
#the column in the dataframe is cast to the correct data type. Only double,
#integer, and character values are considered in this function. If a column is
#not recognized, then it is either ignored or cast to character type depending
#on the value specified in argument ignoreCols.
#
#
#data:       Input dataframe
#
#cols:       Character vector of variable names.By default this argument
#            is set  to NULL. When this value is NULL, variables will be
#            set to values produced by fvsGetCols function. Length of variables
#            argument must match length of types argument.
#
#colTypes:   Character vector of data type types that correspond to variables in
#            argument variables. By default this argument is set to NULL. When
#            this value is NULL, variables will be set to values produced by
#            fvsGetTypes function. Length of types argument must match length of
#            types argument.
#
#verbose:    Boolean variable that determines if debug information should be
#            printed to console. By default this argument set to FALSE.
#
#Return value
#
#Input dataframe
################################################################################

setDataTypes<-function(data,
                       cols = NULL,
                       colTypes = NULL,
                       verbose = F)
{

  #If data is not dataframe stop with error message
  if(!is.data.frame(data))
  {
    stop("Argument data must be a dataframe.")
  }

  #If data has no rows return
  if(nrow(data) <= 0)
  {
    return(data)
  }

  #If cols is NULL, call fvsGetVars
  if(is.null(cols))
  {
    cols = fvsGetCols()
  }

  #If colTypes is NULL, call fvsGetTypes
  if(is.null(colTypes))
  {
    colTypes = fvsGetTypes()
  }

  #If length of variables is not equal to types, return with error.
  if(length(cols) != length(colTypes))
  {
    if(verbose) cat("Length cols:", length(cols), "\n")
    if(verbose) cat("Length types:", length(colTypes), "\n")
    stop("Variables and types arguments must have the same length.")
  }

  #Iterate across columns of input dataframe
  for(i in 1:length(names(data)))
  {
    #Extract column name
    colname<-toupper(names(data)[i])
    if(verbose) cat("Column:", colname, "being processed.", "\n")

    #Attempt to match column name with variable in fvsvars
    varIndex<-match(colname, cols)

    #If varIndex is not NA, extract the data type for the column from colTypes.
    if(!is.na(varIndex))
    {
      #Extract datatype from colTypes.
      datatype<-colTypes[varIndex]

      #If data type of column matches with designated data type, move to next
      #loop iteration.
      if(typeof(data[,i]) == datatype){
        if(verbose) cat("Data type of", colname, "is a match.", "\n")
        next
      }

      #Variable is a character
      if(datatype == "character")
      {
        #Print message that column will be converted to character.
        if(verbose) cat(colname, "being converted to", datatype, "\n")
        data[,i]<-as.character(data[,i])
      }

      #Variable is a integer
      if(datatype == "integer")
      {
        #Print message that column will be converted to integer.
        if(verbose) cat(colname, "being converted to", datatype, "\n")
        data[,i]<-as.integer(data[,i])
      }

      #Variable is a double
      if(datatype == "double")
      {
        #Print message that column will be converted to double.
        if(verbose) cat(colname, "being converted to", datatype, "\n")
        data[,i]<-as.double(data[,i])
      }
    }

    #If varIndex is NA it will be ignored.
    else
    {
      if(verbose) cat(colname, "not recognized and ignored.", "\n")
    }
  }

  return(data)
}

################################################################################
#convertType
#
#This function maps R data types to SQLite data types and vice versa depending
#on the input argument type.
#
#
#Value   Character string corresponding to an R or SQLite data type
#
#type:     Integer variable used to determine if converting to R data type
#          or SQLite data type.
#          1: Convert to SQLite database type
#          2: Convert to R database type
#
#Character string corresponding to R of SQLite data type
################################################################################

convertType <- function(value,
                        type = 1)
{
  result <- NA
  
  #Convert to SQLite database type from R data type
  if(type == 1)
  {
    result = switch(  
      value,  
      "double" = "REAL",  
      "integer"= "INTEGER",  
      "character"= "TEXT",  
      "logical"= "INTEGER",
      "complex"= "TEXT")
    
    #If result is still NA, set to TEXT
    if(is.na(result)) result <- "TEXT"
  }
  
  #Convert to R data type from SQLite data type. SQLite data types are based on
  #those specified in PRAGMA table_info(name of table)
  #Convert to SQLite database type from R data type
  else
  {
    value <- toupper(value)
    result = switch(  
      value,  
      "REAL" = "double",  
      "INT"= "integer",
      "INTEGER"= "integer", 
      "TEXT"= "character")
    
    #If result is still NA, set to character
    if(is.na(result)) result <- "character"
  }
  
  return(result)
}

################################################################################
#checkDataTypes
#
#This function is used to determine if column data types in an input dataframe
#match the field types in a table in a SQLite database where information in 
#dataframe will be inserted. If a column type in the dataframe does not match
#the data type of the field in the SQLite database, then the column in the 
#dataframe is cast to the correct type.
#
#
#conn:      Connection to SQLite database.
#
#tableName: Name of table in SQLite database.
#
#data:      Input dataframe whose columnn data types will compared against data
#           types in argument tableName.
#
#verbose:   Logical variable that determines if debug information should be
#           printed to console. By default this argument set to FALSE.
#

#data frame
################################################################################

checkDataTypes <- function(con,
                           tableName,
                           data,
                           verbose = F)
{

  #Extract field names and data types for tableName
  tableDefs <- RSQLite::dbGetQuery(con,
                                   paste0("PRAGMA table_info('",
                                          tableName,
                                          "')"))[,c(2,3)]
  
  #Obtain column names from data
  dfCols <- colnames(data)
  
  #Start loop across column names in data
  for(i in 1:length(dfCols))
  {
    colName <- dfCols[i]
    
    #Determine type of colName
    colType <- typeof(data[[colName]])
    
    #Find column in tableDefs
    colIndex <- match(colName, tableDefs$name)
    
    #If colName is not in tableDefs skip to next iteration in loop
    if(is.na(colIndex)) next
    
    #Obtain data type of field
    fieldType <- tableDefs$type[colIndex]
    
    #Get R version of fieldType
    fieldType <- convertType(fieldType, 2)
    
    #If verbose, print column name, field type and column type
    if(verbose)
    {
      cat("Column name:", colName, "\n")
      cat("Field type:", fieldType, "\n")
      cat("Column type:", colType, "\n")
    }
    
    #Check if colType and fieldType are the same. If they are, skip to the next
    #iteration
    if(colType == fieldType)
    {
      if(verbose) cat("Data type of", colName, "is a match.", "\n", "\n")
      next
    }
    
    #Types did not match
    else
    {
      #If field type is double, convert colName to double
      #Print message that column will be converted to double.
      if(fieldType == "double")
      {
        if(verbose) cat(colName, "being converted to", fieldType, "\n", "\n")
        data[,i]<-as.double(data[,i])
      }
      
      #If field type is integer, convert colName to integer
      #Print message that column will be converted to integer.
      else if(fieldType == "integer")
      {
        if(verbose) cat(colName, "being converted to", fieldType, "\n", "\n")
        data[,i]<-as.integer(data[,i])
      }
      
      #If field type is anything else, convert to character
      #Print message that column will be converted to character.
      else
      {
        if(verbose) cat(colName, "being converted to character", "\n", "\n")
        data[,i]<-as.character(data[,i])
      }
    }
  }
  
  return(data)
}