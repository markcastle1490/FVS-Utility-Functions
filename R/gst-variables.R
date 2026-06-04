################################################################################
#get_gst_vars:
#
#Description
#
#This function returns a named vector where the names are variables included in
#the GST and the values are the associated data type of the variable. The order
#of the values in this vector are used to control the order of columns when
#growth sample tree data is written to output database (in writeGST). Additional
#variables and order of variables can be changed as needed.
#
#Source Code
#
#Function get_gst_vars is currently located in the GST_Prep_Functions.R file.
#
#Arguments
#
#None
#
#Value
#
#Character vector containing names of GST columns.
################################################################################

#'@export
get_gst_vars <- function()
{
  gst_vars <- c("UNIQUEPLOTID" = "TEXT",
               "UNIQUESUBPID" = "TEXT",
               "UNIQUETREEID" = "TEXT",
               "STATECD" = "INTEGER",
               "INVYR" = "INTEGER",
               "UNITCD" = "INTEGER",
               "COUNTYCD" = "INTEGER",
               "PLOT" = "INTEGER",
               "SUBP" = "INTEGER",
               "TREE" = "INTEGER",
               "CONDID" = "INTEGER",
               "CONDPROP_UNADJ" = "REAL",
               "COND_STATUS_CD" = "INTEGER",
               "DESIGNCD0" = "INTEGER",
               "DESIGNCD1" = "INTEGER",
               "SPECIES" = "INTEGER",
               "GENUS" = "TEXT",
               "TPAR" = "REAL",
               "TPARSUBP" = "REAL",
               "MEASYEAR0" = "INTEGER",
               "MEASYEAR1" = "INTEGER",
               "MEASMON0" = "INTEGER",
               "MEASMON1" = "INTEGER",
               "MEASDAY0" = "INTEGER",
               "MEASDAY1" = "INTEGER",
               "MEASLEN" = "INTEGER",
               "CYCLE0" = "INTEGER",
               "CYCLE1" = "INTEGER",
               "STATUSCD0" = "INTEGER",
               "STATUSCD1" = "INTEGER",
               "RECONCILECD" = "INTEGER",
               "DIAHTCD" = "INTEGER",
               "DIA0" = "REAL",
               "DIA1" = "REAL",
               "HT0" = "REAL",
               "HT1" = "REAL",
               "BT" = "INTEGER",
               "CR0" = "REAL",
               "CR1" = "REAL",
               "HCB0" = "REAL",
               "HCB1" = "REAL",
               "CW0" = "REAL",
               "CW1" = "REAL",
               "MORT" = "INTEGER",
               "AGENTCD0" = "INTEGER",
               "AGENTCD1" = "INTEGER",
               "DI" = "REAL",
               "HI" = "REAL",
               "MRTCD" = "INTEGER",
               "IDGRM" = "INTEGER",
               "IHGRM" = "INTEGER",
               "DIACHECK0" = "INTEGER",
               "DIACHECK1" = "INTEGER",
               "HTDMP0" = "INTEGER",
               "HTDMP1" = "INTEGER",
               "DIACHG" = "INTEGER",
               "CULL" = "REAL",
               "DAMAGE_AGENT_CD1" = "INTEGER",
               "DAMAGE_AGENT_CD2" = "INTEGER",
               "DAMAGE_AGENT_CD3" = "INTEGER",
               "LAT" = "REAL",
               "LON" = "REAL",
               "ELEV" = "REAL",
               "SLOPE" = "REAL",
               "ASPECT" = "REAL",
               "SITEINDEX" = "REAL",
               "BASEAGE" = "REAL",
               "FORTYPCD" = "INTEGER",
               "ECOREGION" = "INTEGER",
               "OWNCD" = "INTEGER",
               "ADFORCD" = "INTEGER",
               "KINDCD" = "INTEGER",
               "DSTRBCD1" = "INTEGER",
               "DSTRBCD2" = "INTEGER",
               "DSTRBCD3" = "INTEGER",
               "TRTCD1" = "INTEGER",
               "TRTCD2" = "INTEGER",
               "TRTCD3" = "INTEGER",
               "DATAPROVIDER" = "TEXT",
               "PLOTQUERYID" = "TEXT",
               "TREEMERGEID" = "TEXT")

  return(gst_vars)
}

################################################################################
#get_gp_vars
#
#Description
#
#This function returns the column names of variables that should be included in
#the data frame that is passed to the merge_inv function. These variables
#can be updated as necessary.
#
#Source Code
#
#Function get_gp_vars is currently located in the GST_Prep_Functions.R
#file.
#
#Arguments
#
#type: 	Integer variable indicating what variables to return.
#       1 = variables required before call to merge_inv
#       2 = variables required after call to merge_inv
#       3 = renamed variables after call to merge_inv
#
#Value
#
#Character vector containing column names.
################################################################################

get_gp_vars <- function(type = 1)
{
  #Set type to 1 if value is anything other than 1-3.
  if(!type %in% c(1, 2, 3)) type <- 1

  #Define variables that will have time 0 and 1 values
  #This list of variables needs to be updated if additional time 0 and 1
  #variables are included in merge_inv function
  time_vars <- c("CYCLE",
                "MEASYEAR",
                "MEASMON",
                "MEASDAY",
                "DIA",
                "HT",
                "CR",
                "STATUSCD",
                "AGENTCD",
                "DIACHECK",
                "HTDMP",
                "DESIGNCD")

  #Variables prior to call to growthPeriods function.
  if(type == 1)
  {
    gp_vars <- c("UNIQUETREEID",
                "UNIQUESUBPID",
                "TREEMERGEID",
                "SPECIES",
                time_vars)
  }

  #Variables following call to growthPeriods function right after merge.
  else if(type == 2)
  {

    #.x variables after merge
    xvars <- paste0(time_vars, ".x")

    #.y variables after merge
    yvars <- paste0(time_vars, ".y")

    gp_vars <- c("UNIQUETREEID.x",
                "TREEMERGEID",
                "SPECIES",
                xvars,
                yvars)
  }

  #Renamed variables following call to growthPeriods function.
  else
  {
    #0 variables after merge (beginning of measurement period)
    vars0 <- paste0(time_vars, "0")

    #1 variables after merge (end of measurement period)
    vars1 <- paste0(time_vars, "1")

    gp_vars <- c("UNIQUETREEID",
                "TREEMERGEID",
                "SPECIES",
                vars0,
                vars1)
  }

  return(gp_vars)
}
