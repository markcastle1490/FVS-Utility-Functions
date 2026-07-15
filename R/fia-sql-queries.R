################################################################################
#This file contains the logic that is used to query a FIA SQLite database to
#build a fitting database (fitdb). The queries in this file are used in
#fia-fitdb.R.
################################################################################

fia_tree_query <- function() {
  # Variables from TREE table
  tree <- paste0("TREE.", c("STATECD", "UNITCD", "COUNTYCD", "PLOT", "SUBP", "TREE", 
                           "INVYR", "CYCLE", "SPCD", "DIA", "DIAHTCD", "TPA_UNADJ", 
                           "STATUSCD", "HTCD", "HT", "ACTUALHT", "PREVDIA", 
                           "DAMAGE_AGENT_CD1", "DAMAGE_AGENT_CD2", "DAMAGE_AGENT_CD3", 
                           "BHAGE", "CONDID", "CR", "CULL", "DIACHECK", "AGENTCD", 
                           "RECONCILECD", "HTDMP"))
  
  # Variables from COND table
  cond <- paste0("COND.", c("COND_STATUS_CD", "FORTYPCD", "CONDPROP_UNADJ", "OWNCD", 
                           "ADFORCD", "DSTRBCD1", "DSTRBCD2", "DSTRBCD3", "TRTCD1", 
                           "TRTCD2", "TRTCD3", "STDORGCD"))
  
  # Variables from PLOT table
  plot <- paste0("PLOT.", c("LAT", "LON", "ELEV", "MEASYEAR", "MEASMON", "MEASDAY", 
                           "DESIGNCD", "KINDCD"))
  
  # Variables from SUBPLOT table
  subplot <- paste0("SUBPLOT.", c("SLOPE", "ASPECT"))
  
  # Variables from REF_SPECIES table
  ref_species <- paste0("REF_SPECIES.", c("SPECIES_SYMBOL", "WOODLAND", "GENUS", "SFTWD_HRDWD"))
  
  # Combine variables into a single string
  fia_vars <- paste(c(tree, cond, plot, subplot, ref_species), collapse = ", ")
  
  # Define query using explicit surrogate keys
  query <- paste(c(
    "SELECT", fia_vars, 
    "FROM TREE",
    
    #Join COND using the unique plot visit ID and condition number
    "LEFT JOIN COND ON TREE.PLT_CN = COND.PLT_CN AND TREE.CONDID = COND.CONDID",
    
    #Join PLOT using the primary visit control number (CN)
    "LEFT JOIN PLOT ON TREE.PLT_CN = PLOT.CN",
    
    #Join SUBPLOT using the plot visit ID and subplot number
    "LEFT JOIN SUBPLOT ON TREE.PLT_CN = SUBPLOT.PLT_CN AND TREE.SUBP = SUBPLOT.SUBP",
    
    #Join REF_SPECIES via the species code
    "LEFT JOIN REF_SPECIES ON TREE.SPCD = REF_SPECIES.SPCD",
    
    #Filters
    "WHERE PLOT.KINDCD IN (1, 2, 3)",
    "  AND COND.COND_STATUS_CD = 1",
    "  AND PLOT.MEASYEAR IS NOT NULL",
    "  AND PLOT.CYCLE IS NOT NULL",
    "  AND TREE.DIA IS NOT NULL",
    "  AND TREE.STATUSCD IN (1, 2)",
    "  AND TREE.TPA_UNADJ IS NOT NULL;"
  ), collapse = "\n")
  
  return(query)
}

fia_si_query = function()
{
  #Variables from SITETREE table
  sitree <- paste0("SITETREE.",
                 c("STATECD", "UNITCD", "COUNTYCD", "PLOT", "INVYR", "SUBP",
                   "CONDID", "SPCD", "TREE", "AGEDIA", "HT", "SITREE", "SIBASE",
                   "VALIDCD", "SITREE_FVS", "SIBASE_FVS"))
  
  #Collapse variables
  fia_si_vars <- paste(sitree, collapse = ", ")
  
  #Build query
  query <- paste(c("SELECT",
                   fia_si_vars,
                   "FROM SITETREE;"),
                   collapse = "\n")

  return(query)
}
  