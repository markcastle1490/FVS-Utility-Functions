################################################################################
#This file contains the logic that is used to query a FIA SQLite database to
#build a growth sample tree database (gst). The queries in this file are used in
#fia-gst.R.
################################################################################

#Variables from TREE table
tree = paste0("TREE.", 
              c("STATECD", "UNITCD", "COUNTYCD", "PLOT", "SUBP", "TREE", "INVYR",
                "CYCLE", "SPCD", "DIA", "DIAHTCD", "TPA_UNADJ", "STATUSCD",
                "HTCD", "HT", "ACTUALHT", "PREVDIA", "DAMAGE_AGENT_CD1",
                "DAMAGE_AGENT_CD2", "DAMAGE_AGENT_CD3", "BHAGE","CONDID",
                "CR", "CULL", "DIACHECK", "AGENTCD", "RECONCILECD", "HTDMP"))

#Variables from COND table
cond = paste0("COND.", 
              c("COND_STATUS_CD", "FORTYPCD", "CONDPROP_UNADJ", "OWNCD",
                "ADFORCD", "DSTRBCD1", "DSTRBCD2", "DSTRBCD3", "TRTCD1",
                "TRTCD2", "TRTCD3", "STDORGCD"))

#Variables from PLOT table
plot = paste0("PLOT.", 
              c("LAT", "LON", "ELEV", "MEASYEAR", "MEASMON", "MEASDAY", 
                "DESIGNCD", "KINDCD"))

#Variables from SUBPLOT table
subplot = paste0("SUBPLOT.",
                 c("SLOPE", "ASPECT"))

#Variables from REF_SPECIES table
ref_species = paste0("REF_SPECIES.",
                     c("SPECIES_SYMBOL", "WOODLAND", "GENUS", "SFTWD_HRDWD"))

#Variables from SITETREE table
sitree <- paste0("SITETREE.",
                 c("STATECD", "UNITCD", "COUNTYCD", "PLOT", "INVYR", "SUBP",
                   "CONDID", "SPCD", "TREE", "AGEDIA", "HT", "SITREE", "SIBASE",
                   "VALIDCD"))

#Combine variables into single string
fia_vars = paste(c(tree, cond, plot, subplot, ref_species), collapse = ", ")
fia_si_vars = paste(sitree, collapse = ", ")

#Define query
fia_query = paste(c("SELECT",
                    fia_vars,
                  "FROM TREE",
                  "LEFT JOIN COND ON
                  TREE.STATECD = COND.STATECD
                  AND TREE.INVYR = COND.INVYR
                  AND TREE.UNITCD = COND.UNITCD 
                  AND TREE.COUNTYCD = COND.COUNTYCD
                  AND TREE.PLOT = COND.PLOT
                  AND TREE.CONDID = COND.CONDID",
                  "LEFT JOIN PLOT ON
                  TREE.STATECD = PLOT.STATECD
                  AND TREE.INVYR = PLOT.INVYR
                  AND TREE.UNITCD = PLOT.UNITCD 
                  AND TREE.COUNTYCD = PLOT.COUNTYCD
                  AND TREE.PLOT = PLOT.PLOT",
                  "LEFT JOIN SUBPLOT ON
                  TREE.STATECD = SUBPLOT.STATECD
                  AND TREE.INVYR = SUBPLOT.INVYR
                  AND TREE.UNITCD = SUBPLOT.UNITCD 
                  AND TREE.COUNTYCD = SUBPLOT.COUNTYCD
                  AND TREE.PLOT = SUBPLOT.PLOT
                  AND TREE.SUBP = SUBPLOT.SUBP",
                  "LEFT JOIN REF_SPECIES ON
                  TREE.SPCD = REF_SPECIES.SPCD",
                  "WHERE PLOT.KINDCD IN (1, 2, 3);"),
                  collapse = "\n")

fia_si_query = paste(c("SELECT",
                      fia_si_vars,
                      "FROM SITETREE;"),
                    collapse = "\n")
