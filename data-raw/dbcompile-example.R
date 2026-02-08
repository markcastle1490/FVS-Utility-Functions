################################################################################
#dbCompile_Example.R
#
#This script shows an example of how to use the dbCompile function from fvsUtil
#R package to combine multiple FIA SQLite databases.
################################################################################

#Load packages 
library(RSQLite)
library(fvsUtil)

#Look at information about dbCompile if you like
help(dbCompile)

#===============================================================================
#Combine CA, OR, and WA into a single output database
#===============================================================================

dbCompile(dbIn = c("C:/FIA_Data/SQLite_FIADB_CA.zip",
                   "C:/FIA_Data/SQLite_FIADB_OR.zip",
                   "C:/FIA_Data/SQLite_FIADB_WA.zip"),
          dbOut = "C:/FIA_Data/FIADB_PNW.db",
          buildGaak = TRUE)

#===============================================================================
#Combine select database tables from FIADB_PNW into a single output database
#===============================================================================

#Connect to newly crated FIADB_PNW.db and get character vector of all database
#tables
con <- dbConnect(SQLite(),
                 "C:/FIA_Data/FIADB_PNW.db")

db_tables <- dbListTables(con)

dbDisconnect(con)

dbCompile(dbIn = c("C:/FIA_Data/FIADB_PNW.db"),
          dbOut = "C:/FIA_Data/FIADB_PNW2.db",
          dbTables = c("FVS_PLOTINIT_PLOT",
                       "FVS_STANDINIT_COND",          
                       "FVS_STANDINIT_PLOT",
                       "FVS_TREEINIT_COND",
                       "FVS_TREEINIT_PLOT"),
          buildGaak = TRUE)
