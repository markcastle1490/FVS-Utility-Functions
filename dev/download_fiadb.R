library(fvstools)
library(future)
library(future.callr)

#Download directory
if(.Platform$OS.type == 'windows') fia_dir <- "C:/FIA_Data" else 
  fia_dir <- "/home/mark/FIA_Data"

#Setup plan for running keyword files in parallel
plan(callr)
on.exit(plan(sequential), add = TRUE)

#Get states to process
states <- fvstools::state_df()[['STATE_ABBRV']]
states <- states[!states %in% c("AS", "FM", "GU", "MH", "MP", "PW", "PR", "VI",
                               "DC", "HI")]

#Setup list for storing returns from future
futures <- vector(mode = "list", length = length(states))

#Run keyword files
for(i in 1:length(states))
{
  futures[[i]] <- future({ download_fia(fia_dir,
                                       states = states[i],
                                       verbose = TRUE)})

}


#Create results - blocks further code from running until all keyword files have 
#completed running.
results = lapply(futures, value)

# system.time(build_fitdb(dbin = 
#                         !list.files(path = fia_dir, pattern = ".db$", full.names = TRUE) %in% 
#                           "/home/mark/FIA_Data/SQLite_FIADB_HI.db",
#             dbout = file.path(fia_dir, "ALL_FITDB.db")))

any(c("TEST1", "TEST2") == 'TEST3')
