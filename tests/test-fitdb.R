library(fvstools)

fia_db = "C:/Users/markcastle/Downloads/SQLite_FIADB_AK/SQLite_FIADB_AK.db"
fit_db = "C:/Users/markcastle/Downloads/SQLite_FIADB_AK/AK_GST.db"

if(file.exists(fit_db)) unlink(fit_db)

system.time(fia_fitdb(dbin = fia_db,
          dbout = fit_db,
          fitdb_name = "TEST",
          verbose = TRUE))

gc()

