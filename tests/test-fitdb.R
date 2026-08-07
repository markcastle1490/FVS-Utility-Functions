library(fvstools)

#Setup input and output names
db_in <- "SQLite_FIADB_AK.db"
db_out <- "AK_GST.db"

#Setup paths
if(.Platform$OS.type == 'windows'){
  fia_db = file.path("C:/FIA_Data", db_in)
  fit_db = file.path("C:/FIA_Data", db_out)
} else {
  fia_db = file.path("/home/FIA_Data", db_in)
  fit_db = file.path("/home/FIA_Data", db_out)
}

if(file.exists(fit_db)) unlink(fit_db)

#60302

test <- fia_fitdb(dbin = fia_db,
          dbout = fit_db,
          fitdb_name = "TEST",
          verbose = TRUE)

test[test$PLOT == '60302', ]
gc()


