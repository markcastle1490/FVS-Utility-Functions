#===============================================================================
# Define paths
#===============================================================================

pkg_root <- "/home/mark/FVS_Tools/fvstools"
pkg_name <- "fvstools"
build_dir <- "/home/mark/FVS_Tools"
r_lib <- "/home/mark/R/x86_64-pc-linux-gnu-library/4.5"

#===============================================================================
# Detach package if needed
#===============================================================================

if (paste0("package:", pkg_name) %in% search()) {
  detach(paste0("package:", pkg_name), unload = TRUE, character.only = TRUE)
}

#===============================================================================
# Document and build package
#===============================================================================

#Rebuild namespace
roxygen2::roxygenise(pkg_root, clean = TRUE)

#Build package
system(paste("cd", shQuote(build_dir), "&&", "R CMD build", shQuote(pkg_root)))

# Find actual tarball after installation
tarball <- list.files(
  path = build_dir,
  pattern = paste0(pkg_name, "_.*\\.tar\\.gz"),
  full.names = TRUE
)

if(!length(tarball)) stop("No fvstools tarball found.")
tarball = tarball[which.max(file.info(tarball)$mtime)]

#===============================================================================
# Install the package from the tarball
#===============================================================================

system(paste(
  "R CMD INSTALL",
  shQuote(tarball),
  "--library=", shQuote(r_lib),
  "--preclean"
))

library(fvstools)

ba_f(dbh = c(10, 11, 12), expf = c(10, 10, 10), species = c(0, 0, 0), select_species = 0)
tpa_f(dbh = c(10, 11, 12), expf = c(10, 10, 10), species = c(3, 3, 3), select_species = 3)
qmd_f(dbh = c(10, 11, 12), expf = c(10, 10, 10), species = c(1, 3, 3), select_species = 3)
gmd_f(dbh = c(10, 11, 12), expf = c(10, 10, 10), species = c(1, 3, 3), select_species = 3)
lorey_dia_f(dbh = c(4, 4, 12), expf = c(10, 10, 10), species = c(3, 3, 3), select_species = 3)
lorey_dia(dbh = c(4, 4, 12), expf = c(10, 10, 10), species = c(3, 3, 3), select_species = 3)
top_dia_f(dbh = c(4, 4, 12), expf = c(10, 10, 10), dia_type = 1)
top_dia(dbh = c(4, 4, 12), expf = c(10, 10, 10), dia_type = 1)
rsdi_stage_f(dbh = c(4, 4, 12), expf = c(10, 10, 10), species = c(3, 3, 3), select_species = 3, dbhmin = 10)
rsdi_stage(dbh = c(4, 4, 12), expf = c(10, 10, 10), species = c(3, 3, 3), select_species = 3, dbhmin = 10)
zsdi_f(dbh = c(4, 4, 12), expf = c(10, 10, 10), species = c(3, 3, 3), select_species = 3, dbhmin = 10)
zsdi(dbh = c(4, 4, 12), expf = c(10, 10, 10), species = c(3, 3, 3), select_species = 3, dbhmin = 10)
cc_f(crwidth = c(NA, 14, 15), dbh = c(4, 4, 12), expf = c(10, 10, 10), species = c(3, 3, 3), select_species = 3, dbhmin = 10)
cc(crwidth = c(12, 14, 15), dbh = c(4, 4, 12), expf = c(10, 10, 10), species = c(3, 3, 3), select_species = 3, dbhmin = 10)
lorey_ht_f(dbh = c(4, 4, 12), ht=c(32, 40, 70), expf = c(10, 10, 10), species = c(3, 3, 3), select_species = 3)
lorey_ht(dbh = c(4, 4, 12), ht=c(32, 40, 70), expf = c(10, 10, 10), species = c(3, 3, 3), select_species = 3)
top_ht_f(dbh = c(4, 4, 12), ht=c(NA, NA, NA), expf = c(NA, NA, NA))
top_ht(dbh = c(4, 4, 12), ht=c(32, 40, 70), expf = c(10, 10, 10))
bal_f(dbh = c(10, 10, 10), expf = c(10, 10, 10), handle_ties = 1)
bal(dbh = c(10, 10, 10), expf = c(10, 10, 10), handle_ties = TRUE)

system.time({
  dbh = runif(n=1000, min=1, max=20)
  expf = rep(10, times = 1000)
  ht= runif(n=1000, min=1, max=120)
  for(i in 1:10000) {
    zsdi(dbh = dbh, expf = expf)
  }
})

system.time({
  dbh = runif(n=1000, min=1, max=20)
  expf = rep(10, times = 1000)
  ht= runif(n=1000, min=1, max=120)
  for(i in 1:10000) {
    zsdi_f(dbh = dbh, expf = expf)
  }
})
