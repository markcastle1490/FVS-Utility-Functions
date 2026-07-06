#===============================================================================
# Define paths
#===============================================================================

#Windows
if(.Platform$OS.type == 'windows'){
  pkg_root <- OS.typepkg_root <- "C:/FVS_Tools/fvstools"
  build_dir <- "C:/FVS_Tools"
  r_lib <- "C:/FVS/FVSSoftware/R/R-4.5.0"
} 

#Linux
if(.Platform$OS.type == 'unix'){
  pkg_root <OS.typepkg_root <- "/home/mark/FVS_Tools/fvstools"
  build_dir <- "/home/mark/FVS_Tools"
  r_lib <- "/home/mark/R/x86_64-pc-linux-gnu-library/4.5"
}

#Package name
pkg_name <- "fvstools"

#===============================================================================
# Detach package if needed
#===============================================================================

if (paste0("package:", pkg_name) %in% search()) {
  detach(paste0("package:", pkg_name), unload = TRUE, character.only = TRUE)
}

#===============================================================================
# Document and build package
#===============================================================================

#Store current working directory and reset once script is complete
old_dir <- getwd()
setwd(build_dir)

#Rebuild namespace
roxygen2::roxygenise(pkg_root, clean = TRUE)

#Build package
system(paste("R CMD build", shQuote(pkg_root)))

# Find actual tarball after installation
tarball <- list.files(
  path = ".",
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

#Reset the directory
setwd(old_dir)
