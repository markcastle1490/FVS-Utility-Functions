
#===============================================================================
# Define paths
#===============================================================================

#Windows
if(.Platform$OS.type == 'windows'){
  pkg_root <- "C:/FVS_Tools/fvstools"
  build_dir <- "C:/FVS_Tools"
  r_lib <- "C:/FVS/FVSSoftware/R/R-4.5.0/library"
  r_path    <- "C:/FVS/FVSSoftware/R/R-4.5.0/bin/R.exe" 
} 

#Linux
if(.Platform$OS.type == 'unix'){
  pkg_root <- "/home/mark/FVS_Tools/fvstools"
  build_dir <- "/home/mark/FVS_Tools"
  r_lib <- "/home/mark/R/x86_64-pc-linux-gnu-library/4.5"
  r_path    <- "/usr/bin/R" 
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

#Save original working directory location
old_dir <- getwd()

#Change to target build directory
setwd(build_dir)

#Document
roxygen_cmd <- sprintf("roxygen2::roxygenise(package.dir = %s, clean = TRUE)", shQuote(pkg_root))

system2(
  command = r_path,
  args    = c("--vanilla", "--quiet", "-e", shQuote(roxygen_cmd)),
  stdout  = TRUE, 
  stderr  = TRUE,
  env     = c(paste0("R_LIBS_USER=", r_lib), paste0("R_LIBS_SITE=", r_lib))
)

#Call system2 to build
system2(
  command = r_path,
  args = c("CMD build", "fvstools", shQuote(pkg_root)),
  stdout = TRUE,
  stderr = TRUE,
  env = c(paste0("R_LIBS_USER=", r_lib), paste0("R_LIBS_SITE=", r_lib)))

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

#Define arguments as character vector
install_args <- c("CMD INSTALL", 
                  shQuote(tarball), 
                  paste0("--library=", shQuote(r_lib)), 
                  "--preclean")

#Execute using system2
system2(
  command = r_path, 
  args = install_args,
  stdout  = TRUE, 
  stderr  = TRUE,
  env     = c(paste0("R_LIBS_USER=", r_lib), paste0("R_LIBS_SITE=", r_lib)))

# Find the exact path where fvstools is installed
pkg_path <- find.package("fvstools", lib.loc = r_lib)

# Read R's internal installation record
meta <- readRDS(file.path(pkg_path, "Meta", "package.rds"))

# Print the R version details
print(meta$Built)

#Reset working directory
setwd(old_dir)

#Clean up
rm(list=ls())
