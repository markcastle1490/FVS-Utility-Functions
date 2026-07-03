#===============================================================================
# Define paths
#===============================================================================

pkg_root <- "/home/mark/FVS_Tools/fvstools"
pkg_name <- "fvstools"
r_lib <- "/home/mark/R/x86_64-pc-linux-gnu-library/4.5"

#===============================================================================
# Clean previous session state
#===============================================================================

if (pkg_name %in% loadedNamespaces()) {
  unloadNamespace(pkg_name)
}

if (paste0("package:", pkg_name) %in% search()) {
  detach(paste0("package:", pkg_name), unload = TRUE, character.only = TRUE)
}

#===============================================================================
# CRAN-style build (NO devtools loading)
#===============================================================================

unlink(file.path(pkg_root, "NAMESPACE"))
roxygen2::roxygenise(pkg_root, clean = TRUE)
system(paste("R CMD build", shQuote(pkg_root)))

# Find actual tarball (don’t assume name)
tarball <- list.files(
  path = ".",
  pattern = paste0(pkg_name, "_.*\\.tar\\.gz"),
  full.names = TRUE
)

if (length(tarball) != 1) {
  stop("Could not uniquely identify tarball")
}

#===============================================================================
# Install
#===============================================================================

cmd <- paste(
  "R CMD INSTALL",
  shQuote(tarball),
  "--library=", shQuote(r_lib),
  "--preclean"
)
system(cmd, intern = TRUE)

library(fvstools, li.b)

ba_f(dbh = c(10, 11, 12), expf = c(10, 10, 10), species = c(0, 1, 0), select_species = 0)

