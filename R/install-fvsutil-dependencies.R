################################################################################
#'install_pkgs
#'@name install_pkgs
#'
#'@description
#'This function is used to install the necessary and suggested packages from 
#'CRAN for use with the fvsUtil package. Packages will only be installed if they
#'are not found in the version of R being used or if the override argument is 
#'set to TRUE.
#'
#'@param override:
#'Logical variable where if TRUE, a given package will be installed even if it 
#'already exists on users system.
#
#'@return None
################################################################################

#'@export
install_pkgs <- function(override = FALSE)
{
  #Vector of required packages
  required_packages <- c("RSQLite",
                         "httr",
                         "callr")
  
  #Get vector of installed packages
  installed_pkg = installed.packages()[, "Package"]
  
  #Loop across packages, install if not available
  for(package in required_packages)
  {
    if (!package %in% installed_pkg)
      install.packages(pkgs = package)
    else if(override)
      install.packages(pkgs = package)
    else
      cat("Package:", package, "is already installed.", "\n")
  }
  
  invisible()
}
