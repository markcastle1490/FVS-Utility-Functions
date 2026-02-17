################################################################################
#'install_dependencies
#'@name install_dependencies
#'
#'@description
#'This function is used to install the required packages from CRAN required to
#'use the fvsUtil R package. If the packages already exist they will simply be
#'loaded into the R session using library().
#'
#'@param override:
#'Logical variable where if TRUE, a given package will be installed even if it 
#'already exists on users system.
#
#'@return None
################################################################################

#'@export
install_dependencies <- function(override = FALSE)
{
  #Vector of required packages
  required_packages <- c("RSQLite",
                         "httr")
  
  #Get vector of installed packages
  installed_pkg = installed.packages()[, "Package"]
  
  #Loop across packages, install if not available
  for(package in required_packages)
  {
    if (!package %in% installed_pkg)
      install.packages(package, type = "binary")
    else if(override)
      install.packages(package, type = "binary")
    else
      cat("Package:", package, "is already installed.", "\n")
  }
  
  invisible()
}
