################################################################################
#'load_dependencies
#'@name load_dependencies
#'@description
#'This function is used to install the required packages from CRAN required to
#'use the fvsUtil R package. If the packages already exist they will simply be
#'loaded into the R session using library().
#
#'@return None
################################################################################

#'@export
load_dependencies <- function()
{
  #Vector of required packages
  required_packages <- c("RSQLite",
                         "httr")
  
  #Loop across packages, install if not available, and then load
  for(package in required_packages)
  {
    
    if (!require(package, character.only=T, quietly=T))
    {
      install.packages(package, type = "binary")
      library(package, character.only = T)
    }
  }
}

