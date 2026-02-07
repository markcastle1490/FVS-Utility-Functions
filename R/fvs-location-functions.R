################################################################################
#'fvs_get_locs
#'@name fvs_get_locs
#'@description
#'This function returns of a dataframe containing valid location codes for all
#'variants included in the FVS system
#
#'@return
#'Dataframe containing valid location codes for all variants included in the FVS
#'system
################################################################################

#'@export
fvs_get_locs <- function()
{
  varLocList <- vector(mode = "list",
                       length = length(fvsLocList))
  
  #Loop across fvsLocList and construct dataframe containing list of valid
  #location codes for given variant
  for(i in 1:length(fvsLocList))
  {
    var <- names(fvsLocList)[i]
    locs <- fvsLocList[[i]]
    
    #Build dataframe
    varDF <- expand.grid(VARIANT = var,
                         LOCATION = locs)
    
    #Add dataframe to varLocList
    varLocList[[i]] <- varDF
  }
  
  #Bind varLocList into dataframe and return
  varLocDF <- do.call("rbind", varLocList)
  
  return(varLocDF)
}
