################################################################################
#'fvs_get_locs
#'@name fvs_get_locs
#'@description
#'This function returns of a dataframe containing valid location codes for 
#'specified FVS variants.
#
#'@param var_code: 
#'Character vector of FVS variant codes (e.g. "CA").
#
#'@return
#'Dataframe containing valid location codes for specified FVS variants.
################################################################################

#'@export
fvs_get_locs <- function(var_code = NULL)
{
  #Initialize empty data.frame
  var_loc_df <- data.frame(VARIANT = character(),
                           LOCATION = integer())
  
  #If no variants specified, return
  if(is.null(var_code)) return(var_loc_df)
  
  #Define list for storing variant locations
  var_loc_list <- vector(mode = "list",
                         length = length(var_code))
  
  #Upper case var_code
  var_code = toupper(var_code)
  
  #Loop across fvs_loc_list and construct dataframe containing list of valid
  #location codes for given variant
  for(i in 1:length(var_code))
  {
    var = var_code[i]
    
    #Get locations, if variant is valid
    if(var %in% names(fvs_loc_list))
    {
      locs <- fvs_loc_list[[var]]
      
      #Build dataframe
      var_df <- expand.grid(VARIANT = var,
                            LOCATION = locs)
      
      #Add dataframe to var_loc_list
      var_loc_list[[i]] <- var_df
    }
  }
  
  #Bind var_loc_list into dataframe if there are values in list
  if(!all(sapply(var_loc_list, is.null)))
    var_loc_df <- do.call("rbind", var_loc_list)
  
  return(var_loc_df)
}
