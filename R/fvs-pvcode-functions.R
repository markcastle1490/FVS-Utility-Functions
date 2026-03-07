################################################################################
#'fvs_pvcodes_var
#'@name fvs_pvcodes_var
#'@description
#'
#'This function is used to obtain dataframe that contains valid PV or habitat
#'type codes for a given variant or set of variants.
#'
#'@param var_code:
#'Character vector of two-character codes corresponding to FVS variant 
#'(e.g. "CA").
#'
#'@param all_var:
#'Logical variable where if TRUE, species codes for all FVS variants will be 
#'returned. This argument will take precedence over values specified in 
#'var_code argument.
#'
#'@return
#'Dataframe with species codes.
################################################################################

#'@export
fvs_pvcodes_var <- function(var_code = NULL,
                            all_var = FALSE)
{
  #Initialize empty dataframe
  pvcode_df = data.frame(VARIANT = character(),
                         SEQ = integer(),
                         PV_CODE = character())
  
  #If var_code is empty and all_var is FALSE return
  if(length(var_code) <= 0 && !all_var)
    return(pvcode_df)
  
  #Get dataframe for all_var
  if(all_var)
    pvcode_df = pv_codes
  
  #Get information for values in var_code
  else
  {
    #Initialize list for storing variant species codes
    var_df_list = vector(mode = "list", length(var_code))
    
    #Start loop over var_code
    for(i in 1:length(var_code))
    {
      #Uppercase var
      var = toupper(var_code[[i]])
      
      #Get species codes and add to list if var is valid
      if(var %in% unique(pv_codes$VARIANT))
      {
        pvcode_df_ = pv_codes[pv_codes$VARIANT == var,]
        var_df_list[[i]] = pvcode_df_
      }
    }
    
    #Bind var_df_list if it is not empty
    if(length(var_df_list) > 0)
    {
      pvcode_df = do.call("rbind", c(list(pvcode_df), var_df_list))
      row.names(pvcode_df) <- NULL
    }
  }
  
  return(pvcode_df)
}

################################################################################
#'fvs_pvcodes_reg
#'@name fvs_pvcodes_reg
#'@description
#'
#'This function is used to obtain a dataframe that contains FS REGION, SEQ, 
#'HABPVR, PVCODE, and PVREF code. The values in this data frame correspond to 
#'those found in the pvref FORTRAN routines for a given FVS variant. 
#'
#'@param var_code:
#'Character vector of two-character codes corresponding to FVS variant 
#'(e.g. "CA").
#'
#'@param all_var:
#'Logical variable where if TRUE, species codes for all FVS variants will be 
#'returned. This argument will take precedence over values specified in 
#'var_code argument.
#'
#'@return
#'Dataframe with species codes.
################################################################################

#'@export
fvs_pvcodes_reg <- function(var_code = NULL,
                            all_var = FALSE)
{
  #Initialize empty dataframe
  pvcode_df = data.frame(VARIANT = character(),
                         PV_CODE = character())
  
  #If var_code is empty and all_var is FALSE return
  if(length(var_code) <= 0 && !all_var)
    return(pvcode_df)
  
  #Get dataframe for all_var
  if(all_var)
    pvcode_df = pv_codes_regions
  
  #Get information for values in var_code
  else
  {
    #Initialize list for storing variant species codes
    var_df_list = vector(mode = "list", length(var_code))
    
    #Start loop over var_code
    for(i in 1:length(var_code))
    {
      #Uppercase var
      var = toupper(var_code[[i]])
      
      #Get species codes and add to list if var is valid
      if(var %in% unique(pv_codes_regions$VARIANT))
      {
        pvcode_df_ = pv_codes_regions[pv_codes_regions$VARIANT == var,]
        var_df_list[[i]] = pvcode_df_
      }
    }
    
    #Bind var_df_list if it is not empty
    if(length(var_df_list) > 0)
    {
      pvcode_df = do.call("rbind", c(list(pvcode_df), var_df_list))
      row.names(pvcode_df) <- NULL
    }
  }
  
  return(pvcode_df)
}