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

################################################################################
#'fvs_pvlookup
#'@name fvs_pvlookup
#'@description
#'
#'This function is used to lookup the index number of a PV code for a specified
#'FVS variant.
#'
#'@param var_code:
#'Character vector of two-character codes corresponding to FVS variant 
#'(e.g. "CA").
#'
#'@param pvcode:
#'Character string corresponding to PV Code. 
#'
#'@return
#'Positive non-zero integer value if index is found, otherwise a NA value.
################################################################################

#'@export
fvs_pvlookup <- function(var_code = "IE",
                         pvcode = "")
  
{
  pvidx = NA
  
  #Return if inputs are invalid
  if(is.na(var_code) || is.na(pvcode))
    return(pvidx)
  
  #Uppercase var_code
  var_code = toupper(var_code)
  
  #If var_code is not valid, return 
  if(! var_code %in% variants)
    return(pvidx)
  
  #Cast pvcode to character if needed
  if(!is.character(pvcode)) pvcode = as.character(pvcode)
  
  #Do the lookup
  pvidx = match(pvcode, pvcode_list[[var_code]])
  
  return(pvidx)
}

################################################################################
#'fvs_pvlookup_reg
#'@name fvs_pvlookup_reg
#'@description
#'
#'This function is used to lookup the index number of a PV code for a specified
#'FVS variant and region number.
#'
#'@param var_code:
#'Character vector of two-character codes corresponding to FVS variant 
#'(e.g. "CA").
#'
#'@param region:
#'Integer value corresponding to USFS region. Valid values are 1, 2, 3, 4, 5, 6,
#'and 9.
#'
#'@param pvcode:
#'Character string corresponding to HABPVR (pv code and reference combination),
#'PVCODE (PV code), PVREF (PV reference code).
#'
#'@param from:
#'Integer value that tells what kind of value should be searched for:
#'
#'1: HABPVR
#'
#'2: PVCODE
#'
#'3: PVREF
#'
#'@return
#'Positive non-zero integer value if index is found, otherwise a NA value.
################################################################################

#'@export
fvs_pvlookup_reg <- function(var_code = "IE",
                             region = 1,
                             pvcode = "",
                             from = 0)
  
{
  pvidx = NA
  
  #Uppercase var_code
  var_code = toupper(var_code)
  
  #If var_code or region is not valid, return 
  if(! var_code %in% variants || ! region %in% c(1, 2, 3, 4, 5, 6, 9))
    return(pvidx)
  
  #Catch bad values
  if(!from %in% c(1, 2, 3)) from = 1
  
  #Cast pvcode to character if needed
  if(!is.character(pvcode)) pvcode = as.character(pvcode)
  
  #Build variant and region combination
  var_reg = paste0(var_code, region)
  
  #Search HABPVR
  if(from == 1)
    pvidx <- match(pvcode, habpvr_list[[var_reg]])
  
  #Search PVCODE
  else if(from == 2) 
    pvidx <- match(pvcode, pvcode_reg_list[[var_reg]])
  
  #Search PVREF
  else
    pvidx <- match(pvcode, pvref_list[[var_reg]])
  
  return(pvidx)
}