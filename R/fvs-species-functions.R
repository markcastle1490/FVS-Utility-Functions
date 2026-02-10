################################################################################
#'fvs_get_spdf
#'@name fvs_get_spdf
#'@description
#'
#'This function returns all species codes for a given FVS variant, set of FVS 
#'variants, or all FVS variants. The species codes included in the returned
#'dataframe are:
#'
#'FVS sequence number
#'
#'FVS species code 
#'
#'FIA species code
#'
#'USDA plant symbol
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
fvs_get_spdf <- function(var_code = NULL,
                         all_var = FALSE)
{
  #Initialize empty dataframe
  spdf = data.frame(VARIANT = character(),
                   SEQ = integer(),
                   FVS = character(),
                   FIA = character(),
                   PLANT = character())
  
  #If var_code is empty and all_var is FALSE return
  if(length(var_code) <= 0 && !all_var)
    return(spdf)
  
  #Get dataframe for all_var
  if(all_var)
    spdf = fvs_species
  
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
      if(var %in% variants)
      {
        spdf_ = fvs_species[fvs_species$VARIANT == var,]
        var_df_list[[i]] = spdf_
      }
    }
    
    #Bind var_df_list if it is not empty
    if(length(var_df_list) > 0)
    {
      spdf = do.call("rbind", var_df_list)
      row.names(spdf) <- NULL
    }
      
  }
  
  return(spdf)
}

################################################################################
#'fvs_get_sp
#'@name fvs_get_sp
#'@description
#'
#'This function returns a vector of species codes for a given variant, set of 
#'variants, or all variants. The following species codes can be returned in a 
#'vector:
#'
#'FVS sequence number
#'
#'FVS species code 
#'
#'FIA species code
#'
#'USDA plant symbol
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
#'@param type:
#'Integer value corresponding to type of species to return.
#'
#'1 = FVS sequence number
#'
#'2 = FVS character code
#'
#'3 = FIA species code
#'
#'4 = USDA plant symbols
#'
#'@return
#'Vector of species codes.
################################################################################

#'@export
fvs_get_sp <- function(var_code = NULL,
                       all_var = FALSE,
                       type = 2)
{
  #Initialize empty vector
  sp = c()
  
  #If var_code is empty and all_var is FALSE return
  if(length(var_code) <= 0 && !all_var)
    return(sp)
  
  #Catch bad type values
  if(!type %in% 1:4) type = 2
  
  #If all_var is TRUE, reset var_code
  if(all_var) var_code = variants
  
  #Initialize list to store species codes
  sp_list = vector(mode = "list", length = length(var_code))
  
  #Start loop over var_code
  for(i in 1:length(var_code))
  {
    #Uppercase variant
    var = toupper(var_code[[i]])
    
    #Add species to list if variant is valid
    if(var %in% variants)
    {
      #Choose species
      if(type == 1) sp_ = fvs_seq_list[[var]]
      else if(type == 2) sp_ = fvs_char_list[[var]]
      else if(type == 3) sp_ = fvs_fia_list[[var]]
      else sp_ = fvs_plant_list[[var]]
      
      #Add to list
      sp_list[[i]] = sp_
      names(sp_list)[[i]] = var
    }
  }
  
  #Combine list if not empty
  if(length(sp_list) > 0)
  {
    sp = unlist(sp_list)
    names(sp) <- gsub("\\d+", "", names(sp))
  }
    
  return(sp)
}

  
  