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

################################################################################
#'fvs_sp_lookup
#'@name fvs_sp_lookup
#'@description 
#'This function is used to look up a FVS sequence number, FVS species character
#'code, FIA code, or USDA plant code for a given variant based on an input
#'species code.
#
#'@param var_code:
#'Two character FVS variant code (e.g. CA).
#
#'@param sp:   
#'Incoming species code as a character string. This can be a FVS character code,
#'FIA species code, or USDA plant symbol. Value will be cast to character value
#'if needed.
#
#'@param from:
#'Option integer value that tells what kind of value is held in sp argument.
#'Specifying a value from 1 - 3 will generally speed up look up times.
#'
#'0: FVS species character code, FIA species code or USDA plant symbol
#'
#'1: FVS species character code
#'
#'2: FIA species code
#'
#'3: USDA plant symbol
#
#'@param to:   
#'Integer value indicating the type of species information to look up.
#'
#'1 = FVS species character code
#'
#'2 = FIA species code
#'
#'3 = USDA plant symbol
#'
#'4 = FVS sequence number
#
#'@return 
#'Value corresponding to output provided in to argument.
################################################################################

#'@export
fvs_sp_lookup <- function(var_code = "",
                          sp = "",
                          from = 0,
                          to = 1)
{
  #Initialize sp_to
  sp_to = NA
  
  #Return if inputs are invalid
  if(!to %in% 1:4 || is.na(var_code) || is.na(sp))
    return(sp_to)
  
  #Determine which species codes to search through based on from code
  sp_index <- fvs_sp_get_index(var_code = var_code,
                               sp = sp,
                               from = from)
  
  #If sp_index is not NA, determine sp_to
  if(!is.na(sp_index))
  {
    #FVS character code
    if(to == 1)
      sp_to = fvs_char_list[[var_code]][sp_index]
    
    #FIA species code
    else if(to == 2)
      sp_to = fvs_fia_list[[var_code]][sp_index]
    
    #USDA plant symbol
    else if(to == 3)
      sp_to = fvs_plant_list[[var_code]][sp_index]
    
    #FVS Sequence number
    else if(to == 4)
      sp_to = fvs_seq_list[[var_code]][sp_index]
  }
  
  return(sp_to)
}  

################################################################################
#'fvs_sp_get_index
#'@name fvs_sp_get_index
#'@description
#'This a function that is used to obtain a row index value from one of the four
#'following lists from commons.R:
#'
#'fvs_char_list
#'
#'fvs_fia_list
#'
#'fvs_plants_list
#'
#'fvs_seq_list
#
#'@param var_code:
#'Two character FVS variant code (e.g. CA).
#
#'@param sp:
#'Species code. This can be a FVS character code, FIA species code, or USDA
#'plant symbol.
#
#'@param from:
#'Option integer value that tells what kind of value is held in sp argument.
#'Specifying a value from 1 - 3 will generally speed up look up times.
#'
#'0: FVS species character code, FIA species code or USDA plant symbol
#'
#'1: FVS species character code
#'
#'2: FIA species code
#'
#'3: USDA plant symbol
#
#'@return
#'Numeric row index value from species code list.
################################################################################

fvs_sp_get_index <- function(var_code = "",
                             sp = "",
                             from = 0)
{
  #Initialize sp_index
  sp_index <- NA
  
  #Uppercase var_code
  var_code = toupper(var_code)
  sp <- toupper(sp)
  
  #If variant is invalid, return
  if(!var_code %in% variants)
    return(sp_index)
  
  #Catch bad values
  if(!from %in% c(1, 2, 3)) from = 0
  
  #Search FVS species codes
  if(from <= 1)
    sp_index <- match(sp, fvs_char_list[[var_code]])
  
  #Search FIA species codes
  if(from == 2 || is.na(sp_index)) 
  {
    sp_ = sp
    if(nchar(sp_) < 3) sp_ = paste0("0", sp_)
    sp_index <- match(sp_, as.integer(fvs_fia_list[[var_code]]))
  }
  
  #USDA plant symbols
  if(from == 3 || is.na(sp_index)) 
    sp_index <- match(sp, fvs_plant_list[[var_code]])
  
  return(sp_index)
}
  