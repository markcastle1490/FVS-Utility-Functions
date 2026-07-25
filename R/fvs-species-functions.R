################################################################################
#'fvs_spdf
#' @name fvs_spdf
#' @description This function returns all species codes for a given FVS variant,
#' set of FVS variants, or all FVS variants. The species codes included in the 
#' returned dataframe are:
#' 
#' FVS sequence number
#' 
#' FVS species code
#' 
#' FIA species code
#' 
#' USDA plant symbol
#' 
#' @param var_code
#' Character vector of two-character codes corresponding to FVS variant 
#' (e.g. "CA"). Defaults to NULL.
#' 
#' @param all_var
#' Logical variable where if TRUE, species codes for all FVS variants will be 
#' returned. This argument will take precedence over values specified in 
#' var_code argument. Defaults to FALSE.
#' 
#' @return
#' Dataframe with species codes.
#' @export
################################################################################

fvs_spdf <- function(var_code = NULL,
                     all_var = FALSE)
{
  #Initialize empty dataframe
  spdf <- data.frame(VARIANT = character(),
                   SEQ = integer(),
                   FVS = character(),
                   FIA = character(),
                   PLANT = character())
  
  #If var_code is empty and all_var is FALSE return
  if(length(var_code) <= 0 && !all_var)
    return(spdf)
  
  #Get dataframe for all_var
  if(all_var)
    spdf <- fvs_species
  
  #Get information for values in var_code
  else
  {
    #Initialize list for storing variant species codes
    var_df_list <- vector(mode = "list", length(var_code))
    
    #Start loop over var_code
    for(i in 1:length(var_code))
    {
      #Uppercase var
      var <- toupper(var_code[[i]])
      
      #Get species codes and add to list if var is valid
      if(var %in% variants)
      {
        spdf_ <- fvs_species[fvs_species$VARIANT == var,]
        var_df_list[[i]] = spdf_
      }
    }
    
    #Bind var_df_list if it is not empty
    if(length(var_df_list) > 0)
    {
      spdf <- do.call("rbind", var_df_list)
      row.names(spdf) <- NULL
    }
      
  }
  
  return(spdf)
}

################################################################################
#' fvs_sp
#' @name fvs_sp
#' @description This function returns a vector of species codes for a given 
#' variant, set of variants, or all variants. The following species codes can be
#' returned in a vector:
#' 
#' FVS sequence number
#' 
#' FVS species code
#' 
#' FIA species code
#' 
#' USDA plant symbol
#' 
#' @param var_code
#' Character vector of two-character codes corresponding to FVS variant 
#' (e.g. "CA"). Defaults to NULL.
#' 
#' @param all_var
#' Logical variable where if TRUE, species codes for all FVS variants will be 
#' returned. This argument will take precedence over values specified in 
#' var_code argument. Defaults to FALSE.
#' 
#' @param type
#' Integer value corresponding to type of species to return.
#' 
#' 1 = FVS sequence number
#' 
#' 2 = FVS character code
#' 
#' 3 = FIA species code
#' 
#' 4 = USDA plant symbols
#' 
#' Defaults to 2.
#' 
#' @return
#' Vector of species codes.
#' @export
################################################################################

fvs_sp <- function(var_code = NULL,
                   all_var = FALSE,
                   type = 2)
{
  #Initialize empty vector
  sp <- c()
  
  #If var_code is empty and all_var is FALSE return
  if(length(var_code) <= 0 && !all_var)
    return(sp)
  
  #Catch bad type values
  if(!type %in% 1:4) type <- 2
  
  #If all_var is TRUE, reset var_code
  if(all_var) var_code <- variants
  
  #Initialize list to store species codes
  sp_list <- vector(mode = "list", length = length(var_code))
  
  #Start loop over var_code
  for(i in 1:length(var_code))
  {
    #Uppercase variant
    var <- toupper(var_code[[i]])
    
    #Add species to list if variant is valid
    if(var %in% variants)
    {
      #Choose species
      if(type == 1) sp_ = fvs_seq_list[[var]]
      else if(type == 2) sp_ = fvs_char_list[[var]]
      else if(type == 3) sp_ = fvs_fia_list[[var]]
      else sp_ = fvs_plant_list[[var]]
      
      #Add to list
      sp_list[[i]] <- sp_
      names(sp_list)[[i]] <- var
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
#' fvs_sp_lookup
#' @name fvs_sp_lookup
#' @description This function is used to look up a FVS sequence number, FVS 
#' species character code, FIA code, or USDA plant code for a given variant 
#' based on an input species code.
#' 
#' @param var_code
#' Character vector of FVS variant code (e.g. CA). Defaults to "".
#' 
#' @param sp
#' Character vector of species codes. This can be a FVS character code, 
#' FIA species code, or USDA plant symbol. Value will be cast to character value 
#' if needed. Sequence numbers will not be checked since they can overlap with 
#' FIA codes. Defaults to "".
#' 
#' @param to
#' Integer value indicating the type of species information to look up.
#' 
#' 1 = FVS species character code
#' 
#' 2 = FIA species code
#' 
#' 3 = USDA plant symbol
#' 
#' 4 = FVS sequence number
#' 
#' Defaults to 1.
#' 
#' @return
#' Value corresponding to output provided in to argument.
#' @export
################################################################################

fvs_sp_lookup <- function(var_code,
                          sp,
                          to = 1)
{
  #Check from and to
  if (!to %in% 1:4) {
    stop("Invalid 'to' (1-4) parameters.")
  }

  #Upper case variant and sp
  var_code <- toupper(var_code)
  sp <- toupper(sp)

  #Extract row indices
  sp_index <- fvs_sp_index(var_code = var_code, sp = sp)
  
  #Determine list to get based on to
  cols <- c("FVS", "FIA", "PLANT", "SEQ")
  target_col <- cols[to]
  
  #Extract species
  sp_to <- fvs_species[[target_col]][sp_index]
  
  #Final type casting
  if (to %in% c(4)) {
    sp_to <- as.integer(sp_to)
  }
  
  return(sp_to)
}  

################################################################################
#' fvs_sp_index
#' @name fvs_sp_index
#' @title Retrieve Row Index for FVS Species Lists
#' @description This a function that is used to obtain a row index value from 
#' one of the four following lists from commons.R:
#' 
#' fvs_char_list
#' 
#' fvs_fia_list
#' 
#' fvs_plants_list
#' 
#' fvs_seq_list
#' 
#' @param var_code
#' Two character FVS variant code (e.g. CA). Defaults to "".
#' 
#' @param sp
#' Species code. This can be a FVS character code, FIA species code, or USDA 
#' plant symbol. Defaults to "".
#' 
#' @return
#' Numeric row index value from species code list.
################################################################################

fvs_sp_index <- function(var_code = NULL,
                         sp = NULL)
{

  #Define columns to search
  search_cols <- c("FVS", "FIA", "PLANT")

  #Initialize sp_index
  sp_index <- rep(NA_integer_, length(sp))

  #Start the search
  for (search in search_cols) {
    still_na <- is.na(sp_index)
    if (!any(still_na)) break
  
    matches <- match(paste0(var_code[still_na], "_", sp[still_na]),
                   paste0(fvs_species$VARIANT, "_", fvs_species[[search]]))
    sp_index[still_na] <- matches
  }

  return(sp_index)
}
  