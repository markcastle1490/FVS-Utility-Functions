################################################################################
#' sp_df
#' @name sp_df
#' @description This function returns the support_sp dataframe that contains 
#' information from FIADB.SPECIES_REF for FIA species codes (SPCD) less than 
#' 1000. See commons.R sfile for more information about support_sp.
#' 
#' @return
#' Dataframe with SPECIES_REF fields.
#' @export
################################################################################

sp_df <- function()
  return(support_sp)

################################################################################
#' sp_lookup
#' @name sp_lookup
#' @description This function is used to look up species specific information 
#' given an input FIA species code, USDA plant symbol, or species scientific 
#' name.
#' 
#' @param sp
#' Species code as a character. This can be either an FIA species code or USDA 
#' plant symbol. Value will get cast to character if needed.
#' 
#' @param to
#' Integer value indicating the type of species information to look up.
#' 
#' 1 = FIA code
#' 
#' 2 = USDA plant symbol
#' 
#' 3 = Genus
#' 
#' 4 = Species scientific name
#' 
#' 5 = Species common name
#' 
#' 6 = Hardwood softwood indicator ('H' / 'S'; hardwood or softwood)
#' 
#' 7 = Woodland species indicator ('Y' / 'N'; YES or NO)
#' 
#' 8 = Jenkins species group
#' 
#' 9 = Sequence number
#' 
#' If the value of to argument is anything other than 1 - 9, then a NA value 
#' will be returned from the function. Defaults to 2.
#' 
#' @return
#' Value corresponding to output provided in to argument.
#' @export
################################################################################

sp_lookup <- function(sp,
                      to = 2)
{
  #Validate from and to
  if (!to %in% 1:9) 
    stop("Invalid 'to' (1-9) parameters.")
  
  #Uppercase sp
  sp <- toupper(sp)
  
  #Retrieve species index
  sp_index <- sp_index(sp = sp)
  
  if(to == 9) sp_to <- sp_index
  else
  {
    cols <- c("SPCD",
              "SPECIES_SYMBOL",
              "GENUS",
              "SCIENTIFIC_NAME", 
              "COMMON_NAME",
              "SFTWD_HRDWD",
              "WOODLAND",
              "JENKINS_SPGRPCD")
    
    target_col <- cols[to]
    
    #Determine and return sp_to
    sp_to <- support_sp[[target_col]][sp_index]
  }
  
  #Cast to integer if needed
  if(to %in% c(1, 8)) sp_to <- as.integer(sp_to)
  
  return(sp_to)
}

################################################################################
#' sp_index
#' @name sp_index
#' @description This a function that is used to obtain a row index value from
#' the support_sp dataframe (see commons.R) based on an incoming species code. 
#' The species code can either be a FIA code or USDA plant symbol.
#' 
#' @param sp
#' Species code. This can be either an FIA species code or UDSA plant symbol. 
#' Defaults to NULL.
#' 
#' @return
#' Numeric row index value from support_sp dataframe.
################################################################################

sp_index <- function(sp = NULL)
{

  #Define search columns
  search_cols <- c("SPCD", "PLANT_SYMBOL")
  
  #Initialize sp_index
  sp_index <- rep(NA_integer_, length(sp))

  #Start the search
  for (search in search_cols) {
    still_na <- is.na(sp_index)
    if (!any(still_na)) break
  
    matches <- match(sp[still_na], support_sp[[search]])
    sp_index[still_na] <- matches
  }

  return(sp_index)
}
