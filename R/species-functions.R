################################################################################
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
#' @name sp_lookup
#' @description This function is used to look up species specific information 
#' given an input FIA species code, USDA plant symbol, or species scientific 
#' name.
#' 
#' @param sp
#' Species code as a character. This can be either an FIA species code, USDA 
#' plant symbol, or species scientific name. Value will get cast to character if 
#' needed. Defaults to "".
#' 
#' @param from
#' Option integer value that tells what kind of value is held in sp argument. 
#' Specifying a value from 1 - 3 will generally speed up look up times.
#' 
#' 0: FIA species code, USDA plant symbol, or scientific name.
#' 
#' 1: FIA species code
#' 
#' 2: USDA plant symbol
#' 
#' 3: Species scientific name
#' 
#' Defaults to 0.
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
                      from = 0,
                      to = 2)
{
  #Validate from and to
  if (!to %in% 1:9 || !from %in% 0:3) 
    stop("Invalid 'from' (0-3) or 'to' (1-9) parameters.")
  
  #Retrieve species index
  sp_index <- sp_index(sp = sp, from = from)
  
  if(to == 9) sp_to <- sp_index
  else
  {
    #Column map matching 1 to 8 positions
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
#' @name sp_index
#' @description This a function that is used to obtain a row index value from
#' the support_sp dataframe (see commons.R) based on an incoming species code. 
#' The species code can either be a FIA code or USDA plant symbol.
#' 
#' @param sp
#' Species code. This can be either an FIA species code or UDSA plant symbol. 
#' Defaults to NULL.
#' 
#' @param from
#' Option integer value that tells what kind of value is held in sp argument.
#' 
#' 1: FIA species code
#' 
#' 2: USDA plant symbol
#' 
#' Defaults to 0.
#' 
#' @return
#' Numeric row index value from support_sp dataframe.
#' @export
################################################################################

sp_index <- function(sp = NULL,
                     from = 0)
{
  #Initialize sp_index
  sp_index <- rep(NA_integer_, length(sp))
  
  #Check inputs
  if(!from %in% 1:2) from <- 0
  sp_upper <- toupper(sp)
  
  #Search FIA Codes
  if (from == 1) {
    sp_num <- suppressWarnings(as.integer(sp_upper))
    sp_index <- match(sp_num, support_sp$SPCD)
  }
  
  #Search USDA Plant Symbols
  else if (from == 2) {
    sp_index <- match(sp_upper, support_sp$SPECIES_SYMBOL)
  }
  
  #Check both FIA and USDA Plant Symbols
  else {
    sp_num <- suppressWarnings(as.integer(sp_upper))
    sp_index <- match(sp_num, support_sp$SPCD)
    
    # Identify which elements missed the FIA lookup
    still_na <- is.na(sp_index)
    
    # Look up only the missing elements in the USDA column
    if (any(still_na)) {
      sp_index[still_na] <- match(sp_upper[still_na], support_sp$SPECIES_SYMBOL)
    }
  }
  
  return(sp_index)
}
