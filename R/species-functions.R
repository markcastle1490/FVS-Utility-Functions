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

sp_lookup <- function(sp = "",
                      from = 0,
                      to = 2)
{
  #Initialize sp_to
  sp_to <- NA
  
  # Validate to value
  if (!to %in% 1:9) return(sp_to)
  
  #Retrieve species index
  sp_index <- sp_index(sp = sp, from = from)
  if (is.na(sp_index)) return(sp_to)
  
  #Column map matching 1 to 8 positions
  cols <- c("SPCD",
            "SPECIES_SYMBOL",
            "GENUS",
            "SCIENTIFIC_NAME", 
            "COMMON_NAME",
            "SFTWD_HRDWD",
            "WOODLAND",
            "JENKINS_SPGRPCD")
  
  #Return sp_to
  sp_to <- if (to == 9) sp_index else support_sp[[cols[to]]][sp_index]
  return(sp_to)
}

################################################################################
#' @name sp_index
#' @title Retrieve Row Index for FIA Support Species List
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
#' 3: Species scientific name
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
  sp_index <- NA
  
  #Return if input is invalid
  if (is.null(sp) || is.na(sp)) return(sp_index)
  
  #Check inputs
  if(!from %in% c(1, 2, 3)) from <- 0
  sp_upper <- toupper(sp)
  
  #Search FIA Codes
  if (from == 1 || from == 0) {
    sp_ <- suppressWarnings(as.integer(sp_upper))
    sp_index <- match(sp_, support_sp$SPCD)
    if(!is.na(sp_index)) return(sp_index)
  }
  
  
  #Search USDA Plant Symbols
  if (from == 2 || from == 0) {
    sp_index <- match(sp_upper, support_sp$SPECIES_SYMBOL)
    if(!is.na(sp_index)) return(sp_index)
  }
  
  #Search Scientific Names
  if (from == 3 || from == 0) {
    sp_index <- match(sp_upper, support_sp$SCIENTIFIC_NAME)
    if(!is.na(sp_index)) return(sp_index)
  }

  return(sp_index)
}
