################################################################################
#' @name fortyp
#' @description This function returns the fortyp_codes dataframe that contains FIA forest 
#' type codes, descriptions, forest type groups and other information. See 
#' fortyp_codes dataframe in commons.R for more information.
#' 
#' @return
#' Dataframe with FIA forest type information.
#' @export
################################################################################

fortyp_get_df <- function()
  return(fortyp_codes)

################################################################################
#' @name fortyp_lookup
#' @description This function is used to look up a forest type code description 
#' or forest type group code based on an input FIA forest type code.
#' 
#' @param fortyp
#' Numeric FIA forest type code.
#' 
#' @param to
#' Integer value indicating the forest type information to look up.
#' 
#' 1 = FIA forest type description
#' 
#' 2 = FIA forest type group code
#' 
#' 3 = Index value of input forest type code
#' 
#' @return
#' Value corresponding to output provided in to argument.
#' @export
################################################################################

fortyp_lookup <- function(fortyp = NULL, to = 1) {

  #Check to argument
  if (!to %in% 1:3) stop("Invalid 'to' (1-3) parameters.")
  
  #Get forest type indices
  fortyp_index <- fortyp_index(fortyp = fortyp)

  #Get the forest type information
  if(to == 3) fortyp_to <- fortyp_index
  else {
    cols <- c("MEANING", "TYPGRPCD")
    target_col <- cols[to]
    fortyp_to <- fortyp_codes[[target_col]][fortyp_index]
  }

  return(fortyp_to)
}

################################################################################
#' @name fortyp_index
#' @description This function returns the row index (indices) in the fortyp_codes 
#' data frame corresponding to input FIA forest type code(s).
#' 
#' @param fortyp
#' Numeric FIA forest type code(s).
#' 
#' @return
#' Integer vector of row indices in fortyp_codes. Returns NA_integer_ for 
#' unmatched codes.
################################################################################

fortyp_index <- function(fortyp = NULL) {

  #Define search column
  search_cols <- c("VALUE")

  #Initialize forest type index vector
  fortyp_index <- rep(NA_integer_, length(fortyp))

  #Search for forest type
  for (search in search_cols) {
    still_na <- is.na(fortyp_index)
    if (!any(still_na)) break
    
    matches <- match(fortyp[still_na], fortyp_codes[[search]])
    fortyp_index[still_na] <- matches
  }

  return(fortyp_index)
}
