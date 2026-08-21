################################################################################
#' shaw_sdi_df
#' @name shaw_sdi_df
#' @description Returns dataframe containing FIA species codes, species common
#' names, number of subplots used to derive SDI, lower SDI max, SDI max, and 
#' upper SDI max.
#' 
#' @return
#' Dataframe with SDI max information.
#' @export
################################################################################

shaw_sdi_df <- function()
  return(shaw_sdi)

################################################################################
#' shaw_sdi_lookup
#' @name fortyp_lookup
#' @description Returns maximum SDI value from incoming FIA species code.
#' 
#' @param species
#' Numeric FIA species code.
#' 
#' @return
#' Numeric maximum SDI value or NA values if species does not have an SDI max.
#' @export
################################################################################

shaw_sdi_lookup <- function(spcd = NULL) {
  
  #Get SDI max indices
  matches <- match(spcd, shaw_sdi[["SPCD"]])
  
  #Get SDI max values
  sdi_max <- shaw_sdi[["SDIMAX"]][matches]
  
  return(sdi_max)
}
