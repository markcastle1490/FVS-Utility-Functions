################################################################################
#' sdi_df
#' @name shaw_sdi_df
#' @description Returns dataframe containing FIA species codes, species common
#' names, number of subplots used to derive SDI, lower SDI max, SDI max, and 
#' upper SDI max.
#' 
#' @return
#' Dataframe with SDI max information.
#' @export
################################################################################

sdi_df <- function()
  return(sdimax_df)

################################################################################
#' sdi_lookup
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

sdi_lookup <- function(spcd = NULL) {
  
  #Get SDI max indices
  matches <- match(spcd, sdimax_df[["SPCD"]])
  
  #Get SDI max values
  sdi_max <- sdimax_df[["SDIMAX"]][matches]
  
  return(sdi_max)
}
