################################################################################
#'fortyp_get_df
#'@name fortyp
#'@description
#'
#'This function returns the fortyp_codes dataframe that countains FIA forest
#'type codes, descriptions, forest type groups and other information. See 
#'fortyp_codes dataframe in commons.R for more information.
#'
#'@return
#'Dataframe with FIA forest type information.
################################################################################

#'@export
fortyp_get_df <- function()
  return(fortyp_codes)

################################################################################
#'fortyp_lookup
#'@name fortyp_lookup
#'@description 
#'This function is used to look up a forest type code description or forest type
#'group code based on an input FIA forest type code.
#
#'@param fortyp:   
#'Numeric FIA forest type code.
#
#'@param to:   
#'Integer value indicating the forest type information to look up.
#'
#'1 = FIA forest type description
#'
#'2 = FIA forest type group code
#
#'@return 
#'Value corresponding to output provided in to argument.
################################################################################

#'@export
fortyp_lookup <- function(fortyp = 999,
                          to = 1)

{
  #Initialize fortyp_attr
  fortyp_attr = NA
  
  #Check valid to and from values
  if(!to %in% c(1:2))
    return(fortyp_attr)
  
  #Initialize fortyp_index
  fortyp_index <- NA
  
  #Lookup index of fortyp
  fortyp_index = bin_search(fortyp_codes$VALUE, fortyp)
  if(fortyp_index <= 0) fortyp_index = NA
  
  #If fortyp_index is not NA, determine fortyp_attr
  if(!is.na(fortyp_index))
  {
    #Forest type description (MEANING)
    if(to == 1)
      fortyp_attr = fortyp_codes$MEANING[fortyp_index]
    
    #Forest type group code (TYPGRPCD)
    else
      fortyp_attr = fortyp_codes$TYPGRPCD[fortyp_index]
  }
  
  return(fortyp_attr)
}
