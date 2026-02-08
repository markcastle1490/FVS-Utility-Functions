################################################################################
#'sp_get_df
#'@name sp_get_df
#'@description
#'
#'This function returns the support_sp dataframe that contains information from
#'FIADB.SPECIES_REF for FIA species codes (SPCD) less than 1000. See commons.R
#'file for more information about support_sp.
#'
#'@return
#'Dataframe with SPECIES_REF fields.
################################################################################

#'@export
sp_get_df <- function()
{
  return(support_sp)
}

################################################################################
#'sp_lookup
#'@name sp_lookup
#'@description 
#'This function is used to convert between various species codes and also to 
#'look up other information pertaining to a specific species based on an input
#'FIA species code or USDA plant symbol
#
#'@param sp:   
#'Incoming species code. This can be either an FIA species code or USDA plant 
#'symbol.
#
#'@param to:   
#'Integer value signifying the type of species you are converting to.
#'
#'1 = FIA code
#'
#'2 = USDA plant symbol
#'
#'3 = Genus
#'
#'4 = Species scientific name
#'
#'5 = Species common name
#'
#'6 = Hardwood softwood indicator
#'
#'7 = Woodland species indicator
#'
#'8 = Jenkins species group
#'
#'9 = Sequence number
#'
#'If the value of to argument  is anything other than 1 - 9, then a NA value 
#'will be returned from the function.
#
#'@return 
#'Character value corresponding to FIA species code, USDA plant symbol, Genus,
#'species scientific name, species common name, hardwood softwood indicator, 
#'woodland species indicator, jenkins species group, sequence number.
################################################################################

#'@export
sp_lookup <- function(sp,
                      to)
{
  #Initialize sp_to
  sp_to = NA
  
  #Check valid to and from values
  if(!to %in% c(1:9))
  {
    return(sp_to)
  }

  #Determine which species codes to search through based on from code
  sp_index <- sp_get_index(sp)

  #If sp_index is not NA, determine sp_to
  if(!is.na(sp_index))
  {
    #FIA code
    if(to == 1)
    {
      sp_to = support_sp$SPCD[sp_index]
    }

    #USDA plant symbol
    else if(to == 2)
    {
      sp_to = support_sp$SPECIES_SYMBOL[sp_index]
    }
    
    #Genus
    else if(to == 3)
    {
      sp_to = support_sp$GENUS[sp_index]
    }

    #Scientific name
    else if(to == 4)
    {
      sp_to = support_sp$SCIENTIFIC_NAME[sp_index]
    }
    
    #Common name
    else if(to == 5)
    {
      sp_to = support_sp$COMMON_NAME[sp_index]
    }
    
    #SFTWD_HRDWD
    else if(to == 6)
    {
      sp_to = support_sp$SFTWD_HRDWD[sp_index]
    }
    
    #WOODLAND
    else if(to == 7)
    {
      sp_to = support_sp$WOODLAND[sp_index]
    }

    #JENKINS SPECIES GROUP
    else if(to == 8)
    {
      sp_to = support_sp$JENKINS_SPGRPCD[sp_index]
    }
    
    #Sequence number
    else
    {
      sp_to = sp_index
    }
  }

  return(sp_to)
}

################################################################################
#'sp_get_index
#'@name sp_get_index
#'@description
#'This a function that is used to obtain a row index value from the support_sp
#'dataframe (see commons. R) based on an incoming species code. The species code
#'can either be a FIA code or USDA plant symbol.
#
#'@param sp:
#'Species code. This can be either an FIA species code or UDSA plant symbol.
#
#'@return
#'Numeric row index value from support_sp dataframe.
################################################################################

sp_get_index <- function(sp)
{
  #Initialize sp_index
  sp_index <- NA
  
  #Search both FIA and USDA plant symbol
  sp_index <- sp_fia_index(sp)
  
  #Search through USDA plant symbol if sp_index is still NA
  if(is.na(sp_index))
    sp_index <- match(sp, support_sp$SPECIES_SYMBOL)
  
  return(sp_index)
}

################################################################################
#'sp_fia_index
#'@name sp_fia_index
#'@description 
#'
#'This function is used to obtain an index value (row number) from the data
#'frame that is returned from sp_get_df function.
#
#'@param spcd: 
#'Numeric FIA species code.
#
#'@return 
#'Numeric index value.
################################################################################

#'@export
sp_fia_index <- function(spcd)
{
  #Initialize sp_index
  sp_index <- NA
  
  #Coerce spcd to numeric
  if(!is.numeric(spcd)) spcd <- suppressWarnings(as.numeric(spcd))
  
  #If species is not NA search for index
  if(! is.na(spcd))
  {
    #Do binary search on SPCD column 
    sp_index <- bin_search(support_sp$SPCD, spcd)
  }
  
  return(sp_index)
}
