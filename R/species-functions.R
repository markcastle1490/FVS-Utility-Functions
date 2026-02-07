################################################################################
#sp_get_index
#
#This a function that is used to obtain an index value based on an incoming
#species code. The species code can either be a FIA code, USDA plant symbol,
#or Scientific name.
#
#
#sp:   incoming species code. This can be either an FIA species code, USDA
#      plant symbol, or scientific name.
#
#from: Integer value signifying the type of species you are requesting an
#      index for.
#      1 = FIA code
#      2 = USDA plant symbol
#      3 = Scientific name
#      If the value of from argument is anything other than 1 - 3, then a
#      NA value  will be returned from the function.
################################################################################

sp_get_index <- function(sp, from)
{
  #If from invalid, set to 0
  if(! from %in% c(0, 1, 2)) from  <- 0
  
  #Initialize spIndex
  spIndex <- NA
  
  #Search both FIA and USDA plant symbol
  if(from == 0)
  {
    #Search through FIA species codes
    spIndex <- match(sp, supportSP$SPCD)
      
    #Search through USDA plant symbol if spIndex is still NA
    if(is.na(spIndex))
    {
      spIndex <- match(sp, supportSP$SPECIES_SYMBOL)
    }
  }
    
  #FIA code
  else if(from == 1)
  {
    spIndex = sp_fia_index(sp)
  }
    
  #PLANT symbol
  else
  {
    spIndex = match(sp, supportSP$SPECIES_SYMBOL)
  }

  return(spIndex)
}

#############################################################################
#'sp_get_data
#'@name sp_get_data
#'@description
#'
#'This function returns a dataframe containing SPECIES_REF information for 
#'FIA species codes (SPCD) less than 1000.
#'
#'@return
#'Dataframe with SPECIES_REF fields.
############################################################################

#'@export
sp_get_data <- function()
{
  return(supportSP)
}

################################################################################
#'sp_convert
#'@name sp_convert
#'@description 
#'This function is used to convert between FIA species codes, USDA plant 
#'symbols, species scientific name, or sequence number.
#
#'@param sp:   
#'Incoming species code. This can be either an FIA species code, USDA plant 
#'symbol, or scientific name
#
#'@param from: 
#'Integer value signifying the type of species you are converting from.
#'
#'0 = FIA code or USDA plant symbol
#'
#'1 = FIA code
#'
#'2 = USDA plant symbol
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
sp_convert<-function(sp, from, to)
{
  #Initialize spTo
  spTo = NA
  
  #Check valid to and from values
  if(!from %in% c(0, 1, 2) | !to %in% c(1:9))
  {
    return(spTo)
  }

  #Determine which species codes to search through based on from code
  spIndex <- sp_get_index(sp, from)

  #If spIndex is not NA, determine spTo
  if(!is.na(spIndex))
  {
    #FIA code
    if(to == 1)
    {
      spTo = supportSP$SPCD[spIndex]
    }

    #USDA plant symbol
    else if(to == 2)
    {
      spTo = supportSP$SPECIES_SYMBOL[spIndex]
    }
    
    #Genus
    else if(to == 3)
    {
      spTo = supportSP$GENUS[spIndex]
    }

    #Scientific name
    else if(to == 4)
    {
      spTo = supportSP$SCIENTIFIC_NAME[spIndex]
    }
    
    #Common name
    else if(to == 5)
    {
      spTo = supportSP$COMMON_NAME[spIndex]
    }
    
    #SFTWD_HRDWD
    else if(to == 6)
    {
      spTo = supportSP$SFTWD_HRDWD[spIndex]
    }
    
    #WOODLAND
    else if(to == 7)
    {
      spTo = supportSP$WOODLAND[spIndex]
    }

    #JENKINS SPECIES GROUP
    else if(to == 8)
    {
      spTo = supportSP$JENKINS_SPGRPCD[spIndex]
    }
    
    #Sequence number
    else
    {
      spTo = spIndex
    }
  }

  return(spTo)
}

################################################################################
#'sp_fia_index
#'@name sp_fia_index
#'@description 
#'
#'This function is used to obtain an index value (row number) from the 'SPCD 
#'column of the supportDB dataframe based on a FIA species code.
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
  #Initialize spIndex
  spIndex <- NA
  
  #Coerce spcd to numeric
  if(!is.numeric(spcd)) spcd <- as.numeric(spcd)
  
  #If species is not NA search for index
  if(! is.na(spcd))
  {
    #Do binary search on SPCD column 
    spIndex <- bin_search(supportSP$SPCD, spcd)
  }
  
  return(spIndex)
}
