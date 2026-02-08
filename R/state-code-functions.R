################################################################################
#'get_states
#'@name get_states
#'@description
#'This function returns the state names, FIPS state codes, and two character
#'state abbreviations.
#
#'@return
#'Dataframe containing state names, FIA state codes, and state abbreviations.
################################################################################

#'@export
get_states <- function()
  return(state_codes)

################################################################################
#'state_lookup
#'@name state_lookup
#'@description
#'This function takes in a US state name, FIA State code, or State abbreviation
#'and returns a US state name, FIA state code or state abbreviation.
#
#'@param state:    
#'Character string of a US state name (e.g. "New York") or state abbreviation
#'(e.g "NY") or a FIPS state code (e.g. 36 for New York). Lookup times will be
#'fastest if state is input as FIPS code.
#
#'@param to:       
#'Integer value signifying the state identifier to convert to.
#'
#'1 = State FIPS code
#'
#'2 = State abbreviation
#'
#'3 = State name
#
#'@return
#'State FIPS code, state abbreviation, or state name.
################################################################################

#'@export
state_lookup <-function(state = NULL,
                        to = 3)
{

  #If to is not valid, set to 2
  if(! to %in% c(1:3)) to <- 2

  #Initialize state_attr
  state_attr <- NA

  #Initialize state_index
  state_index <- NA
  
  #Search FIPs code, then state abbreviations, then state names.
  state_index <- fips_index(state)
  
  #Capitalize state
  state <- toupper(state)
  
  if(is.na(state_index)) state_index <- match(state, state_codes$STATE_ABBRV)
  if(is.na(state_index)) state_index <- match(state, state_codes$STATE_NAME)

  #If state_index is not NA, get the state attribute based on argument to. If
  #state_index is still NA, return.
  if(!is.na(state_index))
  {
    if(to == 1)
    {
      state_attr <- state_codes$FIPS_CODE[state_index]
    }

    else if(to == 2)
    {
      state_attr <- state_codes$STATE_ABBRV[state_index]
    }

    else
    {
      state_attr <- state_codes$STATE_NAME[state_index]
    }
  }

  return(state_attr)
}

################################################################################
#'fips_index
#'@name fips_index
#'@description 
#'
#'This function is used to obtain an index value (row number) from the
#'states_code data frame that is returned from the get_states function.
#
#'@param fips: 
#'Numeric fips code.
#
#'@return 
#'Numeric index value.
################################################################################

#'@export
fips_index <- function(fips)
{
  #Initialize fips_index_ 
  fips_index_ <- NA
  
  #Coerce fips to numeric
  if(!is.numeric(fips)) fips <- suppressWarnings(as.numeric(fips))
  
  #If fips is not NA search for index
  if(!is.na(fips))
  {
    #Do binary search on SPCD column 
    fips_index_ <- bin_search(state_codes$FIPS_CODE, fips)
  }
  
  return(fips_index_)
}