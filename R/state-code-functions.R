################################################################################
#'state_get_df
#'@name state_get_df
#'@description
#'This function returns the state names, FIPS state codes, and two character
#'state abbreviations in a dataframe (see state_codes dataframe in commons.R).
#
#'@return
#'Dataframe containing state names, FIA state codes, and state abbreviations.
################################################################################

#'@export
state_get_df <- function()
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
#'@param from:
#'Option integer value that tells what kind of value is held in state argument.
#'Specifying a value from 1 - 3, will generally speed up lookup times.
#'
#'0: State FIPS code, abbreviation, or name.
#'
#'1: State FIPS code
#'
#'2: State abbreviation
#'
#'3: State name
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
                        from = 0,
                        to = 3)
{
  
  #Initialize state_attr
  state_attr <- NA

  #If to is not valid, set to 2
  if(! to %in% c(1:3))
    return(state_attr)

  #Initialize state_index
  state_index <- NA
  
  #Get state_index
  state_index <- state_get_index(state = state,
                                 from = from)
  
  #If state_index is not NA, get the state attribute based on argument to. If
  #state_index is still NA, return.
  if(!is.na(state_index))
  {
    if(to == 1)
      state_attr <- state_codes$FIPS_CODE[state_index]

    else if(to == 2)
      state_attr <- state_codes$STATE_ABBRV[state_index]

    else
      state_attr <- state_codes$STATE_NAME[state_index]
  }

  return(state_attr)
}

################################################################################
#'state_get_index
#'@name sp_get_index
#'@description
#'This a function that is used to obtain a row index value from the state_codes
#'dataframe (see commons. R) based on an incoming state value. The state value
#'can be a state FIPS code, state abbreviation, or state name.
#
#'@param state:
#'Species code. This can be either an FIA species code or UDSA plant symbol.
#
#'@param from:
#'Option integer value that tells what kind of value is held in state argument.
#'
#'1: State FIPS code
#'
#'2: State abbreviation
#'
#'3: State name
#
#'@return
#'Numeric row index value from state_codes dataframe.
################################################################################

state_get_index <- function(state = NULL,
                            from = 0)
  
{
  #Initialize state_index
  state_index = NA
  
  #Catch bad values
  if(! from %in% c(1, 2, 3)) from = 0
  
  #Search FIPS codes
  if(from <= 1) 
    state_index = state_fips_index(state)
  
  #Search state abbreviations
  if(from == 2 || is.na(state_index)) 
  {
    state = toupper(state)
    state_index <- match(state, state_codes$STATE_ABBRV)
  }
  
  #Search state names
  if(from == 3 || is.na(state_index))
  {
    state = toupper(state)
    state_index <- match(state, state_codes$STATE_NAME)
  }
    
  return(state_index)
}

################################################################################
#'state_fips_index
#'@name state_fips_index
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
state_fips_index <- function(fips)
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
    if(fips_index_ <= 0) fips_index_ = NA
  }
  
  return(fips_index_)
}