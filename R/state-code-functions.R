################################################################################
#'get_states
#'@name get_states
#'@description
#'This function returns the state names, FIA state codes, and fia abbreviations
#'as a dataframe from those defined in REF_RESEARCH_STATION.csv (from FIA
#'DataMart).
#
#'@return
#'Dataframe containing state names, FIA state codes, and state abbreviations.
################################################################################

#'@export
get_states <- function()
{
  states <- data.frame(STATENM = stateName,
                       STATECD = stateCode,
                       STATEAB = stateAbbr)

  return(states)
}

################################################################################
#'state_lookup
#'@name state_lookup
#'@description
#'This function takes in a US state name, FIA State code, or State abbreviation
#'and returns a US state name, FIA state code or state abbreviation.
#
#'@param state:    
#'Character string of a US state name, FIA state code, or state abbreviation.
#
#'@param from:     
#'Integer value signifying the type of state identifier you are converting from.
#'
#'1 = State name
#'
#'2 = State code
#'
#'3 = State abbreviation
#'
#'0 = Unknown. Argument state could be a state name, state code or state 
#'abbreviation.
#
#'@param to:       
#'Integer value signifying the state identifier to convert to.
#'
#'1 = State name
#'
#'2 = State code
#'
#'3 = State abbreviation
#
#'@return
#'State name, state code or state abbreviation.
################################################################################

#'@export
state_lookup <-function(state = "",
                       from = 0,
                       to = 3)
{
  #Capitalize state
  state <- toupper(state)

  #If from is not valid, set to 4
  if(! from %in% c(1:4)) from <- 0

  #If to is not valid, set to 3
  if(! to %in% c(1:3)) to <- 3

  #Initialize stateAttr
  stateAttr <- NA

  #Initialize stateIndex
  stateIndex <- NA

  #Search state names
  if(from == 1)
  {
    stateIndex <- match(state, stateName)
  }

  #Search state codes
  else if(from == 2)
  {
    stateIndex <- match(state, stateCode)
  }

  #Search state abbreviations
  else if(from == 3)
  {
    stateIndex <- match(state, stateAbbr)
  }

  #Search state names, state codes, and state abberviations. Stop when a state
  #index is no longer NA
  else
  {
    stateIndex <- match(state, stateName)
    if(is.na(stateIndex)) stateIndex <- match(state, stateCode)
    if(is.na(stateIndex)) stateIndex <- match(state, stateAbbr)
  }

  #If stateIndex is not NA, get the state attribute based on argument to. If
  #stateIndex is still NA, return.
  if(!is.na(stateIndex))
  {
    if(to == 1)
    {
      stateAttr <- stateName[stateIndex]
    }

    else if(to == 2)
    {
      stateAttr <- stateCode[stateIndex]
    }

    else
    {
      stateAttr <- stateAbbr[stateIndex]
    }
  }

  return(stateAttr)
}
