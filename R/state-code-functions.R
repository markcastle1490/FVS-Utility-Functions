################################################################################
#' state_df
#' @name state_df
#' @description This function returns the state names, FIPS state codes, and two
#' character state abbreviations in a dataframe (see state_codes dataframe in 
#' commons.R).
#' 
#' @return
#' Dataframe containing state names, FIA state codes, and state abbreviations.
#' @export
################################################################################

state_df <- function()
  return(state_codes)

################################################################################
#' state_lookup
#' @name state_lookup
#' @description This function takes in a US state name, FIA State code, or 
#' State abbreviation and returns a US state name, FIA state code or state 
#' abbreviation.
#' 
#' @param state
#' Character string of a US state name (e.g. "New York") or state abbreviation 
#' (e.g "NY") or a FIPS state code (e.g. 36 for New York).
#' 
#' @param to
#' Integer value signifying the state identifier to convert to.
#' 
#' 1 = State FIPS code
#' 
#' 2 = State abbreviation
#' 
#' 3 = State name
#' 
#' Defaults to 3.
#' 
#' @return
#' State FIPS code, state abbreviation, or state name.
#' @export
################################################################################

#'@export
state_lookup <- function(state = NULL,
                         to = 3)
{
  #Validate to
  if (!to %in% 1:3) 
    stop("Invalid 'to' (1-3) parameters.")
  
  #Uppercase state
  state <- toupper(state)
  
  #Retrieve state index
  state_index <- state_index(state = state)
  
  #Column map
  cols <- c("FIPS_CODE",
            "STATE_ABBRV",
            "STATE_NAME")
  
  target_col <- cols[to]
  
  #Determine and return state_attr
  state_attr <- state_codes[[target_col]][state_index]
  
  return(state_attr)
}

################################################################################
#' state_index
#' @name state_index
#' @description This function takes an input state FIPS code, State abbrevation,
#' or state name and returns an indx from the the state_codes dataframe.
#' 
#' @param state
#' State value to evaluate. This can be either a state FIPS code, state 
#' abbreviation, or full state name. Defaults to NULL.
#' 
#' @return
#' Numeric row index value from state_codes dataframe.
################################################################################

state_index <- function(state = NULL)
  
{
  #Define search columns
  search_cols <- c("FIPS_CODE", "STATE_ABBRV", "STATE_NAME")
 
  #Uppercase state input
  state <- toupper(state)
  
  #Initialize state_index
  state_index <- rep(NA_integer_, length(state))
  
  #Start the search
  for (search in search_cols) {
    still_na <- is.na(state_index)
    if (!any(still_na)) break
  
    matches <- match(state[still_na], state_codes[[search]])
    state_index[still_na] <- matches
  }
  
  return(state_index)
}
