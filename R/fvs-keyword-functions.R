################################################################################
#'fvs_keyfile
#'@name fvs_keyfile
#'@description 
#'This function creates a keyword file based on user defined set of keywords for
#'a set of stands IDs and inventory years. 
#'
#'@param keyfile:     
#'Character string corresponding to directory path and file name of keyword file
#'being created.
#'
#'@param standid:     
#'Character vector of stand identification numbers.
#'
#'@param invyear:     
#'Numeric vector of inventory years for each stand in standid and or standcn 
#'arguments.
#'
#'@param standcn:
#'Optional character vector of stand control numbers. If values are not defined
#'for this argument, then the value from standid will be used as the stand 
#'control number in the output keyword file.
#'
#'@param keywords:
#'Optional character vector containing properly formatted FVS keywords or 
#'variables to define with event monitor function call that will be included in
#'keyword file for each stand. 
#'
#'@param stand_keys:
#'Optional character vector containing a set of properly formatted keywords or
#'variables to define with event monitor function call for specific stands in 
#'the standid argument. These keywords will be added for the given stand in the
#'output keyword file. If this argument is used, it should match the length of 
#'the standid argument.
#'
#'@param start_year:   
#'Common start year for simulation. If start_year and end_year are not NULL, then
#'function will attempt to add time keyword sequence (see time_keys function).
#'
#'@param end_year:     
#'Common end year for simulation. If start_year and end_year are not NULL, then
#'function will attempt to add TIMEIMT and NUMCYCLE keyword sequence (see 
#'time_keys function).
#'
#'@param cycle_length: 
#'Default cycle length assumed for simulation. Only used when start_year and 
#'end_year are not NULL.
#'
#'@param cycle_at: 
#'Numeric vector containing additional reporting years (those that do not occur
#'on default cycle boundaries based on start_year, end_year and 'cycle_length 
#'arguments). Only used when start_year and end_year are not NULL.
#'
#'@param dbin: 
#'Character string for name of FVS input database that includes file extension
#'(.db, .sqlite). If this value is not NULL, the function will attempt to add a 
#'DSNIN keyword sequence.
#'
#'@param standinit:   
#'Character string corresponding to name table where stand information will be 
#'read from. Typically this would be "FVS_standinit" of "FVS_PlotInit" but could
#'be "FVS_standinit_Plot", "FVS_standinit_Cond", etc. when FIA data is in use.
#'This is an optional argument and will only be used if dbin is not NULL.
#'
#'@param treeinit:    
#'Character string corresponding to name table where tree information will be 
#'read from. Typically this would be "FVS_treeinit" but could be 
#'"FVS_treeinit_Plot", "FVS_treeinit_Cond", etc. when FIA data is in use. This
#'is an optional argument and will only be used if dbin is not NULL.
#'
#'@param stand_type:   
#'Integer value that determines what stand ID will be used to read data from 
#'dbin when dbin is not NULL.
#'
#'0 = Stand_CN
#'
#'1 = Stand_ID
#
#'@param dbout:        
#'Name of FVS output database that includes file extension (.db or .sqlite). 
#'If this value is not NULL, the function will attempt to add a DSNOUT keyword
#'sequence.
#'
#'@param delotab:     
#'Optional vector of integer values that contain any values from 1-4. If values
#'are provided in this argument, then DELOTAB keywords will be included in 
#'keyword file to suppress creation of text outputs. By default this argument is
#'set to c(1, 2) so the stand composition table and sample tree data tables will
#'be suppressed in the output keyword file.
#
#'@return
#'None
################################################################################

#'@export
fvs_keyfile <- function(keyfile,
                        standid,
                        invyear,
                        standcn = NULL,
                        keywords = NULL,
                        stand_keys = NULL,
                        start_year = NULL,
                        end_year = NULL,
                        cycle_length = 10,
                        cycle_at = NULL,
                        dbin = NULL,
                        standinit = "FVS_StandInit",
                        treeinit = "FVS_TreeInit",
                        stand_type = 1,
                        dbout = NULL,
                        delotab = c(1, 2))
{

  #If keyfile exists already,  delete it
  if(file.exists(keyfile)) unlink(keyfile)
  
  #Switch \\\\ to / in keyfile
  keyfile = gsub("\\\\", "/", keyfile)
  
  #Extract path to output by extract all characters before the last / in output.
  keydir = gsub("/[^/]+$", "", keyfile)
  
  #Extract name of keyword file
  key_name = gsub(".*/", "", keyfile)
  
  #Extract file extension for output argument.
  key_ext = sub("(.*)\\.","", keyfile)
  
  #Test existence of output path and if it does not exist report error.
  if(keydir != key_name)
  {
    if (!(file.exists(keydir)))
      stop(paste("Path to output:", keydir, "was not found.",
                 "Make sure directory path to output is spelled correctly."))
  }

  #Test if output file extension is valid (.key).
  if(key_ext != "key")
    stop(paste("Output argument does not have a valid file extension. File",
               "extension must be .key."))
  
  #If invyear or standid are empty, stop with an error
  if(length(invyear) <= 0 || length(standid) <= 0)
    stop(paste("Values are required for invyear and standid."))
  
  #If invyear and standid are not the same length, stop with an error
  if(length(invyear) != length(standid))
    stop(paste("Arguments invyear and standid need to have the same length."))
  
  #Do checks on standcn
  if(is.null(standcn)) standcn = standid
  else
  {
    if(length(standcn) != length(standid))
      stop("Arguments standcn and standid need to have the same length.")
  }
  
  #Do checks on keywords argument
  if(!is.null(keywords))
  {
    #Stop if keywords is a list or not a character vector
    if(is.list(keywords) || !is.character(keywords))
      stop("keywords argument must be a character vector.")
  }
  
  #Do checks on stand_keys
  if(is.null(stand_keys)) stand_keys = rep("", times = length(standid))
  else
  {
    if(length(stand_keys) != length(standid))
      stop("Arguments stand_keys and standid need to have the same length.")
    
    if(!is.character(stand_keys))
      stop("stand_keys argument must be a character vector.")
  }
  
  #Open file connection
  con = file(description = keyfile, open = "a")
  on.exit(close(con = con))
  
  #Loop across stands
  for(i in 1:length(standid))
  {
    #Get stand ID, inventory year, and stand_cn
    stand_id <- standid[[i]] 
    inv_year <- invyear[[i]]
    stand_cn <- standcn[[i]]

    #Setup standident and standcn keywords and add to keyword file
    stand_key <- paste("STDIDENT",
                       stand_id,
                       "STANDCN",
                       stand_cn,
                       sep = "\n")  
    
    #Write standident and standcn keywords
    writeLines(text = stand_key, con = con, sep = "\n\n")
    
    #Create INVYR keyword
    invyr_key <- fvs_keyword(params = list("INVYEAR", inv_year),
                             type = 1)
    
    #Write INVYR keywords
    writeLines(text = invyr_key, con = con, sep = "\n\n")
    
    #===========================================================================
    #Determine if output DSNOUT keywords will be produced and added to keyword
    #file
    #===========================================================================
    
    if(!is.null(dbout))
    {
      dsnout_keys <- dsnout_keys(dbout = dbout)
      writeLines(text = dsnout_keys, con = con, sep = "\n\n")
    }
    
    #===========================================================================
    #Determine if timing keywords are produced and added to keyword file
    #===========================================================================
    
    if(!is.null(start_year) && !is.null(end_year))
    {
      #Get timing keywords
      time_key <- time_keys(invyear = invyear,
                            start_year = start_year,
                            end_year = end_year,
                            cycle_length = cycle_length,
                            cycle_at = cycle_at)

      writeLines(text = time_key, con = con, sep = "\n\n")
    }
    
    #===========================================================================
    #Check if delotab keywords should be added
    #===========================================================================
    
    if(length(delotab) > 0)
    {
      delo_keys <- delotab_keys(delotab = delotab)
      writeLines(text = delo_keys, con = con, sep = "\n\n")
    }
    
    #===========================================================================
    #Write any keywords provided in keywords argument
    #===========================================================================
    
    if(length(keywords) > 0)
      writeLines(text = keywords, con = con, sep = "\n\n")
    
    #===========================================================================
    #Write any stand specific keywords in stand_keys argument
    #===========================================================================
    
    stand_key = stand_keys[i]
    if(!is.na(stand_key) && !is.null(stand_key) && stand_key != "") 
        writeLines(text = stand_key, con = con, sep = "\n\n")
    
    #===========================================================================
    #Determine if input database keywords will be produced and added to keyword
    #file
    #===========================================================================
    
    if(!is.null(dbin))
    {
      dsnin_keys <- dsnin_keys(dbin = dbin,
                               stand_type = stand_type,
                               standinit = standinit,
                               treeinit = treeinit)
      
      writeLines(text = dsnin_keys, con = con, sep = "\n\n")
    }
    
    #Add process keyword
    writeLines(text = "PROCESS", con = con, sep = "\n\n")
  }
  
  #Add stop keyword
  writeLines(text = "STOP", con = con, sep = "\n\n")
  
  invisible()
}

################################################################################
#'fvs_kcpfile
#'@name fvs_kcpfile
#'@description
#'
#'This function creates a kcp file based on user defined set of keywords.
#
#'@param kcpfile:     
#'Character string corresponding to file path of keyword component file (.kcp)
#'being created.
#
#'@param keywords:   
#'Optional character vector containing properly formatted FVS keywords or 
#'variables to define with event monitor function call that will be included in
#'kcp file for each stand.
#
#'@return
#'None
################################################################################

#'@export
fvs_kcpfile <- function(kcpfile,
                        keywords = NULL)
{
  
  #If kcpfile exists already,  delete it
  if(file.exists(kcpfile)) unlink(kcpfile)
  
  #Extract path to output by extract all characters before the last / in output.
  keydir <- gsub("/[^/]+$", "", kcpfile)
  
  #Test existence of output path and if it does not exist report error.
  if (!(file.exists(keydir))){
    stop(paste("Path to output:", keydir, "was not found.",
               "Make sure directory path to output is spelled correctly."))
  }
  
  #Extract file extension for output argument.
  key_ext<-sub("(.*)\\.","", kcpfile)
  
  #Test if output file extension is valid (.key).
  if(!key_ext %in% c("kcp"))
  {
    stop(paste("Output argument does not have a valid file extension. File",
               "extension must be .kcp."))
  }
  
  #Do checks on keywords argument if not NULL
  if(!is.null(keywords))
  {
    #Stop if keywords is a list or not a character vector
    if(is.list(keywords) || !is.character(keywords))
      stop("keywords argument must be a character vector.")
  }
  
  #Open file connection
  con = file(description = kcpfile, open = "a")
  on.exit(close(con = con))
    
  #===========================================================================
  #Write any keywords provided in keywords argument
  #===========================================================================
    
  if(length(keywords) > 0)
    writeLines(text = keywords, con = con, sep = "\n\n")
  
  invisible()
}

################################################################################
#'fvs_keyword
#'@name fvs_keyword
#'@description fvs_keyword
#'
#'This function takes in a list of keyword parameters and returns a properly 
#'formatted keyword string. This function can accommodate standard keyword
#'formatting and the Parms format.
#
#'@param params:    
#'
#'List containing FVS keyword or event monitor function parameters. The list
#'should be structured in one of the following manners and align with the
#'options for the type argument.
#'
#'1: List corresponds to standard FVS keyword (not in parms format). Value in 
#'index 1 should be name of FVS keyword. All following indices should contain
#'keyword parameters.
#'
#'2: List corresponds to FVS keyword in parms format. Value in index 1 should be
#'name of FVS keyword. All following indices should contain keyword parameters.
#'
#'3: List corresponds to variable defined by event monitor function. Value in
#'index 1, should be name of event monitor variable to define. Index 2 should be
#'name of event monitor function being used. All following indices should 
#'contain event monitor function arguments.
#'
#'4: List corresponds to a collection of values that will be returned as single 
#'string. All indices in list contain values that will be concatenated into a 
#'string where each value is separated by a space and returned without any 
#'additional formatting.
#
#'@param type:      
#'Type of keyword to be returned. By default this argument is set to 1.
#'
#'1 = Standard
#'
#'2 = Parms format
#'
#'3 = event monitor function call
#'
#'4 = freeform, could be standard keyword, parms formatted keyword, or event 
#'monitor function call.
#'
#
#'@return 
#'Character string corresponding to properly formatted keyword or event monitor 
#'function.
################################################################################

#'@export
fvs_keyword <- function(params = list(),
                        type = 1)
{
  keyword <- ""
  
  #if no values are in params, return keyword
  if(length(params) <= 0) return(keyword)
  
  #Check for valid type
  if(!type %in% c(1, 2, 3, 4)) type <- 1
  
  #Initialize empty character vector
  char_params <- vector(mode = "character",
                        length = 0)
  
  #Cast all values in params to character
  for(i in 1:length(params))
  {
    #If params is NA move to next value
    if(is.na(params[i])) next
    
    #Store value as character
    char_val <- as.character(params[[i]])
    
    #If type is not 4 (freeform), grab substring
    if(type != 4)
    {
      char_val <- substring(char_val,
                            first = 1,
                            last = 10)
    }
    
    #Trim white space
    char_val <- trimws(char_val)
    
    #If field is just a blank character, move to next value
    #if(char_val == "") next
    
    #Add value to char_params
    char_params <- append(char_params, char_val)
  }
  
  #If length of char_params is still 0, return
  if(length(char_params) <= 0) return(keyword)
  
  #Handle standard keyword formatting
  if(type == 1)
  {
    
    #Combine keyword records
    keyword <- paste0(format(char_params,
                             width = 10,
                             justify = "left"),
                      collapse = "")
  }
  
  #Handle Parms keyword formatting
  else if (type == 2)
  {
    #Create keyword record in PARMS format if char_params carries at least 3 
    #values.
    if(length(char_params) >= 3)
    {
      #Create the Parm() component of keyword
      parms <- paste0("Parms",
                      "(",
                      paste0(char_params[3:length(char_params)],
                             collapse = ","),
                      ")")
      
      #Create the keyword
      keyword <- paste0(format(char_params[1],
                               width = 10,
                               justify = "left"),
                        format(char_params[2],
                               width = 10,
                               justify = "left"),
                        parms,
                        collapse = "")
    }
  }
  
  #Handle event monitor function
  else if(type == 3)
  {
    
    #Create event monitor function call statement if char_params carries at
    #least 3 values.
    if(length(char_params) >= 3) 
    {
      #Create string with variable and function call name
      event_var <- paste0(char_params[1],
                          "=",
                          char_params[2])
      
      #Create function arguments with parantheses
      function_args <- paste0("(",
                             paste0(char_params[3:length(char_params)],
                                    collapse = ","),
                             ")")
      
      #Create the keyword (function call here)
      keyword <- paste0(event_var,
                        function_args,
                        collapse = "")
    }
  }
  
  #Handle freeform option
  else
  {
    #Paste char_params separated by space and return
    keyword <- paste(char_params,
                     collapse = " ")
  }
  
  return(keyword)
}

################################################################################
#'time_keys
#'@name time_keys
#'@description
#'
#'This function creates a set of keywords for controlling the length of a FVS
#'simulation for a given inventory year, common start year, common end year,
#'default cycle length, and optional cycle_at keywords.
#
#'@param invyear:    
#'Inventory year of stand.
#
#'@param cycle_length: 
#'Default cycle length assumed for simulation.
#
#'@param start_year:  
#'Common start year for simulation.
#
#'@param end_year:
#'End year for simulation.
#
#'@param cycle_at:    
#'Vector of years corresponding to additional reporting years that don't 
#'coincide with default reporting years determined by start_year, end_year, and 
#'cycle_length.
#
#'@param debug:      
#'Logical variable where if TRUE, debug output will be printed in console.
#
#'@return
#'Character string of keywords for controlling the length of a FVS simulation 
#'for a given inventory year, common start year, common end year, default cycle 
#'length, and additional reporting years.
################################################################################

#'@export
time_keys <- function(invyear = NULL,
                      start_year = NULL,
                      end_year = NULL,
                      cycle_length = NULL,
                      cycle_at = NULL,
                      debug = F)
{
  time_keys_ <- ""
  
  #If any required inputs are NULL, return
  if(is.null(invyear) || is.null(start_year) || is.null(end_year) || 
     is.null(cycle_length)) return(time_keys_)
  
  #if any required inputs are NA values, return
  if(is.na(invyear) || is.na(start_year) || is.na(end_year) || 
     is.na(cycle_length)) return(time_keys_)
  
  #If start_year is greater than or equal to end_year, then set end_year to 
  #start_year + 1 default cycle length
  if(start_year >= end_year) end_year <- start_year + cycle_length
  
  #If invyear is greater than start_year, reset it to start_year
  if(invyear > start_year) invyear <- start_year
  
  #Cast cycle length to integer
  cycle_length <- as.integer(cycle_length)
  
  #Determine initial number of cycles from inventory year to end year based
  #on a uniform cycle length.
  numCycles <- ceiling((end_year - invyear)/cycle_length)
  
  #Declare years vector. This vector will initially contain the inventory year,
  #start year, end year and all other cycle years based on a common cycle
  #length. Length of this initial vector will always be one more than value of
  #numCycles determined above.
  years <- vector(mode = "numeric",
                  length = numCycles + 1)
  
  #Initialize current_year and past_start. Set value of year[1] to inventory year.
  current_year <- invyear
  past_start = FALSE
  years[1] <- current_year
  
  #Adjust past_start if inventory year is equal to common start year
  if(invyear == start_year) past_start <- TRUE
  
  #Populate years vector
  for(i in 2:length(years))
  {
    
    # #Store inventory year. If inventory year and common start year are the same
    # #set past_start to TRUE and proceed to next iteration
    # if(i == 1)
    # {
    #   years[i] <- current_year
    #   if(current_year == start_year) past_start <- TRUE
    #   next
    # }
    
    #Calculate following year assuming default cycle length
    next_year <- current_year + cycle_length
    
    #If we have gone past the start_year after incrementing by cycle_length, set
    #next_year to the start_year. past_start will also be set to TRUE
    if(!past_start && next_year >= start_year)
    {
      next_year <- start_year
      past_start <- TRUE
    }
    
    #If we have gone past the end_year after incrementing by cycle_length, set
    #next_year to the end_year.
    if(next_year >= end_year)
    {
      next_year <- end_year
    }
    
    #Store next_year in years variable
    years[i] <- next_year
    
    #next_year becomes current_year for next loop iteration
    current_year <- next_year
  }
  
  #if cycle_at has values, add them to years and then sort years.
  if(length(cycle_at) > 0) 
  {
    years <- c(years, cycle_at)
    years <- sort(years)
  }
  
  #Years will now contain all cycle years to potentially consider in the
  #simulation.
  #Do debug if requested.
  if(debug)
  {
    cat("Inventory year:", invyear, "\n")
    cat("Start year:", start_year, "\n")
    cat("End year:", end_year, "\n")
    cat("Default cycle length:", cycle_length, "\n")
    cat("Years to consider:", "\n")
    for(year in years)
    {
      cat(year, "\n")
    }
  }
  
  #Create initial TIMEINT for all cycles with a common cycle length.
  time_keys_ <- paste(fvs_keyword(params = list("TIMEINT",
                                                "0", 
                                                as.character(cycle_length))),
                      sep = "\n")
  
  #Initialize cycle_num to keep track of number of cycles.
  cycle_num <- 0
  
  #Loop across years and create additional TIMEINT keywords for cycles whose 
  #length in years differs from the default cycle length.
  for(i in 1:(length(years) - 1))
  {
    
    year1 <- years[i]
    year2 <- years[i + 1]
    
    #If year1 less than invyear skip (should only occur if a cycle_at value less
    #than inventory year was entered)
    if(year1 < invyear) next
    
    #If year2 greater than end_year skip (should only occur if a cycle_at value 
    #greater than inventory year was entered)
    if(year2 > end_year) next
    
    #If year 1 equals year 2 skip
    if(year1 == year2) next
    
    #Calculate difference between years
    dif <- (year2 - year1) %% cycle_length
    
    #If 40 cycles has been exceeded break out of loop
    if(cycle_num >= 40) break
    else cycle_num <- cycle_num + 1
    
    #If dif != 0 then add another timeInt keyword to time_keys_
    if(dif != 0)
    {
      time_keys_ <- paste(time_keys_,
                        fvs_keyword(params = list("TIMEINT",
                                                 as.character(cycle_num), 
                                                 as.character(dif))),
                        sep = "\n")
    }
  }
  
  #Add NUMCYCLE keyword
  time_keys_ <- paste(time_keys_,
                    fvs_keyword(params = list("NUMCYCLE",
                                             as.character(cycle_num))),
                    sep = "\n")
  
  return(time_keys_)
}

################################################################################
#'dsnin_keys
#'@name dsnin_keys
#'@description
#'
#'This function creates a set of keywords for reading in data from an input FVS
#'database
#
#'@param dbin:       
#'Name of FVS input database that includes file extension (.db or .sqlite).
#
#'@param standinit:   
#'Character string corresponding to name table where stand information will be 
#'read from. Typically this would be "FVS_standinit" or "FVS_PlotInit" but could
#'"FVS_standinit_Plot", "FVS_standinit_Cond", etc.
#
#'@param treeinit:    
#'Character string corresponding to name table where tree information will be 
#'read from. Typically this would be "FVS_treeinit" but could 
#'"FVS_treeinit_Plot", "FVS_treeinit_Cond", etc.
#
#'@param stand_type:  
#'Integer value that determine what stand ID will be used to read data from.
#'
#'0 = Stand_CN
#'
#'1 = Stand_ID
#
#'@return
#'Character string of keywords with SQL instructions to read data in from
#'FVS_standinit and FVS_treeinit tables.
################################################################################

#'@export
dsnin_keys <- function(dbin = 'FVS_Data.db',
                       stand_type = 1,
                       standinit = "FVS_StandInit",
                       treeinit = "FVS_TreeInit")
{

  #Catch bad stand_type values
  if(!stand_type %in% c(0, 1)) stand_type <- 1
     
  #If standinit contains "stand", assume we are dealing with a StandInit table
  if(grepl(pattern = "stand", x = standinit, ignore.case = TRUE))
  {
    if(stand_type == 1) stand_read <- "WHERE Stand_ID = '%StandID%'"
    else stand_read <- "WHERE Stand_CN = '%Stand_CN%'"
  }
  
  #Otherwise assume a PlotInit table
  else 
  {
    if(stand_type == 1) stand_read <- "WHERE StandPlot_ID = '%StandID%'"
    else stand_read <- "WHERE StandPlot_CN = '%Stand_CN%'"
  }
  
  #Build the db input keywords
  dsnin_keys_ <- paste("DATABASE", 
                       "DSNIN",
                       dbin, 
                       "STANDSQL",
                       "SELECT *",
                       paste("FROM", standinit),
                       stand_read,
                       "ENDSQL",
                       "TREESQL", 
                       "SELECT *",
                       paste("FROM", treeinit),
                       stand_read,
                      "ENDSQL",
                      "END", 
                      sep = "\n")
  
  return(dsnin_keys_)
}

################################################################################
#'dsnout_keys
#'@name dsnout_keys
#'@description
#'This function creates a set of keywords for setting an FVS output database.
#
#'@param dbout: 
#'Name of FVS output database that includes file extension (.db or .sqlite). By
#'default this argument is set to "FVS_Data.db".
#
#'@return
#'Character string containing keywords for setting an FVS output database.
################################################################################

#'@export
dsnout_keys <- function(dbout = 'FVSOut.db')
  
{
  #Build the db output keywords
  dsnout_keys <- paste("DATABASE", 
                       "DSNOUT",
                       dbout, 
                       "END", 
                       sep = "\n")
  
  return(dsnout_keys)
}

################################################################################
#'delotab_keys
#'@name delotab_keys
#'@description
#'This function creates a set of delotab keywords based on input numeric vector.
#
#'@param delotab: 
#'Vector of integer values that contain any values from 1-4.
#
#'@return
#'Character string containing set of DELOTAB keywords or empty string.
################################################################################

#'@export
delotab_keys <- function(delotab = NULL)
{
  keys <- ""
  
  #Create empty character vector to store delotab keywords
  delo_vector <- vector(mode = "character",
                        length = 0)
  
  for(delo in delotab)
  {
    #Skip if delotab is not a value from 1-4
    if(!delo %in% 1:4) next
    
    #Create DELOTAB keyword and append
    delokey <- fvs_keyword(list("DELOTAB", delo),
                          type = 1)
    
    delo_vector <- append(delo_vector, delokey)
  }
  
  #Create DELOTAB keys
  keys <- paste(delo_vector,
                collapse = "\n")
  
  return(keys)
}

#===============================================================================
#Archived dfKeys logic
#===============================================================================

#===========================================================================
#Write any keywords provided in dfKeys argument
#===========================================================================

# if(length(dfKeys) > 0)
# {
#   for(i in 1:length(dfKeys))
#   {
#     #Loop across list of dataframes
#     df_key <- dfKeys[[i]]
#     
#     #If item in list in not dataframe, skip
#     if(class(df_key) != 'data.frame') next
#     
#     #If df_keys has less than 2 columns or now rows, skip
#     if(ncol(df_key) < 2 || nrow(df_key) <= 0) next
#     
#     #Now process each row of dataframe
#     for(j in 1:nrow(df_key))
#     {
#       #Obtain the row as a list
#       df_row <- as.list(df_key[j ,])
#       
#       #Obtain type variable to determine if keyword or function will be
#       #written
#       type <- df_row[1]
#       
#       #If type is not valid, then skip to next item in df_row
#       if(!type %in% c(1, 2, 3, 4)) next
#       
#       #Get keyword or event monitor function string
#       keys <- fvs_keyword(params = df_row[2:length(df_row)],
#                          type = type)
#       
#       cat(keys, "\n")
#     }
#     
#     cat("\n")
#   }
# }


#'param dfKeys:      
#'Optional list of dataframes containing sets of keywords or variables to define
#'with event monitor function call. Each row of a data frame in the list should
#'be encoded in one of the four options described below. 
#'
#'1: Row corresponds to standard FVS keyword (not in parms format). Value in 
#'Column 1, row x should be a value of 1. Column 2, row x should be name of FVS
#'keyword. All following columns in row x correspond to keyword parameters.
#'
#'2: Row corresponds to FVS keyword in parms format. Value in column 1, row x 
#'should be a value of 2. Column 2, row x should be name of FVS keyword. All 
#'following columns in row x 
#'correspond to keyword parameters.
#'
#'3: Row corresponds to variable defined by event monitor function. Value in
#'column 1, row x should be a value of 3. Column 2, row x should be name of 
#'event monitor variable to define. Column 3, row x should be name of event 
#'monitor function. All following columns in row x correspond to event monitor
#'function arguments.
#'
#'4: Row corresponds to a collection of values that will be returned as single 
#'string. Value in column 1, row x should be a value of 4. All following columns
#'in row x will be concatenated into a string where each value is separated by a
#'space and returned without any additional formatting.
