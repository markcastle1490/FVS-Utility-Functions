################################################################################
#'fvs_keyfile
#'@name fvs_keyfile
#'@description 
#'This function creates a keyword file based on user defined set of keywords for
#'a set of stands IDs and inventory years. 
#'
#'@param keyFile:     
#'Character string corresponding to directory path and file name of keyword file
#'being created.
#'
#'@param standID:     
#'Character vector of stand identification numbers.
#'
#'@param invYear:     
#'Numeric vector of inventory years for each stand in standID and or standCN 
#'arguments.
#'
#'@param standCN:
#'Optional character vector of stand control numbers. If values are not defined
#'for this argument, then the value from standID will be used as the stand 
#'control number in the output keyword file.
#'
#'@param keyWords:
#'Optional character vector, list, or list of character vectors containing 
#'properly formatted FVS keywords or variables to define with event monitor 
#'function call that will be included in keyword file for each stand.
#'
#'@param standKeys:
#'Optional character vector containing a set of properly formatted keywords or
#'variables to define with event monitor function call for specific stands in 
#'the standID argument. These keywords will be added for the given stand in the
#'output keyword file. If this argument is used, it should match the length of 
#'the standID argument.
#'
#'@param startYear:   
#'Common start year for simulation. If startYear and endYear are not NULL, then
#'function will attempt to add time keyword sequence (see time_keys function).
#'
#'@param endYear:     
#'Common end year for simulation. If startYear and endYear are not NULL, then
#'function will attempt to add TIMEIMT and NUMCYCLE keyword sequence (see 
#'time_keys function).
#'
#'@param cycleLength: 
#'Default cycle length assumed for simulation. Only used when startYear and 
#'endYear are not NULL.
#'
#'@param cycleAt: 
#'Numeric vector containing additional reporting years (those that do not occur
#'on default cycle boundaries based on startYear, endYear and 'cycleLength 
#'arguments). Only used when startYear and endYear are not NULL.
#'
#'@param dbIn: 
#'Character string for name of FVS input database that includes file extension
#'(.db, .sqlite). If this value is not NULL, the function will attempt to add a 
#'DSNIN keyword sequence.
#'
#'@param standInit:   
#'Character string corresponding to name table where stand information will be 
#'read from. Typically this would be "FVS_StandInit" of "FVS_PlotInit" but could
#'be "FVS_StandInit_Plot", "FVS_StandInit_Cond", etc. when FIA data is in use.
#'This is an optional argument and will only be used if dbIn is not NULL.
#'
#'@param treeInit:    
#'Character string corresponding to name table where tree information will be 
#'read from. Typically this would be "FVS_TreeInit" but could be 
#'"FVS_TreeInit_Plot", "FVS_TreeInit_Cond", etc. when FIA data is in use. This
#'is an optional argument and will only be used if dbIn is not NULL.
#'
#'@param standType:   
#'Integer value that determines what stand ID will be used to read data from 
#'dbIn when dbIn is not NULL.
#'
#'0 = Stand_CN
#'
#'1 = Stand_ID
#
#'@param dbOut:        
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
fvs_keyfile <- function(keyFile,
                        standID,
                        invYear,
                        standCN = NULL,
                        standKeys = NULL,
                        keyWords = NULL,
                        startYear = NULL,
                        endYear = NULL,
                        cycleLength = 10,
                        cycleAt = NULL,
                        dbIn = NULL,
                        standInit = "FVS_StandInit",
                        treeInit = "FVS_TreeInit",
                        standType = 1,
                        dbOut = NULL,
                        delotab = c(1, 2))
{

  #If keyFile exists already,  delete it
  if(file.exists(keyFile)) unlink(keyFile)
  
  #Switch \\\\ to / in keyFile
  keyFile <- gsub("\\\\", "/", keyFile)
  
  #Extract path to output by extract all characters before the last / in output.
  outPath <- gsub("/[^/]+$", "", keyFile)
  
  #Test existence of output path and if it does not exist report error.
  if (!(file.exists(outPath))){
    stop(paste("Path to output:", outPath, "was not found.",
               "Make sure directory path to output is spelled correctly."))
  }
  
  #Extract file extension for output argument.
  fileExtOut<-sub("(.*)\\.","",keyFile)
  
  #Test if output file extension is valid (.key).
  if(!fileExtOut %in% c("key"))
  {
    stop(paste("Output argument does not have a valid file extension. File",
               "extension must be .key."))
  }
  
  #If invYear or standID are empty, stop with an error
  if(length(invYear) <= 0 || length(standID) <= 0)
  {
    stop(paste("Values are required for invYear and standID."))
  }
  
  #If invYear and standID are not the same length, stop with an error
  if(length(invYear) != length(standID))
  {
    stop(paste("Length of invYear and standID arguments must be the same."))
  }
  
  #Create output file and sink
  sink(file = keyFile)
  
  #Loop across stands
  for(i in 1:length(standID))
  {
    #Get stand ID and inventory year
    stand_id <- standID[[i]] 
    inv_year <- invYear[[i]]
    
    #Get standCN if length is not zero and bounds of standCN vector has not 
    #been exceeded
    if(length(standCN) > 0 && i <= length(standCN))
    {
      stand_cn <- standCN[[i]]
    }
    else
    {
      stand_cn <- stand_id
    }

    #Setup standIdent and StandCN keywords and add to keyword file
    standKey <- paste("StdIdent",
                      stand_id,
                      "StandCN",
                      stand_cn,
                      sep = "\n")  
    
    #Write standIdent and StandCN keywords
    cat(standKey, "\n", "\n")
    
    #Create INVYR keyword
    invyrKey <- fvs_keyword(params = list("INVYEAR", inv_year),
                           type = 1)
    
    #Write INVYR keywords
    cat(invyrKey, "\n", "\n")
    
    #===========================================================================
    #Determine if output DSNOUT keywords will be produced and added to keyword
    #file
    #===========================================================================
    
    if(!is.null(dbOut))
    {
      dsnout_keys <- dsnout_keys(dbOut = dbOut)
      
      cat(dsnout_keys, "\n", "\n")
    }
    
    #===========================================================================
    #Determine if timing keywords are produced and added to keyword file
    #===========================================================================
    
    if(!is.null(startYear) && !is.null(endYear))
    {
      #Get timing keywords
      time <- time_keys(invYear = invYear,
                        startYear = startYear,
                        endYear = endYear,
                        cycleLength = cycleLength,
                        cycleAt = cycleAt)
      
      cat(time, "\n", "\n")
    }
    
    #===========================================================================
    #Check if delotab keywords should be added
    #===========================================================================
    
    if(length(delotab) > 0)
    {
      deloKeys <- delotab_keys(delotab = delotab)
      cat(deloKeys, "\n", "\n")
    }
    
    #===========================================================================
    #Write any keywords provided in keyWords argument
    #===========================================================================
    
    if(length(keyWords) > 0)
    {
      for(keyset in keyWords)
      {
        for(keys in keyset)
        {
          cat(keys, "\n")
        }
        
        #Add extra line after keyset is processed
        cat("\n")
      }
    }
    
    #===========================================================================
    #Write any stand specific keywords in standKeys argument
    #===========================================================================
    
    if(i <= length(standKeys))
    {
        stand_key <- as.character(standKeys[i])
        if(!is.na(stand_key) || !is.null(stand_key)) cat(stand_key, "\n", "\n")
    }
    
    #===========================================================================
    #Determine if input database keywords will be produced and added to keyword
    #file
    #===========================================================================
    
    if(!is.null(dbIn))
    {
      dsnin_keys <- dsnin_keys(dbIn = dbIn,
                               standType = standType,
                               standInit = standInit,
                               treeInit = treeInit)
      
      cat(dsnin_keys, "\n", "\n")
    }
    
    #Add process keyword
    cat("Process", "\n", "\n")
  }
  
  #Add stop keyword
  cat("STOP")
  
  #Close file connection
  sink(file = NULL)
  
}

################################################################################
#'fvs_kcpfile
#'@name fvs_kcpfile
#'@description
#'
#'This function creates a kcp file based on user defined set of keywords.
#
#'@param kcpFile:     
#'Character string corresponding to file path of keyword component file (.kcp)
#'being created.
#
#'@param keyWords:   
#'Optional character vector, list, or list of character vectors containing 
#'properly formatted FVS keywords or variables to define with event monitor 
#'function call that will be included in keyword file for each stand.
#
#'@return
#'None
################################################################################

#'@export
fvs_kcpfile <- function(kcpFile,
                        keyWords = list())
{
  
  #If kcpFile exists already,  delete it
  if(file.exists(kcpFile)) unlink(kcpFile)
  
  #Extract path to output by extract all characters before the last / in output.
  outPath <- gsub("/[^/]+$", "", kcpFile)
  
  #Test existence of output path and if it does not exist report error.
  if (!(file.exists(outPath))){
    stop(paste("Path to output:", outPath, "was not found.",
               "Make sure directory path to output is spelled correctly."))
  }
  
  #Extract file extension for output argument.
  fileExtOut<-sub("(.*)\\.","", kcpFile)
  
  #Test if output file extension is valid (.key).
  if(!fileExtOut %in% c("kcp"))
  {
    stop(paste("Output argument does not have a valid file extension. File",
               "extension must be .kcp."))
  }
  
  #Create output file and sink
  sink(file = kcpFile)
    
  #===========================================================================
  #Write any keywords provided in keyWords argument
  #===========================================================================
    
  if(length(keyWords) > 0)
  {
    for(keyset in keyWords)
    {
      for(keys in keyset)
      {
        cat(keys, "\n")
      }
    }
  }
    
  #Close file connection
  sink(file = NULL)
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
      functionArgs <- paste0("(",
                             paste0(char_params[3:length(char_params)],
                                    collapse = ","),
                             ")")
      
      #Create the keyword (function call here)
      keyword <- paste0(event_var,
                        functionArgs,
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
#'default cycle length, and optional cycleAt keywords.
#
#'@param invYear:    
#'Inventory year of stand.
#
#'@param cycleLength: 
#'Default cycle length assumed for simulation.
#
#'@param startYear:  
#'Common start year for simulation.
#
#'@param endYear:
#'End year for simulation.
#
#'@param cycleAt:    
#'Vector of years corresponding to additional reporting years that don't 
#'coincide with default reporting years determined by startYear, endYear, and 
#'cycleLength.
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
time_keys <- function(invYear = NULL,
                      startYear = NULL,
                      endYear = NULL,
                      cycleLength = NULL,
                      cycleAt = NULL,
                      debug = F)
{
  time_keys_ <- ""
  
  #If any required inputs are NULL, return
  if(is.null(invYear) || is.null(startYear) || is.null(endYear) || 
     is.null(cycleLength)) return(time_keys_)
  
  #if any required inputs are NA values, return
  if(is.na(invYear) || is.na(startYear) || is.na(endYear) || 
     is.na(cycleLength)) return(time_keys_)
  
  #If startYear is greater than or equal to endYear, then set endYear to 
  #startYear + 1 default cycle length
  if(startYear >= endYear) endYear <- startYear + cycleLength
  
  #If invYear is greater than startYear, reset it to startYear
  if(invYear > startYear) invYear <- startYear
  
  #Cast cycle length to integer
  cycleLength <- as.integer(cycleLength)
  
  #Determine initial number of cycles from inventory year to end year based
  #on a uniform cycle length.
  numCycles <- ceiling((endYear - invYear)/cycleLength)
  
  #Declare years vector. This vector will initially contain the inventory year,
  #start year, end year and all other cycle years based on a common cycle
  #length. Length of this initial vector will always be one more than value of
  #numCycles determined above.
  years <- vector(mode = "numeric",
                  length = numCycles + 1)
  
  #Initialize currentYear and pastStart. Set value of year[1] to inventory year.
  currentYear <- invYear
  pastStart = FALSE
  years[1] <- currentYear
  
  #Adjust pastStart if inventory year is equal to common start year
  if(invYear == startYear) pastStart <- TRUE
  
  #Populate years vector
  for(i in 2:length(years))
  {
    
    # #Store inventory year. If inventory year and common start year are the same
    # #set pastStart to TRUE and proceed to next iteration
    # if(i == 1)
    # {
    #   years[i] <- currentYear
    #   if(currentYear == startYear) pastStart <- TRUE
    #   next
    # }
    
    #Calculate following year assuming default cycle length
    nextYear <- currentYear + cycleLength
    
    #If we have gone past the startYear after incrementing by cycleLength, set
    #nextYear to the startYear. pastStart will also be set to TRUE
    if(!pastStart && nextYear >= startYear)
    {
      nextYear <- startYear
      pastStart <- TRUE
    }
    
    #If we have gone past the endYear after incrementing by cycleLength, set
    #nextYear to the endYear.
    if(nextYear >= endYear)
    {
      nextYear <- endYear
    }
    
    #Store nextYear in years variable
    years[i] <- nextYear
    
    #nextYear becomes currentYear for next loop iteration
    currentYear <- nextYear
  }
  
  #if cycleAt has values, add them to years and then sort years.
  if(length(cycleAt) > 0) 
  {
    years <- c(years, cycleAt)
    years <- sort(years)
  }
  
  #Years will now contain all cycle years to potentially consider in the
  #simulation.
  #Do debug if requested.
  if(debug)
  {
    cat("Inventory year:", invYear, "\n")
    cat("Start year:", startYear, "\n")
    cat("End year:", endYear, "\n")
    cat("Default cycle length:", cycleLength, "\n")
    cat("Years to consider:", "\n")
    for(year in years)
    {
      cat(year, "\n")
    }
  }
  
  #Create initial TIMEINT for all cycles with a common cycle length.
  time_keys_ <- paste(fvs_keyword(params = list("TIMEINT",
                                             "0", 
                                             as.character(cycleLength))),
                    sep = "\n")
  
  #Initialize cycleNum to keep track of number of cycles.
  cycleNum <- 0
  
  #Loop across years and create additional TIMEINT keywords for cycles whose 
  #length in years differs from the default cycle length.
  for(i in 1:(length(years) - 1))
  {
    
    year1 <- years[i]
    year2 <- years[i + 1]
    
    #If year1 less than invYear skip (should only occur if a cycleAt value less
    #than inventory year was entered)
    if(year1 < invYear) next
    
    #If year2 greater than endYear skip (should only occur if a cycleAt value 
    #greater than inventory year was entered)
    if(year2 > endYear) next
    
    #If year 1 equals year 2 skip
    if(year1 == year2) next
    
    #Calculate difference between years
    dif <- (year2 - year1) %% cycleLength
    
    #If 40 cycles has been exceeded break out of loop
    if(cycleNum >= 40) break
    else cycleNum <- cycleNum + 1
    
    #If dif != 0 then add another timeInt keyword to time_keys_
    if(dif != 0)
    {
      time_keys_ <- paste(time_keys_,
                        fvs_keyword(params = list("TIMEINT",
                                                 as.character(cycleNum), 
                                                 as.character(dif))),
                        sep = "\n")
    }
  }
  
  #Add NUMCYCLE keyword
  time_keys_ <- paste(time_keys_,
                    fvs_keyword(params = list("NUMCYCLE",
                                             as.character(cycleNum))),
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
#'@param dbIn:       
#'Name of FVS input database that includes file extension (.db or .sqlite).
#
#'@param standInit:   
#'Character string corresponding to name table where stand information will be 
#'read from. Typically this would be "FVS_StandInit" or "FVS_PlotInit" but could
#'"FVS_StandInit_Plot", "FVS_StandInit_Cond", etc.
#
#'@param treeInit:    
#'Character string corresponding to name table where tree information will be 
#'read from. Typically this would be "FVS_TreeInit" but could 
#'"FVS_TreeInit_Plot", "FVS_TreeInit_Cond", etc.
#
#'@param standType:  
#'Integer value that determine what stand ID will be used to read data from.
#'
#'0 = Stand_CN
#'
#'1 = Stand_ID
#
#'@return
#'Character string of keywords with SQL instructions to read data in from
#'FVS_StandInit and FVS_TreeInit tables.
################################################################################

#'@export
dsnin_keys <- function(dbIn = 'FVS_Data.db',
                       standType = 1,
                       standInit = "FVS_StandInit",
                       treeInit = "FVS_TreeInit")
{

  #Catch bad standType values
  if(!standType %in% c(0, 1)) standType <- 1
     
  #Set the stand column to read data from
  if(standType == 1) 
  {
    standRead <- "WHERE Stand_ID = '%StandID%'"
  }
  
  else 
  {
    standRead <- "WHERE Stand_CN = '%Stand_CN%'"
  }
  
  #Change standRead if plot level table is being assumed
  if(grepl(pattern = "plot", x = standInit, ignore.case = TRUE))
  {
    if(standType == 1) standRead <- gsub(pattern = "WHERE Stand_ID",
                                         replacement = "WHERE StandPlot_ID",
                                         x = standRead)
    
    else standRead <- gsub(pattern = "WHERE Stand_CN",
                           replacement = "WHERE StandPlot_CN",
                           x = standRead)
  }
  
  #Build the db input keywords
  dsnin_keys_ <- paste("DATABASE", 
                       "DSNIN",
                       dbIn, 
                       "STANDSQL",
                       "SELECT *",
                       paste("FROM", standInit),
                       standRead,
                       "ENDSQL",
                       "TREESQL", 
                       "SELECT *",
                       paste("FROM", treeInit),
                       standRead,
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
#'@param dbOut: 
#'Name of FVS output database that includes file extension (.db or .sqlite). By
#'default this argument is set to "FVS_Data.db".
#
#'@return
#'Character string containing keywords for setting an FVS output database.
################################################################################

#'@export
dsnout_keys <- function(dbOut = 'FVSOut.db')
  
{
  #Build the db output keywords
  dsnout_keys <- paste("DATABASE", 
                       "DSNOUT",
                       dbOut, 
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
delotab_keys <- function(delotab = c())
{
  keys <- ""
  
  #Create empty character vector to store delotab keywords
  delo_vector <- vector(mode = "character",
                        length = 0)
  
  for(delo in delotab)
  {
    #Skip if delotab is not a value from 1-4
    if(delo < 1 || delo > 4) next
    
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
