################################################################################
#'get_fiadb
#'@name get_fiadb
#'@description
#'This function is used to retrieve a state specific FIA database, set of state
#'specific databases, or the master FIA SQLite database from the FIA datamart.
#'The downloaded database(s) are extracted and stored on a specified local
#'directory.
#
#'@param output:     
#'Character string pertaining to file path to output directory where FIA 
#'databases will be stored.
#
#'@param url:        
#'Character string corresponding to URL of FIA datamart. This argument should be
#'changed if FIA changes url to datamart.
#
#'@param states: 
#'Vector of two-character state abbreviations. Valid state codes are listed 
#'below.
#'
#'"AL", "AK", "AZ", "AR", "CA", "CO", "CT", "DE", "FL", "GA",
#'"HI", "ID", "IL", "IN", "IA", "KS", "KY", "LA", "ME", "MD",
#'"MA", "MI", "MN", "MS", "MO", "MT", "NE", "NV", "NH", "NJ",
#'"NM", "NY", "NC", "ND", "OH", "OK", "OR", "PA", "RI", "SC",
#'"SD", "TN", "TX", "UT", "VT", "VA", "WA", "WV", "WI", "WY"
#
#'@param masterDB:   
#'Logical variable used to signify if master FIA database should be downloaded.
#'If TRUE, this will lead to very long processing time.
#
#'@return 
#'None
################################################################################

#'@export
get_fiadb <- function(output = NULL,
                      url = "https://apps.fs.usda.gov/fia/datamart/Databases/",
                      states = NULL,
                      masterDB = FALSE)
{
  #Initialize retCode
  retCode <- 0

  #Vector of state abbreviations
  state_abbrev <- c("AL", "AK", "AZ", "AR", "CA", "CO", "CT", "DE", "FL", "GA",
                    "HI", "ID", "IL", "IN", "IA", "KS", "KY", "LA", "ME", "MD",
                    "MA", "MI", "MN", "MS", "MO", "MT", "NE", "NV", "NH", "NJ",
                    "NM", "NY", "NC", "ND", "OH", "OK", "OR", "PA", "RI", "SC",
                    "SD", "TN", "TX", "UT", "VT", "VA", "WA", "WV", "WI", "WY")

  #Check if output file exists, if not return error
  if(!file.exists(output))
  {
    stop(paste(output, "does not exist"))
  }

  #Check if states is empty, if so return error.
  if(is.null(states) & !masterDB)
  {
    stop(paste("No states specified for download. Enter values in states",
               "argument or set masterDB to TRUE."))
  }

  #if masterDB is TRUE, redefine states
  if(masterDB) states <- "ALL"

  #Capitalize state abbreviations
  states <- toupper(states)

  #Loop across states
  for(i in 1:length(states))
  {
    state <- states[i]
    cat("Processing state:", state, "\n")

    #If state is not in  state_abbrev, skip
    if(!state %in% state_abbrev || !masterDB)
    {
      cat("Invalid state:",
          state,
          "specified and will be skipped.",
          "\n")
      next
    }

    #Create filenameUrl and filenameDisk
    filename <- paste0("SQLite_FIADB_", state, ".zip")

    #If states is ALL, then create filename for master FIA database.
    if(masterDB) filename <-"SQLite_FIADB_ENTIRE.zip"

    filenameUrl <- paste0(url,
                          filename)

    filenameDisk <- paste0(output,
                           "/",
                           filename)

    #if filenameDisk exists, delete it
    if(file.exists(filenameDisk)) retCode <- unlink(filenameDisk)

    #If retCode is 1, stop with error
    if(retCode == 1)
      stop("Failed to delete prexisting file:", filenameDisk, "\n")

    cat("Downloading file:",
        filenameUrl,
        "to",
        filenameDisk,
        "\n")

    #Download the data
    httpResponse <-httr::RETRY("GET",
                         paste0(filenameUrl),
                         httr::write_disk(filenameDisk),
                         httr::progress(),
                         overwrite = T)
    cat("HTTP STATUS:", httpResponse$status_code, "\n")

    #If status_code is >= 300, something went wrong. Stop execution.
    if(httpResponse$status_code >= 300)
    {
      stop(paste("HTTP status code signifies success did not occur."))
    }

    #Check if file exists and then extract and delete zipped folder.
    if(file.exists(filenameDisk))
    {
      #Extract data
      cat("Extracting files from:", filenameDisk, "\n")
      unzip(zipfile = filenameDisk,
            exdir = output)
      cat("Files extracted from:", filenameDisk, "\n")

      #Now delete the zip file
      cat("Deleting file:", filenameDisk, "\n")
      retCode <- unlink(filenameDisk)

      #Print message based upon outcome of retCode
      if(retCode != 1)
      {
        cat(filenameDisk, "was deleted sucessfully.", "\n")
      }
      else
      {
        cat(filenameDisk, "was not deleted sucessfully.", "\n")
      }
    }

    cat("Finished processing state:", state, "\n", "\n")
  }

  cat("All files proccessed.")
}
