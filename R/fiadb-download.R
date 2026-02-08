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
#'@param master_db:   
#'Logical variable used to signify if master FIA database should be downloaded.
#'If TRUE, this will lead to long processing time.
#
#'@return 
#'None
################################################################################

#'@export
get_fiadb <- function(output = NULL,
                      url = "https://apps.fs.usda.gov/fia/datamart/Databases/",
                      states = NULL,
                      master_db = FALSE)
{
  #Initialize ret_code
  ret_code <- 0

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
  if(is.null(states) && !master_db)
  {
    stop(paste("No states specified for download. Enter values in states",
               "argument or set master_db to TRUE."))
  }

  #if master_db is TRUE, redefine states
  if(master_db) states <- "ALL"

  #Capitalize state abbreviations
  states <- toupper(states)

  #Loop across states
  for(i in 1:length(states))
  {
    state <- states[i]
    cat("Processing state:", state, "\n")

    #If state is not in  state_abbrev, skip
    if(!state %in% state_abbrev && !master_db)
    {
      cat("Invalid state:",
          state,
          "specified and will be skipped.",
          "\n")
      next
    }

    #Create filename_url and filename_disk
    filename <- paste0("SQLite_FIADB_", state, ".zip")

    #Create file name for master FIA database ,if TRUE
    if(master_db) filename <-"SQLite_FIADB_ENTIRE.zip"

    filename_url <- paste0(url,
                          filename)

    filename_disk <- paste0(output,
                           "/",
                           filename)

    #if filename_disk exists, delete it
    if(file.exists(filename_disk)) ret_code <- unlink(filename_disk)

    #If ret_code is 1, stop with error
    if(ret_code == 1)
      stop("Failed to delete prexisting file:", filename_disk, "\n")

    #Download the data
    cat("Downloading file:",
        filename_url,
        "to",
        filename_disk,
        "\n")

    http_response <-httr::RETRY("GET",
                                paste0(filename_url),
                                httr::write_disk(filename_disk),
                                httr::progress(),
                                overwrite = T)
    cat("HTTP STATUS:", http_response$status_code, "\n")

    #If status_code is >= 300, something went wrong. Stop execution.
    if(http_response$status_code >= 300)
    {
      stop(paste("HTTP status code indicates success did not occur."))
    }

    #Check if file exists and then extract and delete zipped folder.
    if(file.exists(filename_disk))
    {
      #Extract data
      cat("Extracting files from:", filename_disk, "\n")
      unzip(zipfile = filename_disk,
            exdir = output)
      cat("Files extracted from:", filename_disk, "\n")

      #Now delete the zip file
      cat("Deleting file:", filename_disk, "\n")
      ret_code <- unlink(filename_disk)

      #Print message based upon outcome of ret_code
      if(ret_code != 1)
        cat(filename_disk, "was deleted sucessfully.", "\n")
      else
        cat(filename_disk, "was not deleted sucessfully.", "\n")
    }

    cat("Finished processing state:", state, "\n", "\n")
  }

  cat("All files proccessed.")
}
