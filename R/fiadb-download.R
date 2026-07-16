################################################################################
#' @name get_fiadb
#' @title Retrieve and Extract FIA SQLite Databases
#' @description This function is used to retrieve a state specific FIA database,
#' set of state specific databases, or the master FIA SQLite database from the 
#' FIA datamart. The downloaded database(s) are extracted and stored on a 
#' specified local directory.
#' 
#' @param output
#' Character string pertaining to file path to output directory where FIA 
#' databases will be stored. Defaults to NULL.
#' 
#' @param url
#' Character string corresponding to URL of FIA datamart. This argument should be 
#' changed if FIA changes url to datamart. 
#' 
#' @param states
#' Vector of two-character state abbreviations. Valid state codes are listed 
#' below:
#' 
#' "AL", "AK", "AZ", "AR", "CA", "CO", "CT", "DE", "FL", "GA", 
#' "HI", "ID", "IL", "IN", "IA", "KS", "KY", "LA", "ME", "MD", 
#' "MA", "MI", "MN", "MS", "MO", "MT", "NE", "NV", "NH", "NJ", 
#' "NM", "NY", "NC", "ND", "OH", "OK", "OR", "PA", "RI", "SC", 
#' "SD", "TN", "TX", "UT", "VT", "VA", "WA", "WV", "WI", "WY"
#' 
#' Defaults to NULL.
#' 
#' @param master_db
#' Logical variable used to signify if master FIA database should be downloaded. 
#' If TRUE, this will lead to long processing time. Defaults to FALSE.
#' 
#' @return
#' None
#' @export
################################################################################

get_fiadb <- function(output = NULL,
                      url = "https://apps.fs.usda.gov/fia/datamart/Databases/",
                      states = NULL,
                      master_db = FALSE,
                      verbose = FALSE)
{
  #Check output
  if (is.null(output) || !dir.exists(output)) {
    stop(paste("Output directory does not exist or was not found:", output))
  }
  
  #Check for input states options
  if (is.null(states) && !master_db) {
    stop("No states specified for download. Enter values in states argument or set master_db to TRUE.")
  }
  
  if (!endsWith(url, "/")) url <- paste0(url, "/")

  #Vector of state abbreviations
  state_abbrev <- c("AL", "AK", "AZ", "AR", "CA", "CO", "CT", "DE", "FL", "GA",
                    "HI", "ID", "IL", "IN", "IA", "KS", "KY", "LA", "ME", "MD",
                    "MA", "MI", "MN", "MS", "MO", "MT", "NE", "NV", "NH", "NJ",
                    "NM", "NY", "NC", "ND", "OH", "OK", "OR", "PA", "RI", "SC",
                    "SD", "TN", "TX", "UT", "VT", "VA", "WA", "WV", "WI", "WY")

  #Determine target states to process
  if (master_db) {
    process_targets <- "ENTIRE"
  } else {
    process_targets <- unique(toupper(states))
  }
  
  #Loop across process_targets
  for(i in seq_along(process_targets))
  {
    target <- process_targets[i]
    
    if(verbose) cat("Processing state:", target, "\n")

    #If state is not in  state_abbrev, skip
    if (!master_db && !target %in% state_abbrev) {
      warning("Invalid state code")
      next
    }
    
    # Format target file structures using secure file system operators
    filename <- if (master_db) "SQLite_FIADB_ENTIRE.zip" 
    else paste0("SQLite_FIADB_", target, ".zip")
    filename_url  <- paste0(url, filename)
    filename_disk <- file.path(output, filename)

    #if filename_disk exists, delete it
    if (file.exists(filename_disk)) {
      if(verbose) cat("Removing old local file:", filename, "\n")
      unlink(filename_disk, force = TRUE)
      if (file.exists(filename_disk)) {
        stop(paste("Unable to remove preexisting file:", filename_disk))
      }
    }

    #Download the data
    if(verbose) cat("Downloading file:", filename_url, "to", filename_disk, "\n")

    #Setup the request template with a robust 3-try retry engine
    req <- httr2::request(filename_url) |>
      httr2::req_retry(max_tries = 3, backoff = ~ 2)
    
    #Add progress if verbose is on
    if (verbose) {
      req <- httr2::req_progress(req)
    }
    
    #Execute the download to local disk
    http_response <- tryCatch({
      httr2::req_perform(req, path = filename_disk)
    }, error = function(e) {
      # If a server drops entirely or returns a 4xx/5xx code, httr2 automatically throws an error
      if (file.exists(filename_disk)) unlink(filename_disk)
      stop(paste("Network transaction failed or data was not accessible:", e$message))
    })
    
    # 3. Print status (httr2 structures its response metadata slightly differently)
    if(verbose) cat("HTTP STATUS:", httr2::resp_status(http_response), "\n")

    # Check if file exists and then extract and delete zipped folder.
    if (file.exists(filename_disk)) {
      
      # Extract data
      if(verbose) cat("Extracting files from:", filename_disk, "\n")
      unzip(zipfile = filename_disk, exdir = output)
      if(verbose) cat("Files extracted from:", filename_disk, "\n")
      
      # Now delete the zip file
      if(verbose) cat("Deleting file:", filename_disk, "\n")
      ret_code <- unlink(filename_disk)
      
      # Print message based upon outcome of ret_code
      if (ret_code != 0) {
        warning(paste(filename_disk, "was not deleted successfully."))
      }
    }

    if(verbose) cat("Finished processing target:", target, "\n\n")
  }

  if(verbose) cat("All files proccessed.")
  return(invisible())
}
