################################################################################
#' download_fia
#' @name download_fia
#' @description This function is used to download one of the following:
#'
#' 1) State specific SQLite FIA database(s)
#'
#' 2) All .csv files for state(s).
#'
#' 3) Specific .csv for state. AK_TREE.csv, AK_PLOT.csv, etc.
#' 
#' 4) FIA Reference tables (.zip)
#'
#' The downloaded files are extracted and stored on a specified local directory.
#' 
#' @param output
#' Character string pertaining to file path to output directory where FIA 
#' databases will be stored. Defaults to NULL.
#' 
#' @param type 
#' Numeric value specifying the type of data to download.
#' 
#' 1 = FIADB SQLite database for state(s)
#'
#' 2 = All csv files for state(s)
#' 
#' 3 = Specific .csv files for state(s)
#'
#' 4 = Reference table .csv files (not specific to any state)
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
#' "ENTIRE" (all states)
#' 
#' Defaults to NULL. This argument will be ignored when type is 3. 
#' 
#' @param tables 
#' Character vector containing valid, FIA table names (e.g. 'TREE', PLOT').
#' 
#' @return
#' None
#' @export
################################################################################

download_fia <- function(output = NULL,
                         type = 1,
                         states = NULL,
                         tables = NULL,
                         verbose = FALSE)
{
  #Check output
  if (is.null(output) || !dir.exists(output)) {
    stop(paste("Output directory does not exist or was not found:", output))
  }
  
  #Check for bad type values
  if(!type %in% 1:4)
    stop("Invalid type value entered.")
  
  #Check for input states options
  if (is.null(states) && type %in% 1:2) {
    stop("No states specified for download. Enter values in states argument.")
  }
  
  #Check for input tables options
  if (is.null(tables) && type == 4) {
    stop("Type is 4 and no tables were specified to download.")
  }
  
  #Vector of state abbreviations
  state_abbrev <- c("AL", "AK", "AZ", "AR", "CA", "CO", "CT", "DE", "FL", "GA",
                    "HI", "ID", "IL", "IN", "IA", "KS", "KY", "LA", "ME", "MD",
                    "MA", "MI", "MN", "MS", "MO", "MT", "NE", "NV", "NH", "NJ",
                    "NM", "NY", "NC", "ND", "OH", "OK", "OR", "PA", "RI", "SC",
                    "SD", "TN", "TX", "UT", "VT", "VA", "WA", "WV", "WI", "WY",
                    "ENTIRE")
  
  #Vector of FIA tables
  fia_tables <- c("BOUNDARY", "COND_DWM_CALC", "COND","COUNTY","DWM_COARSE_WOODY_DEBRIS",
                  "DWM_DUFF_LITTER_FUEL","DWM_FINE_WOODY_DEBRIS","DWM_MICROPLOT_FUEL",
                  "DWM_RESIDUAL_PILE", "DWM_TRANSECT_SEGMENT", "DWM_VISIT","GRND_CVR",
                  "INVASIVE_SUBPLOT_SPP","LICHEN_LAB","LICHEN_PLOT_SUMMARY","LICHEN_VISIT",
                  "OZONE_BIOSITE_SUMMARY","OZONE_PLOT_SUMMARY","OZONE_PLOT","OZONE_SPECIES_SUMMARY",
                  "OZONE_VALIDATION","OZONE_VISIT", "P2VEG_SUBP_STRUCTURE","P2VEG_SUBPLOT_SPP",
                  "PLOT_REGEN","PLOT", "PLOTGEOM", "PLOTSNAP","POP_ESTN_UNIT","POP_EVAL_ATTRIBUTE",
                  "POP_EVAL_GRP","POP_EVAL_TYP","POP_EVAL","POP_PLOT_STRATUM_ASSGN","POP_STRATUM",
                  "SEEDLING_REGEN","SEEDLING","SITETREE","SOILS_EROSION","SOILS_LAB","SOILS_SAMPLE_LOC" ,
                  "SOILS_VISIT", "SUBP_COND_CHNG_MTRX","SUBP_COND","SUBPLOT_REGEN","SUBPLOT",
                  "SURVEY","TREE_GRM_BEGIN","TREE_GRM_COMPONENT","TREE_GRM_ESTN", "TREE_GRM_MIDPT",
                  "TREE_REGIONAL_BIOMASS", "TREE_WOODLAND_STEMS","TREE","VEG_PLOT_SPECIES",
                  "VEG_QUADRAT","VEG_SUBPLOT_SPP","VEG_SUBPLOT", "VEG_VISIT",
                  'CITATION', 'DIFFERENCE_TEST_PER_ACRE', 'DIFFERENCE_TEST_TOTALS',
                  'FIADB_VERSION', 'FOREST_TYPE', 'FOREST_TYPE_GROUP',
                  'GRM_TYPE', 'HABTYP_DESCRIPTION', 'HABTYP_PUBLICATION',
                  'INVASIVE_SPECIES', 'LICHEN_SPECIES', 'LICHEN_SPP_COMMENTS',
                  'NVCS_HEIRARCHY_STRCT', 'NVCS_LEVEL_1_CODES', 'NVCS_LEVEL_2_CODES',
                  'NVCS_LEVEL_3_CODES', 'NVCS_LEVEL_4_CODES', 'NVCS_LEVEL_5_CODES',
                  'NVCS_LEVEL_6_CODES', 'NVCS_LEVEL_7_CODES', 'NVCS_LEVEL_8_CODES',
                  'OWNGRPCD', 'PLANT_DICTIONARY', 'POP_ATTRIBUTE', 'POP_EVAL_TYP_DESCR',
                  'RESEARCH_STATION', 'SPECIES', 'SPECIES_GROUP', 'STATE_ELEV', 'UNIT', 
                  'FVS_PLOTINIT_PLOT', 'FVS_GROUPADDFILESANDKEYWORDS', 'FVS_STANDINIT_COND', 
                  'FVS_TREEINIT_PLOT', 'FVS_STANDINIT_PLOT', 'FVS_TREEINIT_COND')
  
  #Check input states
  if(type %in% c(1, 2, 4))
  {
    invalid <- setdiff(states, state_abbrev)
    if(length(invalid) > 0)
      stop(paste("The following states are not valid:",
                 paste(invalid, collapse = ", ")))
  }
  
  #Check input tables
  if(type == 4)
  {
    tables <- toupper(tables)
    invalid <- setdiff(tables, fia_tables)
    if(length(invalid) > 0)
      stop(paste("The following tables are not valid:",
                 paste(invalid, collapse = ", ")))
  }
  
  #Determine url based on type
  url <- if(type == 1) "https://apps.fs.usda.gov/fia/datamart/Databases/" else
    "https://apps.fs.usda.gov/fia/datamart/CSV/"
  
  #Determine targets to download based on type
  if(type %in% c(1, 2, 4)) process_targets <- unique(toupper(states)) else
    process_targets <- "REF"
  
  #Loop across process_targets
  for(i in seq_along(process_targets))
  {
    target <- process_targets[i]
    
    if(verbose) cat("Processing:", target, "\n")
    
    #Determine file name based on type
    filename <- switch(as.character(type),
                       "1" = paste0("SQLite_FIADB_", target, ".zip"),
                       "2" = paste0(target, "_CSV.zip"),
                       "3" = "FIADB_REFERENCE.zip",
                       "4" = paste0(target, "_", tables, ".csv"),
                       stop(paste("Invalid type passed:", type))
    )
    
    #Begin loop over filename
    for(j in seq_along(filename))
    {
      #Setup url and file names
      file <- filename[j]
      filename_url  <- paste0(url, file)
      filename_disk <- file.path(output, file)
      
      #if filename_disk exists, delete it
      if (file.exists(filename_disk)) {
        if(verbose) cat("Removing preexisting local file:", filename, "\n")
        unlink(filename_disk, force = TRUE)
        if (file.exists(filename_disk)) {
          stop(paste("Unable to remove preexisting file:", filename_disk))
        }
      }
      
      #Download the data
      if(verbose) cat("Downloading file:",
                      filename_url, 
                      "to", 
                      filename_disk,
                      "\n")
      
      #Setup the request template
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
        # If a server drops entirely or returns a 4xx/5xx code, throw an error
        if (file.exists(filename_disk)) unlink(filename_disk)
        stop(paste("Network transaction failed or data was not accessible:", e$message))
      })
      
      #Print status
      if(verbose && !is.null(http_response)) 
        cat("HTTP STATUS:", httr2::resp_status(http_response), "\n")
      
      # Check if file exists and then extract and delete zipped folder 
      # (type 1 - 3)
      if (file.exists(filename_disk) && type %in% 1:3) {
        
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
    }
    if(verbose) cat("Finished processing target:", target, "\n\n")
  }
    
  if(verbose) cat("All files proccessed.")
  return(invisible())
}
