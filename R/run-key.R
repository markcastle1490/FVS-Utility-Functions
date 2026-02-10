################################################################################
#'run_key
#'@name run_key
#'@description
#'This function is used to run a single FVS keyword file (.key) with a specified
#'FVS variant dll (.dll) using the rFVS API. The rFVS package has to be 
#'installed for this function to work properly.
#
#'@param dll_path: 
#'Character string corresponding to directory where FVS dlls are stored
#
#'@param var_code: 
#'Two character string corresponding to FVS variant ("IE", "CS", etc.).
#
#'@param keyfile: 
#'Character string corresponding to directory and file name for a single FVS
#'keyword file.
#
#'@param verbose: 
#'Logical variable used to determine if debug output should be echoed to 
#'console.
#
#'@return None
################################################################################

#'@export
run_key = function(dll_path = "C:/FVS/FVSSoftware/FVSbin",
                   var_code = "ie",
                   keyfile = "C:/FVS.key",
                   verbose = FALSE)
{
  #Change \\ to / in dll_path argument
  dll_path = gsub("\\\\", "/", dll_path)
  
  #Change \\ to / in keyfile argument
  keyfile = gsub("\\\\", "/", keyfile)
  
  #Check for existence of dll_path
  if (!(file.exists(dll_path))){
    stop(paste("dll_path not found. Make sure directory path is spelled",
               "correctly."))
  }
  
  #Check for existence of keyfile
  if (!(file.exists(keyfile))){
    stop(paste("keyfile not found. Make sure directory path and file name are",
               "spelled correctly."))
  }
  
  #Make var_code lowercase
  var_code = tolower(var_code)
  
  #Create dll name
  var_code = paste0("FVS", var_code)
  
  #Load variant dll and specify directory path to dll in bin folder
  rFVS::fvsLoad(var_code, bin = dll_path)
  
  #Grab the the directory where the keyword file is stored
  keydir = gsub("/[^/]+$", "", keyfile)
  
  #Get the name of the keyword file
  keyfile_ = sub(".*/", "", keyfile)
  
  #Set working directory for the simulation
  if(keydir != keyfile_)
    setwd(keydir)
  
  #Set keyword file to command line
  rFVS::fvsSetCmdLine(paste0("--keywordfile=", keyfile_))
  
  #Initialize return code
  retcode = 0
  
  if(verbose)
  {
    #Reset keydir for printing purposes if directory path was not specified for
    #keyfile.
    if(keydir == keyfile_) keydir = getwd()
    cat("dll_path:", dll_path, "\n")
    cat("FVS Variant:",  var_code, "\n")
    cat("Keyfile:", keyfile, "\n")
    cat("Keyword file directory:", keydir, "\n")
    cat("Keyword file name:", keyfile_, "\n", "\n")
    cat("Running FVS...", "\n", "\n")
  }
  
  #Keep running FVS until a return code other than 0, is returned.
  while(retcode == 0)
  {
    retcode = rFVS::fvsRun()
    
    if(verbose)
      if(retcode != 0) cat("FVS return code:", retcode, "\n")
  }
  
  #=============================================================================
  #Unload variant dll
  #=============================================================================
  
  #Get fvs dll
  fvs_loaded = as.character(get(".FVSLOADEDLIBRARY",envir=.GlobalEnv)[['ldf']])
  
  #Unload
  dyn.unload(fvs_loaded)
  
  #Remove R object
  remove(".FVSLOADEDLIBRARY",envir=.GlobalEnv)
  
  invisible()
}

################################################################################
#'delete_caseid
#'@name delete_caseid
#'@description
#'
#'This function deletes desired rows in all database tables containing a CaseID 
#'field from a specified output FVS database. The rows that are deleted are 
#'determined from a set of CaseID values associated with any of the following
#'attributes: FVS run title, management ID, FVS variant code, or keyword file. 
#'The case IDs identified for deletion are extracted from the FVS_Cases table 
#'from the output FVS database. Some of the logic in this function was borrowed 
#'from Nick Crookston's deleteRelatedDBRows function from fvsRunUtilities.R 
#'file (fvsOL R package).
#
#'@param fvsout: 
#'Character string corresponding to file path to output FVS SQLite database
#'(.db, .sqlite).
#
#'@param delete_id: 
#'Vector of character strings pertaining to an FVS run title, management ID, FVS
#'variant code, or file path to keyword.
#'
#'@param id_type:
#'Integer value pertaining to type of value specified in delete_id argument.
#'
#'1 = keyword file path or file name (e.g. from KeywordFile in FVS_Cases table)
#'
#'2 = fvs run title (e.g. from RunTitle in FVS_Cases table)
#'
#'3 = management id (e.g. from MgmtID in FVS_Cases table)
#'
#'4 = FVS variant code (e.g. from Variant in FVS_Cases table)
#
#'@param vacuum: 
#'Logical variable used to determine if output database should be vacuumed after
#'deleting rows from output tables. This will release unused hard drive space 
#'left behind from row deletions.
#'
#'@param verbose: 
#'Logical variable used to determine if cat statements should be echoed to 
#'console.
#
#'@return None
################################################################################

#'@export
delete_caseid = function(fvsout = NULL,
                         delete_id = NULL,
                         id_type = 1,
                         vacuum = FALSE,
                         verbose = TRUE)
{
  #Stop with error if fvsOut is NULL
  if(is.null(fvsout)) 
    stop("Output FVS database was not specified.")
  
  #Stop with error if fvsOut does not exist
  if(!file.exists(fvsout)) 
    stop("Output FVS database does not exist.")
  
  #Check file extension of fvsout
  file_ext = sub("(.*)\\.","", fvsout)
  
  #Test if output file extension is valid database
  if(!file_ext %in% c("db", "sqlite"))
  {
    stop("Invalid database file type provided in fvsout.")
  }
  
  #Stop with error if delete_id is NULL
  if(is.null(delete_id)) 
    stop("No value(s) specified for delete_id.")
  
  #Set id_type to 1 if bad value is entered
  if(!id_type %in% c(1:4)) id_type = 1
  
  #Set select_var based on id_type
  select_var <- switch(id_type,
                       'KeywordFile',
                       'RunTitle',
                       'MgmtID',
                       'Variant')
  
  #Connect to output database
  con_out = RSQLite::dbConnect(RSQLite::SQLite(), fvsout)
  
  #Get output database tables
  out_tbl = RSQLite::dbListTables(conn = con_out)
  
  #Test if FVS_Cases table exists. If it doesn't, disconnect from database and
  #stop with error.
  if(!"FVS_Cases" %in% out_tbl)
  {
    RSQLite::dbDisconnect(conn = con_out)
    stop("FVS_Cases table was not found in output database.")
  }
  
  #Start loop across values in delete_id
  for(val in delete_id)
  {
    
    if(verbose)
    {
      cat("id_type:", id_type, "\n")
      cat("Variable used for case ID selection:", select_var, "\n")
      cat("Value for case ID deletion:", val, "\n")
    }
    
    #CaseID query
    query = paste("SELECT CaseID FROM FVS_Cases WHERE",
                   select_var,
                   "=", 
                   paste0("'", val, "'"),
                   "COLLATE NOCASE")
    
    #Attempt to get CaseIDs...
    caseid = RSQLite::dbGetQuery(conn = con_out,
                                  query)[[1]]
  
    if(verbose)
    {
      cat("CaseID Query:", query, "\n")
      cat("Number of Case ID values selected:", length(caseid), "\n", "\n")
    }
    
    #If no case id values are found, skip to next iteration.
    if(length(caseid) <= 0)
    {
      cat("No Case ID values found for deletion using:", val, "\n", "\n")
      next
    }
    
    #Collect caseid in single string 
    caseid = paste0("'", caseid, "'")
    case_string = paste0("(", paste(caseid, collapse = ", "), ")")
    
    #Loop across tables and delete rows
    for(tbl in out_tbl)
    {
      if(verbose) cat("Processing table:", tbl, "\n")
      
      #Remove rows if table contains CaseID field
      if("CaseID" %in% RSQLite::dbListFields(conn = con_out,
                                                name = tbl))
      {
        #Create delete query
        query = paste0("DELETE FROM ",
                        tbl, 
                        " WHERE ",
                        tbl,
                        ".CaseID IN",
                        case_string)
        
        #Execute query
        rtn = RSQLite::dbExecute(conn = con_out,
                                  query)
        
        if(verbose) cat("Number of rows deleted:", rtn, "\n")
        
        #Get number of rows.
        query = paste0("SELECT COUNT(*) FROM ",tbl)
        rows = RSQLite::dbGetQuery(con_out, query)[[1]]
        if(verbose) cat("Number of rows remaining in table:", rows, "\n")
        
        #Drop table if there are no more rows in tbl
        if(rows <= 0)
        {
          query = paste0("DROP TABLE ", tbl)
          rtn = RSQLite::dbExecute(con_out, query)
          if(verbose) cat(tbl, "was dropped.", "\n")
        }
        
        if(verbose) cat("Finished processing table:", tbl, "\n", "\n")
      }
    }
  }
  
  #Run vacuum?
  if(vacuum)
  {
    query = "VACUUM"
    RSQLite::dbExecute(con_out, query)
  }
  
  #Disconnect from database
  RSQLite::dbDisconnect(conn = con_out)
  
  invisible()
}

# delete_caseid(fvsout = "C:/FVS/Project_1/FVSOut.db",
#               delete_id = c("ak", "pn", "wc"),
#               id_type = 4)
