################################################################################
#'run_key
#'@name run_key
#'@description
#'This function is used to run a single FVS keyword file (.key) with a specified
#'FVS variant dll (.dll) using the rFVS API.
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
#'@return 
#'FVS return code returned invisibly
################################################################################

#'@export
run_key = function(dll_path = "C:/FVS/FVSSoftware/FVSbin",
                   var_code = "ie",
                   keyfile = "C:/FVS.key",
                   verbose = FALSE)
{
  #Store the current working directory
  orig_dir = getwd()
  
  #Set working directory on exit
  on.exit(expr = setwd(orig_dir),
          add = TRUE)
  
  #Change \\ to / in dll_path argument
  dll_path = chartr("\\", "/", dll_path)
  
  #Change \\ to / in keyfile argument
  keyfile = chartr("\\", "/", keyfile)
  
  #Make var_code lowercase
  var_code = tolower(var_code)
  
  #Get file ext on keyfile
  ext = tools::file_ext(keyfile)
  
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
  
  #Check file extension
  if (ext != "key"){
    stop(paste("Invalid file extension specified in keyfile argument."))
  }
  
  #Check for valid variant
  if (! toupper(var_code) %in% fvs_get_variants()){
    stop(paste("Invalid FVS variant specified in var_code."))
  }
  
  #Create dll name
  var_code = paste0("FVS", var_code)
  
  #Grab the the directory where the keyword file is stored
  keydir = gsub("/[^/]+$", "", keyfile)
  
  #Get the name of the keyword file
  keyfile_ = sub(".*/", "", keyfile)
  
  #Set working directory for the simulation
  if(keydir != keyfile_)
    setwd(keydir)
  
  #Print inputs
  if(verbose)
  {
    cat("dll_path:", dll_path, "\n")
    cat("fvsdll:",  var_code, "\n")
    cat("keyfile:", keyfile, "\n")
  }
  
  #Call run_fvs
  ret_code = run_fvs(dll_path = dll_path,
                     var_code = var_code,
                     keyfile = keyfile_)
  
  #Print return code
  if(verbose) cat("FVS return code:", ret_code, "\n", "\n")
  
  invisible(ret_code)
}

################################################################################
#'run_key_rscript
#'@name run_key_rscript
#'@description
#'This function is used to write a .R script that contains the code to run a 
#'single keyword file using the run_key function.
#
#'@param script_path: 
#'Optional character string corresponding to file path where R script will be 
#'written. If this value is left as NULL, the script will be stored in the 
#'same directory as the value in keyfile argument and will have the same name 
#'as the keyword file.
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

run_key_rscript(keyfile = "FIAVBC_IE_COMP.key")

#'@export
run_key_rscript = function(script_path = NULL,
                           dll_path = "C:/FVS/FVSSoftware/FVSbin",
                           var_code = "ie",
                           keyfile = "C:/FVS/FVS.key",
                           verbose = FALSE)
{
  
  #Change \\ to / in dll_path argument
  dll_path = chartr("\\", "/", dll_path)
  
  #Change \\ to / in keyfile argument
  keyfile = chartr("\\", "/", keyfile)
  
  #Make var_code lowercase
  var_code = tolower(var_code)
  
  #Get file ext on keyfile
  ext = tools::file_ext(keyfile)
  
  #Generate script_path if NULL
  if(is.null(script_path))
    script_path = gsub(".key$", ".R", keyfile)
    
  #If script_path exists already,  delete it
  if(file.exists(script_path)) unlink(script_path)
  
  #Open file connection
  con = file(description = script_path, open = "a")
  on.exit(try(if(isOpen(con)) close(con = con), silent = TRUE))
  
  #Write the code in R script
  writeLines(text = "library(fvsUtil)", con = con, sep = "\n\n")
  
  writeLines(text = paste("run_key(dll_path =", 
                          paste0("'", dll_path, "'", ",")),
             con = con,
             sep ="\n")
  
  writeLines(paste("var_code =", paste0("'", var_code, "'", ",")),
             con = con,
             sep = "\n")
  
  writeLines(paste("keyfile =", paste0("'", keyfile, "'", ",")),
             con = con,
             sep = "\n")
  
  writeLines(paste("verbose = ", paste0(verbose, ")")),
             con = con,
             sep = "\n")
  
  #Close connection
  close(con = con)
  
  invisible()
}

################################################################################
#'run_key_callr
#'@name run_key_callr
#'@description
#'This function is used to run a single FVS keyword file (.key) with a specified
#'FVS variant dll (.dll) using the rFVS API in a separate R session with the 
#'callr R package. This function will produce the same results as run_key but
#'can be helpful in reducing memory usage and preventing potential out of memory
#'errors when running many large FVS simulations in sequence.
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
#'@return
#'FVS return code returned invisibly.
################################################################################

#'@export
run_key_callr = function(dll_path = "C:/FVS/FVSSoftware/FVSbin", 
                         var_code = "ie",
                         keyfile = "C:/FVS.key",
                         verbose = FALSE)
{
  #Store the current working directory
  orig_dir = getwd()
  
  #Set working directory on exit
  on.exit(expr = setwd(orig_dir),
          add = TRUE)
  
  #Change \\ to / in dll_path argument
  dll_path = chartr("\\", "/", dll_path)
  
  #Change \\ to / in keyfile argument
  keyfile = chartr("\\", "/", keyfile)
  
  #Get file ext on keyfile
  ext = tools::file_ext(keyfile)
  
  #Make var_code lowercase
  var_code = tolower(var_code)
  
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
  
  #Check file extension
  if (ext != "key"){
    stop(paste("Invalid file extension specified in keyfile argument."))
  }
  
  #Check for valid variant
  if (! toupper(var_code) %in% fvs_get_variants()){
    stop(paste("Invalid FVS variant specified in var_code."))
  }
  
  #Create dll name
  var_code = paste0("FVS", var_code)
  
  #Grab the the directory where the keyword file is stored
  keydir = gsub("/[^/]+$", "", keyfile)
  
  #Get the name of the keyword file
  keyfile_ = sub(".*/", "", keyfile)
  
  #Set working directory for the simulation
  if(keydir != keyfile_)
    setwd(keydir)
  
  #Print inputs
  if(verbose)
  {
    cat("dll_path:", dll_path, "\n")
    cat("fvsdll:",  var_code, "\n")
    cat("keyfile:", keyfile, "\n")
  }
  
  #Call run_fvs with callr
  ret_code = callr::r(func = function(dll_path, var_code, keyfile, verbose) {
    fvsUtil:::run_fvs(dll_path = dll_path,
                     var_code = var_code,
                     keyfile = keyfile)},
    args = list(dll_path = dll_path,
                var_code = var_code,
                keyfile = keyfile_))
  
  #Print return code
  if(verbose) cat("FVS return code:", ret_code, "\n", "\n")
  
  invisible(ret_code)
}

################################################################################
#'run_fvs
#'@name run_fvs
#'@description
#'This function is called from run_key and run_key_callr and is used to fulfill
#'a simulation using input arguments corresponding to path where FVS variant 
#'dlls are stored, a variant code, and keyword file name.
#
#'@param dll_path: 
#'Character string corresponding to directory where FVS dlls are stored
#
#'@param var_code: 
#'Character string 'FVS' appended to a lowercase variant code (e.g. 'FVSca').
#
#'@param keyfile: 
#'Character string corresponding to keyword file name.
#
#'@return FVS return code invisibly returned
################################################################################

run_fvs = function(dll_path = "C:/FVS/FVSSoftware/FVSbin",
                   var_code = "FVSie",
                   keyfile = "C:/FVS.key")
  
{
  #Load variant dll and specify directory path to dll in bin folder
  rFVS::fvsLoad(var_code, bin = dll_path)
  
  #Unload the dll on exit
  on.exit(expr = {
    
    #Code used in fvsLoad rFVS function.
    if (exists(".FVSLOADEDLIBRARY", envir=.GlobalEnv)) 
    {
      loaded = get(".FVSLOADEDLIBRARY", envir=.GlobalEnv)$ldf
      dyn.unload(loaded)
      remove(".FVSLOADEDLIBRARY",envir=.GlobalEnv)
    }},
    add = TRUE)
  
  #Set keyword file to command line
  rFVS::fvsSetCmdLine(paste0("--keywordfile=", keyfile))
  
  #Initialize return code
  retcode = 0
  
  #Keep running FVS until a return code other than 0, is returned.
  while(retcode == 0) retcode = rFVS::fvsRun()
  
  invisible(retcode)
}

################################################################################
#'delete_caseid
#'@name delete_caseid
#'@description
#'
#'This function deletes desired rows in all database tables containing a CaseID 
#'field from a specified output FVS database. The rows that are deleted are 
#'determined from a set of CaseID values associated with any of the following
#'attributes: FVS run title, management ID, FVS variant code, keyword file name
#'set of stand ID(s), set of stand CN(s), or a set of CaseID values. The case 
#'IDs identified for deletion are extracted from the FVS_Cases table from the 
#'output FVS database. Some of the logic in this function was borrowed from Nick
#'Crookston's deleteRelatedDBRows function from fvsRunUtilities.R file (fvsOL R
#' package).
#
#'@param fvsout: 
#'Character string corresponding to file path to output FVS SQLite database
#'(.db, .sqlite).
#
#'@param delete_id: 
#'Vector of character strings pertaining to FVS run title(s), management ID(s), 
#'FVS variant code(s), keyword file name(s), stand ID values, stand CN values,
#'or Case ID values.
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
#'
#'5 = Stand ID values (e.g. StandID in FVS_Cases table)
#'
#'6 = Stand CN values (e.g. Stand_CN in FVS_Cases table)
#'
#'7 = Case ID values (e.g. CaseID in FVS_Cases table)
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
  file_ext = tools::file_ext(fvsout)
  
  #Test if output file extension is valid database
  if(!file_ext %in% c("db", "sqlite"))
  {
    stop("Invalid database file type provided in fvsout.")
  }
  
  #Stop with error if delete_id is NULL
  if(is.null(delete_id)) 
    stop("No value(s) specified for delete_id.")
  
  #Set id_type to 1 if bad value is entered
  if(!id_type %in% c(1:7)) id_type = 1
  
  #Set select_var based on id_type
  select_var = switch(id_type,
                      'KeywordFile',
                      'RunTitle',
                      'MgmtID',
                      'Variant',
                      'StandID',
                      'Stand_CN',
                      'CaseID')
  
  #Connect to output database
  con_out = RSQLite::dbConnect(RSQLite::SQLite(), fvsout)
  on.exit(try(if(RSQLite::dbIsValid(con_out)) RSQLite::dbDisconnect(con_out), 
              silent = TRUE))
  
  #Get output database tables
  out_tbl = RSQLite::dbListTables(conn = con_out)
  
  #Test if FVS_Cases table exists. If it doesn't, disconnect from database and
  #stop with error.
  if(!"FVS_Cases" %in% out_tbl)
  {
    RSQLite::dbDisconnect(conn = con_out)
    stop("FVS_Cases table was not found in output database.")
  }
  
  #Collapse delete_id into single string
  id_string = collect_id(delete_id)
  
  if(verbose)
  {
    cat("id_type:", id_type, "\n")
    cat("Variable used for case ID selection:", select_var, "\n")
    cat("Value for case ID deletion:", id_string, "\n")
  }
    
  #CaseID query
  query = paste("SELECT CaseID FROM FVS_Cases WHERE",
                select_var,
                "COLLATE NOCASE",
                "IN", 
                id_string)
    
  #Attempt to get CaseIDs...
  caseid = RSQLite::dbGetQuery(conn = con_out,
                               query)[[1]]
  
  if(verbose)
  {
    cat("CaseID Query:", query, "\n")
    cat("Number of Case ID values selected:", length(caseid), "\n", "\n")
  }
    
  #No case ID values found
  if(length(caseid) <= 0)
    cat("No Case ID values found for deletion using value(s) specified for:",
        select_var, "\n", "\n")
  
  #Delete identified CaseID values
  else
  {
    #Collect caseid in single string 
    case_string = collect_id(caseid)
    
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
