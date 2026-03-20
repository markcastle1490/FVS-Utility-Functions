################################################################################
#'load_nvel
#' @name load_nvel 
#' @description
#' This function is used to load the National Volume Estimator dll into an R 
#' session.
#
#' @param nvel_path: 
#' Character string corresponding to file path of NVEL dll.
#' e.g.: "C:/FVS_Utility/vollib/vollib.dll"
#
#' @return 
#' None
################################################################################

#'@export
load_nvel <- function(nvel_path = "C:/FVS_Utility/vollib/vollib.dll")
{
  #Check for nvel_path existence
  if(!file.exists(nvel_path))
    stop(paste("File specified in nvel_path argument not found."))
  
  #If NVEL dll is already loaded it, remove
  if (exists(".NVELLOADEDLIBRARY",envir=.GlobalEnv)) 
  {
    loaded = get(".NVELLOADEDLIBRARY",envir=.GlobalEnv)$ldf
    remove(".NVELLOADEDLIBRARY", envir=.GlobalEnv)
    dyn.unload(loaded)
  }

  #Now attempt to load the dll
  status = try(dyn.load(nvel_path))
    
  if (class(status) == "try-error") 
    stop (paste (nvel_path,"was not loaded successfully."))
  
  #Set name of NVEL dll in global environment
  assign(".NVELLOADEDLIBRARY",
         list("ldf" = nvel_path,
              "pgm" = "vollib"),
         envir=.GlobalEnv)
  
  invisible()
}

################################################################################
#' unload_nvel
#' @name unload_nvel 
#' @description
#' This function is used to unload a NVEL dll that was loaded by the load_nvel
#' function.
#
#' @return 
#' None
################################################################################

#'@export
unload_nvel <- function()
{
  #If NVEL dll is already loaded it, remove
  if (exists(".NVELLOADEDLIBRARY",envir=.GlobalEnv)) 
  {
    loaded = get(".NVELLOADEDLIBRARY",envir=.GlobalEnv)$ldf
    remove(".NVELLOADEDLIBRARY", envir=.GlobalEnv)
    dyn.unload(loaded)
  }
  
  else
  {
    cat("NVEL dll has already been unloaded.")
  }
}

################################################################################
#'call_nvb
#' @name call_nvb
#' @description 
#' This function is used to call vollibnvb_r interface to return volume, 
#' biomass, or carbon values for and individual tree record based on a set of 
#' input attributes. 
#'   
#' NOTE: In order to use this function, one must load the NVEL dll into their 
#' R session. This can be accomplished using the load_nvel function or through
#' other means.
#
#'@param voleq:   
#'Character string corresponding to NVEL equation number. i.e. "NVBM240202"
#
#'@param regn:
#'Integer value corresponding to USFS region. A value of 1 is assumed as the 
#'default value for this argument.
#
#'@param statecd: 
#'Integer code corresponding to state code. This code is used to set the regn
#'value needed for woodland species calculations.
#
#'@param forst:   
#'Character string corresponding to forest number. "01" is assumed as the 
#'default value for this argument.
#
#'@param dist:
#'Character string corresponding to district number. "01" is assumed as the 
#'default value for this argument.
#
#'@param spcd:
#'Integer value corresponding to FIA species code.
#
#'@param sftwd_hrdwd: 
#'Character value indicating whether tree is considered a hardwood or softwood 
#'as defined by FIA. If a value is not provided then sftwd_hrdwd is determined 
#'by spcd.
#'
#'H = hardwood (deciduous)
#'
#'S = softwood (conifer)
#'
#'@param dbh:         
#'Diameter of tree record in inches. This value could be a DBH or DRC 
#'measurement.
#
#'@param ht:
#'Total height of the tree record in feet.
#
#'@param brkht:
#'Height to broken top in feet. If ht and brkht are equal, brkht will get set to
#' 0.
#
#'@param stems:       
#'Number of woodland stems associated with the tree record.
#
#'@param fclass:
#'Girard’s form class.  Diameter at the top of the first log given as a percent 
#'of DBH. This value will contain the value of stems when dealing with woodland
#'species: 62, 63, 65, 66, 69, 106, 133, 134, 143, 321, 322, 475, 803, 810, 814,
#'843.
#
#'@param cr:          
#'Live crown ratio as a percent. A value from 0 - 100 can be entered.
#
#'@param cull:  
#'Numeric value corresponding to percent cull related to rotten or missing 
#'material. A value from 0 - 100 can be entered.
#
#'@param cullmstop:   
#'Numeric value corresponding to CULLMSTOP as defined in FIADB.TREE. A value 
#'from 0 - 100 can be entered.
#
#'@param decaycd:     
#'Integer value corresponding to DECAYCD as defined in FIADB.TREE.
#
#'@param mtopp:
#'Minimum top diameter inside bark for primary product.This value is interpreted
#'as an outside bark diameter when CTYPE = 'I' and inside bark diameter when 
#'CTYPE = 'F'.
#
#'@param mtops:       
#'Minimum top diameter inside bark for secondary product.This value is 
#'interpreted as an outside bark diameter when ctype = 'I' and inside bark 
#'diameter when CTYPE = 'F'.
#
#'@param volbio:      
#'Integer value used to determine if biomass or volume will be returned.
#'
#'0 = volume value will be returned (from VOL array)
#'
#'1 = dry biomass value will be returned (from DRYBIO array)
#'
#'2 = All 15 volumes and biomass values will be returned in a list. argument 
#'index is ignored when volbio is 2.
#'
#'3 = Log volumes will be returned as matrix.
#'
#'4 = Log diameters will be returned as matrix.
#'
#
#'@param index:       
#'Integer value used to determine what value should be returned from VOL or 
#'DRYBIO array as specified in volbio argument. A value of 0, will have the 
#'function return all 15 values for volume or biomass in vector.
#
#'@param ctype:       
#'Character corresponding to Cruise Type: Flag to set some special volume
#'criteria. 
#' 
#' 'C': Cruise volumes. Will return zero volumes if required fields
#' are missing.
#' 
#' 'F': FVS volumes.Missing merchantable heights and form class variables will
#' be calculated if they are required.
#' 
#' 'I': FIA. This flag to let NSVB equation to calculate merchantable volume as
#' the total volume from stump to merchantable height without removing trims.
#' The log volumes will not be calculated
#
#'@param live:        
#'Character indicator corresponding to whether tree is live or dead.
#'
#'"L" = live
#'
#'"D" = dead
#'
#'@param prod:        
#'Character variable pertaining product code.
#'
#'"01" = Sawtimber tree
#'
#'"02" = Pulpwood tree
#'
#'"06" = Roundwood tree.
#
#'@return
#'A single volume or biomass value, a numeric vector with 15 volume or biomass
#'values, a list with two numeric vectors containing all 15 volume and biomass
#'values, a matrix containing log volumes, or a matrix containing log diameters.
################################################################################

#'@export
call_nvb <- function(voleq,
                    regn = 1,
                    statecd = 1,
                    forst = "01",
                    dist = "01",
                    spcd,
                    sftwd_hrdwd = "NA", 
                    dbh,
                    ht,
                    brkht = 0,
                    stems = 1,
                    fclass = 0,
                    cr = 0,
                    cull = 0,
                    cullmstop = 0,
                    decaycd = 0,
                    stump = 1,
                    mtopp = 0,
                    mtops = 0,
                    ctype = "I",
                    live = "L",
                    volbio = 1,
                    index = 1,
                    prod = "01")
{
  
  #Set regn if NA based on state code
  if(is.na(regn) && !is.na(statecd)) 
  {
    if(statecd %in% c(30, 31)) regn <- 1
    else if(statecd %in% 2) regn <- 10
    else if(statecd %in% c(56, 46, 31, 8, 20)) regn <- 2
    else if(statecd %in% c(4, 35)) regn <- 3
    else if(statecd %in% c(16, 49, 32)) regn <- 4
    else if(statecd %in% c(6)) regn <- 5
    else if(statecd %in% c(41, 53)) regn <- 6
    else if(statecd %in% c(48, 40, 5, 22, 28, 1, 47, 
                           21, 12, 13, 45, 37, 51)) regn <- 8
    else regn <- 9
  }
  
  #Handle NA values for essential arguments with no default values. Assume other 
  #hardwood with dbh of 0.1 in and height of 4.5 ft
  if(is.na(voleq) || is.na(spcd) || is.na(dbh) || is.na(ht))
  {
    voleq <- 'NVB0000999'
    spcd <- 999
    dbh <- 0.1
    ht <- 4.5
  }
     
  #Set cull to 0 if NA, less than 0, or greater than 100
  if(is.na(cull) || cull < 0 || cull > 100) cull <- 0
  
  #Set cullmstop to 0 if NA, less than 0, or greater than 100
  if(is.na(cullmstop)|| cullmstop < 0 || cullmstop > 100) cullmstop <- 0
  
  #Set decaycd to 3 if NA
  if(is.na(decaycd)) decaycd <- 3
  
  #if brkht is the same as ht or NA, set to 0
  if(is.na(brkht) || brkht == ht) brkht <- 0
  
  #if cr is NA, less than 0, or greater than 100, set to 0
  if(is.na(cr)|| cr < 0 || cr > 100) cr <- 0
  
  #If volbio return is not 0 - 4, set to 1
  if(! volbio %in% c(0 ,1, 2, 3, 4)) volbio <- 1
  
  #Make sure index is between 0 and 15.
  if(index < 0 || index > 15) index <- 1
  
  #If stems is NA or less than 0 set to 0
  if(is.na(stems) || stems < 0) stems <- 0
  
  #If set ctype to I if not, I, F, or C.
  ctype <- toupper(ctype)
  if(! ctype %in% c("I", "C", "F")) ctype <- "I"
  
  #Set array index based on volbio argument
  if(volbio == 1) array_idx <- 29
  else array_idx <- 18
  
  #If stump is na, set to 1
  if(is.na(stump)) stump <- 1
  
  #Handle sftwd_hrdwd
  sftwd_hrdwd <- toupper(sftwd_hrdwd)
  if(!sftwd_hrdwd %in% c("H", "S")) 
  {
    sftwd_hrdwd <- "H"
    if(spcd < 300) sftwd_hrdwd <- "S"
  }

  #Set mtopp and mtops if 0 (use FIA defaults)
  if(mtops == 0) mtops <- 4
  if(mtopp == 0)
  {
    if(sftwd_hrdwd == "H") mtopp <- 9
    if(sftwd_hrdwd == "S") mtopp <- 7
  }
  
  #Check for valid prod values
  prod <- toupper(prod)
  if(! prod %in% c("01", "02", "03"))
  {
    prod <- "01"
  }
  
  #Set fclass to stems if dealing with woodland species
  if(spcd %in% c(62,  63,  65,  66, 69, 106, 133, 134, 143, 321, 322, 475, 803,
                 810, 814, 843))
    fclass= stems
  
  #Set other required values prior to vollibnvb_r call
  ht1prd=0
  ht2prd=0
  upsht1=0
  upsd1=0
  fclass= fclass
  dbtbh=0
  btr=0
  vol=c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
  logvol=matrix(0,7,20)
  logdia=matrix(0,21,3)
  loglen=c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
  bolht=c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
  tlogs=0
  nologp=0
  nologs=0
  errflag=0
  brkhtd=0
  drybio=c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
  grnbio=c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
  
  #Call vollibnvb_r
  returns = suppressWarnings(.Fortran("vollibnvb_r",
                     as.character(voleq),
                     as.integer(regn),
                     as.character(forst),
                     as.character(dist),
                     as.integer(spcd),
                     as.double(dbh),
                     as.double(ht),
                     as.double(mtopp),
                     as.double(mtops),
                     as.double(ht1prd),
                     as.double(ht2prd),
                     as.double(upsht1),
                     as.double(upsd1),
                     as.double(stump),
                     as.integer(fclass),
                     as.double(dbtbh),
                     as.double(btr),
                     as.double(vol),
                     as.double(logvol),
                     as.double(logdia),
                     as.double(loglen),
                     as.double(bolht),
                     as.integer(tlogs),
                     as.double(nologp),
                     as.double(nologs),
                     as.integer(errflag),
                     as.double(brkht),
                     as.double(brkhtd),
                     as.double(drybio),
                     as.double(grnbio),
                     as.double(cr),
                     as.double(cull),
                     as.integer(decaycd),
                     as.double(cullmstop),
                     as.character(ctype),
                     as.character(live),
                     as.character(prod),
                     PACKAGE = get(".NVELLOADEDLIBRARY",envir=.GlobalEnv)$pgm))
  
  #Return log diameters
  if(volbio == 4)
  {
    value <- list(returns[[20]])
    value <- matrix(value[[1]],
                    nrow = 21,
                    ncol = 3)
    
    return(value)
  }
  
  #Return log volumes
  if(volbio == 3)
  {
    value <- list(returns[[19]])
    value <- matrix(value[[1]],
                    nrow = 7,
                    ncol = 20)
    
    return(value)
  }
  
  #Return all volumes and biomass values if volbio is 2
  if(volbio == 2)
  {
    value <- list(returns[[18]],
                  returns[[29]])
    
    return(value)
  }
  
  #If index is 0, return the entire vector of volume or biomass values
  if(index == 0)
  {
    value <- returns[[array_idx]]
  }
  
  #Otherwise just return single value
  else
  {
    value <- returns[[array_idx]][index]
  }
  
  return(value)
}

################################################################################
#'call_calcdib
#' @name call_calcdib
#' @description 
#' This function is used to call calcdib_r interface to return inside bark top
#' diameter based on input tree information.
#'   
#' NOTE: In order to use this function, one must load the NVEL dll into their 
#' R session. This can be accomplished using the load_nvel function or through
#' other means.
#
#'@param voleq:   
#'Character string corresponding to NVEL equation number. i.e. "NVBM240202"
#
#'@param regn:
#'Integer value corresponding to USFS regn.
#
#'@param forst:   
#'Character string corresponding to forest number.
#'
#'@param dbh:         
#'Diameter of tree record in inches.
#
#'@param ht:
#'Total height of the tree record in feet.
#'
#'@param stemht:         
#'Height to top diameter.
#
#'@param upsht1:
#'Upper stem height in feet where upper stem diameter was measured.
#'
#'OR
#'
#'For Region 8 the upper stem reference height defined by their profile model
#' (ht0, ht4, ht7, ht9).
#' 
#'OR
#' 
#'region 9 the UPSHT1 is the height to 7.6/9.6 top diameter when HT1PRD is not 
#'the height to 7.6/9.6 top diameter.
#'
#'Probably best to leave this as 0 unless you really know what you are doing
#'with it.
#'
#'@param upsd1:
#'Upper stem diameter measured at upsht1.
#'
#'Probably best to leave this as 0 unless you really know what you are doing
#'with it.
#'
#'@return
#'Inside bark top diameter.
################################################################################

#'@export
call_calcdib <- function(voleq,
                         regn = 1,
                         forst = "01",
                         dbh,
                         ht,
                         stemht,
                         upsht1 = 0,
                         upsd1 = 0)
{
  #Set variables
  stemdib = 0
  errflg=0
  
  dib_ = .Fortran("calcdib_r",
                  as.character(voleq),
                  as.integer(regn),
                  as.character(forst),
                  as.double(dbh),
                  as.double(ht),
                  as.double(stemht),
                  as.double(stemdib),
                  as.integer(errflg),
                  as.double(upsht1),
                  as.double(upsd1),
                  PACKAGE = get(".NVELLOADEDLIBRARY",envir=.GlobalEnv)$pgm)[[7]]
           
  return(dib_)
}

################################################################################
#' zero_attr
#' @name zero_attr
#' @description 
#' This function sets an input volume, biomass, or carbon value to zero when a
#' tree has a DBH less than specified merchantable DBH value or if species is a
#' woodland species recognized by FVS.
#
#'@param attr:
#'Numeric value corresponding to volume, biomass, or carbon estimate.
#
#'@param dbh:     
#'Numeric value corresponding to DBH of input tree record.
#
#'@param dbhmin:  
#'Numeric value corresponding to merchantable DBH value.
#'
#'@param spcd:  
#'Integer value corresponding to FIA species code.
#'
#'@return
#'Value of 0, if dbh is less than dbhmin.
################################################################################

#'@export
zero_attr <- function(attr,
                     dbh,
                     dbhmin,
                     spcd)
{
  #Set value to 0 if dbh is less than min dbh
  if(dbh < dbhmin)
  {
    attr <- 0
  }
  
  #Set value to 0 if spcd is FVS woodland species
  if(spcd %in% c(62,  63,  65,  66, 69, 106, 133, 134, 
                 143, 321, 322, 475, 803, 810, 814, 843))
  {
    attr <- 0
  }
  
  return(attr)
}

################################################################################
#fia_merch
#
#This function returns merchantable volume/biomass or sawlog volume/biomass for
#a tree record.
#
#
#dbh:         DBH of tree record in inches.
#
#hrdwd_sftwd: Character string corresponding to whether tree record is hardwood
#             of softwood (e.g. "H" or "S")
#             H: hardwood
#             S: softwood
#
#type:        Integer value corresponding to whether merchantable or sawlog 
#             material will be returned.
#             0: merchantable stem
#             1: sawlog stem
#
#saw:         Merchantable material (ft^3 or lbs) to sawlog top diameter.
#
#topw:        Topwood material (ft^3 or lbs).
#

#A merchantable or sawlog cubic foot volume or biomass
################################################################################

#'@export
fia_merch <- function(dbh,
                     sftwd_hrdwd,
                     type = 0,
                     saw = 0,
                     top = 0)
{
  if(is.na(dbh) || is.na(sftwd_hrdwd)) return(NA)
  
  #Set return material to 0
  material = 0
  
  #Merchantable material
  if(type == 0)
  {
    if(dbh >= 5) material = saw + top
  }
  
  #Sawlog material
  else
  {
    if(sftwd_hrdwd == 'H' && dbh >= 11) material <- saw
    if(sftwd_hrdwd == 'S' && dbh >= 9) material <- saw
  }
  
  return(material)
}
