################################################################################
#'get_vbceq
#'@name get_vbceq
#'@description
#'This function takes in an FIA species code, ecological division code, stand 
#'origin code, and state code and returns a NVEL equation number string.
#
#'@param spcd:     
#'Integer corresponding to FIA species code.
#
#'@param division: 
#'Character string corresponding to ecological division. Valid divisions are 
#'shown below:
#'
#'"120", "130", "210", "220", "230","240", "250", "260", "310", "320",
#'"330", "340", "M120", "M130", "M210", "M220", "M230", "M240", "M260", "M310",
#'"M330", "M340"
#'
#'Value will get set to '0000' if the division is not one of the codes shown
#'below.
#
#'@param stdorgcd:     
#'Integer value corresponding to stand origin code.
#'0: non-plantation
#'1: plantation
#
#'@param statecd:  
#'Integer state code. Refer to Appendix J of FIADB guide.
#
#'@return
#'NVEL equation number as character string
################################################################################

#'@export
get_vbceq <- function(spcd = 999,
                     division = "130",
                     stdorgcd = 0,
                     statecd = 0)
{
  #if stdorgcd is NA, set to 0
  if(is.na(stdorgcd)) stdorgcd <- 0
  
  #If spcd <= 10 or spcd > 999 set to 999. Doesn't need to be done in FORTRAN;
  #just necessary for R function.
  if(spcd <= 10 || spcd > 999) spcd <- 999
  
  #Initialize nvelEq, wdldSp, spIdx
  nvelEq <- "          "
  wdld <- FALSE
  spIdx <- 0
  
  #Vector (array) of ecological divisions
  divs <- c( "120", "130", "210", "220", "230",
             "240", "250", "260", "310", "320",
             "330", "340", "M120", "M130", "M210",
             "M220", "M230", "M240", "M260", "M310",
             "M330", "M340")
  
  #Set division to '0000' if it is not found in divs.This is done already
  #before call to nvbeqdef in FORTRAN. Just necessary for R function.
  if(!division %in% divs) division <- '0000'
  
  #=============================================================================
  #Vector of woodland species that FVS recognizes
  #=============================================================================
  
  woodSp <- c(62,  63,  65,  66,  69, 106, 133, 134, 143, 321,
              322, 475, 803, 810, 814, 843)
  
  #Vector of default woodland equations that FVS recognizes
  woodDef <- c("R03CHO0065", "R03CHO0066", "R03CHO0065", "R03CHO0066", 
               "R03CHO0065", "R03CHO0106", "400DVEW133", "R03CHO0106",
               "R03CHO0106", "200DVEW475", "200DVEW814", "200DVEW475",
               "300DVEW800", "300DVEW800", "200DVEW814", "300DVEW800")
  
  #=============================================================================
  #Start equation selection process
  #
  #1) Check if species is a woodland species. If it is not a woodland species, 
  #   build NVB equation string using DIVISION, SPCD, and STDORGCD.
  #
  #2) If species is a woodland species, select relevant equation string.
  #=============================================================================
  
  #Check if species is a woodland species
  if(spcd %in% woodSp) wdld <- TRUE
  
  #Build NVB equation for non-woodland species
  if(!wdld)
  {
    division <- trimws(sprintf("%04s", division))
    if(nchar(division) < 4) division <- paste0("0", division)
    
    nvelEq <- paste0("NVB",
                     sprintf("%04s", division),
                     sprintf("%03d", spcd))
    
    #Check if species 111 and 131 should have a 'P' added to equation string.
    if(spcd %in% c(111, 131))
    {
      if((division == '0230' || division == '0000') && stdorgcd == 1) nvelEq <- paste0(nvelEq, "P") 
    }
  }
  
  #Get woodland equation
  else
  {
    #If no NVB equation has been found, then check woodland species equations
    spIdx <- match(spcd, woodSp)
    if(is.na(spIdx)) spIdx <- 0
    
    #Make an initial woodland equation selection
    if(spIdx > 0)
    {
      nvelEq <- woodDef[spIdx]
    }
    
    #Check if woodland equation selection should be adjusted based on state
    #California, Oregon, and Washington
    if(statecd %in% c(6, 41, 53))
    {
      if(spcd %in% c(62, 65)) nvelEq <- "400DVEW065"
      if(spcd %in% c(66)) nvelEq <- "200DVEW066"
      if(spcd %in% c(322)) nvelEq <- "200DVEW475"
    }
    
    #Arizona and New Mexico
    if(statecd %in% c(4, 35))
    {
      if(spcd %in% c(322, 814)) nvelEq <- "300DVEW800"
    }
  }
  
  return(nvelEq)
}

################################################################################
#OLD version
################################################################################

# get_vbceq <- function(spcd = 999,
#                      division = "130",
#                      mgmt = 0,
#                      statecd = 0)
# {
#   
#   divIdx <- -1
#   spIdx <- -1
#   spDivIdx <- FALSE
#   
#   #if mgmt is NA, set to 0
#   if(is.na(mgmt)) mgmt <- 0
#   
#   nvelEq <- "          "
#   
#   #Vector (array) of ecological divisions
#   divs <- c("130", "210", "220", "230", "240",
#             "250", "260", "310", "330", "340",
#             "M130", "M210", "M220", "M230", "M240",
#             "M260", "M310", "M330", "M340")
#   
#   #=============================================================================
#   #Data block of species that have species specific coefficients and/or species
#   #specific equation coefficients for a given ecological division.
#   #=============================================================================
#   nsvbSpDiv <- matrix(
#     data = c(10, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 15, 0, 0, 0, 0, 
#              11, 0, 0, 0, 0, 5, 0, 0, 0, 0, 0, 0, 12, 0, 0, 15, 0, 0, 0, 0, 
#              12, 1, 2, 3, 0, 0, 0, 0, 0, 0, 0, 0, 12, 0, 0, 0, 0, 0, 0, 0, 
#              15, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 16, 17, 18, 0, 
#              16, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
#              17, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 18, 0, 
#              18, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
#              19, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 11, 12, 0, 0, 15, 0, 0, 18, 0, 
#              20, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 16, 0, 0, 0, 
#              42, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 15, 0, 0, 0, 0, 
#              43, 0, 0, 0, 4, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
#              68, 0, 0, 3, 4, 0, 6, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
#              71, 1, 2, 0, 0, 0, 0, 0, 0, 0, 0, 11, 12, 0, 0, 0, 0, 0, 18, 0, 
#              73, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 18, 0, 
#              81, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 16, 0, 0, 0, 
#              90, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 11, 12, 0, 0, 0, 0, 0, 18, 0, 
#              91, 0, 2, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
#              93, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 11, 12, 0, 0, 15, 0, 0, 18, 19, 
#              94, 1, 2, 3, 0, 0, 0, 0, 0, 0, 0, 11, 0, 0, 0, 0, 0, 0, 0, 0, 
#              95, 1, 2, 0, 0, 0, 0, 0, 0, 0, 0, 11, 0, 0, 0, 0, 0, 0, 0, 0, 
#              96, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
#              97, 1, 2, 0, 0, 0, 0, 0, 0, 0, 0, 0, 12, 0, 0, 0, 0, 0, 0, 0, 
#              98, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 15, 0, 0, 0, 0, 
#              100, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 12, 0, 0, 0, 0, 0, 0, 0, 
#              101, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
#              105, 1, 2, 3, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
#              107, 0, 0, 0, 4, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
#              108, 1, 0, 0, 0, 0, 0, 0, 0, 9, 0, 11, 12, 0, 0, 15, 0, 0, 18, 0, 
#              110, 0, 0, 3, 4, 0, 6, 0, 0, 0, 0, 0, 0, 13, 14, 0, 0, 0, 0, 0, 
#              111, 0, 0, 0, 4, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
#              113, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
#              115, 0, 0, 0, 4, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
#              116, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
#              117, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 16, 0, 0, 0, 
#              119, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 15, 0, 0, 18, 0, 
#              121, 0, 0, 0, 4, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
#              122, 0, 0, 0, 0, 0, 0, 0, 8, 9, 0, 0, 12, 0, 0, 15, 16, 17, 18, 19, 
#              123, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 13, 0, 0, 0, 0, 0, 0, 
#              125, 1, 2, 3, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
#              126, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 13, 0, 0, 0, 0, 0, 0, 
#              128, 0, 0, 0, 4, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
#              129, 1, 2, 3, 4, 0, 0, 0, 0, 0, 0, 0, 12, 13, 0, 0, 0, 0, 0, 0, 
#              130, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
#              131, 0, 0, 3, 4, 0, 0, 0, 0, 0, 0, 0, 0, 0, 14, 0, 0, 0, 0, 0, 
#              132, 0, 0, 3, 4, 0, 0, 0, 0, 0, 0, 0, 0, 13, 0, 0, 0, 0, 0, 0, 
#              202, 1, 0, 0, 0, 5, 0, 7, 0, 0, 10, 0, 12, 0, 0, 15, 16, 17, 18, 0, 
#              211, 0, 0, 0, 0, 0, 0, 7, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
#              221, 0, 0, 0, 4, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
#              222, 0, 0, 0, 4, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
#              241, 0, 2, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
#              242, 0, 0, 0, 0, 5, 0, 0, 0, 0, 10, 0, 12, 0, 0, 15, 0, 0, 18, 0, 
#              260, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 12, 0, 0, 15, 0, 0, 18, 0, 
#              261, 0, 2, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 13, 0, 0, 0, 0, 0, 0, 
#              263, 0, 0, 0, 0, 5, 0, 0, 0, 0, 10, 0, 12, 0, 0, 15, 0, 0, 18, 0, 
#              264, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 15, 0, 0, 0, 0, 
#              311, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
#              312, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 15, 0, 0, 0, 0, 
#              313, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
#              315, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
#              316, 0, 2, 3, 4, 0, 0, 0, 0, 0, 0, 0, 0, 13, 0, 0, 0, 0, 0, 0, 
#              317, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
#              318, 0, 2, 3, 0, 0, 0, 0, 0, 0, 0, 0, 12, 13, 0, 0, 0, 0, 0, 0, 
#              330, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 13, 0, 0, 0, 0, 0, 0, 
#              351, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 15, 0, 0, 0, 0, 
#              370, 0, 0, 0, 4, 0, 0, 0, 0, 0, 0, 0, 0, 13, 0, 0, 0, 0, 0, 0, 
#              371, 1, 2, 3, 0, 0, 0, 0, 0, 0, 0, 0, 12, 0, 0, 0, 0, 0, 0, 0, 
#              372, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
#              373, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
#              374, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
#              375, 1, 2, 3, 0, 0, 0, 0, 0, 0, 0, 11, 12, 0, 0, 0, 0, 0, 18, 0, 
#              391, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
#              400, 0, 0, 3, 4, 0, 0, 0, 0, 0, 0, 0, 0, 13, 14, 0, 0, 0, 0, 0, 
#              402, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
#              403, 0, 0, 3, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
#              404, 0, 0, 0, 4, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
#              405, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
#              407, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
#              409, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
#              421, 0, 0, 3, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
#              460, 0, 0, 0, 4, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
#              461, 0, 0, 0, 4, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
#              462, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
#              471, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
#              491, 0, 0, 0, 4, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
#              521, 0, 0, 0, 4, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
#              531, 0, 2, 3, 4, 0, 0, 0, 0, 0, 0, 0, 12, 13, 0, 0, 0, 0, 0, 0, 
#              540, 0, 0, 0, 4, 0, 0, 0, 0, 0, 0, 0, 0, 13, 0, 0, 0, 0, 0, 0, 
#              541, 0, 2, 3, 0, 0, 0, 0, 0, 0, 0, 0, 12, 13, 0, 0, 0, 0, 0, 0, 
#              543, 0, 2, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
#              544, 0, 0, 0, 4, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
#              552, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
#              555, 0, 0, 0, 4, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
#              591, 0, 0, 0, 4, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
#              601, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
#              602, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 13, 0, 0, 0, 0, 0, 0, 
#              611, 0, 0, 0, 4, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
#              621, 0, 0, 3, 4, 0, 0, 0, 0, 0, 0, 0, 0, 13, 0, 0, 0, 0, 0, 0, 
#              651, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
#              652, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
#              653, 0, 0, 0, 4, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
#              680, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
#              691, 0, 0, 0, 4, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
#              693, 0, 0, 3, 4, 0, 0, 0, 0, 0, 0, 0, 0, 13, 0, 0, 0, 0, 0, 0, 
#              694, 0, 0, 0, 4, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
#              701, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
#              711, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
#              731, 0, 0, 0, 4, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
#              740, 1, 0, 0, 4, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 18, 0, 
#              741, 1, 2, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
#              742, 0, 0, 0, 0, 0, 6, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
#              743, 1, 2, 3, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
#              746, 1, 2, 3, 0, 0, 0, 0, 0, 0, 0, 11, 12, 0, 0, 15, 0, 0, 18, 0, 
#              747, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 11, 12, 0, 0, 0, 0, 0, 18, 0, 
#              762, 0, 2, 0, 4, 0, 0, 0, 0, 0, 0, 0, 0, 13, 0, 0, 0, 0, 0, 0, 
#              800, 0, 0, 0, 4, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
#              802, 0, 2, 3, 4, 0, 6, 0, 0, 0, 0, 0, 0, 13, 14, 0, 0, 0, 0, 0, 
#              804, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
#              806, 0, 0, 3, 4, 0, 0, 0, 0, 0, 0, 0, 0, 13, 0, 0, 0, 0, 0, 0, 
#              809, 0, 0, 3, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
#              812, 0, 0, 0, 4, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
#              813, 0, 0, 0, 4, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
#              817, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
#              819, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
#              820, 0, 0, 0, 4, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
#              822, 0, 0, 0, 4, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
#              823, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
#              824, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
#              825, 0, 0, 0, 4, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
#              826, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
#              827, 0, 0, 0, 4, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
#              828, 0, 0, 0, 4, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
#              830, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
#              831, 0, 0, 0, 4, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
#              832, 0, 0, 3, 4, 0, 0, 0, 0, 0, 0, 0, 0, 13, 0, 0, 0, 0, 0, 0, 
#              833, 0, 2, 3, 4, 0, 0, 0, 0, 0, 0, 0, 0, 13, 0, 0, 0, 0, 0, 0, 
#              834, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
#              835, 0, 0, 3, 4, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
#              837, 0, 2, 3, 4, 0, 6, 0, 0, 0, 0, 0, 12, 13, 0, 0, 0, 0, 0, 0, 
#              838, 0, 0, 0, 4, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
#              840, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
#              842, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
#              901, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 13, 0, 0, 0, 0, 0, 0, 
#              920, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
#              922, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
#              931, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
#              950, 0, 2, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 13, 0, 0, 0, 0, 0, 0, 
#              951, 0, 2, 3, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
#              970, 0, 0, 0, 4, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
#              971, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
#              972, 0, 2, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
#              975, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
#              999, 0, 0, 0, 4, 0, 0, 0, 0, 0, 0, 0, 0, 13, 0, 0, 0, 0, 0, 0),
#     nrow = 152,
#     ncol = 20,
#     byrow = T)
#   
#   #=============================================================================
#   #Data block that contains species which use NSVB system of equations but do 
#   #NOT have species specific coefficients. They use coefficients based on
#   #Jenkins species groups.
#   #=============================================================================
#   
#   jenkSp <- matrix(
#     data = c(14,3, 
#              16,3, 
#              21,3, 
#              22,3, 
#              40,1, 
#              41,1, 
#              50,1, 
#              51,1, 
#              52,1, 
#              53,1, 
#              54,1, 
#              55,1, 
#              56,1, 
#              57,1, 
#              64,1, 
#              67,1, 
#              70,1, 
#              72,1, 
#              74,1, 
#              75,1, 
#              92,5, 
#              102,4, 
#              103,4, 
#              104,4, 
#              109,4, 
#              112,4, 
#              114,4, 
#              118,4, 
#              120,4, 
#              124,4, 
#              127,4, 
#              135,4, 
#              136,4, 
#              137,4, 
#              139,4, 
#              142,4, 
#              144,4, 
#              200,2, 
#              201,2, 
#              212,1, 
#              220,1, 
#              223,1, 
#              230,3, 
#              231,3, 
#              232,3, 
#              240,1, 
#              250,3, 
#              251,3, 
#              252,3, 
#              262,3, 
#              299,4, 
#              310,7, 
#              314,9, 
#              319,7, 
#              320,7, 
#              323,8, 
#              324,8, 
#              325,8, 
#              331,8, 
#              332,8, 
#              333,8, 
#              334,8, 
#              335,8, 
#              336,8, 
#              337,8, 
#              341,8, 
#              345,8, 
#              350,6, 
#              352,6, 
#              353,6, 
#              355,6, 
#              356,8, 
#              357,8, 
#              358,8, 
#              360,8, 
#              361,8, 
#              362,8, 
#              367,8, 
#              369,8, 
#              376,7, 
#              377,7, 
#              378,7, 
#              379,7, 
#              381,8, 
#              390,8, 
#              401,9, 
#              406,9, 
#              408,9, 
#              410,9, 
#              411,9, 
#              412,9, 
#              413,9, 
#              420,8, 
#              422,8, 
#              423,8, 
#              424,8, 
#              431,8, 
#              450,8, 
#              451,8, 
#              452,8, 
#              463,8, 
#              481,8, 
#              490,8, 
#              492,8, 
#              500,8, 
#              501,8, 
#              502,8, 
#              503,8, 
#              504,8, 
#              505,8, 
#              506,8, 
#              507,8, 
#              508,8, 
#              509,8, 
#              510,8, 
#              511,8, 
#              512,8, 
#              513,8, 
#              514,8, 
#              520,8, 
#              539,8, 
#              542,8, 
#              545,8, 
#              546,8, 
#              547,8, 
#              548,8, 
#              549,8, 
#              550,8, 
#              551,8, 
#              561,8, 
#              571,8, 
#              580,8, 
#              581,8, 
#              582,8, 
#              583,8, 
#              600,8, 
#              603,8, 
#              604,8, 
#              605,8, 
#              606,8, 
#              631,8, 
#              641,8, 
#              650,8, 
#              654,8, 
#              655,8, 
#              657,8, 
#              658,8, 
#              660,8, 
#              661,8, 
#              662,8, 
#              663,8, 
#              664,8, 
#              681,8, 
#              682,8, 
#              683,8, 
#              684,8, 
#              690,8, 
#              692,8, 
#              712,8, 
#              715,8, 
#              718,8, 
#              720,8, 
#              721,8, 
#              722,8, 
#              729,8, 
#              730,8, 
#              732,8, 
#              744,6, 
#              745,6, 
#              748,6, 
#              749,6, 
#              752,6, 
#              753,6, 
#              759,8, 
#              760,8, 
#              761,8, 
#              763,8, 
#              764,8, 
#              765,8, 
#              766,8, 
#              767,8, 
#              768,8, 
#              769,8, 
#              770,8, 
#              771,8, 
#              772,8, 
#              773,8, 
#              774,8, 
#              801,9, 
#              805,9, 
#              807,9, 
#              808,9, 
#              811,9, 
#              815,9, 
#              816,9, 
#              818,9, 
#              821,9, 
#              836,9, 
#              839,9, 
#              841,9, 
#              844,9, 
#              845,9, 
#              851,9, 
#              852,8, 
#              853,8, 
#              854,8, 
#              855,8, 
#              856,8, 
#              857,8, 
#              858,8, 
#              859,8, 
#              860,8, 
#              863,8, 
#              864,8, 
#              865,8, 
#              866,8, 
#              868,8, 
#              869,8, 
#              870,8, 
#              873,8, 
#              874,8, 
#              876,8, 
#              877,8, 
#              882,8, 
#              883,8, 
#              884,8, 
#              885,8, 
#              886,8, 
#              887,8, 
#              888,8, 
#              890,8, 
#              891,8, 
#              895,8, 
#              896,8, 
#              897,8, 
#              906,8, 
#              907,8, 
#              908,8, 
#              909,8, 
#              911,8, 
#              912,8, 
#              913,8, 
#              914,8, 
#              915,8, 
#              919,8, 
#              921,6, 
#              923,6, 
#              924,6, 
#              925,6, 
#              926,6, 
#              927,6, 
#              928,6, 
#              929,6, 
#              934,8, 
#              935,8, 
#              936,8, 
#              937,8, 
#              940,8, 
#              952,8, 
#              953,8, 
#              973,8, 
#              974,8, 
#              976,8, 
#              977,8, 
#              981,8, 
#              982,8, 
#              986,8, 
#              987,8, 
#              988,8, 
#              989,8, 
#              991,8, 
#              992,8, 
#              993,8, 
#              994,8, 
#              995,8, 
#              996,8, 
#              997,8, 
#              998,8),
#     nrow = 278,
#     ncol = 2,
#     byrow = T)
#   
#   #=============================================================================
#   #Vector of woodland species that FVS recognizes
#   #=============================================================================
#   
#   woodSp <- c(62,  63,  65,  66,  69, 106, 133, 134, 143, 321,
#               322, 475, 803, 810, 814, 843)
#   
#   #Vector of default woodland equations that FVS recognizes
#   woodDef <- c("R03CHO0065", "R03CHO0066", "R03CHO0065", "R03CHO0066", 
#                "R03CHO0065", "R03CHO0106", "400DVEW133", "R03CHO0106",
#                "R03CHO0106", "200DVEW475", "200DVEW814", "200DVEW475",
#                "300DVEW800", "300DVEW800", "200DVEW814", "300DVEW800")
#   
#   #=============================================================================
#   #Start equation selection process
#   #
#   #3 steps process
#   #
#   #1) Determine if species has species and/or ecodivsion specific coefficients
#   #   defined by NSVB publication. If species meets these criteria then build 
#   #   equation string. If species is 111 or 131, check value of management code
#   #   to determine if plantation coefficients should be assumed. Build equation
#   #   and return.
#   #   
#   #2) Determine if species has jenkins group coefficients defined by NSVB
#   #   publication. If so, build the equation string with jenkins group and 
#   #   return. 
#   #
#   #3) Determine if species is a woodland species. If so, select a predefined
#   #   equation string for the given woodland species. Check if equation string 
#   #   should be adjusted by state code and then return.
#   #=============================================================================
#   
#   #Search for a valid division
#   idx <- match(division, 
#                divs)
#   
#   #If divIdx is not NA, set divIdx
#   if(!is.na(idx)) divIdx <- idx
#   
#   #Check if species is in nsvbSpDiv
#   # idx <- match(spcd, nsvbSpDiv[, 1])
#   # if(!is.na(idx)) spIdx <- idx
#   
#   idx <- bin_search(nsvbSpDiv[, 1], spcd)
#   if(idx > 0) spIdx <- idx
#   
#   #If species is in nsvbSpDiv, check to see if it has coefficients for a given
#   #ecological division. 
#   if(spIdx > 0 && divIdx > 0)
#   {
#     idx <- match(divIdx, nsvbSpDiv[spIdx, 2:20])
#     if(!is.na(idx)) spDivIdx <- TRUE
#   }
#   
#   #If division exists and there is a species-division match, build equation 
#   #string that includes species and division
#   if(spIdx > 0 && spDivIdx)
#   {
#     division <- trimws(sprintf("%04s", division))
#     if(nchar(division) < 4) division <- paste0("0", division)
#     
#     nvelEq <- paste0("NVB",
#                      sprintf("%04s", division),
#                      sprintf("%03d", spcd))
#     
#     #Check if species with management specific equations are being processed
#     if(spcd %in% c(111, 131) && division == "0230" && mgmt == 1) 
#       nvelEq <- paste0(nvelEq, "P") 
#     
#     return(nvelEq)
#   }
#   
#   #If we found a valid nsvb species but don't have division match, create
#   #equation string that only contains species numbers.
#   if(spIdx > 0)
#   {
#     nvelEq <- paste0("NVB",
#                      "0000",
#                      sprintf("%03d", spcd))
#     
#     #Check if species with management specific equations are being processed
#     if(spcd %in% c(111, 131) && mgmt == 1) 
#       nvelEq <- paste0(nvelEq, "P") 
#     
#     return(nvelEq)
#   }
#   
#   #If no NVB equation has been found, look for NVB equation based upon jenkins
#   #group.
#   # idx <- match(spcd, jenkSp[, 1])
#   # if(!is.na(idx)) spIdx <- idx
#   
#   idx <- bin_search(jenkSp[, 1], spcd)
#   if(idx > 0) spIdx <- idx
#   
#   #If we found a species, build NVB equation based on jenkins group
#   if(spIdx > 0)
#   {
#     jenk <- jenkSp[spIdx, 2]
#     
#     nvelEq <- paste0("NVB",
#                      "0000",
#                      sprintf("%03d", jenk))
#     return(nvelEq)
#   }
#   
#   #If no NVB equation has been found, then check woodland species equations
#   # idx <- match(spcd, woodSp)
#   # if(!is.na(idx)) spIdx <- idx
#   
#   idx <- bin_search(woodSp, spcd)
#   if(idx > 0) spIdx <- idx
#   
#   #Make an initial woodland equation selection
#   if(spIdx > 0)
#   {
#     nvelEq <- woodDef[spIdx]
#   }
#   
#   #Check if woodland equation selection should be adjusted based on state
#   #California, Oregon, and Washington
#   if(statecd %in% c(6, 41, 53))
#   {
#     if(spcd %in% c(62, 65)) nvelEq <- "400DVEW065"
#     if(spcd %in% c(66)) nvelEq <- "200DVEW066"
#     if(spcd %in% c(322)) nvelEq <- "200DVEW475"
#   }
#   
#   #Arizona and New Mexico
#   else if(statecd %in% c(4, 35))
#   {
#     if(spcd %in% c(322, 814)) nvelEq <- "300DVEW800"
#   }
#   
#   #Assume the default equation string selection
#   else
#   {
#     nvelEq <- nvelEq
#   }
#   
#   return(nvelEq)
# }

################################################################################
#'get_ecoregion
#'@name get_ecoregion
#'@description
#'This function takes in an ecological subsection and returns an ecological 
#'province or division
#'
#'@param type:     
#'Integer variable corresponding to whether a ecoprovince or ecodivision should
#'be returned
#'0 = ecoprovince
#'1 = ecodivision
#
#'@param ecosubcd: 
#'Character string corresponding to ecological subsection.
#'
#'@return
#'Ecological division or province as character string.
################################################################################

#'@export
get_ecoregion <- function(ecosubcd = "",
                         type = 1)
{
  #If ecosubcd is NA return
  if(is.na(ecosubcd)) return(NA)
  
  #Catch bad type values
  if(type < 0 || type > 1) type <- 1
  
  #Make sure ecosubcd is uppercase
  ecosubcd <- toupper(ecosubcd)
  
  #Remove blank spaces in ecosubcd
  ecosubcd <- trimws(ecosubcd)
  
  #Get first 4 characters if ecosubcd starts with 'M' otherwise get the first 3
  if(substring(ecosubcd, 1, 1) == 'M') 
  {
    eco <- substring(ecosubcd, 1, 4)
  }
  
  else 
  {
    eco <- substring(ecosubcd, 1, 3)
  }
  
  #Set last character to 0, if type is 1 (ecodivision)
  if(type == 1)
  {
    eco <- paste0(substring(eco, 1, nchar(eco) - 1),
                  "0")
  }
  
  return(eco)
}
