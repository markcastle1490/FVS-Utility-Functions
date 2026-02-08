################################################################################
#'jenkins_bio
#'@name jenkins_bio
#'@description
#'
#'This takes in a FIA species code and DBH value of tree record and calculates 
#'the following pools of biomass:
#'
#'Aboveground biomass
#'
#'Stem wood biomass
#'
#'Bark biomass
#'
#'Foliage biomass
#'
#'Root biomass
#'
#'@param spcd: 
#'Numeric FIA species code.
#'
#'@param dbh: 
#'DBH in inches of tree record. By default, this argument is set to 1.
#'
#'@return
#'Numeric vector of biomass values for the tree record.
################################################################################

#'@export
jenkins_bio <- function(spcd,
                        dbh = 1)
{
  #b0 coefficients
  b0 = c(-2.0336, -2.2304, -2.5384, -2.5356, -2.0773,
         -2.2094, -1.9123, -2.4800, -2.0127, -0.7152)
  
  #b1 coefficients
  b1 = c(2.2592,  2.4435,  2.4814,  2.4349,  2.3323,
         2.3867,  2.3651,  2.4835,  2.4342,  1.7029)
  
  #Initialize vector with that will store biomass results
  bio <- vector(mode = "numeric",
                length = 5)
  
  names(bio) <- c("ABT", "WOOD", "BARK", "FOL", "ROOT")
  
  #If spcd missing, return bio
  if(missing(spcd)) return(bio)
  
  #if dbh is missing, set to 1
  if(missing(dbh)) dbh <- 1
  
  #Find jenkins group
  jenkgrp <- spConvert(sp = spcd,
                       from = 1,
                       to = 8)
  
  #If Jenkgrp is not value from 1 - 10, return bio
  if(! jenkgrp %in% 1:10) return(bio)
  
  #Kilogram to pound conversion
  kg2lb <- 2.20462
  
  #Store dbh in inches
  dbhin <- dbh
  
  #Calculate dbh in cm
  dbhcm <- dbh * 2.54
  
  #Calculate above ground biomass
  abt <- exp(b0[jenkgrp] + b1[jenkgrp] * log(dbhcm))
  
  #Calculate jenkins ratios
  ratios <- jenkins_ratios(spcd = spcd,
                           dbh = dbhin)
  
  #Calculate foliage and root biomass
  fol <- abt * ratios[1]
  root <- abt * ratios[2]
  
  #Calculate wood and bark biomass
  if(dbh < 5)
  {
    bark <- 0
    wood <- 0
  }
  else
  {
    bark = abt * ratios[3]
    wood = abt * ratios[4]
  }
  
  #Store biomass variables and return
  bio[1] <- abt * kg2lb
  bio[2] <- wood * kg2lb
  bio[3] <- bark * kg2lb
  bio[4] <- fol * kg2lb
  bio[5] <- root * kg2lb
  
  return(bio)
}

################################################################################
#'jenkins_ratios
#'@name jenkins_ratios
#'@description
#'
#'This takes in a FIA species code and dbh value and calculates the following
#'Jenkins component ratios:
#'
#'Foliage ratio
#'
#'Roots ratio
#'
#'Bark ratio
#'
#'Wood ratio
#'
#'@param spcd: 
#'Numeric FIA species code.
#'
#'@param dbh: 
#'DBH in inches of tree record. By default, this argument is set to 1.
#'
#'@return
#'Numeric vector of Jenkins component ratios.
################################################################################

#'@export
jenkins_ratios <- function(spcd,
                           dbh = 1)
{
  #Initialize vector with that will Jenkins component ratios
  ratios <- vector(mode = "numeric",
                  length = 4)
  
  names(ratios) <- c("FOLRATIO", "RORATIO", "BKRATIO", "WDRATIO")
  
  #If spcd missing, return bio
  if(missing(spcd)) return(ratios)
  
  #if dbh is missing, set to 1
  if(missing(dbh)) dbh <- 1
  
  #Get hardwood/softwood code based on spcd
  spcls <- spConvert(sp = spcd,
                     from = 1,
                     to = 6)
  
  #if value is not S or H, return ratios
  if(! spcls %in% c('H', 'S')) return(ratios)
  
  #Determine coefficients
  #Softwood
  if(spcls == 'S')
  {
    a0f = -2.9584
    a1f =  4.4766
    a0r = -1.5619
    a1r =  0.6614
    a0b = -2.0980
    a1b = -1.1432
    a0w = -0.3737
    a1w = -1.8055
  }
  
  #Hardwood
  else
  {
    a0f = -4.0813
    a1f =  5.8816
    a0r = -1.6911
    a1r =  0.8160
    a0b = -2.0129
    a1b = -1.6805
    a0w = -0.3065
    a1w = -5.4240
  }
  
  #Calculate dbh in cm
  dbhcm <- dbh * 2.54

  #Calculate ratios
  folratio = exp(a0f + a1f/dbhcm)
  roratio = exp(a0r + a1r/dbhcm) 
  bkratio = exp(a0b + a1b/dbhcm)
  wdratio = exp(a0w + a1w/dbhcm)   
  
  #Store and return ratios
  ratios[1] <- folratio
  ratios[2] <- roratio
  ratios[3] <- bkratio
  ratios[4] <- wdratio
  
  return(ratios)
}
