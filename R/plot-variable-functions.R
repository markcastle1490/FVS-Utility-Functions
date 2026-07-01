################################################################################
#This file contains a suite of functions that can be used to derive competition
#and density attributes. The general expectation when using these functions is 
#that the user is working with a tree-level dataset that contains attributes 
#including DBH, expansion factors, species, total tree height, and others 
#relevant as described in the documentation for each function. The attributes 
#are generally calculated from input numeric and character vectors and other 
#additional arguments. Most of the attributes in this file can be calculated for
#custom size ranges (DBH and total tree height) and desired species.
#
#Development notes: 
#The functions in this file rely heavily on vectorized base-R functions (sum, 
#mean, weighted.mean, etc.) to derive competition and density attributes. The 
#vectorized functions are optimized for R and are preferable to using for loops. 
#Faster and more memory efficient implementations of the functions in this file
#could likely be achieved by writing the functions in compiled languages like C,
#C++, FORTRAN, etc. and calling them through an API. The primary gains in speed
#from coding in a low level language would likely be realized from not having to
#create intermediate vectors or copy vectors during the calculation of the 
#attributes. Coding in a lower level language would still provide some unique
#challenges.
#
#Usage notes:
#Although these functions can be called within loops for subsets of data, they 
#are best used in dplyr or data.table calculation sequences. Below are examples
#of how one could calculate the following variables in a dplyr and data.table
#summarize statement for each stand and year in an input dataframe:
#
#Variables
#BA: basal area of all trees in Stand
#BA_GE5: basal area of trees with DBH >= 5" in stand
#BA_PINE: basal area of trees with DBH >= 5" and are from a user defined set of
#pine species in stand.
#
#dplyr example
# fvs_sum = tree %>%
#   group_by(StandID, Year) %>%
#   summarize(BA_ = ba(dbh = DBH, expf = TPA),
#             BA_GE5 = ba(dbh = DBH, expf = TPA, dbhmin = 5,),
#             BA_PINE = ba(dbh = DBH, expf = TPA, species = SpeciesFVS, 
#             dbhmin = 5, select_species = c('PP', 'LP')))
#
#data.table example
# fvs_sum = tree[, .(
#   BA_ = ba(dbh = DBH, expf = TPA),
#   BA_GE5 = ba(dbh = DBH, expf = TPA, dbhmin = 5,),
#   BA_PINE = ba(dbh = DBH, expf = TPA, species = SpeciesFVS, 
#             dbhmin = 5, select_species = c('PP', 'LP')),
#   by = .(StandID, Year)]
#
#Tree is an input dataframe. StandID, Year, DBH, and TPA are variables within 
#the tree dataframe.
################################################################################

#Constants for calculations
f_con = 0.005454154
r_slope = 1.605

################################################################################
#' valid_vectors
#' @name valid_vectors
#' @description
#' 
#' This function takes in a set of vectors and checks if any are NULL or of 
#' unequal length. A value of TRUE will be returned if either of these
#' conditions are met. Additional checks could be added to this function.
#
#' @param ...
#' The ... should be a set of vectors that will be checked.
#' 
#' @return
#' Logical TRUE or FALSE value.
################################################################################

valid_vectors = function(...)
{
  valid = TRUE
  if(null_vector(...)) valid = FALSE
  if(unequal_vector(...)) valid = FALSE
  return(valid)
}

###########################################################################
#'null_vector
#'@name null_vector
#'@description
#
#'This function takes in a set of vectors and checks if any are NULL. If any of
#'the vectors are NULL then a TRUE value is returned. This a helper function
#'that is used inside many of functions in this file.
#
#'@param ...
#'The ... should be a set of vectors that will be checked if any are NULL.
#
#'@return
#'Logical TRUE or FALSE value.
################################################################################

null_vector = function(...)
{
  #Initialize value that will be returned if any vectors is NULL. This starts
  #off as FALSE until proven otherwise.
  null_vector_ = FALSE
  
  #Check for nulls in vectors
  vectors = lapply(list(...), is.null)
  
  #Reset null_vector if there is a null value in vectors
  if(TRUE %in% vectors) null_vector_ = TRUE
  
  return(null_vector_)
}

################################################################################
#' unequal_vector
#' @name unequal_vector
#' @description
#' 
#' This function takes in a set of vectors and checks if they are the same 
#' length. If any of the vectors are not the same length then a TRUE value is
#' returned. This a helper function that is used inside many of functions in 
#' this file. 
#
#' @param ...
#' The ... should be a set of vectors that will be checked if any are of unequal
#' length.
#
#' @return
#' Logical TRUE or FALSE value.
################################################################################

unequal_vector = function(...)
{
  #Initialize value that will be returned if any vectors is NULL. This starts 
  #off as FALSE until proven otherwise.
  unequal_vector_ = FALSE
  
  #Get the vectors
  vectors = list(...)
  
  #Loop through vectors and check if any are not equal to the length of the
  #first vector.
  if(length(vectors) > 0)
  {
    #Get length of first vector
    vector_length = length(vectors[[1]])
    
    for(i in 1:length(vectors))
    {
      if(length(vectors[[i]]) != vector_length)
      {
        unequal_vector_ = TRUE
        break
      }
    }
  }
  
  return(unequal_vector_)
}

################################################################################
#' ba
#' @name ba
#' @description
#' 
#' This function calculates a basal area per acre given input vectors 
#' containing diameter at breast height and expansion factor values. This 
#' attribute can be calculated for user defined size ranges and for select 
#' species.
#' 
#' @param dbh:
#' Numeric vector containing DBH values.
#'
#' @param expf: 
#' Numeric vector containing expansion factors.
#' 
#' @param ht:
#' Optional numeric vector containing total tree height values. If heights are
#' provided, then attribute will be calculated between the values specified in
#' htmin and htmax.
#'
#' @param species:
#' Optional vector containing species codes. If species are provided then
#' attribute will be calculated for species entered in select_species argument.
#' Attribute will be calculated for all species if select_species is left as 
#' NULL.
#' 
#' @param dbhmin:
#' Numeric value corresponding to lower DBH bound to calculate attribute in. 
#' This value is inclusive (>=).
#'
#' @param dbhmax: 
#' Numeric value corresponding to upper DBH bound to calculate attribute in. 
#' This value is exclusive (<).
#' 
#' @param htmin:
#' Numeric value corresponding to lower tree height bound to calculate attribute
#' in. This value is inclusive (>=). This argument is only used if ht argument 
#' is specified.
#
#' @param htmax: 
#' Numeric value corresponding to upper tree height bound to calculate attribute
#' in. This value is exclusive (<). This argument is only used if ht argument 
#' is specified.
#' 
#' @param select_species
#' Optional vector containing species codes. This variable will be used to
#' select which species get included in calculation of attribute. If left as
#' NULL, attribute will be calculated using observations from across all 
#' species.
#' 
#' @return
#' Numeric basal area per acre value
################################################################################

#'@export
ba = function(dbh = NULL,
              expf = NULL,
              ht = NULL,
              species = NULL,
              dbhmin = 0,
              dbhmax = 999,
              htmin = 0,
              htmax = 999,
              select_species = NULL)
{
  
  ba_ = 0
  all_species = TRUE
  
  #Check optional vectors
  if(is.null(ht) && !is.null(expf)) ht = rep(0, times = length(expf))
  if(is.null(species) && !is.null(expf)) species = rep("ALL", times = length(expf))
  if(!is.null(select_species)) all_species = FALSE
  
  #Check validity of vectors
  if(!valid_vectors(dbh, expf, ht, species)) return(ba_)
  
  #Identify records to include in calculation
  include = (dbh >= dbhmin & dbh < dbhmax) & (ht >= htmin & ht < htmax) &
    (all_species | species %in% select_species)
  
  #Calculate BA over DBH, HT, and species
  ba_ = sum((dbh^2 * expf * f_con)[include], na.rm = TRUE)
  
  #Capture bad values
  if(is.na(ba_)) ba_ = 0
  
  #Return ba
  return(ba_)
}

################################################################################
#' tpa
#' @name tpa
#' @description
#' 
#' This function calculates a trees/stems per acre given an input vector 
#' containing expansion factors. This attribute can be calculated for user
#' defined size ranges and for select species.
#
#' @param expf: 
#' Vector of numeric vector containing expansion factors.
#' 
#' @param dbh:
#' Optional numeric vector containing DBH values. If DBH values are provided, 
#' then attribute will be calculated between the values specified in dbhmin and 
#' dbhmax.
#
#' @param ht:
#' Optional vector containing total tree height values. If heights are provided,
#' then attribute will be calculated between the values specified in htmin and
#' htmax.
#
#' @param species:
#' Optional vector containing species codes. If species are provided then
#' attribute will be calculated for species entered in select_species argument.
#' Attribute will be calculated for all species if select_species is left as 
#' NULL.
#
#' @param dbhmin:
#' Numeric value corresponding to lower DBH bound to calculate attribute in. 
#' This value is inclusive (>=).
#
#' @param dbhmax: 
#'Numeric value corresponding to upper DBH bound to calculate attribute in. This
#'value is exclusive (<).
#' 
#' @param htmin:
#' Numeric value corresponding to lower tree height bound to calculate attribute
#' in. This value is inclusive (>=). This argument is only used if ht argument 
#' is specified.
#
#' @param htmax: 
#' Numeric value corresponding to upper tree height bound to calculate attribute
#' in. This value is exclusive (<). This argument is only used if ht argument 
#' is specified.
#' 
#' @param select_spcies:
#' Optional vector containing species codes. This variable will be used to
#' select which species get included in calculation of attribute. If left as
#' NULL, attribute will be calculated using observations from across all 
#' species.
#' 
#'@return
#' Numeric trees per acre value
################################################################################

#'@export
tpa = function(expf = NULL,
               dbh = NULL,
               ht = NULL,
               species = NULL,
               dbhmin = 0,
               dbhmax = 999,
               htmin = 0,
               htmax = 999,
               select_species = NULL)
{
  
  tpa_ = 0
  all_species = TRUE
  
  #Check optional vectors.
  if(is.null(dbh) && !is.null(expf)) dbh = rep(0, times = length(expf))
  if(is.null(ht) && !is.null(expf)) ht = rep(0, times = length(expf))
  if(is.null(species) && !is.null(expf)) species = rep("ALL", times = length(expf))
  if(!is.null(select_species)) all_species = FALSE
  
  #Check validity of vectors
  if(!valid_vectors(dbh, expf, ht, species)) return(tpa_)
  
  #Identify records to include in calculation
  include = (dbh >= dbhmin & dbh < dbhmax) & (ht >= htmin & ht < htmax) &
    (all_species | species %in% select_species)

  #Calculate TPA Over DBH, HT, and species
  tpa_ = sum(expf[include], na.rm = TRUE)
  
  #Capture bad values
  if(is.na(tpa_)) tpa_ = 0
  
  #Return tpa
  return(tpa_)
}

################################################################################
#' qmd
#' @name qmd
#' @description
#' 
#' This function calculates quadratic mean diameter given vectors containing 
#' DBH and expansion factors. This attribute can be calculated for user
#' defined size ranges and for select species.
#
#' @param dbh:
#' Numeric vector containing DBH values.
#
#' @param expf: 
#' Numeric vector containing expansion factors.
#' 
#' @param ht:
#' Optional numeric vector containing total tree height values. If heights are 
#' provided, then attribute will be calculated between the values specified in
#' htmin and htmax.
#' 
#' @param species:
#' Optional vector containing species codes. If species are provided then
#' attribute will be calculated for species entered in select_species argument.
#' Attribute will be calculated for all species if select_species is left as 
#' NULL.
#' 
#' @param dbhmin:
#' Numeric value corresponding to lower DBH bound to calculate attribute in. 
#' This value is inclusive (>=).
#
#' @param dbhmax: 
#' Numeric value corresponding to upper DBH bound to calculate attribute in. 
#' This value is exclusive (<).
#' 
#' @param htmin:
#' Numeric value corresponding to lower tree height bound to calculate attribute
#' in. This value is inclusive (>=). This argument is only used if ht argument 
#' is specified.
#
#' @param htmax: 
#' Numeric value corresponding to upper tree height bound to calculate attribute
#' in. This value is exclusive (<). This argument is only used if ht argument 
#' is specified.
#' 
#' @param select_species:
#' Optional vector containing species codes. This variable will be used to
#' select which species get included in calculation of attribute. If left as
#' NULL, attribute will be calculated using observations from across all 
#' species.
#'
#'@return
#' Numeric quadratic mean diameter value
################################################################################

#'@export
qmd = function(dbh = NULL,
               expf = NULL,
               ht = NULL,
               species = NULL,
               dbhmin = 0,
               dbhmax = 999,
               htmin = 0,
               htmax = 999,
               select_species = NULL)
{
  
  qmd_ = 0
  all_species = TRUE
  
  #Check optional vectors.
  if(is.null(ht) && !is.null(expf)) ht = rep(0, times = length(expf))
  if(is.null(species) && !is.null(expf)) species = rep("ALL", times = length(expf))
  if(!is.null(select_species)) all_species = FALSE
  
  #Check validity of vectors
  if(!valid_vectors(dbh, expf, ht, species)) return(qmd_)
  
  #Identify records to include in calculation
  include = (dbh >= dbhmin & dbh < dbhmax) & (ht >= htmin & ht < htmax) &
    (all_species | species %in% select_species)
  
  #Calculate QMD over DBH, HT, and species
  dbhsq = sum((dbh^2*expf)[include], na.rm = TRUE)
  tpa_ = sum(expf[include], na.rm = TRUE)
  if(tpa_ > 0) qmd_ = sqrt(dbhsq/tpa_)

  #Capture bad values
  if(is.na(qmd_)) qmd_ = 0
  
  #Return qmd
  return(qmd_)
}

################################################################################
#' gmd
#' @name gmd
#' @description
#' 
#' This function calculates generalized mean diameter (Reineke diameter) given 
#' vectors containing DBH and expansion factors. This attribute can be 
#' calculated for user defined size ranges and for select species.
#
#' @param dbh:
#' Numeric vector containing DBH values.
#
#' @param expf: 
#' Numeric vector containing expansion factors.
#' 
#' @param ht:
#' Optional numeric vector containing total tree height values. If heights are 
#' provided, then attribute will be calculated between the values specified in
#' htmin and htmax.
#' 
#' @param species:
#' Optional vector containing species codes. If species are provided then
#' attribute will be calculated for species entered in select_species argument.
#' Attribute will be calculated for all species if select_species is left as 
#' NULL.
#' 
#' @param dbhmin:
#' Numeric value corresponding to lower DBH bound to calculate attribute in. 
#' This value is inclusive (>=).
#
#' @param dbhmax: 
#' Numeric value corresponding to upper DBH bound to calculate attribute in. 
#' This value is exclusive (<).
#' 
#' @param htmin:
#' Numeric value corresponding to lower tree height bound to calculate attribute
#' in. This value is inclusive (>=). This argument is only used if ht argument 
#' is specified.
#
#' @param htmax: 
#' Numeric value corresponding to upper tree height bound to calculate attribute
#' in. This value is exclusive (<). This argument is only used if ht argument 
#' is specified.
#' 
#' @param select_species:
#' Optional vector containing species codes. This variable will be used to
#' select which species get included in calculation of attribute. If left as
#' NULL, attribute will be calculated using observations from across all 
#' species.
#'
#'@return
#' Numeric GMD value
################################################################################

#'@export
gmd = function(dbh = NULL,
               expf = NULL,
               ht = NULL,
               species = NULL,
               dbhmin = 0,
               dbhmax = 999,
               htmin = 0,
               htmax = 999,
               select_species = NULL)
{
  
  gmd_ = 0
  all_species = TRUE
  
  #Check optional vectors.
  if(is.null(ht) && !is.null(expf)) ht = rep(0, times = length(expf))
  if(is.null(species) && !is.null(expf)) species = rep("ALL", times = length(expf))
  if(!is.null(select_species)) all_species = FALSE
  
  #Check validity of vectors
  if(!valid_vectors(dbh, expf, ht, species)) return(gmd_)
  
  #Identify records to include in calculation
  include = (dbh >= dbhmin & dbh < dbhmax) & (ht >= htmin & ht < htmax) &
    (all_species | species %in% select_species)
  
  #Calculate Reineke diameter over DBH, HT, and species
  gmd_sum = sum((expf * dbh^r_slope)[include], na.rm = TRUE)
  tpa_ = sum(expf[include], na.rm = TRUE)
  if(tpa_ > 0 ) gmd_ = (gmd_sum / tpa_)^(1 / r_slope)
  
  #Capture bad values
  if(is.na(gmd_)) gmd_ = 0
  
  #Return gmd
  return(gmd_)
}

################################################################################
#' lorey_dia
#' @name lorey_dia
#' @description
#' 
#' This function calculates Lorey diameter (basal area weighted average 
#' diameter). This attribute can be calculated for user defined size ranges and 
#' for select species.
#
#' @param dbh:
#' Numeric vector containing DBH values.
#
#' @param expf: 
#' Numeric vector containing expansion factors.
#' 
#' @param ht:
#' Optional numeric vector containing total tree height values. If heights are 
#' provided, then attribute will be calculated between the values specified in
#' htmin and htmax.
#' 
#' @param species:
#' Optional vector containing species codes. If species are provided then
#' attribute will be calculated for species entered in select_species argument.
#' Attribute will be calculated for all species if select_species is left as 
#' NULL.
#' 
#' @param dbhmin:
#' Numeric value corresponding to lower DBH bound to calculate attribute in. 
#' This value is inclusive (>=).
#
#' @param dbhmax: 
#' Numeric value corresponding to upper DBH bound to calculate attribute in. 
#' This value is exclusive (<).
#' 
#' @param htmin:
#' Numeric value corresponding to lower tree height bound to calculate attribute
#' in. This value is inclusive (>=). This argument is only used if ht argument 
#' is specified.
#
#' @param htmax: 
#' Numeric value corresponding to upper tree height bound to calculate attribute
#' in. This value is exclusive (<). This argument is only used if ht argument 
#' is specified.
#' 
#' @param select_species:
#' Optional vector containing species codes. This variable will be used to
#' select which species get included in calculation of attribute. If left as
#' NULL, attribute will be calculated using observations from across all 
#' species.
#'
#'@return
#' Numeric basal area weighted (lorey) diameter value
################################################################################

#'@export
lorey_dia = function(dbh = NULL,
                     expf = NULL,
                     ht = NULL,
                     species = NULL,
                     dbhmin = 0,
                     dbhmax = 999,
                     htmin = 0,
                     htmax = 999,
                     select_species = NULL)
{
  
  lorey_dia_ = 0
  all_species = TRUE
  
  #Check optional vectors.
  if(is.null(ht) && !is.null(expf)) ht = rep(0, times = length(expf))
  if(is.null(species) && !is.null(expf)) species = rep("ALL", times = length(expf))
  if(!is.null(select_species)) all_species = FALSE
  
  #Check validity of vectors
  if(!valid_vectors(dbh, expf, ht, species)) return(lorey_dia_)
  
  #Calculate treeba
  treeba = dbh^2 * expf * f_con 
  
  #Identify records to include in calculation
  include = (dbh >= dbhmin & dbh < dbhmax) & (ht >= htmin & ht < htmax) &
    (all_species | species %in% select_species)
  
  #Calculate Lorey diameter over DBH, HT, and species
  dbhsum = sum((dbh * treeba)[include], na.rm = TRUE)
  ba_ = sum(treeba[include], na.rm = TRUE)
  if(ba_ > 0 ) lorey_dia_ = dbhsum / ba_
  
  #Capture bad values
  if(is.na(lorey_dia_)) lorey_dia_ = 0
  
  #Return Lorey diameter
  return(lorey_dia_)
}

################################################################################
#' rsdi
#' @name rsdi
#' @description
#' 
#' This function calculates Reineke SDI using input vectors containing DBH and
#' expansion factor values.
#
#' @param dbh:
#' Numeric vector containing DBH values.
#
#' @param expf: 
#' Numeric vector containing expansion factors.
#' 
#' @return
#' Numeric Reineke SDI value
################################################################################

#'@export
rsdi = function(dbh = NULL,
                expf = NULL)
{
  #Calculate TPA
  tpa_ = tpa(dbh = dbh, expf = expf)
  
  #Calculate qmd
  qmd_ = qmd(dbh = dbh, expf = expf)
  
  #Calculate rsdi
  rsdi = tpa_ * (qmd_/10)^r_slope
  
  #Return rsdi
  return(rsdi)
}

################################################################################
#' zsdi
#' @name zsdi
#' @description
#' 
#' This function calculates Zeide SDI using input vectors containing dbh and
#' expansion factor values. This attribute can be calculated for user defined 
#' size ranges and for select species.
#
#' @param dbh:
#' Numeric vector containing DBH values.
#
#' @param expf: 
#' Numeric vector containing expansion factors.
#' 
#' @param ht:
#' Optional numeric vector containing total tree height values. If heights are 
#' provided, then attribute will be calculated between the values specified in 
#' htmin and htmax.
#' 
#' @param species:
#' Optional vector containing species codes. If species are provided then
#' attribute will be calculated for species entered in select_species argument.
#' Attribute will be calculated for all species if select_species is left as 
#' NULL.
#' 
#' @param dbhmin:
#' Numeric value corresponding to lower DBH bound to calculate attribute in. This
#' value is inclusive (>=).
#
#' @param dbhmax: 
#' Numeric value corresponding to upper DBH bound to calculate attribute in. This
#' value is exclusive (<).
#' 
#' @param htmin:
#' Numeric value corresponding to lower tree height bound to calculate attribute
#' in. This value is inclusive (>=). This argument is only used if ht argument 
#' is specified.
#
#' @param htmax: 
#' Numeric value corresponding to upper tree height bound to calculate attribute
#' in. This value is exclusive (<). This argument is only used if ht argument 
#' is specified.
#' 
#' @param select_species:
#' Optional vector containing species codes. This variable will be used to
#' select which species get included in calculation of attribute.
#' 
#'@return
#' Numeric Zeide SDI value
################################################################################

#'@export
zsdi = function(dbh = NULL,
                expf = NULL,
                ht = NULL,
                species = NULL,
                dbhmin = 0,
                dbhmax = 999,
                htmin = 0,
                htmax = 999,
                select_species = NULL)
{
  
  zsdi_ = 0
  all_species = TRUE
  
  #Check optional vectors.
  if(is.null(ht) && !is.null(expf)) ht = rep(0, times = length(expf))
  if(is.null(species) && !is.null(expf)) species = rep("ALL", times = length(expf))
  if(!is.null(select_species)) all_species = FALSE
  
  #Check validity of vectors
  if(!valid_vectors(dbh, expf, ht, species)) return(zsdi_)
  
  #Identify records to include in calculation
  include = (dbh >= dbhmin & dbh < dbhmax) & (ht >= htmin & ht < htmax) &
    (all_species | species %in% select_species)
  
  #Calculate ZSDI over DBH, HT, and species
  zsdi_ = sum(((dbh/10)^r_slope * expf)[include], na.rm = TRUE)
  
  #Capture bad values
  if(is.na(zsdi_)) zsdi_ = 0
  
  #Return zsdi
  return(zsdi_)
}

################################################################################
#' cc
#' @name cc
#' @description
#' 
#' This function calculates a percent canopy cover value corrected for overlap
#' using input vectors containing crown width values and expansion factors. This
#' attribute can be calculated for user defined size ranges and for select 
#' species.
#' 
#' @param crwidth:
#' Numeric vector containing crown width (diameter) values.
#
#' @param expf: 
#' Numeric vector containing expansion factors.
#' 
#' @param dbh:
#' Optional numeric vector containing DBH values. If DBH values are provided, 
#' then attribute will be calculated between the values specified in dbhmin and 
#' dbhmax.
#' 
#' @param ht:
#' Optional numeric vector containing total tree height values. If heights are
#' provided, then attribute will be calculated between the values specified in
#' htmin and htmax.
#' 
#' @param species:
#' Optional vector containing species codes. If species are provided then
#' attribute will be calculated for species entered in select_species argument.
#' Attribute will be calculated for all species if select_species is left as 
#' NULL.
#' 
#' @param dbhmin:
#' Numeric value corresponding to lower DBH bound to calculate attribute in. This
#' value is inclusive (>=).
#' 
#' @param htmin:
#' Numeric value corresponding to lower tree height bound to calculate attribute
#' in. This value is inclusive (>=). This argument is only used if ht argument 
#' is specified.
#
#' @param htmax: 
#' Numeric value corresponding to upper tree height bound to calculate attribute
#' in. This value is exclusive (<). This argument is only used if ht argument 
#' is specified.
#' 
#' @param select_species:
#' Optional vector containing species codes. This variable will be used to
#' select which species get included in calculation of attribute.
#' 
#'@return
#' Numeric percent canopy cover value
################################################################################

#'@export
cc = function(crwidth = NULL,
              expf = NULL,
              dbh = NULL,
              ht = NULL,
              species = NULL,
              dbhmin = 0,
              dbhmax = 999,
              htmin = 0,
              htmax = 999,
              select_species = NULL)
{
  cc_ = 0
  all_species = TRUE
  
  #Check optional vectors.
  if(is.null(dbh) && !is.null(expf)) dbh = rep(0, times = length(expf))
  if(is.null(ht) && !is.null(expf)) ht = rep(0, times = length(expf))
  if(is.null(species) && !is.null(expf)) species = rep("ALL", times = length(expf))
  if(!is.null(select_species)) all_species = FALSE
  
  #Check validity of vectors
  if(!valid_vectors(dbh, expf, ht, species)) return(cc_)
  
  #Identify records to include in calculation
  include = (dbh >= dbhmin & dbh < dbhmax) & (ht >= htmin & ht < htmax) &
    (all_species | species %in% select_species)
  
  #Calculate CC over DBH, HT, and species
  cc_ = sum((((crwidth/2)^2) * (expf/43560) * pi * 100)[include], na.rm = TRUE)
    
  #Capture bad values
  if(is.na(cc_)) cc_ = 0
  
  #Correct for overlap
  cc_ = correct_cc(cc_)
  
  return(cc_)
}

################################################################################
#' correct_cc
#' @name correct_cc
#' @description
#' This function takes in an uncorrected percent canopy cover value and returns
#' a corrected value using the relationship described on page 2 of Crookston,
#' Nicholas L.; Stage, Albert R. 1999. Percent canopy cover and stand structure
#' statistics from the Forest Vegetation Simulator. Gen. Tech. Rep. RMRS-GTR-24.
#' Ogden, UT: U. S. Department of Agriculture, Forest Service, Rocky Mountain
#' Research Station. 11 p.
#
#' @param cc
#' cc: Numeric uncorrected CC value
#
#' @return
#' Numeric corrected canopy cover value
################################################################################

#'@export
correct_cc = function(cc = 0)
{
  cor_cc = 100 * (1 - exp ( - 0.01* cc))
  return(cor_cc)
}

################################################################################
#' bal
#' @name bal
#' @description
#' 
#' This function calculates basal area in trees larger than subject tree (BAL)
#' from input vectors containing dbh and expansion factor values.
#
#' @param dbh
#' Numeric vector containing DBH values.
#
#' @param expf: 
#' Numeric vector containing expansion factors.
#' 
#' @param handle_ties
#' Logical variable used to determine if dbh values with equivalent values get 
#' the same BAL return. If this value is TRUE, then trees with equivalent DBH 
#' values will have the same BAL value (e.g. 3 trees with 10 inch DBH will all
#' have the same BAL). If this value is FALSE, then trees with equivalent DBH 
#' values will have a different BAL (3 trees with 10 inch DBH will each have a
#' different BAL).
#' 
#' @return
#' Numeric vector containing BAL values
################################################################################

#'@export
bal = function(dbh = NULL,
               expf = NULL,
               handle_ties = FALSE)
{
  #Return if dbh or expf is NULL or not of equal length
  if(!valid_vectors(dbh, expf)) return(0)
  
  #Create sequence of integers. This will be used to reorder bal at the end of 
  #the function.
  orig_order = 1:length(dbh)
  
  #Get indices of sorted DBH in descending order
  dbh_order = order(-dbh)
  
  #Don't handle ties.
  if(!handle_ties)
  {
    #Do a cumulative sum of basal area and then subtract ba of tree from each 
    #record.
    bal = cumsum((dbh^2)[dbh_order] * expf[dbh_order] * f_con) -
      (dbh^2)[dbh_order] * expf[dbh_order] * f_con
  }
  
  #Handle ties.
  else
  {
    #Setup temp_dbh vector and then replace duplicate values with 0
    temp_dbh = dbh
    temp_dbh[dbh_order][duplicated(temp_dbh[dbh_order])] = 0

    #Do a cumulative sum of basal area and then subtract ba of tree from each 
    #record. Note use of temp_dbh in cumulative sum and dbh in subtraction.
    bal = cumsum((temp_dbh^2)[dbh_order] * expf[dbh_order] * f_con) -
      (dbh^2)[dbh_order] * expf[dbh_order] * f_con
  }
  
  #Reorder bal by original order
  return(bal[match(orig_order, dbh_order)])
}

################################################################################
#'rsdi_stage
#'@name rsdi_stage
#'@description
#'
#' This function is used to calculate Reineke SDI used the methodology proposed
#' by Stage 1968. From Section 7.3.2.1 of EFVS using input vectors containing 
#' DBH and expansion factors. This attribute can be calculated for user defined 
#' size ranges and for select species.
#
# SDI = sum(a * TPAi + b * DBHi^2 * TPA)
# a = 10^(-1.605) * (1-(1.605/2)) * qmd^1.605
# b = 10^(−1.605) * (1.605/2) * QMD^(1.605-2)
#
#' @param dbh:     
#' Numeric vector containing DBH values.
#
#' @param expf:     
#' Numeric vector containing expansion factors.
#'
#' @param ht:
#' Optional numeric vector containing total tree height values. If heights are 
#' provided, then attribute will be calculated between the values specified in 
#' htmin and htmax.
#' 
#' @param species:
#' Optional vector containing species codes. If species are provided then
#' attribute will be calculated for species entered in select_species argument.
#' Attribute will be calculated for all species if select_species is left as 
#' NULL.
#
#' @param dbhmin:
#' Numeric value corresponding to lower DBH bound to calculate attribute in. 
#' This value is inclusive (>=).
#
#' @param dbhmax: 
#' Numeric value corresponding to upper DBH bound to calculate attribute in. 
#' This value is exclusive (<).
#' 
#' @param htmin:
#' Numeric value corresponding to lower tree height bound to calculate attribute
#' in. This value is inclusive (>=). This argument is only used if ht argument 
#' is specified.
#
#' @param htmax: 
#' Numeric value corresponding to upper tree height bound to calculate attribute
#' in. This value is exclusive (<). This argument is only used if ht argument 
#' is specified.
#' 
#' @param select_species:
#' Optional vector containing species codes. This variable will be used to
#' select which species get included in calculation of attribute. This argument
#' will only be used if values are provided for species. d
#
#' @return 
#' Numeric Reineke SDI calculated using stage formulation.
################################################################################

#'@export
rsdi_stage = function(dbh = NULL,
                     expf = NULL,
                     ht = NULL,
                     species = NULL,
                     dbhmin = 0,
                     dbhmax = 999,
                     htmin = 0,
                     htmax = 999,
                     select_species = NULL)
{
  rsdi_ = 0
  all_species = TRUE
  
  #Check optional vectors
  if(is.null(ht) && !is.null(expf)) ht = rep(0, times = length(expf))
  if(is.null(species) && !is.null(expf)) species = rep("ALL", times = length(expf))
  if(!is.null(select_species)) all_species = FALSE
  
  #Check validity of vectors
  if(!valid_vectors(dbh, expf, ht, species)) return(rsdi_)

  #Calculate stand level tpa and dbhsq. Also initialize qmd
  stand_tpa = sum(expf)
  dbhsq = sum(dbh^2 * expf)
  qmd = 0
  
  #Return if stand_tpa is less than or equal to 0
  if(stand_tpa <= 0) return(rsdi_)
  
  #Initialize a and b parameters
  a = 10^(-r_slope) * (1-(r_slope/2)) * (dbhsq/stand_tpa)^(r_slope/2)
  b = 10^(-r_slope) * (r_slope/2) * (dbhsq/stand_tpa)^(r_slope/2 - 1)
  
  #Identify records to include in calculation
  include = (dbh >= dbhmin & dbh < dbhmax) & (ht >= htmin & ht < htmax) &
    (all_species | species %in% select_species)
  
  #Calculate RSDI over DBH, HT, and species
  rsdi_ = sum((a*expf)[include], (b * dbh^2 * expf)[include], na.rm = TRUE)
  
  #Capture bad values
  if(is.na(rsdi_)) rsdi_ = 0
  
  #Return rsdi
  return(rsdi_)
}

################################################################################
#' lorey_ht
#' @name lorey_ht
#' @description
#' 
#' This function calculates Lorey height (basal area weighted average height). 
#' This attribute can be calculated for user defined size ranges and for select
#' species.
#
#' @param dbh:
#' Numeric vector containing DBH values.
#' 
#' @param ht:
#' Numeric vector containing total tree height values.
#
#' @param expf: 
#' Numeric vector containing expansion factors.
#'
#' @param species:
#' Optional vector containing species codes. If species are provided then
#' attribute will be calculated for species entered in select_species argument.
#' Attribute will be calculated for all species if select_species is left as 
#' NULL.
#' 
#' @param dbhmin:
#' Numeric value corresponding to lower DBH bound to calculate attribute in. 
#' This value is inclusive (>=).
#
#' @param dbhmax: 
#' Numeric value corresponding to upper DBH bound to calculate attribute in. 
#' This value is exclusive (<).
#' 
#' @param htmin:
#' Numeric value corresponding to lower tree height bound to calculate attribute
#' in. This value is inclusive (>=). This argument is only used if ht argument 
#' is specified.
#
#' @param htmax: 
#' Numeric value corresponding to upper tree height bound to calculate attribute
#' in. This value is exclusive (<). This argument is only used if ht argument 
#' is specified.
#' 
#' @param select_species:
#' Optional vector containing species codes. This variable will be used to
#' select which species get included in calculation of attribute. If left as
#' NULL, attribute will be calculated using observations from across all 
#' species.
#'
#'@return
#' Numeric Lorey height value
################################################################################

#'@export
lorey_ht = function(dbh = NULL,
                    ht = NULL,
                    expf = NULL,
                    species = NULL,
                    dbhmin = 0,
                    dbhmax = 999,
                    htmin = 0,
                    htmax = 999,
                    select_species = NULL)
{
  
  lorey_ht_ = 0
  all_species = TRUE
  
  #Check optional vectors.
  if(is.null(dbh) && !is.null(expf)) dbh = rep(0, times = length(expf))
  if(is.null(species) && !is.null(expf)) species = rep("ALL", times = length(expf))
  if(!is.null(select_species)) all_species = FALSE
  
  #Check validity of vectors
  if(!valid_vectors(dbh, expf, ht, species)) return(lorey_ht_)
  
  #Calculate treeba
  treeba = dbh^2 * expf * f_con 
  
  #Identify records to include in calculation
  include = (dbh >= dbhmin & dbh < dbhmax) & (ht >= htmin & ht < htmax) &
    (all_species | species %in% select_species)
  
  #Calculate Lorey height over DBH, HT, and species
  htsum = sum((ht * treeba)[include], na.rm = TRUE)
  ba_ = sum(treeba[include], na.rm = TRUE)
  if(ba_ > 0 ) lorey_ht_ = htsum / ba_
  
  #Capture bad values
  if(is.na(lorey_ht_)) lorey_ht_ = 0
  
  #Return Lorey height
  return(lorey_ht_)
}

################################################################################
#'top_ht
#'@name top_ht
#'@description
#'
#'This function is used to calculate top height for a specified percentage of
#'trees in the stand or and explicit number of trees (trees per acre) value. 
#'
#'@param dbh:     
#'Numeric vector containing DBH values.
#
#'@param expf:     
#'Numeric vector containing expansion factors values.
#'
#'@param ht:     
#'Numeric vector of tree heights.
#
#'@param top_tpa:
#'Amount of TPA to include in top height calculation. Top 40, trees, top 100, 
#'etc.
#
#'@param top_per:
#'Percentage of trees to include in the top height calculation. Largest 20% of 
#'trees, largest 40% of trees etc. If this value is not null then it will 
#'take precedence over the value in top_tpa argument. 
#
#'@return 
#'Top height value.
################################################################################

#'@export
top_ht = function(dbh = NULL,
                  expf = NULL,
                  ht = NULL,
                  top_tpa = 40,
                  top_per = NULL)
{
  #Initialize top_ht_
  top_ht_ = 0
  
  #Check validity of vectors
  if(!valid_vectors(dbh, expf, ht)) return(top_ht_)
  
  #Validate top_tpa
  if(is.null(top_tpa) || top_tpa < 0) top_tpa = 40
  
  #Validate top_per. If top_per is not null but has an invalid value, set it to 
  #20%.
  if(!is.null(top_per) && (top_per < 0 || top_per > 100)) top_per = 20
  
  #Calculate TPA for the entire stand
  tpa_ = tpa(dbh = dbh, expf = expf)
  
  #Do calculations if tpa > 0
  if(tpa_ > 0)
  {
    #Determine amount TPA value that will be included in top height calculation
    top = top_tpa
    if(top > tpa_) top = tpa_
    if(!is.null(top_per)) top = tpa_ * (top_per/100)
    
    #Calculate top height for trees in top  
    #Get order of DBH values in descending order
    dbh_order = order(-dbh)
    
    #Find the index where top is exceeded
    top_exceed = which.max(cumsum(expf[dbh_order]) >= top)
    
    #Sum expf up to this index
    tpa_sum = sum(expf[dbh_order][1:top_exceed], na.rm = TRUE)
    
    #Calculate tpa_dif and adjust tpa_sum
    tpa_dif = tpa_sum - top
    tpa_sum = tpa_sum - tpa_dif
    
    #Top height
    ht_sum = sum((ht * expf)[dbh_order][1:top_exceed-1], na.rm = TRUE) + 
      (ht)[dbh_order][top_exceed] * ((expf)[dbh_order][top_exceed] - tpa_dif)
    if(tpa_sum > 0) top_ht_ = ht_sum / tpa_sum
  }
  
  return(top_ht_)
}

################################################################################
#' top_dia
#' @name top_dia
#' @description
#'
#' This function is used to calculate QMD or average diameter weighted by TPA
#' for the largest trees by DBH within a specified percentage of TPA or an 
#' explicit TPA value. This value is calculated from a set of input vectors
#' containing DBH values and expansion factors.
#
#' @param dbh:     
#' Numeric vector containing DBH values.
#
#' @param expf:     
#' Numeric vector containing expansion factors values.
#'
#' @param top_tpa:
#' Amount of TPA to include in top QMD calculation. Largest 40 trees, Largest 
#' 100, etc.
#
#' @param top_per:
#' Percentage of trees to include in the top QMD calculation. If this value is
#' not null then it will supersede the value in top_tpa argument. 
#'
#' @param dia_type:
#' Integer value used to specify what type of diameter should be calculated.
#' 1 = QMD
#' 2 = average diameter weighted by trees per acre
#' 3 = Reineke diameter
#
#' @return 
#' Numeric top diameter value.
################################################################################

#'@export
top_dia = function(dbh = NULL,
                   expf = NULL,
                   top_tpa = 40,
                   top_per = NULL,
                   dia_type = 1)
{
  #Initialize top_dia_
  top_dia_ = 0
  
  #Check validity of vectors
  if(!valid_vectors(dbh, expf)) return(top_dia_)
  
  #Validate top_tpa
  if(is.null(top_tpa) || top_tpa < 0) top_tpa = 40
  
  #Validate top_per. If top_per is not null but has an invalid value, set it to 
  #0.
  if(!is.null(top_per)) 
  {
    if(top_per < 0 || top_per > 100) top_per = 20
  }
  
  #Validate dia_type
  if(!dia_type %in% c(1, 2, 3)) dia_type = 1
  
  #Calculate TPA for the entire stand
  tpa_ = tpa(dbh = dbh, expf = expf)
  
  #Do calculations if tpa is > 0
  if(tpa_ > 0)
  {
    #Determine amount TPA value that will be included in top height calculation
    top = top_tpa
    if(top > tpa_) top = tpa_
    if(!is.null(top_per))top = tpa_ * (top_per/100)
    
    #Get order of DBH values in descending order
    dbh_order = order(-dbh)
    
    #Find the index where top is exceeded
    top_exceed = which.max(cumsum(expf[dbh_order]) >= top)
    
    #Sum expf up to this index
    tpa_sum = sum(expf[dbh_order][1:top_exceed], na.rm = TRUE)
    
    #Calculate tpa_dif and adjust tpa_sum
    tpa_dif = tpa_sum - top
    tpa_sum = tpa_sum - tpa_dif
    
    #QMD
    if(dia_type == 1) {
      dbh_sum = sum((dbh^2*expf)[dbh_order][1:top_exceed-1], na.rm = TRUE) + 
        (dbh^2)[dbh_order][top_exceed] * (expf[dbh_order][top_exceed] - 
                                            tpa_dif)
      
      if(tpa_sum > 0) top_dia_ = sqrt(dbh_sum / tpa_sum)
    }
    
    #Average diameter weighted by TPA
    else if (dia_type == 2)
    {
      dbh_sum = sum((dbh*expf)[dbh_order][1:top_exceed-1], na.rm = TRUE) + 
        (dbh)[dbh_order][top_exceed] * (expf[dbh_order][top_exceed] - 
                                          tpa_dif)
      
      if(tpa_sum > 0) top_dia_ = dbh_sum / tpa_sum
    }
    
    #GMD
    else 
    {
      dbh_sum = sum((dbh^r_slope * expf)[dbh_order][1:top_exceed-1], 
                    na.rm = TRUE) + 
        (dbh^r_slope)[dbh_order][top_exceed] * (expf[dbh_order][top_exceed] - 
                                                  tpa_dif)
      
      if(tpa_sum > 0) top_dia_ = (dbh_sum / tpa_sum)^(1 / r_slope)
    }
  }
  
  return(top_dia_)
}

################################################################################
#' mean_attr
#' @name mean_attr
#' @description
#'
#' This function is used to calculate the arithmetic or weighted mean (average) 
#' of an attribute. The weighted mean will only be calculated if weights are 
#' provided as an input argument. These mean values can be calculated within 
#' custom size ranges and for select species.
#'
#' @param attr:
#' Numeric vector containing numeric attribute
#'
#' @param weight:     
#' Optional numeric vector containing a weighting value. This could be an 
#' expansion factor, tree basal area, or other user defined weight. If this 
#' argument is left as NULL, then the arithmetic average will be returned.. 
#'
#' @param dbh:     
#' Optional numeric vector containing DBH values. If DBH values are provided, 
#' then attribute will be calculated between the values specified in dbhmin and 
#' dbhmax.
#'
#' @param ht:
#' Optional numeric vector containing total tree height values. If heights are
#' provided, then attribute will be calculated between the values specified in
#' htmin and htmax.
#' 
#' @param species:
#' Optional vector containing species codes. If species are provided then
#' attribute will be calculated for species entered in select_species argument.
#' Attribute will be calculated for all species if select_species is left as 
#' NULL.
#
#' @param dbhmin:
#' Numeric value corresponding to lower DBH bound to calculate attribute in. 
#' This value is inclusive (>=).
#
#' @param dbhmax: 
#' Numeric value corresponding to upper DBH bound to calculate attribute in. 
#' This value is exclusive (<).
#' 
#' @param htmin:
#' Numeric value corresponding to lower tree height bound to calculate attribute
#' in. This value is inclusive (>=). This argument is only used if ht argument 
#' is specified.
#
#' @param htmax: 
#' Numeric value corresponding to upper tree height bound to calculate attribute
#' in. This value is exclusive (<). This argument is only used if ht argument 
#' is specified.
#' 
#' @param select_species:
#' Optional vector containing species codes. This variable will be used to
#' select which species get included in calculation of attribute. This argument
#' will only be used if values are provided for species. 
#
#' @return 
#' Average or weighted average of attribute.
################################################################################

#'@export
mean_attr = function(attr = NULL,
                     weight = NULL,
                     dbh = NULL,
                     ht = NULL,
                     species = NULL,
                     dbhmin = 0,
                     dbhmax = 999,
                     htmin = 0,
                     htmax = 999,
                     select_species = NULL)
{
  mean_attr_ = 0
  all_species = TRUE
  
  #Check optional vectors.
  if(is.null(dbh) && !is.null(attr)) dbh = rep(0, times = length(attr))
  if(is.null(ht) && !is.null(attr)) ht = rep(0, times = length(attr))
  if(is.null(species) && !is.null(attr)) species = rep("ALL", times = length(attr))
  if(!is.null(select_species)) all_species = FALSE
  if(is.null(weight)) weight = rep(x = 1, times = length(attr))
  
  #Check validity of vectors
  if(!valid_vectors(attr, dbh, ht, species)) return(mean_attr_)
  
  #Identify records to include in calculation
  include = (dbh >= dbhmin & dbh < dbhmax) & (ht >= htmin & ht < htmax) &
    (all_species | species %in% select_species)

  #Calculate mean
  mean_attr_ = weighted.mean(x = attr[include], 
                             w = weight[include], 
                             na.rm = TRUE)
  
  #Capture bad values
  if(is.na(mean_attr_)) mean_attr_ = 0
  
  return(mean_attr_)
}

################################################################################
#' expand_attr
#' @name expand_attr
#' @description
#' 
#' This function sums and expands an input numeric attribute to a per unit area
#' basis using numeric vectors containing dbh, attribute of interest, and 
#' expansion factors. The numeric attribute could be a tree-level volume, 
#' biomass, carbon, etc.This attribute can be calculated for user defined size 
#' ranges and for select species.
#' 
#' @param attr:
#' Numeric vector containing numeric attribute
#
#' @param expf: 
#' Numeric vector containing expansion factors.
#' 
#' @param dbh:
#' Optional numeric vector containing DBH values. If DBH values are provided, 
#' then attribute will be calculated between the values specified in dbhmin and 
#' dbhmax.
#' 
#' @param ht:
#' Optional numeric vector containing total tree height values. If heights are 
#' provided, then attribute will be calculated between the values specified in 
#' htmin and htmax.
#' 
#' @param species:
#' Optional vector containing species codes. If species are provided then
#' attribute will be calculated for species entered in select_species argument.
#' Attribute will be calculated for all species if select_species is left as 
#' NULL.
#
#' @param dbhmin:
#' Numeric value corresponding to lower DBH bound to calculate attribute in. 
#' This value is inclusive (>=).
#
#' @param dbhmax: 
#' Numeric value corresponding to upper DBH bound to calculate attribute in. This
#' value is exclusive (<).
#' 
#' @param htmin:
#' Numeric value corresponding to lower tree height bound to calculate attribute
#' in. This value is inclusive (>=). This argument is only used if ht argument 
#' is specified.
#
#' @param htmax: 
#' Numeric value corresponding to upper tree height bound to calculate attribute
#' in. This value is exclusive (<). This argument is only used if ht argument 
#' is specified.
#' 
#' @param select_species:
#' Optional vector containing species codes. This variable will be used to
#' select which species get included in calculation of attribute.
#'
#'@return
#' Sum of attribute expanded to a per unit area
################################################################################

#'@export
expand_attr = function(attr = NULL,
                       expf = NULL,
                       dbh = NULL,
                       ht = NULL,
                       species = NULL,
                       dbhmin = 0,
                       dbhmax = 999,
                       htmin = 0,
                       htmax = 999,
                       select_species = NULL)
{
  
  attr_expand_ = 0
  all_species = TRUE
  
  #Check optional vectors.
  if(is.null(dbh) && !is.null(expf)) dbh = rep(0, times = length(expf))
  if(is.null(ht) && !is.null(expf)) ht = rep(0, times = length(expf))
  if(is.null(species) && !is.null(expf)) species = rep("ALL", times = length(expf))
  if(!is.null(select_species)) all_species = FALSE
  
  #Check validity of vectors
  if(!valid_vectors(attr, dbh, ht, species)) return(attr_expand_)
  
  #Identify records to include in calculation
  include = (dbh >= dbhmin & dbh < dbhmax) & (ht >= htmin & ht < htmax) &
    (all_species | species %in% select_species)
  
  #Expand attr over DBH, HT, and species
  attr_expand_ = sum((attr * expf)[include], na.rm = TRUE)
  
  #If attr_expand_ is NaN or NA set to 0
  if(is.na(attr_expand_)) attr_expand_ = 0
  
  return(attr_expand_)
}

################################################################################
#' median_attr
#' @name median_attr
#' @description
#' 
#' This function determines the median value for an input attribute. This can be
#' calculated for custom size ranges and for select species.
#' 
#' @param attr:
#' Numeric vector containing numeric attribute
#' 
#' @param dbh:
#' Optional numeric vector containing DBH values. If DBH values are provided, 
#' then attribute will be calculated between the values specified in dbhmin and 
#' dbhmax.
#' 
#' @param ht:
#' Optional numeric vector containing total tree height values. If heights are 
#' provided, then attribute will be calculated between the values specified in 
#' htmin and htmax.
#' 
#' @param species:
#' Optional vector containing species codes. If species are provided then
#' attribute will be calculated for species entered in select_species argument.
#' Attribute will be calculated for all species if select_species is left as 
#' NULL.
#
#' @param dbhmin:
#' Numeric value corresponding to lower DBH bound to calculate attribute in. 
#' This value is inclusive (>=).
#
#' @param dbhmax: 
#' Numeric value corresponding to upper DBH bound to calculate attribute in. This
#' value is exclusive (<).
#' 
#' @param htmin:
#' Numeric value corresponding to lower tree height bound to calculate attribute
#' in. This value is inclusive (>=). This argument is only used if ht argument 
#' is specified.
#
#' @param htmax: 
#' Numeric value corresponding to upper tree height bound to calculate attribute
#' in. This value is exclusive (<). This argument is only used if ht argument 
#' is specified.
#' 
#' @param select_species:
#' Optional vector containing species codes. This variable will be used to
#' select which species get included in calculation of attribute.
#'
#'@return
#' Median value of attribute.
################################################################################'

#'@export
median_attr = function(attr = NULL,
                       dbh = NULL,
                       ht = NULL,
                       species = NULL,
                       dbhmin = 0,
                       dbhmax = 999,
                       htmin = 0,
                       htmax = 999,
                       select_species = NULL)
{
  
  median_attr_ = 0
  all_species = TRUE
  
  #Check optional vectors.
  if(is.null(dbh) && !is.null(attr)) dbh = rep(0, times = length(attr))
  if(is.null(ht) && !is.null(attr)) ht = rep(0, times = length(attr))
  if(is.null(species) && !is.null(attr)) species = rep("ALL", times = length(attr))
  if(!is.null(select_species)) all_species = FALSE
  
  #Check validity of vectors
  if(!valid_vectors(attr, dbh, ht, species)) return(median_attr_)
  
  #Identify records to include in calculation
  include = (dbh >= dbhmin & dbh < dbhmax) & (ht >= htmin & ht < htmax) &
    (all_species | species %in% select_species)
  
  #Over DBH, HT, and species
  median_attr_ = median(attr[include], na.rm = TRUE)
  
  #Capture bad values
  if(is.na(median_attr_)) median_attr_ = 0
  
  return(median_attr_)
}

################################################################################
#' min_attr
#' @name min_attr
#' @description
#' 
#' This function determines the minimum value for an input attribute. This can 
#' be calculated for custom size ranges and for select species.
#' 
#' @param attr:
#' Numeric vector containing numeric attribute
#' 
#' @param dbh:
#' Optional numeric vector containing DBH values. If DBH values are provided, 
#' then attribute will be calculated between the values specified in dbhmin and 
#' dbhmax.
#' 
#' @param ht:
#' Optional numeric vector containing total tree height values. If heights are 
#' provided, then attribute will be calculated between the values specified in 
#' htmin and htmax.
#' 
#' @param species:
#' Optional vector containing species codes. If species are provided then
#' attribute will be calculated for species entered in select_species argument.
#' Attribute will be calculated for all species if select_species is left as 
#' NULL.
#
#' @param dbhmin:
#' Numeric value corresponding to lower DBH bound to calculate attribute in. 
#' This value is inclusive (>=).
#
#' @param dbhmax: 
#' Numeric value corresponding to upper DBH bound to calculate attribute in. This
#' value is exclusive (<).
#' 
#' @param htmin:
#' Numeric value corresponding to lower tree height bound to calculate attribute
#' in. This value is inclusive (>=). This argument is only used if ht argument 
#' is specified.
#
#' @param htmax: 
#' Numeric value corresponding to upper tree height bound to calculate attribute
#' in. This value is exclusive (<). This argument is only used if ht argument 
#' is specified.
#' 
#' @param select_species:
#' Optional vector containing species codes. This variable will be used to
#' select which species get included in calculation of attribute.
#'
#'@return
#' Minimum value of attribute.
################################################################################

#'@export
min_attr = function(attr = NULL,
                    dbh = NULL,
                    ht = NULL,
                    species = NULL,
                    dbhmin = 0,
                    dbhmax = 999,
                    htmin = 0,
                    htmax = 999,
                    select_species = NULL)
{
  
  min_attr_ = 0
  all_species = TRUE
  
  #Check optional vectors.
  if(is.null(dbh) && !is.null(attr)) dbh = rep(0, times = length(attr))
  if(is.null(ht) && !is.null(attr)) ht = rep(0, times = length(attr))
  if(is.null(species) && !is.null(attr)) species = rep("ALL", times = length(attr))
  if(!is.null(select_species)) all_species = FALSE
  
  #Check validity of vectors
  if(!valid_vectors(attr, dbh, ht, species)) return(min_attr_)
  
  #Identify records to include in calculation
  include = (dbh >= dbhmin & dbh < dbhmax) & (ht >= htmin & ht < htmax) &
    (all_species | species %in% select_species) & !is.na(attr)
  
  #Find minimum over DBH, HT, and species
  if(any(include))
    min_attr_ = min(attr[include], na.rm = TRUE)
  
  #Capture bad values
  if(is.na(min_attr_)) min_attr_ = 0
  
  return(min_attr_)
}

################################################################################
#' max_attr
#' @name max_attr
#' @description
#' 
#' This function determines the maximum value for an input attribute. This can 
#' be calculated for custom size ranges and for select species.
#' 
#' @param attr:
#' Numeric vector containing numeric attribute
#' 
#' @param dbh:
#' Optional numeric vector containing DBH values. If DBH values are provided, 
#' then attribute will be calculated between the values specified in dbhmin and 
#' dbhmax.
#' 
#' @param ht:
#' Optional numeric vector containing total tree height values. If heights are 
#' provided, then attribute will be calculated between the values specified in 
#' htmin and htmax.
#' 
#' @param species:
#' Optional vector containing species codes. If species are provided then
#' attribute will be calculated for species entered in select_species argument.
#' Attribute will be calculated for all species if select_species is left as 
#' NULL.
#
#' @param dbhmin:
#' Numeric value corresponding to lower DBH bound to calculate attribute in. 
#' This value is inclusive (>=).
#
#' @param dbhmax: 
#' Numeric value corresponding to upper DBH bound to calculate attribute in. This
#' value is exclusive (<).
#' 
#' @param htmin:
#' Numeric value corresponding to lower tree height bound to calculate attribute
#' in. This value is inclusive (>=). This argument is only used if ht argument 
#' is specified.
#
#' @param htmax: 
#' Numeric value corresponding to upper tree height bound to calculate attribute
#' in. This value is exclusive (<). This argument is only used if ht argument 
#' is specified.
#' 
#' @param select_species:
#' Optional vector containing species codes. This variable will be used to
#' select which species get included in calculation of attribute.
#'
#'@return
#' Maximum value of attribute.
################################################################################

#'@export
max_attr = function(attr = NULL,
                    dbh = NULL,
                    ht = NULL,
                    species = NULL,
                    dbhmin = 0,
                    dbhmax = 999,
                    htmin = 0,
                    htmax = 999,
                    select_species = NULL)
{
  
  max_attr_ = 0
  all_species = TRUE

  #Check optional vectors.
  if(is.null(dbh) && !is.null(attr)) dbh = rep(0, times = length(attr))
  if(is.null(ht) && !is.null(attr)) ht = rep(0, times = length(attr))
  if(is.null(species) && !is.null(attr)) species = rep("ALL", times = length(attr))
  if(!is.null(select_species)) all_species = FALSE
  
  #Check validity of vectors
  if(!valid_vectors(attr, dbh, ht, species)) return(max_attr_)
  
  #Identify records to include in calculation
  include = (dbh >= dbhmin & dbh < dbhmax) & (ht >= htmin & ht < htmax) &
    (all_species | species %in% select_species) & !is.na(attr)
  
  #Find minimum over DBH, HT, and species
  if(any(include))
    max_attr_ = max(attr[include], na.rm = TRUE)
  
  #Capture bad values
  if(is.na(max_attr_)) max_attr_ = 0
  
  return(max_attr_)
}

################################################################################
#' count_attr
#' @name count_attr
#' @description
#' 
#' This function counts the number of tree records between specified DBH and HT
#' ranges and for select species. 
#' 
#' @param attr:
#' Vector containing an attribute. In this context, this would likely be a tree
#' ID value. Technically you can pass any vector into this argument to have
#' counted.
#' 
#' @param dbh:
#' Optional numeric vector containing DBH values.
#' 
#' @param ht:
#' Optional vector containing total tree height values. If heights are provided,
#' then attribute will be calculated between the values specified in htmin and
#' htmax.
#' 
#' @param species:
#' Optional vector containing species codes. If species are provided then
#' attribute will be calculated for species entered in select_species argument.
#' Attribute will be calculated for all species if select_species is left as 
#' NULL.
#
#' @param dbhmin:
#' Numeric value corresponding to lower DBH bound to calculate attribute in. 
#' This value is inclusive (>=).
#
#' @param dbhmax: 
#' Numeric value corresponding to upper DBH bound to calculate attribute in. This
#' value is exclusive (<).
#' 
#' @param htmin:
#' Numeric value corresponding to lower tree height bound to calculate attribute
#' in. This value is inclusive (>=). This argument is only used if ht argument 
#' is specified.
#
#' @param htmax: 
#' Numeric value corresponding to upper tree height bound to calculate attribute
#' in. This value is exclusive (<). This argument is only used if ht argument 
#' is specified.
#' 
#' @param select_species:
#' Optional vector containing species codes. This variable will be used to
#' select which species get included in calculation of attribute.
#'
#'@return
#' Numeric count of records.
################################################################################

#'@export
count_attr = function(attr = NULL,
                      dbh = NULL,
                      ht = NULL,
                      species = NULL,
                      dbhmin = 0,
                      dbhmax = 999,
                      htmin = 0,
                      htmax = 999,
                      select_species = NULL)
{
  
  count_ = 0
  all_species = TRUE
  
  #Check optional vectors.
  if(is.null(dbh) && !is.null(attr)) dbh = rep(0, times = length(attr))
  if(is.null(ht) && !is.null(attr)) ht = rep(0, times = length(attr))
  if(is.null(species) && !is.null(attr)) species = rep("ALL", times = length(attr))
  if(!is.null(select_species)) all_species = FALSE
  
  #Check validity of vectors
  if(!valid_vectors(attr, dbh, ht, species)) return(count_)
  
  #Identify records to include in calculation
  include = (dbh >= dbhmin & dbh < dbhmax) & (ht >= htmin & ht < htmax) &
    (all_species | species %in% select_species)
  
  #Count over DBH, ht, and species
  count_ = length(attr[include])
  
  #Capture bad values
  if(is.na(count_)) count_ = 0
  
  return(count_)
}

################################################################################
#' ba_f
#' @name ba_f
#' @description
#' 
#' This function calculates a basal area per acre given input vectors 
#' containing diameter at breast height and expansion factor values. This 
#' attribute can be calculated for user defined size ranges and for select 
#' species.
#' 
#' @param dbh:
#' Numeric vector containing DBH values.
#'
#' @param expf: 
#' Numeric vector containing expansion factors.
#' 
#' @param ht:
#' Optional numeric vector containing total tree height values. If heights are
#' provided, then attribute will be calculated between the values specified in
#' htmin and htmax.
#'
#' @param species:
#' Optional vector containing species codes. If species are provided then
#' attribute will be calculated for species entered in select_species argument.
#' Attribute will be calculated for all species if select_species is left as 
#' NULL.
#' 
#' @param dbhmin:
#' Numeric value corresponding to lower DBH bound to calculate attribute in. 
#' This value is inclusive (>=).
#'
#' @param dbhmax: 
#' Numeric value corresponding to upper DBH bound to calculate attribute in. 
#' This value is exclusive (<).
#' 
#' @param htmin:
#' Numeric value corresponding to lower tree height bound to calculate attribute
#' in. This value is inclusive (>=). This argument is only used if ht argument 
#' is specified.
#
#' @param htmax: 
#' Numeric value corresponding to upper tree height bound to calculate attribute
#' in. This value is exclusive (<). This argument is only used if ht argument 
#' is specified.
#' 
#' @param select_species
#' Optional vector containing species codes. This variable will be used to
#' select which species get included in calculation of attribute. If left as
#' NULL, attribute will be calculated using observations from across all 
#' species.
#' 
#' @return
#' Numeric basal area per acre value
################################################################################
