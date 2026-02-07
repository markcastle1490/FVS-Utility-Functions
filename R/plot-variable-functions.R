################################################################################
#This file contains a suite of functions that can be used to derive competition
#and density attributes. The general expectation when using these functions is 
#that the user is working with a tree-level dataset that contains attributes 
#including DBH, expansion factors, species, total tree height, and others 
#relevant as described in the documentation for each function. The attributes 
#are generally calculated from input numeric and character vectors and other 
#additional arguments. Many of the attributes in this file can be calculated for
#custom size ranges (DBH and total tree height) and desired species.
#
#Development notes: 
#The functions in this file rely heavily on vectorized base-R functions (sum, 
#mean, weighted.mean, etc.) to derive competition and density attributes. The 
#vectorized base-R functions are optimized for R, however faster implementations
#of the functions in this file could likely be achieved by writing functions 
#in low level language like C++, FORTRAN, etc. and called from an API in R. 
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
for_constant = 0.005454154
reineke_slope = 1.605

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
  
  #Check optional vectors
  if(is.null(ht)) ht = 0
  if(is.null(species)) species = 'ALL'
  all_species = TRUE
  if(!is.null(select_species)) all_species = FALSE
  
  #Identify records to include in calculation
  include = (dbh >= dbhmin & dbh < dbhmax) & (ht >= htmin & ht < htmax) &
    (all_species | species %in% select_species)
  
  #Calculate BA over DBH, HT, and species
  if(TRUE %in% include)
    ba_ = sum((dbh^2)[include] * expf[include] * for_constant, na.rm = TRUE)
  
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
  
  #Check optional vectors.
  if(is.null(dbh)) dbh = 0
  if(is.null(ht)) ht = 0
  if(is.null(species)) species = 'ALL'
  all_species = TRUE
  if(!is.null(select_species)) all_species = FALSE
  
  #Identify records to include in calculation
  include = (dbh >= dbhmin & dbh < dbhmax) & (ht >= htmin & ht < htmax) &
    (all_species | species %in% select_species)

  #Calculate TPA Over DBH, HT, and species
  if(TRUE %in% include)
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
  
  #Return if sum of expf is 0
  if(sum(expf, na.rm = TRUE) <= 0) return(qmd_)
  
  #Check optional vectors.
  if(is.null(ht)) ht = 0
  if(is.null(species)) species = 'ALL'
  all_species = TRUE
  if(!is.null(select_species)) all_species = FALSE
  
  #Identify records to include in calculation
  include = (dbh >= dbhmin & dbh < dbhmax) & (ht >= htmin & ht < htmax) &
    (all_species | species %in% select_species)
  
  #Calculate QMD over DBH, HT, and species
  if(TRUE %in% include)
    qmd_ = sqrt(sum((dbh^2*expf)[include], na.rm = TRUE) / sum(expf[include],
                                                                na.rm = TRUE))
  
  #Capture bad values
  if(is.na(qmd_)) qmd_ = 0
  
  #Return qmd
  return(qmd_)
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
  rsdi = tpa_ * (qmd_/10)^reineke_slope
  
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
  
  #Check optional vectors.
  if(is.null(ht)) ht = 0
  if(is.null(species)) species = 'ALL'
  all_species = TRUE
  if(!is.null(select_species)) all_species = FALSE
  
  #Identify records to include in calculation
  include = (dbh >= dbhmin & dbh < dbhmax) & (ht >= htmin & ht < htmax) &
    (all_species | species %in% select_species)
  
  #Calculate ZSDI over DBH, HT, and species
  if(TRUE %in% include)
    zsdi_ = sum(((dbh/10)^reineke_slope)[include] * expf[include], na.rm = TRUE)
  
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
  
  #Check optional vectors.
  if(is.null(dbh)) dbh = 0
  if(is.null(ht)) ht = 0
  if(is.null(species)) species = 'ALL'
  all_species = TRUE
  if(!is.null(select_species)) all_species = FALSE
  
  #Identify records to include in calculation
  include = (dbh >= dbhmin & dbh < dbhmax) & (ht >= htmin & ht < htmax) &
    (all_species | species %in% select_species)
  
  #Calculate CC over DBH, HT, and species
  if(TRUE %in% include)
    cc_ = sum(((crwidth/2)^2)[include] * (expf/43560)[include], na.rm = TRUE) * 
    pi * 100
  
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
  #Return if dbh or expf is NULL
  if(is.null(dbh) || is.null(expf)) return(0)
  
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
    bal = cumsum((dbh^2)[dbh_order] * expf[dbh_order] * for_constant) -
      (dbh^2)[dbh_order] * expf[dbh_order] * for_constant
  }
  
  #Handle ties.
  else
  {
    #Setup temp_dbh vector and then replace duplicate values with 0
    temp_dbh = dbh
    temp_dbh[dbh_order][duplicated(temp_dbh[dbh_order])] = 0

    #Do a cumulative sum of basal area and then subtract ba of tree from each 
    #record. Note use of temp_dbh in cumulative sum and dbh in subtraction.
    bal = cumsum((temp_dbh^2)[dbh_order] * expf[dbh_order] * for_constant) -
      (dbh^2)[dbh_order] * expf[dbh_order] * for_constant
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
#' Reineke SDI calculated using stage formulation.
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
  
  #Check optional vectors
  if(is.null(ht)) ht = 0
  if(is.null(species)) species = 'ALL'
  all_species = TRUE
  if(!is.null(select_species)) all_species = FALSE

  #Calculate stand level tpa and dbhsq. Also initialize qmd
  stand_tpa = sum(expf)
  dbhsq = sum(dbh^2 * expf)
  qmd = 0
  
  #Return if stand_tpa is less than or equal to 0
  if(stand_tpa <= 0) return(rsdi_)
  
  #Initialize a and b parameters
  a = 10^(-1.605) * (1-(1.605/2)) * (dbhsq/stand_tpa)^(1.605/2)
  b = 10^(-1.605) * (1.605/2) * (dbhsq/stand_tpa)^(1.605/2 - 1)
  
  #Identify records to include in calculation
  include = (dbh >= dbhmin & dbh < dbhmax) & (ht >= htmin & ht < htmax) &
    (all_species | species %in% select_species)
  
  #Calculate RSDI over DBH, HT, and species
  if(TRUE %in% include)
    rsdi_ = sum((a*expf)[include], (b * dbh^2 * expf)[include], na.rm = TRUE)
  
  #Capture bad values
  if(is.na(rsdi_)) rsdi_ = 0
  
  #Return rsdi
  return(rsdi_)
}

################################################################################
#'top_ht
#'@name top_ht
#'@description
#'
#'This function is used to calculate top height for a specified percentage of
#'trees in the stand or and explicit TPA value. This value is calculated from a
#'set of input vectors containing DBH values, expansion factors, and tree 
#'heights.
#
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
  
  #Validate top_tpa
  if(is.null(top_tpa) || top_tpa < 0) top_tpa = 40
  
  #Validate top_per. If top_per is not null but has an invalid value, set it to 
  #20%.
  if(!is.null(top_per) && (top_per < 0 || top_per > 100)) top_per = 20
  
  #Calculate TPA for the entire stand
  tpa_ = tpa(dbh = dbh, expf = expf)
  
  #If tpa_ is 0, then return
  if(tpa_ <= 0) return(top_ht_)
  
  #Determine amount TPA value that will be included in top height calculation
  top = top_tpa
  if(top > tpa_) top = tpa_
  if(!is.null(top_per))
  {
    top = tpa_ * (top_per/100)
  }
  
  #If top >= tpa_, calculate top height for all trees
  if(top >= tpa_)
  {
    top_ht_ = avg_attr(dbh = dbh, attr = ht, weight = expf, avgtype = 2)
  }
  
  #Calculate top height for trees in top  
  else
  {
    #Get order of DBH values in descending order
    dbh_order = order(-dbh)
    
    #Find the index where top is exceeded
    top_exceeded = which.max(cumsum(expf[dbh_order]) >= top)
    
    #Sum expf up to this index
    tpa_sum = sum(expf[dbh_order][1:top_exceeded], na.rm = TRUE)
    
    #Calculate tpa_dif and adjust tpa_sum
    tpa_dif = tpa_sum - top
    tpa_sum = tpa_sum - tpa_dif
    
    #Do the top height calculation for trees in top
    ht_sum = sum((ht*expf)[dbh_order][1:top_exceeded-1], na.rm = TRUE) + 
      (ht)[dbh_order][top_exceeded] * (expf[dbh_order][top_exceeded] - 
                                              tpa_dif)
    top_ht_ = ht_sum/tpa_sum
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
#' 2 = average diameter weighted by TPA
#
#' @return 
#' Top diameter weighted by TPA or QMD.
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
  
  #Validate top_tpa
  if(is.null(top_tpa) || top_tpa < 0) top_tpa = 40
  
  #Validate top_per. If top_per is not null but has an invalid value, set it to 
  #0.
  if(!is.null(top_per)) 
  {
    if(top_per < 0 || top_per > 100) top_per = 20
  }
  
  #Validate dia_type
  if(!dia_type %in% c(1, 2)) dia_type = 1
  
  #Calculate TPA for the entire stand
  tpa_ = tpa(dbh = dbh, expf = expf)
  
  #If tpa_ is 0, then return
  if(tpa_ <= 0) return(top_dia_)
  
  #Determine amount TPA value that will be included in top height calculation
  top = top_tpa
  if(top > tpa_) top = tpa_
  if(!is.null(top_per))top = tpa_ * (top_per/100)

  #If top >= tpa_, calculate top diameter for all trees
  if(top >= tpa_)
  {
    #QMD
    if(dia_type == 1)
    {
      top_dia_ = qmd(dbh = dbh, expf = expf)
    }
    
    #Average diameter weighted by TPA
    else
    {
      top_dia_ = avg_attr(dbh = dbh, attr = dbh, weight = expf, avgtype = 2)
    }
  }
  
  #Calculate top diameter for trees in top  
  else
  {
    #Get order of DBH values in descending order
    dbh_order = order(-dbh)
    
    #Find the index where top is exceeded
    top_exceeded = which.max(cumsum(expf[dbh_order]) >= top)
    
    #Sum expf up to this index
    tpa_sum = sum(expf[dbh_order][1:top_exceeded], na.rm = TRUE)
    
    #Calculate tpa_dif and adjust tpa_sum
    tpa_dif = tpa_sum - top
    tpa_sum = tpa_sum - tpa_dif
    
    #QMD
    if(dia_type == 1) {
      dbh_sum = sum((dbh^2*expf)[dbh_order][1:top_exceeded-1], na.rm = TRUE) + 
      (dbh^2)[dbh_order][top_exceeded] * (expf[dbh_order][top_exceeded] - 
                                            tpa_dif)
      
      top_dia_ = sqrt(dbh_sum/tpa_sum)
    }
    
    #Average diameter weighted by TPA
    else
    {
      dbh_sum = sum((dbh*expf)[dbh_order][1:top_exceeded-1], na.rm = TRUE) + 
        (dbh)[dbh_order][top_exceeded] * (expf[dbh_order][top_exceeded] - 
                                            tpa_dif)
      
      top_dia_ = dbh_sum/tpa_sum
    }
  }
  
  return(top_dia_)
}

################################################################################
#' avg_attr
#' @name avg_attr
#' @description
#'
#' This function is used to calculate the average of an attribute. The average
#' can be a arithmetic average or weighted average. This average can be 
#' calculated within custom size ranges and for select species.
#'
#' @param attr:
#' Numeric vector containing numeric attribute
#'
#' @param weight:     
#' Numeric numeric vector containing a weighting value. This could be an 
#' expansion factor or tree basal area, or other user defined weight. This 
#' argument will only be used when avgtype is 2.
#' 
#' @param avgtype: 
#' Integer value corresponding to the type of average to calculate. This argument
#' will be set to 1, if input is not a value of 1 or 2.
#' 1 = average unweighted 
#' 2 = weighted average based on weight argument
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
avg_attr = function(attr = NULL,
                    weight = NULL,
                    avgtype = 1,
                    dbh = NULL,
                    ht = NULL,
                    species = NULL,
                    dbhmin = 0,
                    dbhmax = 999,
                    htmin = 0,
                    htmax = 999,
                    select_species = NULL)
{
  avg_attr_ = 0
  input_weights = FALSE
  
  #Check optional vectors.
  if(is.null(dbh)) dbh = 0
  if(is.null(ht)) ht = 0
  if(is.null(species)) species = 'ALL'
  all_species = TRUE
  if(!is.null(select_species)) all_species = FALSE
  if(!is.null(weight)) input_weights = TRUE
  
  #Catch bad values for avgtype
  if(!avgtype %in% c(1, 2)) avgtype = 1
  
  #Identify records to include in calculation
  include = (dbh >= dbhmin & dbh < dbhmax) & (ht >= htmin & ht < htmax) &
    (all_species | species %in% select_species)
  
  #Calculate arithmetic unweighted average if avgtype is 1 or input_weights 
  #FALSE
  if(avgtype == 1 || !input_weights)
  {
    #Arithmetic average over DBH, HT, and Species
    avg_attr_ = mean(attr[include],
                     na.rm = TRUE)
  }

  #Calculate weighted average if avgtype and input_weights TRUE
  else if(avgtype == 2 && input_weights)
  {
    
    #Weighted average over DBH, HT, and Species
    avg_attr_ = weighted.mean(x = attr[include],
                              w = weight[include],
                              na.rm = TRUE)
  }
  
  #Capture bad values
  if(is.na(avg_attr_)) avg_attr_ = 0
  
  return(avg_attr_)
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
  
  #Check optional vectors.
  if(is.null(dbh)) dbh = 0
  if(is.null(ht)) ht = 0
  if(is.null(species)) species = 'ALL'
  all_species = TRUE
  if(!is.null(select_species)) all_species = FALSE
  
  #Identify records to include in calculation
  include = (dbh >= dbhmin & dbh < dbhmax) & (ht >= htmin & ht < htmax) &
    (all_species | species %in% select_species)
  
  #Expand attr over DBH, HT, and species
  if(TRUE %in% include)
    attr_expand_ = sum(attr[include] * expf[include], na.rm = TRUE)
  
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
################################################################################

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
  
  #if attr is NULL, return 0 value. Median value cannot be calculated.
  if(is.null(attr)) return(median_attr_)
  
  #Check optional vectors.
  if(is.null(dbh)) dbh = 0
  if(is.null(ht)) ht = 0
  if(is.null(species)) species = 'ALL'
  all_species = TRUE
  if(!is.null(select_species)) all_species = FALSE
  
  #Identify records to include in calculation
  include = (dbh >= dbhmin & dbh < dbhmax) & (ht >= htmin & ht < htmax) &
    (all_species | species %in% select_species)
  
  #Over DBH, HT, and species
  if(TRUE %in% include)
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
  
  #if attr is NULL, return 0 value. Minimum value cannot be calculated.
  if(is.null(attr)) return(min_attr_)
  
  #Check optional vectors.
  if(is.null(dbh)) dbh = 0
  if(is.null(ht)) ht = 0
  if(is.null(species)) species = 'ALL'
  all_species = TRUE
  if(!is.null(select_species)) all_species = FALSE
  
  #Identify records to include in calculation
  include = (dbh >= dbhmin & dbh < dbhmax) & (ht >= htmin & ht < htmax) &
    (all_species | species %in% select_species)
  
  #Find minimum over DBH, HT, and species
  if(TRUE %in% include)
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
  
  #if attr is NULL, return 0 value. Maximum value cannot be calculated.
  if(is.null(attr)) return(max_attr_)
  
  #Check optional vectors.
  if(is.null(dbh)) dbh = 0
  if(is.null(ht)) ht = 0
  if(is.null(species)) species = 'ALL'
  all_species = TRUE
  if(!is.null(select_species)) all_species = FALSE
  
  #Identify records to include in calculation
  include = (dbh >= dbhmin & dbh < dbhmax) & (ht >= htmin & ht < htmax) &
    (all_species | species %in% select_species)
  
  #Find minimum over DBH, HT, and species
  if(TRUE %in% include)
    max_attr_ = max(attr[include], na.rm = TRUE)
  
  #Capture bad values
  if(is.na(max_attr_)) max_attr_ = 0
  
  return(max_attr_)
}

################################################################################
#' sd_attr
#' @name sd_attr
#' @description
#' 
#' This function determines the standard deviation for an input attribute. This
#' can be calculated for custom size ranges and for select species.
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
#' Standard deviation of attribute.
################################################################################

#'@export
sd_attr = function(attr = NULL,
                   dbh = NULL,
                   ht = NULL,
                   species = NULL,
                   dbhmin = 0,
                   dbhmax = 999,
                   htmin = 0,
                   htmax = 999,
                   select_species = NULL)
{
  
  sd_attr_ = 0
  
  #if attr is NULL, return 0 value. Maximum value cannot be calculated.
  if(is.null(attr)) return(sd_attr_)
  
  #Check optional vectors.
  if(is.null(dbh)) dbh = 0
  if(is.null(ht)) ht = 0
  if(is.null(species)) species = 'ALL'
  all_species = TRUE
  if(!is.null(select_species)) all_species = FALSE
  
  #Identify records to include in calculation
  include = (dbh >= dbhmin & dbh < dbhmax) & (ht >= htmin & ht < htmax) &
    (all_species | species %in% select_species)
  
  #Find minimum over DBH, HT, and species
  if(TRUE %in% include)
    sd_attr_ = sd(attr[include], na.rm = TRUE)
  
  #Capture bad values
  if(is.na(sd_attr_)) sd_attr_ = 0
  
  return(sd_attr_)
}

################################################################################
#' quant_attr
#' @name quant_attr
#' @description
#' 
#' This function determines the value at specific quantile for an input 
#' attribute. This can be calculated for custom size ranges and for select 
#' species.
#' 
#' @param attr:
#' Numeric vector containing numeric attribute
#' 
#' @param prob:
#' Numeric value ranging from 0 - 1. This value corresponds to a percentile 
#' value. A value of 0 would return the minimum value in attr, a value of 0.5
#' would return the median in attr, and a value of 1 would return the maximum
#' value in attr.A value of 0.5 will be assumed if invalid value is provided.
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
#' select which species get included in calculation of attribute.
#'
#'@return
#' Numeric value of attribute at specific quantile.
################################################################################

#'@export
quant_attr = function(attr = NULL,
                      prob = 0.5,
                      dbh = NULL,
                      ht = NULL,
                      species = NULL,
                      dbhmin = 0,
                      dbhmax = 999,
                      htmin = 0,
                      htmax = 999,
                      select_species = NULL)
{
  
  quant_attr_ = 0
  
  #if attr is NULL, return 0 value. Maximum value cannot be calculated.
  if(is.null(attr)) return(quant_attr_)
  
  #Store first value of prob in case multiple were entered
  prob_ = prob[1]
  
  #Handle bad prob value
  if(is.null(prob_) || is.na(prob_) || (prob_ < 0 || prob_ > 1)) prob_ = 0.5
  
  #Check optional vectors.
  if(is.null(dbh)) dbh = 0
  if(is.null(ht)) ht = 0
  if(is.null(species)) species = 'ALL'
  all_species = TRUE
  if(!is.null(select_species)) all_species = FALSE
  
  #Identify records to include in calculation
  include = (dbh >= dbhmin & dbh < dbhmax) & (ht >= htmin & ht < htmax) &
    (all_species | species %in% select_species)
  
  #Find quantile value over DBH, HT, and species
  if(TRUE %in% include)
    quant_attr_ = quantile(attr[include], probs = prob_, na.rm = TRUE)
  
  #Capture bad values
  if(is.na(quant_attr_)) quant_attr_ = 0
  
  return(quant_attr_)
}

################################################################################
#' count_records
#' @name count_attr
#' @description
#' 
#' This function counts the number of tree records between specified DBH and HT
#' ranges and for select species. 
#' 
#' @param id:
#' Vector containing tree ID values. Technically if you don't have tree ids, you
#' can pass any vector into this argument.
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
count_records = function(id = NULL,
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
  
  #Check optional vectors.
  if(is.null(dbh)) dbh = 0
  if(is.null(ht)) ht = 0
  if(is.null(species)) species = 'ALL'
  all_species = TRUE
  if(!is.null(select_species)) all_species = FALSE
  
  #Identify records to include in calculation
  include = (dbh >= dbhmin & dbh < dbhmax) & (ht >= htmin & ht < htmax) &
    (all_species | species %in% select_species)
  
  #Count over DBH, ht, and species
  if(TRUE %in% include)
    count_ = length(id[include])
  
  #Capture bad values
  if(is.na(count_)) count_ = 0
  
  return(count_)
}

################################################################################
#' sdi_max
#' @name sdi_max
#' @description
#'
#' This function is used to calculate SDI max individual species SDI max values 
#' as described in Essential FVS. This is the weighted avarage of species SDI 
#' max value where the weight is basal area.
#'
#' @param dbh:
#' Numeric vector containing DBH values.
#'
#' @param species:
#' Vector containing species codes. 
#'
#' @param sdi:     
#' Numeric vector containing SDI max values (correspond to each species in 
#' species argument).
#'
#' @param expf:     
#' Numeric vector containing a expansion factor values.
#
#' @return 
#' Numeric SDI max value
################################################################################

#'@export
sdi_max <- function(dbh = NULL,
                    species = NULL, 
                    sdi = NULL,
                    expf = NULL)
{
  
  sdi_max_ = 0
  input_hts = FALSE
  
  #Identify unique species codes and setup sp_ba vector
  if(typeof(species) != 'character') species = as.character(species)
  unique_sp = unique(species)
  sp_ba = vector(mode = "numeric", length = length(unique_sp))
  names(sp_ba) = unique(species)
  
  #Return if sp_ba is length 0
  if(length(sp_ba) <=0) return(sdi_max_)
  
  #Calculate BA for each unique species
  for(i in 1:length(sp_ba))
  {
    #Get species
    sp_ = names(sp_ba[i])
    
    #Calculate basal area weighted SDI for all records of this species and 
    #update ba_sum
    sp_ba[sp_] = sum((dbh^2)[species == sp_] * expf[species == sp_] * 
      for_constant * sdi[species == sp_], na.rm = TRUE)
  }
  
  #Calculate BA across all trees
  ba_sum = sum((dbh^2) * expf * for_constant, na.rm = TRUE)
  
  #Calculate sdi_max_ if ba_sum > 0
  if(ba_sum > 0) sdi_max_ = sum(sp_ba) / ba_sum
  
  #If sdi_max_ is NaN or NA set to 0
  if(is.na(sdi_max_)) sdi_max_ = 0
  
  return(sdi_max_)
}

################################################################################
### Previous code that is based on for loop structure STARTS HERE ###
################################################################################

#BA

# #Calculate ba_
# for(i in 1:length(dbh))
# {
#   dbh_ = dbh[i]
#   expf_ = expf[i]
#   if(input_hts) ht_ = ht[i]
#   if(input_species) species_ = species[i]
# 
#   #Skip if dbh is not in bounds
#   if(dbh_ < dbhmin || dbh_ >= dbhmax) next
# 
#   #Skip if ht is not in bounds
#   if(input_hts && (ht_ < htmin || ht_ >= htmax)) next
# 
#   #Skip if not a selected species
#   if(input_species && !is.null(select_species) && !
#      species_ %in% select_species) next
# 
#   ba_ = ba_ + (dbh_^2 * for_constant* expf_)
# }

#TPA

# #Calculate tpa_
# for(i in 1:length(dbh))
# {
#   dbh_ = dbh[i]
#   expf_ = expf[i]
#   if(input_hts) ht_ = ht[i]
#   if(input_species) species_ = species[i]
# 
#   #Skip if dbh is not in bounds
#   if(dbh_ < dbhmin || dbh_ >= dbhmax) next
# 
#   #Skip if ht is not in bounds
#   if(input_hts && (ht_ < htmin || ht_ >= htmax)) next
# 
#   #Skip if not a selected species
#   if(input_species && !is.null(select_species) && !
#      species_ %in% select_species) next
# 
#   tpa_ = tpa_ + expf_
# }
# 

#QMD

# #Calculate dbhsq and tpa_
# for(i in 1:length(dbh))
# {
#   dbh_ = dbh[i]
#   expf_ = expf[i]
#   if(input_hts) ht_ = ht[i]
#   if(input_species) species_ = species[i]
#   
#   #Skip if dbh is not in bounds
#   if(dbh_ < dbhmin || dbh_ >= dbhmax) next
#   
#   #Skip if ht is not in bounds
#   if(input_hts && (ht_ < htmin || ht_ >= htmax)) next 
#   
#   #Skip if not a selected species
#   if(input_species && !is.null(select_species) && ! 
#      species_ %in% select_species) next 
#   
#   dbhsq = dbhsq + (dbh_^2 * expf_)
#   tpa_ = tpa_ + expf_
# }
# 
#Calculate qmd if tpa is greater than 0
#if(tpa_ > 0) qmd_ = sqrt(dbhsq / tpa_)

#ZSDI

# #Calculate ZSDI
# for(i in 1:length(dbh))
# {
#   dbh_ = dbh[i]
#   expf_ = expf[i]
#   if(input_hts) ht_ = ht[i]
#   if(input_species) species_ = species[i]
#   
#   #Skip if dbh is not in bounds
#   if(dbh_ < dbhmin || dbh_ >= dbhmax) next
#   
#   #Skip if ht is not in bounds
#   if(input_hts && (ht_ < htmin || ht_ >= htmax)) next 
#   
#   #Skip if not a selected species
#   if(input_species && !is.null(select_species) && ! 
#      species_ %in% select_species) next 
#   
#   zsdi_ = zsdi_ + ((dbh_/10)^reineke_slope * expf_)
# }

#CC

#For loop algorithm
# #Calculate dbhsq and tpa_
# for(i in 1:length(crwidth))
# {
#   dbh_ = dbh[i]
#   crwidth_ = crwidth[i]
#   expf_ = expf[i]
#   if(input_hts) ht_ = ht[i]
#   if(input_species) species_ = species[i]
#   
#   #Skip if dbh is not in bounds
#   if(dbh_ < dbhmin || dbh_ >= dbhmax) next
#   
#   #Skip if ht is not in bounds
#   if(input_hts && (ht_ < htmin || ht_ >= htmax)) next 
#   
#   #Skip if not a selected species
#   if(input_species && !is.null(select_species) && ! 
#      species_ %in% select_species) next 
#   
#   cc_ = cc_ + (pi * (crwidth_/2)^2 *(expf_/43560) * 100)
# }

#expand_attr

# #Calculate expanded attr
# for(i in 1:length(attr))
# {
#   dbh_ = dbh[i]
#   attr_ = attr[i]
#   expf_ = expf[i]
#   if(input_hts) ht_ = ht[i]
#   if(input_species) species_ = species[i]
#   
#   #Skip if dbh is not in bounds
#   if(dbh_ < dbhmin || dbh_ >= dbhmax) next
#   
#   #Skip if ht is not in bounds
#   if(input_hts && (ht_ < htmin || ht_ >= htmax)) next
#   
#   #Skip if not a selected species
#   if(input_species && !is.null(select_species) && ! 
#      species_ %in% select_species) next 
#   
#   attr_expand = attr_expand + (attr_ * expf_)
# 
# }

#BAL

# #Create temporary dataframe
# temp = data.frame(dbh = dbh,
#                   expf = expf)
# 
# #Create sequence of tree numbers and calculate treeba
# temp$treeid = seq(from = 1, to = length(dbh), by = 1)
# temp$treeba = temp$dbh^2 * for_constant * temp$expf
# 
# #Order dataframe by dbh
# temp = temp[order(-temp$dbh), ]

#temp$BAL = cumsum(temp$treeba)
#temp$BAL = temp$BAL - temp$treeba

#Now reorder the dataframe by treeid
#temp = temp[order(temp$treeid), ]

#RSDI Stage
# if(verbose)
# {
#   cat("TPA:", stand_tpa, "\n")
#   cat("QMD:", qmd, "\n")
#   cat("STAGEA:", a, "\n")
#   cat("STAGEB:", b, "\n", "\n")
# }

# #Now calculate Reineke SDI for the size range
# for(i in 1:length(dbh))
# {
#   #dbh of tree
#   dbh_ = dbh[i]
#   
#   #tpa of tree
#   expf_ = expf[i]
#   
#   #ht of tree
#   if(input_hts) ht_ = ht[i]
#   
#   #Species of tree
#   if(input_species) species_ = species[i]
#   
#   #Skip if dbh is not in bounds
#   if(dbh_ < dbhmin || dbh_ >= dbhmax) next
#   
#   #Skip if ht is not in bounds
#   if(input_hts && (ht_ < htmin || ht_ >= htmax)) next 
#   
#   #Skip if not a selected species
#   if(input_species && !is.null(select_species) && ! 
#      species_ %in% select_species) next 
#   
#   #Add tree contribution to rsdi
#   rsdi = rsdi + (a * expf_ + b * dbh_^2 * expf_)
#   
#   # if(verbose)
#   # {
#   #   cat("PROB:", expf_, "DBH:", dbh_, "RSDI:", rsdi, "\n")
#   # }
# }

#Initialize variables for average
# n = 0
# attr_wt = 0
# attr_sum = 0
# weight_sum = 0

#Avg Attr
# #Calculate the average
# for(i in 1:length(dbh))
# {
#   #dbh of tree
#   dbh_ = dbh[i]
#   
#   #ht of tree
#   if(input_hts) ht_ = ht[i]
#   
#   #Species of tree
#   if(input_species) species_ = species[i]
#   
#   #Skip if dbh is not in bounds
#   if(dbh_ < dbhmin || dbh_ >= dbhmax) next
#   
#   #Skip if ht is not in bounds
#   if(input_hts && (ht_ < htmin || ht_ >= htmax)) next 
#   
#   #Skip if not a selected species
#   if(input_species && !is.null(select_species) && ! 
#      species_ %in% select_species) next 
#   
#   #attr of tree
#   attr_ = attr[i]
#   
#   #weight for tree
#   weight_ = weight[i]
#   
#   #Update attr_wt
#   attr_wt = attr_wt + (attr_ * weight_)
#   
#   #Update attr_sum
#   attr_sum = attr_sum + attr_
#   
#   #Update weight_sum
#   weight_sum = weight_sum + weight_
#   
#   #Update n
#   n = n + 1
# }

  # #Calculate arithmetic average first
  # if(n > 0 ) avg_attr = attr_sum / n
  # 
  # #If weighted average is being calculated, reset avg_attr
  # if(avgtype == 2)
  # {
  #   if(weight_sum > 0) avg_attr = attr_wt / weight_sum
  # }

#TOPQMD (top diameter)
# #Create temporary dataframe
# temp = data.frame(dbh = dbh,
#                   expf = expf)
# 
# #Sort data by dbh in descending order
# temp = temp[order(-temp$dbh), ]
# 
# #Initialize dbhsq and tpa_sum
# dbhsq = 0
# tpa_sum = 0
# 
# #Loop across data and calculate top height
# for(i in 1:nrow(temp))
# {
#   #Get dbh
#   dbh_ = temp$dbh[i]
#   
#   #Add tpa value to tpa_sum
#   tpa_sum = tpa_sum + temp$expf[i]
#   
#   #top has not been met or exceeded. Update ht_tpa
#   if(tpa_sum < top)
#   {
#     dbhsq = dbhsq + dbh_^2 * temp$expf[i]
#   }
#   
#   #top has been met or exceeded 
#   else
#   {
#     #Calculate difference between tpa_sum and top. This will be deducted from
#     #expansion factor tree i and tpa_sum
#     tpa_dif = (tpa_sum - top)
#     tpa_sum = tpa_sum - tpa_dif
#     dbhsq = dbhsq + dbh_^2 * (temp$expf[i] - tpa_dif)
#     
#     #Break out of loop
#     break
#   }
# }
#
#Calculate and return top_dia_
#if(tpa_sum > 0) topQMD_ = sqrt(dbhsq/tpa_sum)

# #Create temporary dataframe
# temp = data.frame(dbh = dbh,
#                   expf = expf,
#                   ht = ht)
# 
# #Sort data by dbh in descending order
# temp = temp[order(-temp$dbh), ]
# 
# #Initialize ht_tpa and tpa_sum
# ht_tpa = 0
# tpa_sum = 0
# 
# #Loop across data and calculate top height
# for(i in 1:nrow(temp))
# {
#   #Get dbh
#   dbh_ = temp$dbh[i]
#   
#   #Get ht
#   ht_ = temp$ht[i]
#   
#   #Add tpa value to tpa_sum
#   tpa_sum = tpa_sum + temp$expf[i]
#   
#   #top has not been met or exceeded. Update ht_tpa
#   if(tpa_sum < top)
#   {
#     ht_tpa = ht_tpa + ht_ * temp$expf[i]
#   }
#   
#   #top has been met or exceeded 
#   else
#   {
#     #Calculate difference between tpa_sum and top. This will be deducted from
#     #expansion factor tree i and tpa_sum
#     tpa_dif = (tpa_sum - top)
#     tpa_sum = tpa_sum - tpa_dif
#     ht_tpa = ht_tpa + ht_ * (temp$expf[i] - tpa_dif)
#     
#     #Break out of loop
#     break
#   }
# }
# 
# #Calculate and return top_ht_
# if(tpa_sum > 0) top_ht_ = ht_tpa/tpa_sum
