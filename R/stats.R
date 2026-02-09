################################################################################
#'per_dif
#' @name per_dif
#' @description
#' This function is used to calculate percentage difference between two numeric
#' values, x and y.
#' 
#' per_dif_ = abs(x - y) / ((x + y)/2) * 100
#
#' @param x:
#' numeric value.
#
#' @param x: 
#' numeric value.
#
#' @return 
#' Percent difference between x and y.
################################################################################

#'@export
per_dif <- function(x,
                    y)
{
  #Initialize per_dif_ and valid
  per_dif_ = NA
  valid = TRUE
  
  #Cast to numeric if needed
  if(!is.numeric(x)) x = suppressWarnings(is.numeric(x))
  if(!is.numeric(y)) y = suppressWarnings(is.numeric(y))
  
  #If x or y is less than 0, multiply by -1
  if(x < 0) x = -x
  if(y < 0) y = -y
  
  #Set valid to FALSE if x and y are NA
  if(is.na(x)) valid = FALSE
  if(is.na(y)) valid = FALSE
  
  #Set valid to FALSE if sum of x and y is 0
  if(x + y <= 0) valid = FALSE
  
  #Calculate percent difference
  if(valid)
    per_dif_ = abs(x - y) / ((x + y)/2) * 100
  
  return(per_dif_)
}

################################################################################
#'per_change
#' @name per_change
#' @description
#' This function is used to calculate percentage change between two numeric
#' values, x and y.
#' 
#' per_change_ = ((y - x) / abs(x)) * 100
#
#' @param x:
#' numeric value. This is treated as the old/initial value.
#
#' @param y: 
#' numeric value. This is treated as the new/end value.
#
#' @return 
#' Percent change between x and y.
################################################################################

#'@export
per_change <- function(x,
                       y)
{
  #Initialize per_change_ and valid
  per_change_ = NA
  valid = TRUE
  
  #Cast to numeric if needed
  if(!is.numeric(x)) x = suppressWarnings(is.numeric(x))
  if(!is.numeric(y)) y = suppressWarnings(is.numeric(y))
  
  #Set valid to FALSE if x and y are NA
  if(is.na(x)) valid = FALSE
  if(is.na(y)) valid = FALSE
  
  #Calculate percent change
  if(valid)
    per_change_ = ((y - x) / abs(x)) * 100
  
  return(per_change_)
}

################################################################################
#'error_fun
#' @name error_fun
#' @description
#'
#' Function error_fun calculates the following fit statistic metrics:
#' Number of observations (N)
#' Observed mean response (OBS_MEAN)
#' Predicted mean response (PRED_MEAN)
#' Root mean square error (RMSE)
#' Percent root mean square error (PRMSE)
#' Mean bias (MBIAS)
#' Absolute bias (ABIAS)
#' Relative absolute bias (PABIAS)
#' Percent bias (PBIAS)
#
#' @param obs:
#' Numeric vector of observed response variable
#
#' @param pred:
#' Numeric vector of predicted response variable
#
#' @return 
#' Dataframe containing N, OBS_MEAN, PRED_MEAN, RMSE, PRMSE, MBIAS, ABIAS,
#' PABIAS, and PBIAS.
################################################################################

#'@export
error_fun <- function(obs,
                     pred)
{
  #Define number of obserations
  n <- length(obs)
  
  #Root mean square error
  rmse <- sqrt((sum((pred-obs)^2)/(n)))
  
  #Percent root mean square error
  prmse<- rmse/mean(obs)*100
  
  #Mean bias
  mbias <- sum((pred-obs))/n
  
  #Relative bias
  #pbias= sum(pred-obs)/sum(obs)*100
  pbias <- (sum((pred-obs) / obs) * 100)/n
  
  #Absolute bias
  abias <- sum(abs(pred-obs))/n
  
  #Relative absolute bias
  #pabias=sum(abs(pred-obs))/sum(obs)*100
  pabias <- (sum(abs(pred-obs) / obs) * 100)/n
  
  #Vector of fit statistics
  fitStats <- data.frame(N = round(n,0),
                         OBS_MEAN = round((mean(obs)),4),
                         PRED_MEAN = round((mean(pred)),4),
                         RMSE = round(rmse,4),
                         PRMSE = round(prmse,4),
                         MBIAS = round(mbias,4),
                         PBIAS = round(pbias,4),
                         ABIAS = round(abias,4),
                         PABIAS = round(pabias,4))
  
  return(fitStats)
}
