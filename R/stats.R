################################################################################
#'per_dif
#' @name per_dif
#' @description
#' This function is used to calculate percentage difference between two numeric
#' values, x and y.
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
  percent_dif <- NA
  
  #Set x and y to 0 if they are NA or not numeric
  if(is.na(x) | !is.numeric(x)) x <- 0
  if(is.na(y) | !is.numeric(y)) y <- 0
  
  #Calculate percent difference if x and y are not 0
  if(x != 0 & y != 0)
  {
    percent_dif <- abs(x - y) / ((x + y)/2) * 100
  }
  
  return(percent_dif)
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
