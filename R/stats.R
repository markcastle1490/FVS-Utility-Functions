################################################################################
#'per_dif
#' @name per_dif
#' @description
#' This function is used to calculate percent difference between two numeric
#' values, x and y.
#' 
#' percent difference = abs(x - y) / ((x + y)/2) * 100
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
  
  #Take absolute value of x and y
  x = abs(x)
  y = abs(y)
  
  #Set valid to FALSE if needed
  if(is.na(x) || is.na(y) || (x + y <= 0)) valid = FALSE
  
  #Calculate percent difference if valid
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
#' percent change = ((y - x) / abs(x)) * 100
#
#' @param x:
#' numeric value. This is treated as the baseline value.
#
#' @param y: 
#' numeric value. This is treated as the new value.
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
  
  #Set valid to FALSE if needed
  if(is.na(x) || is.na(y) || x == 0) valid = FALSE

  #Calculate percent change if valid
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
#' Percent absolute bias (PABIAS)
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
error_fun <- function(obs = NULL,
                      pred = NULL)
{
  #Setup fit_stats data frame
  fit_stats <- data.frame(N = 0,
                          BS_MEAN = 0,
                          PRED_MEAN = 0,
                          RMSE = 0,
                          PRMSE = 0,
                          MBIAS = 0,
                          PBIAS = 0,
                          ABIAS = 0,
                          PABIAS = 0)
  
  #Return of obs and pred are null or are unequal in length
  if(is.null(obs) || is.null(pred) || (length(obs) != length(pred)))
    return(fit_stats)
  
  #Obtain mean observed and predicted
  mean_obs = mean(obs)
  mean_pred = mean(pred)
  
  #Define number of observations
  n <- length(obs)
  
  #Root mean square error
  rmse <- sqrt((sum((pred - obs)^2) / (n)))
  
  #Percent root mean square error
  prmse <- rmse / mean_obs * 100
  
  #Mean bias
  mbias <- sum((pred - obs)) / n
  
  #Percent bias
  #pbias= sum(pred-obs)/sum(obs)*100
  pbias <- (sum((pred - obs) / obs) * 100) / n
  
  #Absolute bias
  abias <- sum(abs(pred - obs)) / n
  
  #Percent absolute bias
  #pabias=sum(abs(pred-obs))/sum(obs)*100
  pabias <- (sum(abs(pred - obs) / obs) * 100) / n
  
  #Vector of fit statistics
  fit_stats$N = round(n,0)
  fit_stats$OBS_MEAN = round(mean_obs, 4)
  fit_stats$PRED_MEAN = round(mean_pred, 4)
  fit_stats$RMSE = round(rmse, 4)
  fit_stats$PRMSE = round(prmse, 4)
  fit_stats$MBIAS = round(mbias, 4)
  fit_stats$PBIAS = round(pbias, 4)
  fit_stats$ABIAS = round(abias, 4)
  fit_stats$PABIAS = round(pabias, 4)
  
  return(fit_stats)
}
