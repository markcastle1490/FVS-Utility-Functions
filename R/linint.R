################################################################################   
#'linint
#'@name linint
#'@description
#'This function is used to mimic the linint function from the FVS event monitor.
#
#'@param value:
#'Input value (numeric) that is being evaluated by the linint interpolation
#' ogic.
#
#'@param x:     
#'Numeric vector containing the x variables evaluated in the linint function.
#
#'@param y:     
#'Numeric vector containing the x variables evaluated in the linint function.
#
#'@param verbose: 
#'Logical variable used to determine if debug output should be echoed to 
#'console.
#
#'@return
#'Numeric value determined by the linear interpolation.
################################################################################

#'@export
linint <- function(value = 0, 
                   x = NULL, 
                   y = NULL,
                   verbose = FALSE)
{
  algslp = 0.0
  
  #If x and y are empty or not the same length, return
  if(length(x) <= 0 || length(y) <= 0 || (length(x) != length(y)))
    return(algslp)
  
  #If x, y and z are not numeric, return
  if(!is.numeric(value) || !is.numeric(x) || !is.numeric(y)) 
    return(algslp)

  #If value is less than first value in x, return the first value in y
  if(value < x[1])
    algslp = y[1]
  
  #If value is greater than or equal to last value in x, return the last value
  #in y
  else if(value >= x[length(x)])
    algslp = y[length(y)]
  
  #Begin interpolation logic
  else
  {
    #Loop across length of x vector minus one.
    for(i in 1:(length(x) - 1))
    {
      if(verbose)
        cat("value:", value, "\n",
            "i:", i, "\n",
            "x[i]:", x[i], "\n",
            "y[i]:", y[i], "\n",
            "x[i + 1]:", x[i + 1], "\n",
            "y[i + 1]:", y[i + 1], "\n", "\n")
      
      #If value is greater than or equal to next value in x, skip the next
      #iteration of the loop
      if(value >= x[i + 1]) next
      
      #Else, retrieve the interpolated value and break from loop
      algslp = y[i] + ((y[i + 1] - y[i])/(x[i + 1] - x[i])) * (value - x[i])
      if(verbose)
        cat("y[i] + ((y[i + 1] - y[i])/(x[i + 1] - x[i])) * (value - x[i]) = ", 
            algslp, "\n")
      break
    }
  }
  
  return(algslp)
}

# linint(value = 5,
#        x = c(0,  5, 5, 99),
#        y = c(.5,.5, 2, 2))
# 
# algslp = 0.5+((0.5-0.5)/(5-0))*(3-0);algslp