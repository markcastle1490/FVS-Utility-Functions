################################################################################   
#'linint
#'@name linit
#'@description
#'This function is used to mimic the linint function from the FVS event monitor.
#
#'@param value:
#' Input value (numeric) that is being evaluated by the linint interpolation
#' logic.
#
#'@param x:     
#'Numeric Vector containing the x variables evaluated in the linint function.
#
#'@param y:     
#'Numeric Vector containing the x variables evaluated in the linint function.
#
#'@return
#'Numeric value determined by the linear interpolation.
################################################################################ 

#'@export
linint <- function(value, x, y)
{
  #If the length of x and y variables are not equal return with value of 0
  if(length(x) != length(y))
  {
    algslp = 0
    return(algslp)
  }
  
  #If value is less than first value in x, return the first value in y
  if(value < x[1])
  {
    algslp = y[1]
  }
  
  #If value is greater than or equal to last value in x, return the last value
  #in y
  else if(value >= x[length(x)])
  {
    algslp = y[length(y)]
  }
  
  #Begin interpolation logic
  else
  {
    #Loop across length of x vector minus one.
    for(i in 1:length(x) - 1)
    {
      #If value is greater than or equal to next value in x, skip the next
      #iteration of the loop
      if(value >= x[i + 1]) next
      
      #Else, retrieve the interpolated value and break from loop
      algslp = y[i]+((y[i+1]-y[i])/(x[i+1]-x[i]))*(value-x[i])
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