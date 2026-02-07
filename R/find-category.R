################################################################################
#findCategory
#
#NOTE: This function was formally used by the vegClass R package and is saved
#here in case it is somehow useful for future work. This function is not 
#currently used anywhere in the fvsUtil package.
#
#This function returns a category or classification from input argument
#validOutputs based on the value of input argument x. The following logic
#is used to determine a value from validOutputs using x and/or inputValues
#Arguments
#
#1) If inputValues has only one value, then first value in outputValues is
#   returned.
#
#2) If x is less than first item in inputValues, then value specified in
#   invalidReturn arugment is returned. If useLowerBound is true, then
#   value in the first item of outputValues is returned.
#
#3) If x is greater than last value in inputValues, the last item in
#   validOutputs is returned.
#
#4) When 1 - 3 are not true, then x is compared against all ith and ith+1
#   values in inputValues. If x is a value GE to ith value and LT i-th + 1
#   value in inputValues, then ith value in outputValues is returned.
#
#
#x:             Incoming value to evaluate. Must be numeric and not be an NA
#               value.
#
#inputValues:   Values to compare x against. This argument has to be the same
#               length as outputValues. There must be at least 2 values
#               specified in this argument.
#
#outputValues:  Classification values to return containing depending on value
#               x. This argument has to be the same length as inputValues.
#
#invalidReturn: Default value to return when:
#               1) x is not a valid value
#               2) InputValues is empty, a character vector, or contains any
#                  NA values.
#               3) Length of inputValues does not equal outputValues.
#
#Return value
#
#Numeric (or integer) category or classification
################################################################################

findCategory<-function(x,
                       inputValues = 0,
                       outputValues = 0)
{
  
  #Initialize return category (cat) to invalidReturn
  cat = NA
  
  #If x is character or NA, return cat.
  if(is.character(x) | is.na(x))
  {
    return(cat)
  }
  
  #If InputValues is less than length 1, a character vector, or contains any NA
  #values, then return cat.
  if(length(inputValues) < 2 | is.character(inputValues) | NA %in% inputValues)
  {
    return(cat)
  }
  
  #===========================
  #Look for category
  #===========================
  
  done = F
  i = 1
  while(done == F)
  {
    
    #If at the end of inputValues vector, then done becomes true. This is
    #a precautionary condition if a valid output is not found for x during
    #search.
    if(i == length(inputValues))
    {
      done = T
    }
    
    #If x is between the ith and ith+ 1 items in inputValues, then cat is
    #assigned the ith value in outputValues vector and done becomes true.
    else if(x >= inputValues[i] & x < inputValues[i + 1])
    {
      cat = outputValues[i]
      done = T
    }
    
    #Else keep searching
    else
    {
      i = i + 1
    }
  }
  
  #Case where cat is still NA
  if(is.na(cat))
  {
    if(x < inputValues[1]) cat <- outputValues[1]
    if(x > inputValues[1]) cat <- outputValues[length(outputValues)]
  }
  
  return(cat)
}
