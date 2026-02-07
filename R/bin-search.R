################################################################################
#bin_search
#
#Vector Binary Search function pulled from the following webpage:
#https://visualstudiomagazine.com/articles/2016/09/01/r-language-searching-and-sorting.aspx
#
#v:   Vector containing numeric values
#
#t:   Numeric value related to target to search for in argument v.
#
#eps: Acceptable for tolerance for comparing target to value in v argument.
#
#
#Numeric value corresponding to index of target value or 0.
################################################################################

#'@export
bin_search = function(v, 
                      t, 
                      eps = 0.001) {
  lo <- 1; hi <- length(v)
  
  while (lo <= hi) {
    
    mid <- as.integer(round((lo + hi) / 2)) # always even!
    
    if (abs(v[mid] - t) <= eps) 
    {
      return(mid)
    } 
    else if (v[mid] < t) 
    { # C style would be OK
      lo <- mid + 1
    } 
    
    else 
    {
      hi <- mid - 1
    }
  }
  return(0)
}