## HelperFunctions.R
#' Contains little functions that may be called from other functions.

subtract.bounded <- function(x,y,lower.bound=-Inf,upper.bound=Inf){
  # calculates (x-y)
  # if result is < lower.bound, result is lower.bound
  # if result is > upper.bound, result is upper.bound
  min(max(c((x-y), lower.bound)), upper.bound)
}
