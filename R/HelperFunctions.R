## HelperFunctions.R
#' Contains little functions that may be called from other functions.

subtract_bounded <- function(x, y, lower_bound = -Inf, upper_bound = Inf) {
  #' subtraction bounded with lower/upper limits
  #'
  #' Called from \code{intermittentPumping}
  #'
  #' @param x first term in subtraction
  #' @param y second term in subtraction
  #' @param lower_bound minimum allowed result. Default to Inf.
  #' @param upper_bound maximum allowed result. Default to -Inf.
  #' @return (x-y), within the limits of lower_bound and upper_bound
  #' @examples
  #' subtract_bounded(10, 15)  # returns -5; default bounds are \code{-Inf, Inf}
  #' subtract_bounded(10, 15, lower_bound=0)  # returns 0 due to lower_bound
  #' @export
  min(max(c((x - y), lower_bound)), upper_bound)
}
