intermittent_pumping <- function(t, starts, stops, rates, method = "glover", d, S, Tr, ...) {
  #' Streamflow depletion for an intermittent pumping schedule using superposition.
  #'
  #' @param t times you want output for [T]
  #' @param starts vector of times to start pumping [T] (must be same length as stops and rates)
  #' @param stops vector of times pumping stops [T] (must be same length as starts and rates)
  #' @param rates vector of pumping rates [L3/T] (must be same length as starts and stops)
  #' @param method analytical solution to use (options= 'glover', 'hunt'). Defaults to 'glover'.
  #' @param d distance from well to stream [L]
  #' @param S aquifer storage coefficient (specific yield if unconfined; storativity if confined)
  #' @param Tr aquifer transmissivity [L2/T]
  #' @param ... any other inputs required for your \code{method} of choice; for example, \code{hunt} needs \code{lmda} (streambed conductance)
  #' @return A numeric of \code{Qs}, streamflow depletion rate [L3/T]. Unlike the streamflow depletion models
  #' (e.g. \link{glover}, \link{hunt}) this is not fractional depletion (\code{Qf}) because there can
  #' be different pumping rates at different times.
  #' @examples
  #' intermittent_pumping(t = seq(0,60,10), starts = 0, stops = 30,
  #'   rates = 100, method = "glover", d = 100, S = 0.1, Tr = 100)
  #' @references
  #' Jenkins, C.T. (1968). Techniques for Computing Rate and Volume of Stream Depletion
  #' by Wells. Ground Water 6(2): 37-46. doi:10.1111/j.1745-6584.1968.tb01641.x
  #' @export
  
  # make a matrix for computations: 1 column per start/stop/rate combo
  Q.all <- matrix(NaN, nrow = length(t), ncol = length(starts))

  # select analytical model and calculate depletion
  if (method == "glover") {
    for (i in 1:length(starts)) {
      # loop through start/stop/rate sets
      Q.all[, i] <-
        rates[i] * (glover(t = sapply(t, FUN = subtract_bounded, y = starts[i], lower_bound = 0), d = d, S = S, Tr = Tr) -
          glover(t = sapply(t, FUN = subtract_bounded, y = stops[i], lower_bound = 0), d = d, S = S, Tr = Tr))
    }
  } else if (method == "hunt") {
    # extract lmda
    lmda <- list(...)$lmda

    for (i in 1:length(starts)) {
      # loop through start/stop/rate sets
      Q.all[, i] <-
        rates[i] * (hunt(
          t = sapply(t, FUN = subtract_bounded, y = starts[i], lower_bound = 0),
          d = d, S = S, Tr = Tr, lmda = lmda
        ) -
          hunt(
            t = sapply(t, FUN = subtract_bounded, y = stops[i], lower_bound = 0),
            d = d, S = S, Tr = Tr, lmda = lmda
          ))
    }
  }

  # take row sums for output
  Q.out <- rowSums(Q.all)
}
