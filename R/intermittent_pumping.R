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
  #' Qs <- intermittent_pumping(t = seq(0, 1000, 5),
  #'  starts = seq(0, 900, 10), stops = seq(9, 909, 10), rates = seq(1, 1000, length.out=91),
  #'  method = "hunt", d = 100, S = 0.1, Tr = 100, lmda = 10)
  #'
  #' Qs <- intermittent_pumping(t = seq(0, 1000, 5),
  #'  starts = seq(0, 900, 10), stops = seq(9, 909, 10), rates = seq(1, 1000, length.out=91),
  #'  method = "hunt", d = 100, S = 0.1, Tr = 100, lmda = 100000, lmda_max = 10)
  #' @references
  #' Jenkins, C.T. (1968). Techniques for Computing Rate and Volume of Stream Depletion
  #' by Wells. Ground Water 6(2): 37-46. doi:10.1111/j.1745-6584.1968.tb01641.x
  #' @export

  # make matrices for computations: 1 column per start/stop/rate combo
  starts.all <- base::matrix(starts, nrow = length(t), ncol = length(starts), byrow = T)
  stops.all <- base::matrix(stops, nrow = length(t), ncol = length(starts), byrow = T)
  rates.all <- base::matrix(rates, nrow = length(t), ncol = length(starts), byrow = T)
  t.all <- base::matrix(t, nrow = length(t), ncol = length(starts))

  # calculate time since each pumping interval starts/stops, bounded at 0
  t.starts <- t.all - starts.all
  t.starts[t.starts < 0] <- 0

  t.stops <- t.all - stops.all
  t.stops[t.stops < 0] <- 0

  # vectorize for calculations
  t.starts.vec <- c(t.starts)
  t.stops.vec <- c(t.stops)
  rates.all.vec <- c(rates.all)
  Qs.all.vec <- rep(0, length(t.starts.vec))

  # select analytical model and calculate depletion
  if (method == "glover") {

    # calculate depletion
    Qs.all.vec[t.starts.vec > 0] <-
      rates.all.vec[t.starts.vec > 0] *
        (glover(t = t.starts.vec[t.starts.vec > 0], d = d, S = S, Tr = Tr) -
          glover(t = t.stops.vec[t.starts.vec > 0], d = d, S = S, Tr = Tr))
  } else if (method == "hunt") {
    # extract lmda
    if (!exists("lmda", where = list(...))) stop("Need to supply lmda value for Hunt model")
    lmda <- list(...)$lmda
    if (exists("lmda_max", where = list(...))) {
      lmda_max <- list(...)$lmda_max
    } else {
      lmda_max <- Inf
    }


    # calculate depletion
    Qs.all.vec[t.starts.vec > 0] <-
      rates.all.vec[t.starts.vec > 0] *
        (hunt(t = t.starts.vec[t.starts.vec > 0], d = d, S = S, Tr = Tr, lmda = lmda, lmda_max = lmda_max) -
          hunt(t = t.stops.vec[t.starts.vec > 0], d = d, S = S, Tr = Tr, lmda = lmda, lmda_max = lmda_max))
  }

  # convert back to matrix and take rowsums
  Qs.all <- matrix(Qs.all.vec, nrow = length(t), ncol = length(starts))
  Q.out <- rowSums(Qs.all)
}
