depletion_max_distance <- function(Qf_thres = 0.01, d_interval = 100, d_min = NULL, d_max = 5000, method = "glover", t, S, Tr, ...) {
  #' Calculate maximum distance at which streamflow depletion will exceed a user-selected threshold.
  #'
  #' Note that this only considers a single stream - depletion apportionment does not occur.  #'
  #' @param Qf_thres streamflow depletion fraction (\code{Qf}) threshold used to define maximum distance. Defaults to 0.01 (1\%).
  #' @param d_interval interval to use for testing; this defines the spatial resolution at which output will be returned [L]
  #' @param d_min minimum search distance [L]. If `Qf` < `Qf_thres` at `d_min`, function will return `d_min` and a warning.
  #' If `d_min`=NULL (default), `d_min` will be set to `d_interval'`
  #' @param d_max maximum search distance [L]. If `Qf` > `Qf_thres` at `d_max`, function will return `d_max` and a warning.
  #' @param t time you want output for [T]
  #' @param method analytical solution to use (options= 'glover', 'hunt'). Defaults to 'glover'.
  #' @param S aquifer storage coefficient (specific yield if unconfined; storativity if confined)
  #' @param Tr aquifer transmissivity [L2/T]
  #' @param ... any other inputs required for your \code{method} of choice; for example, \code{hunt} needs \code{lmda} (streambed conductance)
  #' @return A numeric of the distance at which streamflow depletion fraction (\code{Qf}) drops below the threshold at time `t`.
  #' @examples
  #' depletion_max_distance(method = "glover", t = 730, S = 0.1, Tr = 100)
  #' depletion_max_distance(Qf_thres = 0.001, method = "glover", t = 730, S = 0.1, Tr = 100)
  #' depletion_max_distance(Qf_thres = 0.001, method = "hunt", t = 730, S = 0.1, Tr = 100, lmda = 0.01)
  #' depletion_max_distance(Qf_thres = 0.001, method = "hunt", t = 7300, S = 0.1, Tr = 100, lmda = 0.01)
  #' @export

  # initial conditions
  Qf <- 1
  if (is.null(d_min)) d_min <- d_interval
  d_test <- d_min

  # select analytical model and calculate depletion
  if (method == "glover") {
    # loop through distances, starting at d_interval, until Qf is < Qf_thres
    while (Qf > Qf_thres) {
      # calculate depletion
      Qf <- glover(t = t, d = d_test, S = S, Tr = Tr)

      if (Qf < Qf_thres) {
        if (d_test == d_min) {
          warning("Qf < Qf_thres at distance d_min")
          return(d_min)
        } else {
          return((d_test - d_interval)) # since you exceeded the threshold, return the previous d_test
        }
      }

      # increase test distance
      d_test <- d_test + d_interval

      # check if max distance reached
      if (d_test > d_max) {
        warning(paste0("Maximum distance reached; Qf = ", round(Qf, 4), " @ d = ", (d_test - d_interval)))
        return(d_max)
      }
    }
  } else if (method == "hunt") {
    # extract lmda
    lmda <- list(...)$lmda
    if (exists("lmda_max", where = list(...))) {
      lmda_max <- list(...)$lmda_max
    } else {
      lmda_max <- Inf
    }

    while (Qf > Qf_thres) {
      # calculate depletion
      Qf <- hunt(t = t, d = d_test, S = S, Tr = Tr, lmda = lmda, lmda_max = lmda_max)

      # check if Qf < Qf_thres
      if (Qf < Qf_thres) {
        if (d_test == d_min) {
          warning("Qf < Qf_thres at distance d_min")
          return(d_min)
        } else {
          return((d_test - d_interval)) # since you exceeded the threshold, return the previous d_test
        }
      }

      # increase test distance
      d_test <- d_test + d_interval

      # check if max distance reached
      if (d_test > d_max) {
        warning(paste0("Maximum distance reached; Qf = ", round(Qf, 4), " @ d = ", (d_test - d_interval)))
        return(d_max)
      }
    }
  }
}
