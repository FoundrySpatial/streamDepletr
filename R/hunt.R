hunt <- function(t, d, S, Tr, lmda, lmda_max = Inf, prec = 80) {
  #' Streamflow depletion in partially penetrating stream with semipervious streambed.
  #'
  #' Described in Hunt (1999). When \code{lmda} term gets very large, this is equivalent to \code{glover}.
  #' Assumptions:
  #' \itemize{
  #'   \item Horizontal flow >> vertical flow (Dupuit assumptions hold)
  #'   \item Homogeneous, isotropic aquifer
  #'   \item Constant \code{Tr}: Aquifer is confined, or if unconfined change in head is small relative to aquifer thickness
  #'   \item Stream is straight, infinitely long, and remains in hydraulic connection to aquifer
  #'   \item Constant stream stage
  #'   \item No changes in recharge due to pumping
  #'   \item No streambank storage
  #'   \item Constant pumping rate
  #'   \item Aquifer extends to infinity
  #' }
  #'
  #' @param t times you want output for [T]
  #' @param d distance from well to stream [L]
  #' @param S aquifer storage coefficient (specific yield if unconfined; storativity if confined)
  #' @param Tr aquifer transmissivity [L2/T]
  #' @param lmda streambed conductance term, lambda [L/T]. Can be estimated with \code{streambed_conductance}.
  #' @param lmda_max maximum allowed `lmda` [L/T]. If `lmda` is too high, exp and erfc calculations in Hunt solution are not computationally possible, so you may need to artifically reduce `lmda` using this term.
  #' @param prec precision for \code{Rmpfr} package for storing huge numbers; 80 seems to generally work but tweak this if you get weird results. Reducing this value will reduce accuracy but speed up computation time.
  #' @return A numeric of \code{Qf}, streamflow depletion as fraction of pumping rate [-].
  #' If the pumping rate of the well (\code{Qw}; [L3/T]) is known, you can calculate volumetric streamflow depletion [L3/T] as \code{Qf*Qw}
  #' @importFrom magrittr %>%
  #' @references
  #' Hunt, B (1999). Unsteady Stream Depletion from Ground Water Pumping.
  #' Ground Water 37 (1): 98-102. doi:10.1111/j.1745-6584.1999.tb00962.x.
  #' @examples
  #' hunt(t = 1826, d = 1000, S = 0.2, Tr = 8640, lmda = 864)    # ~equal to glover because lmda=Tr
  #' hunt(t = 1826, d = 1000, S = 0.2, Tr = 8640, lmda = 0.864)  # less depletion due to lower lmda
  #'
  #' lmda <- streambed_conductance(w = 10, Kriv = 0.0864, briv = 1) # estimate lmda
  #' hunt(t = 1826, d = 1000, S = 0.2, Tr = 8640, lmda = lmda)
  #'
  #' Qf <- hunt(t = seq(1, 1826), d = 1000, S = 0.2, Tr = 8640, lmda = 0.864)
  #' plot(x = seq(1, 1826), y = Qf, type = "l")
  #' @export

  # reduce lmda if exceeds lmda_max
  lmda[lmda > lmda_max] <- lmda_max

  # erfc and exp terms can get really huge; use the Rmpfr package to deal with them
  term1 <- Rmpfr::erfc(Rmpfr::mpfr(sqrt((S * d * d) / (4 * Tr * t)), prec))
  term2 <- exp(Rmpfr::mpfr(((lmda * lmda * t) / (4 * S * Tr) + (lmda * d) / (2 * Tr)), prec))
  term3 <- Rmpfr::erfc(Rmpfr::mpfr(sqrt((lmda * lmda * t) / (4 * S * Tr)) + sqrt((S * d * d) / (4 * Tr * t)), prec))

  # check for issues
  errors <- which(!is.finite(term2))
  if (length(errors) > 0) stop(paste0("Term 2 = Inf for ", length(errors), " calculation(s). Usually means lmda is too high or Tr is too low. Try using lmda_max?"))

  Qf <- as.numeric(term1 - term2 * term3)

  return(Qf)
}
