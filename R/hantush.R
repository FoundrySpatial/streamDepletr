hantush <- function(t, d, S, Kh, b, Kriv, briv, prec = 80) {
  #' Streamflow depletion in partially penetrating stream with semipervious streambed.
  #'
  #' Described in Hantush (1965). As the leakance term \code{(b*Kh/Kriv)} approaches 0 this is equivalent to \code{glover}.
  #'
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
  #' @param Kh aquifer horizontal hydraulic conductivity [L/T]
  #' @param b aquifer saturated thickness [L]
  #' @param Kriv streambed semipervious layer hydraulic conductivity [L/T]
  #' @param briv streambed semipervious layer thickness [L]
  #' @param prec precision for mpfr package for storing huge numbers; 80 seems to generally work but tweak this if you get weird results.
  #' @return A numeric of \code{Qf}, streamflow depletion as fraction of pumping rate [-].
  #' If the pumping rate of the well (\code{Qw}; [L3/T]) is known, you can calculate volumetric streamflow depletion [L3/T] as \code{Qf*Qw}
  #' @references
  #' Hantush, MS (1965). Wells near Streams with Semipervious Beds. Journal of Geophysical Research 70(12): 2829-38. doi:10.1029/JZ070i012p02829.
  #' @export

  # streambed leakance
  L <- (Kh / Kriv) * briv

  # transmissivity
  Tr <- Kh * b

  # erfc and exp terms can get really huge; use the mpfr package to deal with them
  term1 <- Rmpfr::mpfr(sqrt(S * d * d / (4 * Tr * t)), prec)
  term2 <- Rmpfr::mpfr((((Tr * t) / (S * L * L)) + (d / L)), prec)
  term3 <- Rmpfr::mpfr((sqrt((Tr * t) / (S * L * L)) + sqrt((S * d * d) / (4 * Tr * t))), prec)

  # calculate streamflow depletoin
  Qf <- as.numeric(
    Rmpfr::erfc(term1) - exp(term2) * Rmpfr::erfc(term3)
  )

  return(Qf)
}