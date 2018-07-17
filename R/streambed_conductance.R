streambed_conductance <- function(w, Kriv, briv) {
  #' Estimate streambed conductance.
  #'
  #' @param w stream width [L]
  #' @param Kriv streambed semipervious layer hydraulic conductivity [L/T].
  #'          Reeves et al. (2009) estimate this as the vertical hydraulic
  #'          conductivity of the aquifer (\code{Kv}; L/T), which is itself often estimated
  #'          as 10\% of the horizontal hydraulic conductivity (\code{Kh*0.1}; L/T)
  #' @param briv streambed semipervious layer thickness [L]
  #'          Reeves et al. (2009) estimate this as the vertical distance from
  #'          the streambed to the top of the well screen, or the length of the
  #'          well screen, whichever is greater [L].
  #' @return A numeric of \code{lmda}, the streambed conductance term [L/T]
  #' @references
  #' Reeves, HW, DA Hamilton, PW Seelbach, and AJ Asher (2009). Ground-Water-Withdrawal Component of the
  #' Michigan Water-Withdrawal Screening Tool. USGS Scientific Investigations Report, Reston VA.
  #' https://pubs.usgs.gov/sir/2009/5003/.
  #' @examples
  #' streambed_conductance(w = 10, Kriv = 0.0864, briv = 1)
  #' streambed_conductance(w = 5, Kriv = 0.0864, briv = 1)
  #' streambed_conductance(w = 10, Kriv = 0.864, briv = 1)
  #' streambed_conductance(w = 10, Kriv = 0.0864, briv = 0.1)
  #' @export

  lmda <- w * Kriv / briv
  return(lmda)
}
