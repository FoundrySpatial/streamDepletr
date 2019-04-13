apportion_wedge <- function(angle_total, angle_well) {
  #' Distribute streamflow depletion between two streams in a wedge-shaped aquifer.
  #'
  #' @param angle_total angle [radians] between the two streams.
  #' @param angle_well angle [radians] from the first (lower boundary) stream and the well.
  #' @details This function assumes that streams are two linear tributaries which meet at the origin. This function
  #' specifically corresponds to Equations 18 and 19 in Yeh et al. (2008).
  #' @return A numeric of length two with the proportion of steady-state
  #' capture fraction from the first (lower) and second (upper) streams.
  #' @references
  #' Yeh, HD, YC Chang, VA Zlotnik (2008). Stream depletion rate and volume from groundwater pumping in
  #' wedge-shaped aquifers. Journal of Hydrology 349(3): 501-511. doi:10.1029/2018WR022707.
  #' @examples
  #' apportion_wedge(angle_total = (50*pi/2), angle_well = (10*pi/2))
  #'
  #' apportion_wedge(angle_total = (50*pi/2), angle_well = (25*pi/2))
  #' @export

  frac_depletion <- c((1 - (angle_well / angle_total)), angle_well / angle_total)
  return(frac_depletion)
}
