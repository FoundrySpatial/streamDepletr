apportion_web <- function(reach_dist, w) {
  #' Distribute streamflow depletion within a stream network using web distance weighting.
  #'
  #' Since analytical models assume the presence of 1 (or sometimes 2) linear streams,
  #' functions are needed to distribute that depletion to various reaches within a
  #' real stream network. These geometric functions are described in Zipper et al (2018).
  #'
  #' @param reach_dist data frame with two columns: \code{reach}, which is a grouping variable with
  #' the name of each stream reach, and \code{dist} which is the distance of a point on that stream reach to
  #' the well of interest. There can (and likely will) be more than one \code{dist} per \code{reach};
  #' if there is only one dist per reach, results will be the same as the \link{apportion_inverse} method.
  #' @param w weighting factor; 1 for inverse distance, 2 for inverse distance squared.
  #' @return A data frame with two columns:
  #' \describe{
  #'   \item{reach}{the grouping variable input in \code{reach_dist}}
  #'   \item{frac_depletion}{the proportion of streamflow depletion from the well occurring in that reach.}
  #' }
  #' @references
  #' Zipper, SC, T Dallemagne, T Gleeson, TC Boerman, A Hartmann (2018). Groundwater Pumping Impacts
  #' on Real Stream Networks: Testing the Performance of Simple Management Tools. Water Resources Research.
  #' doi:10.1029/2018WR022707.
  #' @examples
  #' reach_dist <- data.frame(reach = seq(1,5),
  #'   dist = c(100, 150, 900, 300, 200))
  #' apportion_web(reach_dist, w = 2)  # same as apportion_inverse, since only one dist per reach
  #'
  #' reach_dist <- data.frame(reach = c("A", "A", "A", "B", "B"),
  #'   dist = c(100, 150, 900, 300, 200))
  #' apportion_web(reach_dist, w = 1)
  #' @export

  reach_dist %>%
    transform(frac_depletion_pt = (1 / dist^w) / sum((1 / dist^w))) %>%
    dplyr::group_by(reach) %>%
    dplyr::summarize(frac_depletion = sum(frac_depletion_pt)) %>%
    dplyr::select(reach, frac_depletion)
}
