apportion_web <- function(reach_dist, w, max_dist = Inf) {
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
  #' @param max_dist the maximum distance of a stream to be depleted; defaults to \code{Inf}, which means all reaches will be considered.
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
  #' apportion_web(reach_dist, w = 2)  # same as inverse because there's only one dist per reach
  #' apportion_web(reach_dist, w = 2, max_dist = 500)
  #'
  #' reach_dist <- data.frame(reach = c("A", "A", "A", "B", "B"),
  #'   dist = c(100, 150, 900, 300, 200))
  #' apportion_web(reach_dist, w = 1)
  #' apportion_web(reach_dist, w = 1, max_dist = 500)
  #' @export

  reach_dist %>%
    subset(dist <= max_dist) %>%
    transform(frac_depletion_pt = (1 / dist^w) / sum((1 / dist^w))) %>%
    dplyr::group_by(reach) %>%
    dplyr::summarize(frac_depletion = sum(frac_depletion_pt)) %>%
    dplyr::select(reach, frac_depletion)
}
