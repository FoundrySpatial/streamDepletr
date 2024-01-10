prep_reach_dist <- function(wel_lon, wel_lat, stream_sf, reach_id, stream_pt_spacing, nseed = 1) {
  #' Calculate the distance from a well to each reach within a stream network. This function splits a polyline stream network up into a series of evenly spaced points and
  #' calculates the distance from each of those points to a well.
  #'
  #' @param wel_lon longitude of well
  #' @param wel_lat latitude of well
  #' @param stream_sf simple feature collection of stream lines, i.e., loaded from a shapefile using \code{sf::st_read}.
  #' @param reach_id string indicating name of column in \code{stream_sf} that has the unique identifier for each stream line segment.
  #' @param stream_pt_spacing distance between points used for sampling each stream reach. The actual distance
  #' between points will be close to this (but not necessarily exact) due to sampling rounding error. The finer
  #' spacing you use, the more accurate your results will be but the function will run slower and use more memory.
  #' @param nseed seed for random number generator (this is used to convert stream polylines to points)
  #' @return A data frame with four columns:
  #' \describe{
  #'   \item{reach}{a grouping variable with the name of each stream reach}
  #'   \item{dist}{distance of a point on that stream reach to the well of interest}
  #'   \item{lat}{latitude of that point}
  #'   \item{lon}{longitude of that point}
  #' }
  #' This data frame can be plugged directly into \link{apportion_inverse}, \link{apportion_polygon} (if \code{latlon=T}),
  #' or \link{apportion_web}
  #' @examples
  #' rdll <- prep_reach_dist(wel_lon = 295500, wel_lat = 4783200,
  #'    stream_sf = stream_lines, reach_id = "reach", stream_pt_spacing = 5)
  #' head(rdll)
  #' @export

  # figure out how many points you will need to make
  n_stream_pts <- round(as.numeric(sum(sf::st_length(stream_sf))) / stream_pt_spacing)

  # sample points
  set.seed(nseed)
  stream_sf_pts <- sf::st_sample(stream_sf, size = n_stream_pts, type = "regular")

  # figure out what stream reach each point corresponds to
  for (i in 1:dim(stream_sf)[1]) {
    df_i <-
      stream_sf_pts[[i]] |>
      sf::st_coordinates() |>
      as.data.frame() |>
      dplyr::rename(lon = X, lat = Y) |>
      dplyr::select(-L1)
    df_i[, reach_id] <- sf::st_drop_geometry(stream_sf[i, reach_id])

    if (i == 1) {
      stream_df_pts <- df_i
    } else {
      stream_df_pts <- dplyr::bind_rows(stream_df_pts, df_i)
    }
  }

  # calculate euclidean distance to well
  stream_df_pts$dist <- sqrt((stream_df_pts$lon - wel_lon)^2 + (stream_df_pts$lat - wel_lat)^2)

  # set name of 'reach' column to reach
  names(stream_df_pts)[names(stream_df_pts) == reach_id] <- "reach"
  
  # put columns in order and return
  return(stream_df_pts[, c("reach", "dist", "lat", "lon")])
}
