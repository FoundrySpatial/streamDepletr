apportion_polygon <- function(reach_dist_lon_lat, wel_lon, wel_lat, crs, max_dist = Inf, min_frac = 0,
                              reach_name = NULL, dist_name = NULL, lon_name = NULL, lat_name = NULL) {
  #' Distribute streamflow depletion within a stream network using web distance Thiessen polygons.
  #'
  #' Since analytical models assume the presence of 1 (or sometimes 2) linear streams,
  #' functions are needed to distribute that depletion to various reaches within a
  #' real stream network. These geometric functions are described in Zipper et al (2018).
  #'
  #' @param reach_dist_lon_lat data frame with four columns: \code{reach}, which is a grouping variable with
  #' the name of each stream reach; \code{dist} which is the distance of a point on that stream reach to
  #' the well of interest; \code{lon} which is the longitude of that point on the stream; and
  #' \code{lat} which is the latitude of that point on the stream. There can (and likely will) be multiple rows per \code{reach}.
  #' Columns can either be named exactly as defined here, or set using \code{reach_name}, \code{dist_name}, \code{lon_name}, and \code{lat_name}.
  #' @param wel_lon longitude of well
  #' @param wel_lat latitude of well
  #' @param crs object of class CRS with projection info of latitude and longitude input
  #' @param max_dist the maximum distance of a stream to be depleted; defaults to \code{Inf}, which means all reaches will be considered.
  #' @param min_frac the minimum \code{frac_depletion} to be returned; defaults to \code{0}, which means all reaches will be considered.
  #' If \code{min_frac > 0} and some reaches have an estimated \code{frac_depletion < min_frac}, depletion in those reaches will be set to 0
  #' and that depletion will be reallocated based on the proportional depletion in the remaining reaches.
  #' @param reach_name The name of the column in \code{reach_dist} indicating your stream reach grouping variable. If set to \code{NULL} (default), it will assume that the column name is \code{reach}.
  #' @param dist_name The name of the column in \code{reach_dist} indicating your distance variable. If set to \code{NULL} (default), it will assume that the column name is \code{dist}.
  #' @param lon_name The name of the column in \code{reach_dist} indicating your longitude variable. If set to \code{NULL} (default), it will assume that the column name is \code{lon}.
  #' @param lat_name The name of the column in \code{reach_dist} indicating your latitude variable. If set to \code{NULL} (default), it will assume that the column name is \code{lat}.
  #' @importFrom magrittr %>%
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
  #' rdll <- prep_reach_dist(wel_lon = 295500, wel_lat = 4783200,
  #'    stream_shp = stream_lines, reach_id = "reach", stream_pt_spacing = 1)
  #' apportion_polygon(reach_dist_lon_lat = rdll, wel_lon = 295500, wel_lat = 4783200,
  #'    max_dist = 5000, crs = raster::crs(stream_lines))
  #' @export

  # only a couple functions need a bunch of spatial packages, so they are
  # Suggests rather than Imports. Check to make sure they are loaded here.
  if (!requireNamespace(c("dismo", "sp", "raster", "deldir", "rgeos"), quietly = TRUE)) {
    stop("Several spatial packages (\"dismo\", \"sp\", \"raster\", \"deldir\", \"rgeos\") are needed for this function to work. Please install them.",
      call. = FALSE
    )
  }

  # set column names in data frame if necessary
  if (!is.null(reach_name)) names(reach_dist_lon_lat)[names(reach_dist_lon_lat) == reach_name] <- "reach"
  if (!is.null(dist_name)) names(reach_dist_lon_lat)[names(reach_dist_lon_lat) == dist_name] <- "dist"
  if (!is.null(lon_name)) names(reach_dist_lon_lat)[names(reach_dist_lon_lat) == lon_name] <- "lon"
  if (!is.null(lat_name)) names(reach_dist_lon_lat)[names(reach_dist_lon_lat) == lat_name] <- "lat"

  # extent of bounding box which is location of well +/- local area distance in all directions
  if (max_dist == Inf) {
    wel_extent <- raster::extent(c(
      min(reach_dist_lon_lat$lon),
      max(reach_dist_lon_lat$lon),
      min(reach_dist_lon_lat$lat),
      max(reach_dist_lon_lat$lat)
    ))
  } else {
    wel_extent <- raster::extent(c(
      wel_lon - max_dist,
      wel_lon + max_dist,
      wel_lat - max_dist,
      wel_lat + max_dist
    ))
  }

  # get closest point on each stream reach to the well
  reach_closest <-
    reach_dist_lon_lat[, c("reach", "dist", "lon", "lat")] %>%
    dplyr::group_by(reach) %>%
    dplyr::summarize(dist_closest = min(dist)) %>%
    dplyr::left_join(reach_dist_lon_lat[, c("reach", "dist", "lon", "lat")],
      by = c("reach" = "reach", "dist_closest" = "dist")
    )
  reach_closest <- reach_closest[!duplicated(reach_closest$reach), ]

  # make spatial polygons data frame
  stream_spdf <- sp::SpatialPointsDataFrame(
    coords = reach_closest[, c("lon", "lat")],
    data = reach_closest,
    proj4string = crs
  )

  # make polygons for all stream points with no well
  stream_polys <- suppressWarnings(dismo::voronoi(raster::crop(stream_spdf, wel_extent), ext = wel_extent))

  # make polygon for all stream points AND well; use -9999 as identifier for the well's polygon
  well_spdf <-
    data.frame(
      lon = wel_lon, lat = wel_lat,
      reach = -9999, dist_closest = 0
    ) %>%
    sp::SpatialPointsDataFrame(
      coords = data.frame(lon = wel_lon, lat = wel_lat),
      data = .,
      proj4string = crs
    ) %>%
    rbind(., stream_spdf)
  well_polys <- suppressWarnings(dismo::voronoi(raster::crop(well_spdf, wel_extent), ext = wel_extent))
  well_polys@data$well <- "well" # need to have a column that does not have the same name as stream_polys

  # for the polygon containing the well (reach=-9999), figure out what % is contained
  # in the polygons corresponding to each stream reach without the well
  overlap_polys <- suppressWarnings(raster::intersect(subset(well_polys, reach == -9999)[, c("well")], stream_polys))
  overlap_polys@data$area.m2 <- raster::area(overlap_polys)

  df_out <-
    data.frame(
      reach = overlap_polys@data$reach,
      frac_depletion = overlap_polys@data$area.m2 / sum(overlap_polys@data$area.m2)
    )

  # screen for depletion below min_frac
  if (min(df_out$frac_depletion) < min_frac) {
    depl_low <- sum(df_out$frac_depletion[df_out$frac_depletion < min_frac])
    df_out <- subset(df_out, frac_depletion >= min_frac)
    df_out$frac_depletion <- df_out$frac_depletion + depl_low * (df_out$frac_depletion / (1 - depl_low))
    return(df_out)
  } else {
    return(df_out)
  }
}
