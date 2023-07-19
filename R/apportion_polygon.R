apportion_polygon <- function(reach_dist_lon_lat, wel_lon, wel_lat, coord_crs, max_dist = Inf, min_frac = 0,
                              reach_name = NULL, dist_name = NULL, lon_name = NULL, lat_name = NULL) {
  #' Distribute streamflow depletion within a stream network using web distance Thiessen polygons.
  #'
  #' @param reach_dist_lon_lat data frame with four columns: \code{reach}, which is a grouping variable with
  #' the name of each stream reach; \code{dist} which is the distance of a point on that stream reach to
  #' the well of interest; \code{lon} which is the longitude of that point on the stream; and
  #' \code{lat} which is the latitude of that point on the stream. There can (and likely will) be multiple rows per \code{reach}.
  #' Columns can either be named exactly as defined here, or set using \code{reach_name}, \code{dist_name}, \code{lon_name}, and \code{lat_name}.
  #' @param wel_lon longitude of well
  #' @param wel_lat latitude of well
  #' @param coord_crs coordinate reference system for sf or sfc object (create with \code{sf::st_crs}).
  #' @param max_dist the maximum distance of a stream to be depleted; defaults to \code{Inf}, which means all reaches will be considered.
  #' @param min_frac the minimum \code{frac_depletion} to be returned; defaults to \code{0}, which means all reaches will be considered.
  #' If \code{min_frac > 0} and some reaches have an estimated \code{frac_depletion < min_frac}, depletion in those reaches will be set to 0
  #' and that depletion will be reallocated based on the proportional depletion in the remaining reaches.
  #' @param reach_name The name of the column in \code{reach_dist} indicating your stream reach grouping variable. If set to \code{NULL} (default), it will assume that the column name is \code{reach}.
  #' @param dist_name The name of the column in \code{reach_dist} indicating your distance variable. If set to \code{NULL} (default), it will assume that the column name is \code{dist}.
  #' @param lon_name The name of the column in \code{reach_dist} indicating your longitude variable. If set to \code{NULL} (default), it will assume that the column name is \code{lon}.
  #' @param lat_name The name of the column in \code{reach_dist} indicating your latitude variable. If set to \code{NULL} (default), it will assume that the column name is \code{lat}.
  #' @details Since analytical models assume the presence of 1 (or sometimes 2) linear streams, the \code{apportion_*} functions
  #' can be used to distribute that depletion to various reaches within a real stream network. These geometric functions are described
  #' in Zipper et al (2018), which found that \code{apportion_web} a weighting factor (\code{w}) of 2 produced the best results.
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
  #'    stream_sf = stream_lines, reach_id = "reach", stream_pt_spacing = 5)
  #' apportion_polygon(reach_dist_lon_lat = rdll, wel_lon = 295500, wel_lat = 4783200,
  #'    max_dist = 5000, coord_crs = sf::st_crs(stream_lines))
  #' apportion_polygon(reach_dist_lon_lat = rdll, wel_lon = 295500, wel_lat = 4783200,
  #'    max_dist = 5000, min_frac = 0.05, coord_crs = sf::st_crs(stream_lines))
  #' @export

  # set column names in data frame if necessary
  if (!is.null(reach_name)) names(reach_dist_lon_lat)[names(reach_dist_lon_lat) == reach_name] <- "reach"
  if (!is.null(dist_name)) names(reach_dist_lon_lat)[names(reach_dist_lon_lat) == dist_name] <- "dist"
  if (!is.null(lon_name)) names(reach_dist_lon_lat)[names(reach_dist_lon_lat) == lon_name] <- "lon"
  if (!is.null(lat_name)) names(reach_dist_lon_lat)[names(reach_dist_lon_lat) == lat_name] <- "lat"

  # get closest point on each stream reach to the well
  reach_closest <-
    reach_dist_lon_lat[, c("reach", "dist", "lon", "lat")] |>
    dplyr::group_by(reach) |>
    dplyr::summarize(dist_closest = min(dist)) |>
    dplyr::left_join(reach_dist_lon_lat[, c("reach", "dist", "lon", "lat")],
      by = c("reach" = "reach", "dist_closest" = "dist")
    )
  reach_closest <- reach_closest[!duplicated(reach_closest$reach), ]

  reach_closest_sf <-
    reach_closest |>
    subset(dist_closest <= max_dist) |>
    sf::st_as_sf(coords = c("lon", "lat"), crs = coord_crs)

  # make polygons for all stream points with no well
  stream_polys <-
    reach_closest_sf |>
    sf::st_union() |>
    sf::st_voronoi() |>
    sf::st_collection_extract()
  # voronoi returns polys in random order - use intersection with points to reorder to match input sf
  sp <- unlist(sf::st_intersects(reach_closest_sf, stream_polys))
  stream_polys_ordered <-
    stream_polys[sp] |>
    sf::st_sf()
  stream_polys_ordered$reach <- sf::st_drop_geometry(reach_closest_sf$reach)

  # make polygons for all stream points including well (reach for well = -9999)
  reaches_with_well_sf <-
    data.frame(
      lon = wel_lon, lat = wel_lat,
      reach = "-9999", dist_closest = 0
    ) |>
    sf::st_as_sf(coords = c("lon", "lat"), crs = coord_crs) |>
    dplyr::bind_rows(reach_closest_sf)

  well_polys <-
    reaches_with_well_sf |>
    sf::st_union() |>
    sf::st_voronoi() |>
    sf::st_collection_extract()
  wp <- unlist(sf::st_intersects(reaches_with_well_sf, well_polys))
  well_polys_ordered <-
    well_polys[wp] |>
    sf::st_sf()
  well_polys_ordered$reach <- sf::st_drop_geometry(reaches_with_well_sf$reach)
  well_poly <- subset(well_polys_ordered, reach == -9999)

  # for the polygon containing the well (reach=-9999), figure out what % is contained
  # in the polygons corresponding to each stream reach without the well
  stream_polys_overlap <- suppressWarnings(sf::st_intersection(stream_polys_ordered, well_poly)) # get overlapping polygons
  stream_polys_overlap$overlap_area_m2 <- as.numeric(sf::st_area(stream_polys_overlap)) # calculate overlapping area

  df_out <-
    data.frame(
      reach = stream_polys_overlap$reach,
      frac_depletion = stream_polys_overlap$overlap_area_m2 / sum(stream_polys_overlap$overlap_area_m2)
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
