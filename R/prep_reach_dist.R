prep_reach_dist <- function(wel_lon, wel_lat, stream_shp, reach_id, stream_pt_spacing, buffer_width=0.1, nseed=1){
  #' Calculate the distance from a well to each reach within a stream network.
  #' 
  #' This function splits a polyline stream network up into a series of evenly spaced points and 
  #' calculates the distance from each of those points to a well. 
  #' 
  #' @param wel_lon longitude of well
  #' @param wel_lat latitude of well
  #' @param stream_shp shapefile of stream reaches
  #' @param reach_id string indicating name of column in \code{stream_shp} that 
  #' @param stream_pt_spacing distance between points used for sampling each stream reach. The actual distance
  #' between points will be close to this (but not necessarily exact) due to sampling rounding error. The finer
  #' spacing you use, the more accurate your results will be but the function will run slower and use more memory.
  #' @param lat_lon logical indicating whether output data frame should include latitude and longitude of 
  #' each point along the stream
  #' @param buffer_width width of buffer around stream used to match points with polylines
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
  #' @export
  
  # only a couple functions need a bunch of spatial packages, so they are 
  # Suggests rather than Imports. Check to make sure they are loaded here.
  if (!requireNamespace(c("dismo", "sp", "raster", "deldir", "rgeos"), quietly = TRUE)) {
    stop("Several spatial packages (\"dismo\", \"sp\", \"raster\", \"deldir\", \"rgeos\") are needed for this function to work. Please install them.",
         call. = FALSE)
  }
  
  # figure out how many points you will need to make
  n_stream_pts <- round(rgeos::gLength(stream_shp)/stream_pt_spacing)
  
  # sample points
  set.seed(nseed)
  stream_shp_pts <- sp::spsample(stream_shp, n=n_stream_pts, type="regular")
  
  # figure out what stream reach each point corresponds to
  stream_shp_buffer <- raster::buffer(stream_shp, buffer_width, dissolve=F)
  int <- raster::intersect(stream_shp_pts, stream_shp_buffer)
  stream_df_pts <- 
    as.data.frame(stream_shp_pts) %>% 
    cbind(int@data[,reach_id]) %>% 
    magrittr::set_colnames(c("lon", "lat", "reach"))
  
  # calculate euclidean distance to well
  stream_df_pts$dist <- sqrt((stream_df_pts$lon-wel_lon)^2 + (stream_df_pts$lat-wel_lat)^2)
  
  # put columns in order and return
  return(stream_df_pts[,c("reach", "dist", "lat", "lon")])
  
}