#' Stream network for Sixmile Creek Watershed, Wisconsin, USA. Extracted from US NHDPlus v2.1 national seamless dataset.
#'
#' @format A simple feature LINESTRONG collection with 49 rows and 2 variables:
#' \describe{
#'   \item{reach}{identifier code for each stream reach}
#'   \item{stream}{name of stream for each stream reach (Sixmile Creek or Dorn Creek)}
#'   ...
#' }
#' @source \url{http://www.horizon-systems.com/NHDPlusData/NHDPlusV21/Data/NationalData/}
"stream_lines"

#' Streamflow for Sixmile Creek and Dorn Creek.
#'
#' Daily discharge data for the 2014-2015 water years for the US Geological Survey
#' gauging stations DORN (SPRING) CREEK AT CT HIGHWAY M NR WAUNAKEE,WI (station ID 05427930)
#' and SIXMILE CREEK @ COUNTY TRNK HGHWY M NR WAUNAKEE,WI (station ID 05427910).
#'
#' @format A data frame with 1450 rows and 2 variables:
#' \describe{
#'   \item{date}{date of streamflow measurement}
#'   \item{Q_m3d}{discharge, in cubic meters per day}
#'   \item{stream}{name of stream for each stream reach (Sixmile Creek or Dorn Creek)}
#'   ...
#' }
#' @source \url{https://waterdata.usgs.gov/nwis/dv?referred_module=sw&agency_code=USGS&site_no=05427910}
#' @source \url{https://waterdata.usgs.gov/nwis/dv?referred_module=sw&agency_code=USGS&site_no=05427930}
"discharge_df"
