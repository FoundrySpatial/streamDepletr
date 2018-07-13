## PrepPackageData.R
#' This script is intended to prepare the data that will be contained within the package:
#'  -Stream network shapefile (Sixmile Creek upstream of Lake Mendota, Wisconsin)
#'  -Daily streamflow from two USGS NWIS stations:
#'    (i) DORN (SPRING) CREEK AT CT HIGHWAY M NR WAUNAKEE,WI (05427930), data available 2012-07-04 - present
#'    (ii) SIXMILE CREEK @ COUNTY TRNK HGHWY M NR WAUNAKEE,WI (05427910), data avaiable 2012-07-26 - present

## required packages
require(rgdal)
require(ggplot2)
require(magrittr)
require(waterData)
require(broom)
require(dplyr)
require(devtools)
require(gridExtra)

## Prep stream network shapefile
# load data, which was downloaded from the National Map Viewer (https://viewer.nationalmap.gov/advanced-viewer/)
# on 2018-07-13 for HUC07090002, trimmed to only Sixmile Creek, and reprojected to EPSG:26916
stream_lines <- 
  rgdal::readOGR(dsn = "PrepPackageData", 
              layer = "NHDPlusV21_Flowline_SixmileCreek",
              stringsAsFactors=F) %>% 
  # only keep useful columns
  subset(select=c("REACHCODE"))
colnames(stream_lines@data) <- "reach"

# which reaches are part of Dorn Creek?
# this was determine by manually inspecting the stream network in QGIS
reaches_dorn <- 
  c("07090002008377",
    "07090002007664",
    "07090002007665",
    "07090002007666",
    "07090002007667",
    "07090002007668",
    "07090002007669",
    "07090002007670",
    "07090002007671",
    "07090002007672")

# make a column with stream name
stream_lines@data$stream <- "Sixmile Creek"
stream_lines@data$stream[stream_lines@data$reach %in% reaches_dorn] <- "Dorn Creek"

## download streamflow data
df_dorn <- 
  waterData::importDVs("05427930", code="00060", stat="00003", sdate="2013-10-01", edate="2015-09-30") %>% 
  dplyr::select(dates, val) %>% 
  transform(Q_m3d = val*0.3048*0.3048*0.3048*86400,  # convert discharge to cubic meters/day
            stream = "Dorn Creek") %>% 
  dplyr::select(dates, Q_m3d, stream) %>% 
  magrittr::set_colnames(c("date", "Q_m3d", "stream"))

df_sixmile <- 
  waterData::importDVs("05427910", code="00060", stat="00003", sdate="2013-10-01", edate="2015-09-30") %>% 
  dplyr::select(dates, val) %>% 
  transform(Q_m3d = val*0.3048*0.3048*0.3048*86400,  # convert discharge to cubic meters/day
            stream = "Sixmile Creek") %>% 
  dplyr::select(dates, Q_m3d, stream) %>% 
  magrittr::set_colnames(c("date", "Q_m3d", "stream"))

# merge into one data frame
discharge_df <- rbind(df_sixmile, df_dorn)

# get lat/lon of gauging stations and reproject to EPSG:26916
df_info  <- waterData::siteInfo(c("05427930", "05427910"))
xy <- df_info[,c("lng", "lat")]
  
spdf_info <- 
  sp::SpatialPointsDataFrame(coords = xy, data = df_info,
                             proj4string = CRS("+init=epsg:4326")) %>% 
  sp::spTransform(CRS("+init=epsg:26916")) %>% 
  subset(select=c("staid", "staname")) %>% 
  as.data.frame()

## save data for use in package
devtools::use_data(stream_lines, discharge_df, overwrite=T)

## make plots
# map
stream_df <- tidy(stream_lines)
p.map <- 
  ggplot2::ggplot() +
  geom_path(data=stream_df, aes(x=long, y=lat, group=group), color="blue") +
  geom_point(data=spdf_info, aes(x=lng, y=lat)) +
  annotate("text", x=spdf_info$lng[1], y=spdf_info$lat[1]-400, label=spdf_info$staid[1], size=3) +
  annotate("text", x=spdf_info$lng[2]+1400, y=spdf_info$lat[2], label=spdf_info$staid[2], size=3) +
  annotate("point", x=295500, y=4783200, color="red") +
  annotate("text", x=293900, y=4783200, label="Proposed well", color="red", size=3) +
  scale_x_continuous(name="UTM 16N Easting [m]") +
  scale_y_continuous(name="UTM 16N Northing [m]") +
  labs(title="Sixmile Creek Watershed", subtitle="North of Lake Mendota, WI") +
  theme_bw() +
  theme_bw() +
  theme(panel.grid=element_blank(),
        axis.text.y=element_text(angle = 90, hjust = 0.5))

# discharge
p.Q <- 
  ggplot(discharge_df, aes(x=date, y=Q_m3d)) +
  geom_line(color="blue") +
  scale_y_log10(name="Discharge [cubic meters/day]") +
  scale_x_date(name="Date", expand=c(0,0)) +
  facet_wrap(~stream, ncol=1, 
             labeller=as_labeller(c("Dorn Creek"=paste0(spdf_info$staid[1], ", Dorn Creek"),
                                    "Sixmile Creek"=paste0(spdf_info$staid[2], ", Sixmile Creek")))) +
  theme_bw() +
  theme(panel.grid=element_blank(),
        axis.text.y=element_text(angle = 90, hjust = 0.5),
        strip.background=element_blank())

# save map for use in vignette
ggsave(file.path("vignettes", "Sixmile_Map+Discharge.png"),
       grid.arrange(p.map, p.Q, ncol=2, widths=c(1.75,1)),
       width=180, height=135, units="mm")
