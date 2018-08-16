## compareApportionmentApproaches.R
#' This script does some comparisons between different apportionment approaches.

require(ggplot2)
require(streamDepletr)
require(dplyr)

# Comparison: Wedge vs Inverse Squared ------------------------------------

# for a constant total angle, compare depletion in stream 0 for different angle_well and dr values
angle_total_deg <- 90
angle_total <- angle_total_deg*pi/180

# weighting exponents to test
w.test <- c(1, 1.25, 1.5, 2)

start_flag <- T
for (dr in c(100)){
  for (w in w.test){
    for (ang in seq(1,(angle_total_deg-1))){
      # convert degrees to radians
      angle_well <- ang*pi/180
      
      # calculate distance to each stream reach from well
      d0 <- dr*sin(angle_well)
      d1 <- dr*sin(angle_total-angle_well)
      
      # calculate depletion fraction
      frac_wedge <- apportion_wedge(angle_total, angle_well)
      frac_inv <- apportion_inverse(data.frame(reach=c(0,1), 
                                               dist=c(d0,d1)),
                                    w=w)[,"frac_depletion"]
      
      df_ang <- data.frame(angle = ang,
                           dr = dr,
                           wedge = frac_wedge[1],
                           inverse = frac_inv[1],
                           weight = w)
      
      if (start_flag){
        df <- df_ang
        start_flag <- F
      } else {
        df <- rbind(df, df_ang)
      }
    }
    
  }
}

# looks like dr doesn't matter for either method:
subset(df, angle==45)

# plot
df %>% 
  subset(dr==100) %>% 
  ggplot(aes(x=wedge, y=inverse, color=angle, shape=factor(weight))) +
  geom_point() +
  geom_abline(intercept=0, slope=1) +
  labs(title="Steady-State Depletion Fraction in Stream 1", 
       subtitle=paste0("Total angle between streams [deg] = ", angle_total_deg)) +
  scale_x_continuous(name="Yeh et al. (2008)", limits=c(0,1), expand=c(0,0)) +
  scale_y_continuous(name="Inverse Distance", limits=c(0,1), expand=c(0,0)) +
  scale_color_continuous("Angle to Stream 1 [deg]") +
  scale_shape_discrete(name="Inverse Distance Weight") +
  theme(legend.position=c(1,0),
        legend.justification=c(1,0))


# Compare: Wedge, Inverse, Web --------------------------------------------

euclid_dist <- function(x1, y1, x2, y2){
  # find distance from (x1,y1) to (x2,y2)
  ((x1-x2)^2 + (y1-y2)^2)^0.5
}

df <- data.frame(reach = rep(c("A", "B"), each=5),
                 dist = c(euclid_dist(4,2,1,0),
                          euclid_dist(4,2,2,0),
                          euclid_dist(4,2,3,0),
                          euclid_dist(4,2,4,0),
                          euclid_dist(4,2,5,0),
                          euclid_dist(4,2,0,1),
                          euclid_dist(4,2,0,2),
                          euclid_dist(4,2,0,3),
                          euclid_dist(4,2,0,4),
                          euclid_dist(4,2,0,5)))

apportion_wedge(90, 26.6)
apportion_inverse(df, 1)
apportion_inverse(df, 2)
apportion_web(df, 1)
apportion_web(df, 2)

apportion_inverse(df, 1.225)
apportion_web(df, 1.5)



df <- data.frame(reach = rep(c("A", "B"), each=9),
                 dist = c(euclid_dist(4,2,1,0),
                          euclid_dist(4,2,1.5,0),
                          euclid_dist(4,2,2,0),
                          euclid_dist(4,2,2.5,0),
                          euclid_dist(4,2,3,0),
                          euclid_dist(4,2,3.5,0),
                          euclid_dist(4,2,4,0),
                          euclid_dist(4,2,4.5,0),
                          euclid_dist(4,2,5,0),
                          euclid_dist(4,2,0,1),
                          euclid_dist(4,2,0,1.5),
                          euclid_dist(4,2,0,2),
                          euclid_dist(4,2,0,2.5),
                          euclid_dist(4,2,0,3),
                          euclid_dist(4,2,0,3.5),
                          euclid_dist(4,2,0,4),
                          euclid_dist(4,2,0,4.5),
                          euclid_dist(4,2,0,5)))

apportion_wedge(90, 26.6)
apportion_inverse(df, 1)
apportion_inverse(df, 2)
apportion_web(df, 1)
apportion_web(df, 2)
