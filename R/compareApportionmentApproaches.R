## compareApportionmentApproaches.R
#' This script does some comparisons between different apportionment approaches.

# Comparison: Wedge vs Inverse Squared ------------------------------------

# for a constant total angle, compare depletion in stream 0 for different angle_well and dr values
angle_total_deg <- 90
angle_total <- angle_total_deg*pi/180

start_flag <- T
for (dr in seq(100, 500, 100)){
  for (ang in seq(1,(angle_total_deg-1))){
    # convert degrees to radians
    angle_well <- ang*pi/180
    
    # calculate distance to each stream reach from well
    d0 <- dr*sin(angle_well)
    d1 <- dr*sin(angle_total-angle_well)
    
    # calculate depletion fraction
    w1 <- 1
    w2 <- 2
    frac_wedge <- apportion_wedge(angle_total, angle_well)
    frac_inv1 <- apportion_inverse(data.frame(reach=c(0,1), 
                                              dist=c(d0,d1)),
                                   w=w1)[,"frac_depletion"]
    frac_inv2 <- apportion_inverse(data.frame(reach=c(0,1), 
                                              dist=c(d0,d1)),
                                   w=w2)[,"frac_depletion"]
    
    df_ang <- data.frame(angle = ang,
                         dr = dr,
                         wedge = frac_wedge[1],
                         inverse = c(frac_inv1[1], frac_inv2[1]),
                         weight = c(w1, w2))
    
    if (start_flag){
      df <- df_ang
      start_flag <- F
    } else {
      df <- rbind(df, df_ang)
    }
  }
}

# looks like dr doesn't matter for either method:
subset(df, angle==20)

# plot
require(ggplot2)

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
  
