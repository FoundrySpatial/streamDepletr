PumpingSchedule <- function(times, starts, stops, rates, method="Glover", d, S, Tr, ...){
  ## Calculates streamflow depletion using Jenkins (1968) superposition technique
  #'
  #' Reference: 
  #' Jenkins, C.T. (1968). Techniques for Computing Rate and Volume of Stream Depletion
  #' by Wells. Ground Water 6(2): 37-46. doi:10.1111/j.1745-6584.1968.tb01641.x
  #' 
  #' Inputs:
  #'  times = vector of times you want output for [T]
  #'  starts = vector of times to start pumping [T] (must be same length as stops and rates)
  #'  stops = vector of times pumping stops [T] (must be same length as starts and rates)
  #'  rates = vector of pumping rates [L3/T] (must be same length as starts and stops)
  #'  method = analytical solution to use (options= 'Glover', 'Hunt')
  #'  d  = distance from well to stream [L]
  #'  S  = aquifer storage coefficient [-] (specific yield for unconfined storativity for confined)
  #'  Tr = aquifer transmissivity [L2/T]
  #'   ... = any other inputs required for your depletion function of choice; for example, Hunt1999 needs 'lmda' (streambed conductance)
  #'  
  #' Output:
  #'   Q = streamflow depletion [L3/T]
  #'          Note that this is NOT Qf (depletion fraction) because depletion fraction
  #'          would not work if there were different pumping rates at different times
  
  # make a matrix for computations: 1 column per start/stop/rate combo
  Q.all <- matrix(NaN, nrow=length(times), ncol=length(starts))
  
  # select analytical model and calculate depletion
  if (method=="Glover"){
    
    for (i in 1:length(starts)){
      # loop through start/stop/rate sets
      Q.all[,i] <- 
        rates[i]*(GloverBalmer1954(d=d,  S=S, Tr=Tr, t=sapply(times, FUN=subtract.bounded, y=starts[i], lower.bound=0)) -
                    GloverBalmer1954(d=d,  S=S, Tr=Tr, t=sapply(times, FUN=subtract.bounded, y=stops[i], lower.bound=0)))
    }
    
  } else if (method=="Hunt"){
    # extract lmda
    lmda <- list(...)$lmda
    lmda_max <- list(...)$lmda_max
    
    for (i in 1:length(starts)){
      # loop through start/stop/rate sets
      Q.all[,i] <- 
        rates[i]*(Hunt1999(d=d,  S=S, Tr=Tr, t=sapply(times, FUN=subtract.bounded, y=starts[i], lower.bound=0), 
                           lmda=lmda, lmda_max=lmda_max) -
                    Hunt1999(d=d,  S=S, Tr=Tr, t=sapply(times, FUN=subtract.bounded, y=stops[i], lower.bound=0), 
                             lmda=lmda, lmda_max=lmda_max))
    }
    
  }
  
  # take row sums for output
  Q.out <- rowSums(Q.all)
  
}

# ## examples
# require(ggplot2)
# source("src/Hunt1999.R")
# source("src/GloverBalmer1954.R")
# source("src/HelperFunctions.R")
# 
# ## Rathfelder (2016) Figure 40
# times <- seq(0,100)
# starts <- 0
# stops <- 30
# rates <- 100
# method <- "Glover"
# S <- 0.25
# Tr <- 10*20
# 
# df <- data.frame(t=times)
# start.flag <- T
# for (d in c(5, 50, 200)){
#   df$Q <- PumpingSchedule(times=times, starts=starts, stops=stops, rates=rates, method="Glover", d=d, S=S, Tr=Tr)
#   df$d <- d
# 
#   if (start.flag){
#     df.all <- df
#     start.flag <- F
#   } else {
#     df.all <- rbind(df.all, df)
#   }
# }
# 
# # plot
# ggplot(df.all, aes(x=t, y=Q, color=factor(d))) +
#   geom_line()
# 
# ## Rathfelder (2016) Figure 61
# #### NOT working for Hunt (1999) solution
# # inputs are in Rathfelder (2016) Table 8
# times <- seq(0,730)
# starts <- c(0,366)
# stops <- c(65, 430)
# rates <- rep(100, length(starts))
# d <- 167
# S <- 0.09
# Tr <- 640
# 
# # streambed properties needed to calculate lambda for Hunt
# str_width <- 60
# str_cond <- 3
# str_thick <- 1
# lmda <- str_width*str_cond/str_thick
# lmda_max <- 6  # this is my estimate to make it line up with results in Rathfelder (1961)
# 
# df <- data.frame(t=times)
# start.flag <- T
# for (method in c("Glover", "Hunt")){
#   if (method=="Glover"){
#     df$Q <- PumpingSchedule(times=times, starts=starts, stops=stops, rates=rates, method=method, d=d, S=S, Tr=Tr)
#   } else if (method=="Hunt"){
#     df$Q <- PumpingSchedule(times=times, starts=starts, stops=stops, rates=rates, method=method, d=d, S=S, Tr=Tr, 
#                             lmda=lmda, lmda_max=lmda_max)
#   }
# 
#   df$method <- method
# 
#   if (start.flag){
#     df.all <- df
#     start.flag <- F
#   } else {
#     df.all <- rbind(df.all, df)
#   }
# }
# 
# ggplot(df.all, aes(x=t, y=Q, color=method)) +
#   geom_line() +
#   scale_y_continuous(limits=c(0,100))
