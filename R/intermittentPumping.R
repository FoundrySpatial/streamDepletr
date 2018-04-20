intermittentPumping <- function(t, starts, stops, rates, method="glover", d, S, Tr, ...){
  #' intermittentPumping
  #'
  #' Calculate streamflow depletion for an intermittent pumping schedule using the Jenkins (1968) superposition technique.
  #' @param t vector of times you want output for [T]
  #' @param starts vector of times to start pumping [T] (must be same length as stops and rates)
  #' @param stops vector of times pumping stops [T] (must be same length as starts and rates)
  #' @param rates vector of pumping rates [L3/T] (must be same length as starts and stops)
  #' @param method analytical solution to use (options= 'glover', 'hunt'). Defaults to 'glover'.
  #' @param d distance from well to stream [L]
  #' @param S aquifer storage coefficient (specific yield if unconfined; storativity if confined)
  #' @param Tr aquifer transmissivity [L2/T]
  #' @param ... any other inputs required for your \code{method} of choice; for example, \code{hunt} needs \code{lmda} (streambed conductance)
  #' @examples
  #' intermittentPumping(t=seq(0,60,10), starts=0, stops=30, rates=100, method="glover", d=100, S=0.1, Tr=100)
  #'
  #' Reference: 
  #' Jenkins, C.T. (1968). Techniques for Computing Rate and Volume of Stream Depletion
  #' by Wells. Ground Water 6(2): 37-46. doi:10.1111/j.1745-6584.1968.tb01641.x
  #' 
  #' Output:
  #'   Q = streamflow depletion [L3/T]
  #'          Note that this is NOT Qf (depletion fraction) because depletion fraction
  #'          would not work if there were different pumping rates at different times
  
  # make a matrix for computations: 1 column per start/stop/rate combo
  Q.all <- matrix(NaN, nrow=length(times), ncol=length(starts))
  
  # select analytical model and calculate depletion
  if (method=="glover"){
    
    for (i in 1:length(starts)){
      # loop through start/stop/rate sets
      Q.all[,i] <- 
        rates[i]*(glover(d=d,  S=S, Tr=Tr, t=sapply(times, FUN=subtract.bounded, y=starts[i], lower.bound=0)) -
                    glover(d=d,  S=S, Tr=Tr, t=sapply(times, FUN=subtract.bounded, y=stops[i], lower.bound=0)))
    }
    
  } else if (method=="hunt"){
    # extract lmda
    lmda <- list(...)$lmda
    lmda_max <- list(...)$lmda_max
    
    for (i in 1:length(starts)){
      # loop through start/stop/rate sets
      Q.all[,i] <- 
        rates[i]*(hunt(d=d,  S=S, Tr=Tr, t=sapply(times, FUN=subtract.bounded, y=starts[i], lower.bound=0), 
                           lmda=lmda, lmda_max=lmda_max) -
                    hunt(d=d,  S=S, Tr=Tr, t=sapply(times, FUN=subtract.bounded, y=stops[i], lower.bound=0), 
                             lmda=lmda, lmda_max=lmda_max))
    }
    
  }
  
  # take row sums for output
  Q.out <- rowSums(Q.all)
  
}

# ## examples
# require(ggplot2)
# source("R/hunt.R")
# source("R/glover.R")
# source("R/HelperFunctions.R")
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
#   df$Q <- intermittentPumping(times=times, starts=starts, stops=stops, rates=rates, method="glover", d=d, S=S, Tr=Tr)
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
# for (method in c("glover", "hunt")){
#   if (method=="glover"){
#     df$Q <- intermittentPumping(times=times, starts=starts, stops=stops, rates=rates, method=method, d=d, S=S, Tr=Tr)
#   } else if (method=="Hunt"){
#     df$Q <- intermittentPumping(times=times, starts=starts, stops=stops, rates=rates, method=method, d=d, S=S, Tr=Tr, 
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
