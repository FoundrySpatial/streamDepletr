## Hunt1999_CheckAsymptote.R
#' This script is intended to make a plot of Qf through time for a given set of parameters.
#' The goal is to check where Qf asymptotes.

rm(list=ls())

dir.git <- "C:/Users/Sam/WorkGits/StreamflowDepletionModels/"

require(ggplot2)
source(paste0(dir.git, "Hunt1999.R"))
source(paste0(dir.git, "Hunt1999_lmda.R"))

start.flag <- T
for (d in c(10,100,1000)){
    
    ## define parameters that don't change
#    d <- 1000    # [m]
    S <- 1e-5   # [-]
    Kv <- (1e-5)*86400 # [m/d]
    Tr <- 100*Kv # [m2/d] - thickness * K
    w <- 10      # [m]
    b <- 50      # [m]
    t <- seq(1,20*365)
    
    # calculate lmda
    lmda <- Hunt1999_lmda(Kv, w, b)
    
    # calculate depletion
    Qf <- Hunt1999(d, S, Tr, t, lmda)
    
    # data frame
    df <- data.frame(time=t,
                     Qf=Qf,
                     Kv=Kv,
                     d=d)
    
    if (start.flag){
      df.all <- df
      start.flag <- F
    } else {
      df.all <- rbind(df.all, df)
    }
    
}

# plot
ggplot(df.all, aes(x=time/365, y=Qf, color=factor(d))) +
  geom_line() +
  scale_x_continuous(name="Time [years]") +
  scale_y_continuous(name="Depletion Factor, Qf [-]", expand=c(0,0), limits=c(0.5,1)) +
  theme_bw()
