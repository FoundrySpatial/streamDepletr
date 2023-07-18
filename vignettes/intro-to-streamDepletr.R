## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)
require(ggplot2)
require(dplyr)
require(tidyr)
require(scales)
require(streamDepletr)

## -----------------------------------------------------------------------------
times <- seq(1, 100) # time [days]
K <- 1e-5 * 86400 # hydraulic conductivity [m/d]
b <- 50 # aquifer thickness [m]
trans <- b * K # transmissivity [m2/d]
d <- 250 # well to stream distance [m]
Sy <- 0.1 # specific yield [-]

## -----------------------------------------------------------------------------
str_cond <- streambed_conductance(
  w = 5, # river width [m]
  Kriv = 0.1 * K, # streambed K is 10% that of the aquifer
  briv = 1
) # thickness of streambed

## ----fig.width = 6, fig.height = 4--------------------------------------------
df_depletion <-
  data.frame(
    times = times,
    Qf_glover = glover(t = times, d = d, S = Sy, Tr = trans),
    Qf_hunt = hunt(t = times, d = d, S = Sy, Tr = trans, lmda = str_cond)
  )

df_depletion |>
  tidyr::pivot_longer(-times, values_to = "Qf", names_to = "model") |>
  ggplot2::ggplot(aes(x = times, y = Qf, color = model)) +
  geom_line() +
  scale_y_continuous(limits = c(0, 1))

## -----------------------------------------------------------------------------
df_depletion[dim(df_depletion)[1], ] # glover is ~2x hunt

## ----fig.width = 6, fig.height = 4--------------------------------------------
Qw <- 500 # pumping rate, [m3/d]
df_depletion$Qs_glover <- df_depletion$Qf_glover * Qw # streamflow depletion, [m3/d]
df_depletion$Qs_hunt <- df_depletion$Qf_hunt * Qw # streamflow depletion, [m3/d]

# plot results
df_depletion |>
  dplyr::select(c("times", "Qs_glover", "Qs_hunt")) |>
  tidyr::pivot_longer(-times, values_to = "Qs", names_to = "model") |>
  ggplot2::ggplot(aes(x = times, y = Qs, color = model)) +
  geom_line()

## ----fig.width = 6, fig.height = 4--------------------------------------------
# define pumping schedule
t_starts <- c(10, 200, 400) # days that well turns on
t_stops <- c(60, 350, 700) # days that well turns off

# calculate depletion through time
df_intermittent <-
  data.frame(
    times = seq(1, 730),
    Qs_intermittent =
      intermittent_pumping(
        t = seq(1, 730), starts = t_starts, stops = t_stops,
        rates = rep(Qw, length(t_starts)),
        method = "glover", d = d, S = Sy, Tr = trans
      )
  )


# plot - times when the well is turned on are shaded red
ggplot2::ggplot(
  df_intermittent,
  aes(x = times, y = Qs_intermittent)
) +
  annotate("rect",
    xmin = t_starts[1], xmax = t_stops[1],
    ymin = -Inf, ymax = Inf, fill = "red", alpha = 0.5
  ) +
  annotate("rect",
    xmin = t_starts[2], xmax = t_stops[2],
    ymin = -Inf, ymax = Inf, fill = "red", alpha = 0.5
  ) +
  annotate("rect",
    xmin = t_starts[3], xmax = t_stops[3],
    ymin = -Inf, ymax = Inf, fill = "red", alpha = 0.5
  ) +
  geom_line()

## ----fig.width = 6, fig.height = 4--------------------------------------------
pump_rates <- c(100, 1000, 100) # [m3/d] - must be same length as t_starts and t_stops
df_intermittent$Qs_variableRate <-
  intermittent_pumping(
    t = seq(1, 730), starts = t_starts, stops = t_stops,
    rates = pump_rates, method = "glover", d = d, S = Sy, Tr = trans
  )


# plot - times when the well is turned on are shaded red
df_intermittent |>
  tidyr::pivot_longer(-times, values_to = "Qs", names_to = "pumpSchedule") |>
  ggplot2::ggplot(aes(x = times, y = Qs, linetype = pumpSchedule)) +
  annotate("rect",
    xmin = t_starts[1], xmax = t_stops[1],
    ymin = -Inf, ymax = Inf, fill = "red", alpha = 0.5
  ) +
  annotate("rect",
    xmin = t_starts[2], xmax = t_stops[2],
    ymin = -Inf, ymax = Inf, fill = "red", alpha = 0.5
  ) +
  annotate("rect",
    xmin = t_starts[3], xmax = t_stops[3],
    ymin = -Inf, ymax = Inf, fill = "red", alpha = 0.5
  ) +
  geom_line()

## -----------------------------------------------------------------------------
# well properties
Qw <- 1000 # well pumping rate [m3/d]
wel_lon <- 295500 # easting of well [m]
wel_lat <- 4783200 # northing of well [m]
date_pump_start <- as.Date("2014-03-01") # pumping start date
date_pump_stop <- as.Date("2015-08-01") # pumping stop date

# aquifer properties
K <- 1e-5 * 86400 # hydraulic conductivity [m/d]
b <- 250 # aquifer thickness [m]
trans <- b * K # transmissivity [m2/d]
Sy <- 0.05 # specific yield [-]

