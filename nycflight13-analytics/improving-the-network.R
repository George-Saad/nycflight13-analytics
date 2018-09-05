if("tidyverse" %in% rownames(installed.packages()) == FALSE) 
{install.packages("tidyverse")}
library(tidyverse)

if("h2o" %in% rownames(installed.packages()) == FALSE) 
{install.packages("h2o")}
library(h2o)


if (!exists("init_h2o")) {
  init_h2o <- h2o.init()
}

if (!file.exists("files/fllights.csv")) {
  source("add-delay-degree.R")
}
flights.read <- as_tibble(read.csv(file = 'C:/Users/tehno-gate/Desktop/nycflight13-analytics/files/fllights.csv'))
flights.read <- flights.read %>% mutate(origin = as.numeric(origin), carrier = as.numeric(carrier))

flights.df <- as.h2o(flights.read)
flights_split <- h2o.splitFrame(data = flights.df, ratios = 0.8)

flight_train <- flights_split[[1]]
flight_test <- flights_split[[2]]

predictors <- c("sched_dep_time", "origin", "dep_delay", "air_time", "carrier",
                "sched_arr_time", "distance", "month", "hour", "day", "minute")

response <- "delay_degree"


flights_err_per_layer <- tibble(units_per_layer = 0, layers_number = 0, err = 0)

min_ver_err <- Inf
for (i in c(3 : 50)) {
  min_hor_err <- Inf  
  for (j in c(3 : 11)) {
    hid_lay = c()
    for (k in c(1:j)) {
      hid_lay <- combine(hid_lay, c(i))
    }
    
    flights.dl <- h2o.deeplearning(x = predictors, 
                                   y = response,
                                   training_frame = flight_train,
                                   momentum_start = 0.5,
                                   momentum_ramp = 10^4,
                                   momentum_stable = 0.99,
                                   adaptive_rate = FALSE,
                                   nesterov_accelerated_gradient = TRUE,
                                   reproducible = TRUE,
                                   hidden = hid_lay,
                                   epochs = 100,
                                   seed = 42
                                   )
    
    perf <- h2o.performance(flights.dl, flight_test)
    conf.ma <- h2o.confusionMatrix(perf)
    err <- conf.ma$Error[4]
    if (err >= min_hor_err) {
      break()
    }
    min_hor_err <- err
  }
  
  min_ver_err<- min_hor_err
  flights_err_per_layer <- add_row(flights_err_per_layer, units_per_layer = i, layers_number = j, err = min_ver_err)
}

as.data.frame(flights_err_per_layer[order(flights_err_per_layer$err), ])