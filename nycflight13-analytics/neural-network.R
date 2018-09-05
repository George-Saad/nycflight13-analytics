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

flights.dl <- h2o.deeplearning(x = predictors,  y = response, training_frame = flight_train, seed = 42)

flights.dl

h2o.performance(flights.dl, flight_test)