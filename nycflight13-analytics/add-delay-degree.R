if("dplyr" %in% rownames(installed.packages()) == FALSE) 
  {install.packages("nycflights13")}
library(dplyr)

if("sparklyr" %in% rownames(installed.packages()) == FALSE) 
  {install.packages("nycflights13")}
library(sparklyr)

sc <- spark_connect(master = "local")
flights.tbl <- copy_to(sc, nycflights13::flights, "flights")

flights.dd <- flights.tbl %>% 
          ft_bucketizer("arr_delay", "delay_degree", splits = c(-Inf, 1, 16, Inf)) %>% collect()

flights.dd <- flights.dd %>% mutate(delay_degree = as.factor(delay_degree))
flights.dd <- flights.dd %>% mutate(delay_degree = if_else(delay_degree == 0, "no delay",
                                                           if_else(delay_degree == 1, "normal delay", "big delay")))

flights.dd <- na.omit(flights.dd)
write.csv(flights.dd, file = 'C:/Users/tehno-gate/Desktop/nycflight13-analytics/files/fllights.csv')
