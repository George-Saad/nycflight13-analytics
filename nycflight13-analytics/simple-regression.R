if("tidyverse" %in% rownames(installed.packages()) == FALSE) 
  {install.packages("tidyverse")}
library(tidyverse)

if("nycflights13" %in% rownames(installed.packages()) == FALSE)
  {install.packages("nycflights13")}
library(nycflights13)
data("flights")

ggplot(flights, mapping = aes(y = dep_delay, x = arr_delay)) + 
  geom_point() +
  geom_smooth(mapping = aes(x  = dep_delay, y = arr_delay))  

arr_delay.lm <- lm(arr_delay ~ dep_delay, data = flights) 

# residual versus fitted plot
plot(arr_delay.lm, which = 1)    # Checking the assumptions
#  1- Linearity is unreasonable
#  2- constant spread is unreasonable as the points are not equally spread
