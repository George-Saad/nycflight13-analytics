if("tidyverse" %in% rownames(installed.packages()) == FALSE) 
  {install.packages("tidyverse")}
library(tidyverse)

if("nycflights13" %in% rownames(installed.packages()) == FALSE) 
  {install.packages("nycflights13")}
library(nycflights13)
data("flights")

if (!file.exists("files/fllights.csv")) {
  source("add-delay-degree.R")
}
flights.read <- as_tibble(read.csv(file = 'C:/Users/tehno-gate/Desktop/nycflight13-analytics/files/fllights.csv'))
carrier.flights <- flights.read %>%
                   mutate(delayed = if_else(delay_degree == "no delay", 0, 1))  %>%
                      mutate(delayed = as.factor(delayed))
                                
delayed.log <- glm(delayed ~ carrier, family = binomial(), carrier.flights)

carrier <- unique(flights$carrier)
delay_prob <- data.frame(carrier = carrier)
delay_prob <- delay_prob %>% mutate(probability = predict(delayed.log, 
                                                  data.frame(carrier = carrier), type="response"))

delay_prob$carrier <- factor(delay_prob$carrier, 
                             levels = delay_prob$carrier[order(delay_prob$probability)])

delay_prob <- delay_prob %>% mutate(probability = format(round(probability * 100, 2), nsmall = 2))

ggplot(delay_prob, mapping = aes(y = probability, x = carrier)) + 
        geom_bar(stat = "identity", colour = "red") + labs(y = "Probability   %") 