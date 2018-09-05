if("tidyverse" %in% rownames(installed.packages()) == FALSE) 
  {install.packages("tidyverse")}

library(tidyverse)
if("nycflights13" %in% rownames(installed.packages()) == FALSE) 
  {install.packages("nycflights13")}
library(nycflights13)

full.lm  <- lm(arr_delay ~ dep_delay + sched_dep_time + carrier + origin + air_time + distance +  month + day + hour, flights)
null.lm <- lm(arr_delay ~ 1, flights)

stepwise.lm <- step(full.lm, scope = list(lower = null.lm), direction="backward") # Best linear regression model we have


if("modelr" %in% rownames(installed.packages()) == FALSE) 
  {install.packages("modelr")}
library(modelr)

cv  <- crossv_kfold(flights, k = 5)
training.model  <- map(cv$train, ~
                         lm(formula = stepwise.lm$call$formula, data = .))

get_pred  <- function(model, test_data){
  data  <- as.data.frame(test_data)
  pred  <- add_predictions(data, model)
  return(pred)
}

pred  <- map2_df(training.model, cv$test, get_pred, .id = "Run")

MSE  <- pred %>% group_by(Run) %>% 
  summarise(MSE = mean( (arr_delay - pred)^2, na.rm=TRUE))

RMSE <- sqrt(mean(MSE$MSE))
#taking the square root of MSE yields the root-mean-square error or root-mean-square deviation (RMSE),
RMSE    # Standard daviation of error

