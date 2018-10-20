# Galton heredity : Least Square estimates/

library(tidyverse)
library(dplyr)
library(HistData)

data("Galton")

beta1 = seq(0, 1, len=nrow(Galton))
rss <- function(beta0, beta1, data){
  resid <- Galton$child - (beta0 + beta1*Galton$parent)
  return(sum(resid^2))
}

results <- data.frame(beta1 = beta1, rss = sapply(beta1, rss, beta0 = 36))
results %>% ggplot(aes(beta1, rss)) + geom_line() + 
  geom_line(aes(beta1, rss), col=2)
lm(child ~ parent , data = Galton)

Galton %>% ggplot(aes(child,parent)) +
  geom_point() + geom_smooth(method = "lm")

Galton %>%
  mutate(Y_hat = predict(lm(child~parent, data = .))) %>%
  ggplot(aes(parent,Y_hat)) + geom_line()


model <- lm(child ~ parent, data = Galton)
predictions <- predict(model, interval = c("confidence"), level = 0.95)
data <- as.tibble(predictions) %>% bind_cols(parent = Galton$parent)

ggplot(data, aes(x = parent, y = fit)) +
  geom_line(color = "blue", size = 1) + 
  geom_ribbon(aes(ymin=lwr, ymax=upr), alpha=0.2) + 
  geom_point(data = Galton, aes(x = parent, y = child))


