library(tidyverse)
library(caret)
library(purrrlyr)


wine <- read_csv('C:\\Users\\Brian\\Desktop\\GradClasses\\Summer18\\621\\621week5\\wine-training-data.csv') %>%
  select(-INDEX) %>%
  mutate(LabelAppeal = factor(LabelAppeal),
         STARS = factor(STARS))

#small number of missing values, impute them
wine %>%
  map_dbl(~sum(is.na(.))/nrow(wine))


VIM::aggr(wine[, -1], col=c('navyblue', 'yellow'),
          numbers=TRUE, sortVars=TRUE, 
          labels=names(wine[, -1]), cex.axis=.7,
          gap=3, ylab=c('Missing Data', 'Pattern'), combined=TRUE)

#only keep wines missing at most 1 piece of information. #goes from 12795 to 11216. Lose 1579 or 12% of data
temp.wine <- wine %>%
  filter(rowSums(is.na(wine)) <= 1) 

VIM::aggr(temp.wine[, -1], col=c('navyblue', 'yellow'),
          numbers=TRUE, sortVars=TRUE, 
          labels=names(temp.wine[, -1]), cex.axis=.7,
          gap=3, ylab=c('Missing Data', 'Pattern'), combined=TRUE)

set.seed(123)
library(mice)
imputed.data <- mice::mice(temp.wine[, -1], m=5, maxit=50, method='pmm', seed=500, printFlag=FALSE)

#imputed values, no more missing
wine.complete <- cbind(temp.wine[, 1], complete(imputed.data, 1))


lm.all <- lm(TARGET ~ ., wine.complete)
plot(lm.all)










