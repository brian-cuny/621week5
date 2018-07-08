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

#pairs all seem to be linear but its hard to tell with the small amount of counts for the response variable
ggplot(wine.complete, aes(Alcohol, TARGET)) +
  geom_point() +
  geom_smooth(method='lm')


#multiple linear regression
library(nlme)
library(MASS)

lm.all <- rlm(TARGET ~ . -FixedAcidity -ResidualSugar, wine.complete)
summary(lm.all)
plot(lm.all)

car::mmps(lm.all)

MASS::stepAIC(lm.all)

car::vif(lm.all)

#poisson regression 
lm.pois <- glm(TARGET ~ ., wine.complete, family=quasipoisson)
summary(lm.pois)

library(lars)
lm.pois.lasso <- lars(model.matrix(~ . -TARGET, wine.complete), wine.complete$TARGET)
plot(lm.pois.lasso)
set.seed(123)
cvlmod <- cv.lars(model.matrix(~ . -TARGET, wine.complete), wine.complete$TARGET)
cvlmod$index[which.min(cvlmod$cv)]
predict(lm.pois.lasso, s=0.979798, type='coef', mode='fraction')$coef

lm.pois <- glm(TARGET ~ . -FixedAcidity, wine.complete, family=quasipoisson)
summary(lm.pois)
plot(lm.pois)

#poisson regression -- inflated zero count model

library(pscl)
lm.hurdle <- hurdle(TARGET ~ . -CitricAcid | . -FixedAcidity -ResidualSugar -Density -Alcohol, wine.complete)
summary(lm.hurdle)

wine.no.zeros <- wine.complete %>%
  filter(TARGET != 0)

library(lars)
lm.pois.lasso <- lars(model.matrix(~ . -TARGET, wine.no.zeros), wine.no.zeros$TARGET)
plot(lm.pois.lasso)
set.seed(1)
cvlmod <- cv.lars(model.matrix(~ . -TARGET, wine.no.zeros), wine.no.zeros$TARGET)
cvlmod$index[which.min(cvlmod$cv)]
predict(lm.pois.lasso, s=0.989899, type='coef', mode='fraction')$coef

wine.bi <- wine.complete %>%
  mutate(TARGET_BI = factor(ifelse(TARGET > 0, 1, 0))) %>%
  dplyr::select(-TARGET)

lm.bi <- glm(TARGET_BI ~ . -FixedAcidity -ResidualSugar -Density -Alcohol, data=wine.bi, family=binomial)
summary(lm.bi)
MASS::stepAIC(lm.bi)


#negative binomial regression

lm.neg <- glm.nb(TARGET ~ ., data=wine.complete)
summary(lm.neg)

stepAIC(lm.neg)

lm.neg.step <- glm.nb(TARGET ~ . -FixedAcidity -ResidualSugar, data=wine.complete)
summary(lm.neg.step)



