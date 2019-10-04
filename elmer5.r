#PSET 5
#nrow is for matrix, length is for vector, use select if you want to maintain dataframe?

rm(list = ls())
#dev.off()
shell("cls")
library("MASS")
library("tidyverse")
library("plotROC")
data(Boston)

?Boston

set.seed(1861)

trainSize <- 0.75
train_idx <-sample(1:nrow(Boston), size = floor(nrow(Boston)*trainSize))

housing <- Boston %>% mutate(PriceyHome = ifelse(medv > 40, 1,0), chas_factor = factor(chas))

housing_train <- housing%>% slice(train_idx)
housing_test <- housing%>% slice(-train_idx)

view(housing_train)

#b

housing_train <- housing_train %>% group_by(PriceyHome)
sum_train <- housing_train %>% summarise_all(list(mean = mean), na.rm = TRUE)

housing_test <- housing_test %>% group_by(PriceyHome)
sum_test <- housing_test %>% summarise_all(list(mean = mean), na.rm = TRUE)

#c

#GGPLOTS

#d

log_train_mod = glm(PriceyHome ~ chas,
              data = housing_train,
              family = binomial)

log_test_mod = glm(PriceyHome ~ chas,
                    data = housing_test,
                    family = binomial)

#e

log_train_mod_plus = glm(PriceyHome ~ chas + crim + lstat + ptratio + zn + rm + rad + nox,
              data = housing_train,
              family = binomial)

summary(log_train_mod_plus)


log_test_mod_plus = glm(PriceyHome ~ chas + crim + lstat + ptratio + zn + rm + rad + nox,
                         data = housing_test,
                         family = binomial)

summary(log_test_mod_plus)

#f

preds_train_DF <- data.frame(
  scores_train = predict(log_train_mod_plus, 
                        type = "response"),
  housing_train
)

preds_test_DF <- data.frame(
  scores_test = predict(log_test_mod_plus, 
                         type = "response"),
  housing_test
)

#g

preds_train_DF <-  preds_train_DF %>% mutate(PosNeg05 = 
                                               ifelse(scores_train > 0.5 & PriceyHome == 1, "TP",
                                               ifelse(scores_train > 0.5 & PriceyHome == 0, "FP",
                                               ifelse(scores_train <= 0.5 & PriceyHome == 0, "TN",
                                               ifelse(scores_train <= 0.5 & PriceyHome == 0, "FN",0)))))

preds_test_DF <-  preds_test_DF %>% mutate(PosNeg05 = 
                                               ifelse(scores_test > 0.5 & PriceyHome == 1, "TP",
                                               ifelse(scores_test > 0.5 & PriceyHome == 0, "FP",
                                               ifelse(scores_test <= 0.5 & PriceyHome == 0, "TN",
                                               ifelse(scores_test <= 0.5 & PriceyHome == 1, "FN",0)))))



preds_train_DF <- data.frame(
  
  class_pred05 = ifelse(preds_train_DF$scores_train
                        > 0.5, 1, 0),
  preds_train_DF
)

preds_test_DF <- data.frame(
  
  class_pred05 = ifelse(preds_test_DF$scores_test
                        > 0.5, 1, 0),
  preds_test_DF
)

table(preds_train_DF$PosNeg05)
table(preds_train_DF$class_pred05,preds_train_DF$PriceyHome)

table(preds_test_DF$PosNeg05)
table(preds_test_DF$class_pred05,preds_test_DF$PriceyHome)

#h
#true positve rate / sensitivity is good == 76%, we might want to change 19/25 tp/tp+fn
# true negative rate/ specifity == 99.7% , super good no change 353/354 fn/tn+fp
# false positive rate / type 1 error ==  0%, fp/fp+tn
#we should increase the prob cut off (moviong closer to 0) because it should increase our true positive rate at the expense of our false positive rate 
# Other factors we should consider for changing the cutoff would be what the intention behind our model is
# If we are only concerned about predicting homes that are not PriceyHomes, we're doing good and could move
# the cutoff until we no longer are at that 99+ percentage prediction rate. However, if we are more concerned with
# predicting homes that are actually pricey, we would want to adjust out cutoff until we're I'd argue 90+ percent
# accurate on predicting them. The trade off is ever present and we should act in according to our best interest here.


#i
trainROC <- ggplot(data = preds_train_DF,
       aes(m = scores_train,
              d = PriceyHome)) +
       geom_roc(labelsize = 3.5,
                cutoffs.at = c(.99,.9,.7,.5,.3,.1,0))

testROC <- ggplot(data = preds_test_DF,
       aes(m = scores_test,
              d = PriceyHome)) +
       geom_roc(labelsize = 3.5,
                cutoffs.at = c(.99,.9,.7,.5,.3,.1))

# png(height=800, width=800, file="output/_plot.png")
# 
# dev.off()
# 
# png(height=800, width=800, file="output/_plot.png")
# 
# dev.off()

calc_auc(trainROC)
calc_auc(testROC)
#close to 1 == :)
# The training model is most likely overfit as the area of the curve is nearly one. In order to reduce
# this overfitting we would reccomended taking out some variables and get more data.
# 
# The test model appears decently fit based on the AUC number calculated, leading us to believe
# that only minor changes would be neccesary to imporve model fit. More data would help though.
