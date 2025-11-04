install.packages("sandwich")
install.packages("lmtest")
install.packages("randomForest")
install.packages("pscl")
library(sandwich)
library(lmtest)
library(dplyr)
library(car)
library(ggplot2)
library(randomForest)
library(pscl)

baseball = read.csv("../data/Data set.csv")

## filter out draws ##
baseball <- baseball %>% filter(Win %in% c(0, 1))

## standardization ##
baseball$OPS<-scale(baseball$OPS)
baseball$BA<-scale(baseball$BA)
baseball$RBI<-scale(baseball$RBI)
baseball$R<-scale(baseball$R)
baseball$ERA<-scale(baseball$ERA)
baseball$K<-scale(baseball$K)
baseball$WHIP<-scale(baseball$WHIP)

## Logistic regression ##
baseball_glm= glm(Win~OPS+RBI+R+ERA+WHIP+Home+Star+Foreign+ball, family=binomial , data=baseball)
vif(baseball_glm) ## check multicollinearity (remove BA) ##
summary(baseball_glm)
pR2(baseball_glm)

## Two-Way Design to Address Repeated Measures ##
baseball$GameID <- paste0(baseball$Date, "_",
                          pmin(baseball$Team, baseball$Opponent), "_",
                          pmax(baseball$Team, baseball$Opponent))
coeftest(baseball_glm, vcov = vcovCL(baseball_glm, cluster = ~ GameID))

baseball_glm2= glm(Win~OPS+RBI+R*WHIP+ERA+Home+Star+Foreign+ball, family=binomial , data=baseball)
vif(baseball_glm2)
summary(baseball_glm2)


## random forest to check contribution ##
baseball$Win = as.factor(baseball$Win)
rf_model <- randomForest(Win ~ OPS + RBI + R + ERA + WHIP + Star + Foreign + Home + ball,
                         data = baseball, ntree = 500, importance = TRUE)
importance_rf_model <- importance(rf_model)
importance_rf_model[order(importance_rf_model[, "MeanDecreaseAccuracy"], decreasing = TRUE),]
varImpPlot(rf_model)

imp_df <- as.data.frame(importance_rf_model)
imp_df$Variable<- rownames(imp_df)
imp_df <- imp_df %>%
  arrange(desc(MeanDecreaseAccuracy))

# figure  ##
ggplot(imp_df, aes(x = reorder(Variable, MeanDecreaseAccuracy), y = MeanDecreaseAccuracy)) +
  geom_col(fill = "darkgrey") +
  coord_flip() +
  labs(
    title = "Mean Decrease in Accuracy",
    x = " Varaible ",
    y = " Accuracy "
  )



