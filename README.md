Project II ST558
================
Pratap Adhikari
10/9/2020

`{r setup, include=FALSE} knitr::opts_chunk$set( message = F, warning =
F, catch=T ) library(tidyverse) library(haven) library(knitr)
library(class) library(caret) library(tree) library(gbm) library(rattle)
library(rpart)`

Read the data

``` {r}
popData<- read_csv("OnlineNewsPopularity.csv")
head(popData)
```

``` {r}
#day<- function(day, ...){}
```

### Processing

``` {r}
# checking if any missing values in the raw data
anyNA(popData)
#summary(popData)
```

FALSE indicates there are no missing values. Next, to split the train
and test set

``` {r}
# Create train and test data set.
set.seed(2)
train<- sample(1:nrow(popData), size = nrow(popData) *0.7)
test<- dplyr::setdiff(1:nrow(popData), train)

popDataTrain<- popData[train, -1]
popDataTest<- popData[test,-1]
```

``` {r}
#difine all of  the days variables on data set
weekday<- c("weekday_is_monday", "weekday_is_tuesday", "weekday_is_wednesday", "weekday_is_thursday", "weekday_is_friday", "weekday_is_saturday", "weekday_is_sunday", "is_weekend" )

#create final train data set
popDataFinalTrain1<- popDataTrain  %>% mutate(weekday= popDataTrain$weekday_is_monday) %>% select( -all_of(weekday), -is_weekend )
popDataFinalTrain<- popDataTrain  %>% filter(weekday_is_monday==1)

#create final test data set
popDataFinalTest<- popDataTest  %>% filter(weekday_is_monday==1) 
```

# Tree Based model

``` {r}
popTreeFit<- tree(shares~ ., data=popDataFinalTrain,
                  control= tree.control(nrow(popDataFinalTrain), mincut = 3, minsize = 6, mindev = 0.001))

plot(popTreeFit, type = c("proportional", "uniform")); text(popTreeFit, pretty = 1, cex=0.8)

#fancyRpartPlot(popTreeFit)

summary(popTreeFit)
```

# Train function

``` {r}
popFit<- train(shares~ ., data=popDataFinalTrain[1:100,],
             method="rpart",
             preProcess= c("center", "scale"),
             trControl= trainControl(method = "LOOCV")
```

``` {r}
plot(popFit)

#plot(popFit$finalModel); text(popFit$finalModel, pretty = 1, cex=0.8)
fancyRpartPlot(popFit$finalModel)
```

`{r eval=F} #Using selected predictors only #popDataFinalTrain$weekday<-
as.factor(popDataFinalTrain$weekday) popTreeFit1<- tree(shares~
kw_avg_avg + self_reference_min_shares + weekday_is_monday ,
data=popDataFinalTrain) plot(popTreeFit1); text(popTreeFit)
summary(popTreeFit1)`

``` {r}
#CV error
cvTree<- cv.tree(popTreeFit)
cvTree
```

Plot of CV error change

``` {r}
plot(cvTree$size, cvTree$dev, type="b")
```

### Prediction

``` {r}
predpop<- predict(popTreeFit, newdata= dplyr::select(popDataFinalTest, -shares) )

#RMSE
treeRMSE<- sqrt(mean(predpop - popDataFinalTest$shares)^2)
```

# 

# Boosted Tree method:

``` {r}
boostFit<- gbm(shares~ ., data=popDataFinalTrain[1:50,], distribution = "gaussian", n.trees = 10, shrinkage = 0.1, interaction.depth = 4)

boostPred<- predict(boostFit, newdata= dplyr::select(popDataFinalTest, -shares), n.trees=10)
```

compare RMSE values (root of test prediction error)

``` {r}
boostRMSE<- sqrt(mean(boostPred - popDataFinalTest$shares)^2 )

#table the RMSE from both of the model fits
RMSE<- c(boost=boostRMSE, tree= treeRMSE)

kable(as.data.frame( RMSE), caption = "RMSE table")
```

# 

\`\`\`{r, eval=F, error=F} library(knitr) library(dplyr)

\`\`\`
