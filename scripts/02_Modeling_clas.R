#
#
#

# Selecting the best variables thanks to previous comparisons and plots
# and creating the data frames for modeling (classification)

df.model <- df.original %>% 
    select(department, team, targeted_productivity, smv, wip, over_time,
           incentive, no_of_workers, actual_productivity) %>%
    mutate( team = print(sprintf("%02.0f", team) ),
            no_of_workers = ceiling(no_of_workers),
            targeted_productivity = ifelse(targeted_productivity==0.07, 
                                           0.70, targeted_productivity)) %>%
    mutate( targeted_productivity = print(sprintf("%02.2f", targeted_productivity) ),
            productivity_cat = ifelse(actual_productivity >= targeted_productivity,
                                      "achieved", "unsuccessful") ) %>%
    replace_na( list( wip = 0) ) %>%
    mutate_if(is.character, as.factor)

# Data frame with logarithmic transformation and normalization
df.model.lognor <- df.model %>%
    mutate( smv = log(smv + 1),
            wip = log(wip + 1),
            over_time = log(over_time + 1),
            incentive = log(incentive + 1),
            no_of_workers = log(no_of_workers + 1),
            actual_productivity = log(actual_productivity + 1) ) %>%
    mutate_if( is.numeric, scale )

# Data frame with only normalization
df.model.norm <- df.model %>%
    mutate_if( is.numeric, scale )

#
# Training
#

    ## Splitting into 80/20

set.seed(69)

n.rows <- nrow(df.model)
idx <- createDataPartition(df.model$productivity_cat, p = 0.8, list = FALSE)

train.base <- df.model[idx,-c(1,9)]
test.base <- df.model[-idx,-c(1,9)]

train.log <- df.model.lognor[idx,]
test.log <- df.model.lognor[-idx,]

train.norm <- df.model.norm[idx,-9]
test.norm <- df.model.norm[-idx,-9]

    ## Testing proportions of target variable

cat("Original set:\n",
    prop.table( table(df.model$productivity_cat) ),
    "\n==================\n",
    "Train set:\n",
    prop.table( table(train.base$productivity_cat) ),
    "\n==================\n",
    "Test set:\n",
    prop.table( table(test.base$productivity_cat) ) )

#
# Generating the models
#

library(randomForest)

    ## Random Forest

model.rf <- randomForest(productivity_cat ~ ., data = train.base,
                         importance = TRUE,
                         ntree = 210)

plot(model.rf)

varImpPlot(model.rf)

        ### Evaluating the model

model.rf.predict <- predict( model.rf, test.base)
confusionMatrix( model.rf.predict, test.base$productivity_cat, positive = "unsuccessful")

        ### ROC Curve

rf.df.predict <- data.frame( predict( model.rf, test.base, type = 'prob') )
rf.roc <- prediction( rf.df.predict$unsuccessful, test.base$productivity_cat)

roc <- performance(rf.roc, 'tpr', 'fpr')
auc <- performance(rf.roc, 'auc')
plot(roc, colorize = T, lwd = 2,
     main = "Roc curve. 'unsuccessful' as positive class",
     sub = paste( "AUC =", auc@y.values[[1]] ) )
abline(0.0, 1.0)


    ## Trying Monte-Carlo with Random Forest

train.Control <- trainControl( method = "repeatedcv",
                               number = 12,
                               repeats = 8,
                               search = "grid")

model.rf.mc <- train( productivity_cat ~ .,
                      data = train.base,
                      method = "rf",
                      tuneGrid = data.frame(mtry = 3),
                      ntree = 220,
                      trControl = train.Control)

print(model.rf.mc)

        ### Evaluating the model
        
model.rf.mc.predict <- predict( model.rf.mc, test.base)
confusion_matrix <- confusionMatrix( model.rf.mc.predict, test.base$productivity_cat, positive = "unsuccessful")


        ### ROC Curve

rf.df.predict <- data.frame( predict( model.rf.mc, test.base, type = 'prob') )
rf.roc <- prediction( rf.df.predict$unsuccessful, test.base$productivity_cat)

roc <- performance(rf.roc, 'tpr', 'fpr')
auc <- performance(rf.roc, 'auc')
plot(roc, colorize = T, lwd = 2,
     main = "Roc curve. 'unsuccessful' as positive class",
     sub = paste( "AUC =", auc@y.values[[1]] ) )
abline(0.0, 1.0)

    ## Linear Regression

model.lm <- lm(actual_productivity ~ .,
               data = train.log,
               method = )



