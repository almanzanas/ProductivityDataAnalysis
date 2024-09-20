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
            productivity_cat = ifelse(actual_productivity < targeted_productivity,
                                      "unsuccess", "achieved") ) %>%
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


#
# Logistic Regression
#

df.model.bin <- df.model %>%
    mutate( across( where(is.numeric), ~ log(. + 1) ) ) %>%
    mutate( across( where(is.numeric), scale ) ) %>%
    mutate( actual_productivity = df.original$actual_productivity,
            productivity_cat = as.numeric(productivity_cat) ) %>%
    mutate( productivity_cat = ifelse(productivity_cat == 2, 0, 1 ) )

    ## Splitting into 80/20

set.seed(69)

n.rows <- nrow(df.model.bin)
idx <- createDataPartition(df.model.bin$productivity_cat, p = 0.8, list = FALSE)

train.bin <- df.model.bin[idx,-c(4,5,9)]
test.bin <- df.model.bin[-idx,-c(4,5,9)]



fit.bin <- glm(data = train.bin,
               productivity_cat ~ .,
               family = binomial(link="logit") )
summary(fit.bin)

# Suggestions:
    # Teams 6 to 11 are more likely to achieve actual_productivity
    # Also when target productivity are 0.70 and 0.80 the productivity are more
        # favorable
    # And 'incentive' variable has high impact

# odd-ratio percentage, how likely is "achieve"
(exp(coefficients(fit.bin)) -1 ) *100

bin.prob <- predict(fit.bin, test.bin,
                       type = "response") # 'response' for probabilities

# Confusion Matrix
table(Predicted = fit.bin$fitted.values > 0.5, Actual = train.bin$productivity_cat)
    # True positive: 818 (0.645)
    # True Negative: 146 (0.152)
    # False Negative: 73 (0.076)
    # False positive: 121 (0.126)

#Confusion matrix for training set
( cm.bin <- table(Predicted = fit.bin$fitted.values > 0.5, Actual = train.bin$productivity_cat) %>%
    prop.table() %>% addmargins() )
#                   Actual
#    Predicted          0          1        Sum
#        FALSE 0.15240084 0.07620042 0.22860125
#        TRUE  0.12630480 0.64509395 0.77139875
#        Sum   0.27870564 0.72129436 1.00000000

# Confusion matrix for prediction:
table(Predicted = bin.prob > 0.5, Actual = test.bin$productivity_cat) %>%
    prop.table() %>% addmargins() 
#                Actual
#    Predicted         0         1       Sum
#        FALSE 0.1422594 0.1004184 0.2426778
#        TRUE  0.1171548 0.6401674 0.7573222
#        Sum   0.2594142 0.7405858 1.0000000

tn <- cm.bin[1,1]
tp <- cm.bin[2,2]
fn <- cm.bin[1,2]
fp <- cm.bin[2,1]

cat("Acuracy:", ((tp+tn)/(tp+tn+fp+fn))/1,
    "\nPrecision:", tp/(tp+fn),
    "\nRecall:", tp/(tp+fp),
    "\nF1 Score:", 2*((tp/(tp+fn))*(tp/(tp+fp)))/((tp/(tp+fn))+(tp/(tp+fp))),
    # F1 Score: 2 * (Precision * Recall) / (Precision + Recall)
    "\nTrue Negative Rate:", tn/(tn+fp),
    "\nFalse Positive Rate:", fp/(tn+fp) )

# Accuracy: Proportion of correctly classified instances (both TP and TN) out of all instances.
# Precision: Proportion of true positives among all predicted positive instances.
# Recall: Proportion of true positives among all actual positive instances.
# F1 Score: Harmonic mean of precision and recall, providing a balanced measure of both.

pred.labels <- ifelse(bin.prob > 0.5, 1, 0)
confusionMatrix(data = factor(pred.labels),
                reference = factor(test.bin$productivity_cat), 
                positive = "1")


#
# Support Vector Machines
#

library(kernlab)

fit.svm <- ksvm(productivity_cat ~ . ,
                data = train.base,
                kernel = "vanilladot",
                prob.model = TRUE)

fit.svm

pred.svm <- predict(fit.svm, test.base)

table(Predicted = pred.svm == "achieved", Actual = test.base$productivity_cat) %>%
    prop.table() %>% addmargins() 
#                    Actual
#        Predicted   achieved  unsuccess        Sum
#            FALSE 0.06302521 0.06302521 0.12605042
#            TRUE  0.66386555 0.21008403 0.87394958
#            Sum   0.72689076 0.27310924 1.00000000

confusionMatrix(data = factor(pred.svm),
                reference = factor(test.base$productivity_cat), 
                positive = "achieved")

#           Accuracy : 0.7269          
#             95% CI : (0.6656, 0.7825)
#        Sensitivity : 0.9133          
#        Specificity : 0.2308          
#     Pos Pred Value : 0.7596  

rf.df.predict <- data.frame( predict( fit.svm, test.base, type = 'prob') )
rf.roc <- prediction( rf.df.predict$unsuccess, test.base$productivity_cat)

roc <- performance(rf.roc, 'tpr', 'fpr')
auc <- performance(rf.roc, 'auc')
plot(roc, colorize = T, lwd = 2,
     main = "Roc curve.",
     sub = paste( "AUC =", auc@y.values[[1]] ) )
abline(0.0, 1.0)

