#
#
#

# Selecting the best variables thanks to previous comparisons and plots
# and creating the data frames for modeling (classification)

df.model.reg <- df.original %>% 
    mutate( team = print(sprintf("%02.0f", team) ),
            no_of_workers = ceiling(no_of_workers),
            targeted_productivity = ifelse(targeted_productivity==0.07, 
                                           0.70, targeted_productivity)) %>%
    mutate( targeted_productivity = print(sprintf("%02.2f",
                                                  targeted_productivity) ) ) %>%
    mutate( quarter = replace(quarter, quarter == "Quarter5", "Quarter4") ) %>%
    replace_na( list( wip = 0) ) %>%
    mutate_if(is.character, as.factor)

# Checking lambda to choose transformations
library(car)
for (var in colnames(select_if(df.model.reg, is.numeric))) {
    s <- summary(powerTransform(df.model.reg[[var]] + 1))
    cat("\n==========\nVariable:", var, "results:\n")
    print(s)
}

# Data frame with logarithmic transformation and normalization
    # excluding -idle_time, -idle_men, -no_of_style_change

df.model.reg <- df.model.reg %>%
    mutate( across( where(is.numeric), ~ log(. + 1) ) ) %>%
    mutate( across( where(is.numeric), scale ) ) %>%
    mutate( actual_productivity = df.original$actual_productivity,
            idle_time = df.original$idle_time,
            idle_men = df.original$idle_men,
            no_of_style_change = df.original$no_of_style_change)


#
# Training
#

## Splitting into 80/20

set.seed(69)

n.rows <- nrow(df.model.reg)
pct80 <- n.rows * 0.8
idx <- sample(n.rows, pct80)

train.reg <- df.model.reg[idx,]
test.reg <- df.model.reg[-idx,]


# Extracting dummies for factor, J - 1 - If necessary

train.dummies.reg <- list()
interest.factors <- c("quarter", "day", "department", "team", "targeted_productivity")
for (var in interest.factors) {
    dum <- model.matrix(as.formula(paste0("~", var, "-1")),
                        data = train.reg )
    #dum <- dum[,-1]
    train.dummies.reg[[paste0("dummy.",var)]] <- dum
}

test.dummies.reg <- list()
interest.factors <- c("quarter", "day", "department", "team", "targeted_productivity")
for (var in interest.factors) {
    dum <- model.matrix(as.formula( paste0("~", var, "-1") ),
                        data = test.reg )
    #dum <- dum[,-1]
    test.dummies.reg[[paste0("dummy.",var)]] <- dum
}

## Training Linear Regression Stepwise

library(leaps)

leaps <- regsubsets(data = train.reg, 
                    actual_productivity ~ . - date 
                    - department - day - quarter
                    - idle_time - idle_men - team, nbest = 4,
                    nvmax = 50, really.big = T, 
                    )

subsTable <- function(obj, scale){
    x <- summary(leaps)
    m <- cbind(round(x[[scale]],3), x$which[,-1])
    colnames(m)[1] <- scale
    m[order(m[,1], decreasing = T), ]
}

subsTable(leaps, scale="adjr2")[1:6,]



#
#   # Factor Analisis Principal Components

library(psych)
train.reg %>% select_if(is.numeric) %>% .[,-15] %>%
fa.parallel(fa="pc", n.iter=100,
            show.legend=FALSE, main="Scree plot with parallel analysis")
abline(h=1)

principal(train.reg[,-c(1:6, 15)], nfactors = 3)

# Regression with selected variables

sel.var <- c("smv", "wip", "over_time", "incentive", "no_of_workers",
             "team", 
             "no_of_style_change",
             "targeted_productivity", "actual_productivity")

train.reg %>%
    select("smv", "wip", "over_time", "incentive", "no_of_workers",
           "team", 
           "no_of_style_change",
           "targeted_productivity", "actual_productivity") %>% 
    select_if(is.numeric) %>%
    cor()

train.reg[,sel.var] %>% select_if(is.numeric) %>%
    scatterplotMatrix(smooth = FALSE, main = "Scatter Plot Matrix")

fit <- lm(actual_productivity ~ #smv + wip + 
              over_time + incentive +
              no_of_workers + no_of_style_change +
            #  team + 
              targeted_productivity,
          data = train.reg)
    # smv and wip excluded for Multicollinearity (VIF > 10)


summary(fit)

pred.fit <- predict(fit, test.reg)

Metrics::rmse(test.reg$actual_productivity, pred.fit) # 0.15
Metrics::mae(test.reg$actual_productivity, pred.fit) # 0.109
Metrics::mse(test.reg$actual_productivity, pred.fit) # 0.023
R2(test.reg$actual_productivity, pred.fit) # 0.207

# Suggestions:
    # Intercept: Dependent Variable value when independent variables are 0
        # means DV when there are no effect of IV.
    # One standard deviation increase for log(incentive) will  
        # rise 0.399 of actual productivity
    # One standard deviation increase for log(no_of_workers) will 
        # rise 0.467 of actual productivity
    # Most relevant increases is when targeted_productivity are
        # at 0.65 or above, rising between 0.67 and 1.24

confint(fit)

plot(fit)
# Suggestions:
    # Homoscedasticity (scale-location) and Linearity (Residuals vs Fitted)
        # are assumptions reached.
    # Normality it's been not met

# Normality
qqPlot(fit, labels=row.names(states), id=list(method="identify"),
       simulate=TRUE, main="Q-Q Plot")

# Linearity
crPlots(fit)

# Homoscedasticity
spreadLevelPlot(fit)

# Multicollinearity
vif(fit)

#
# Robust Linear Regression
#

fit.rlm <- MASS::rlm(actual_productivity ~ #smv + wip + 
              over_time + incentive +
              no_of_workers + no_of_style_change +
              #team + 
                  targeted_productivity,
          data = train.reg)

summary(fit.rlm)

fit.rlm.pred <- predict(fit.rlm, test.reg)

Metrics::rmse(test.reg$actual_productivity, fit.rlm.pred) # 0.149
Metrics::mae(test.reg$actual_productivity, fit.rlm.pred) # 0.106
Metrics::mse(test.reg$actual_productivity, fit.rlm.pred) # 0.022
R2(test.reg$actual_productivity, fit.rlm.pred) # 0.213

#
# Random Forest Regression
#

fit.rf <- randomForest(actual_productivity ~ #smv + wip + 
                 over_time + incentive +
                 no_of_workers + 
                 team + targeted_productivity,
             data = train.reg)

fit.rf

plot(fit.rf)

importance(fit.rf)

varImpPlot(fit.rf)

fit.rf.pred <- predict(fit.rf, test.reg)

# RMSE:
sqrt(mean((fit.rf.pred - test.reg$actual_productivity)^2))
# Coefficient of Determination:
summary(lm(fit.rf.pred ~ test.reg$actual_productivity))$r.squared

Metrics::rmse(test.reg$actual_productivity, fit.rf.pred) # 0.1288
Metrics::mae(test.reg$actual_productivity, fit.rf.pred) # 0.08681
Metrics::mse(test.reg$actual_productivity, fit.rf.pred) # 0.01658
R2(test.reg$actual_productivity, fit.rf.pred) # 0.4193

#
#   Regression Trees
#

df.model.rpart <- df.original %>%
    select(-date, -day, -quarter, -department) %>%
    mutate( team = print(sprintf("%02.0f", team) ),
            no_of_workers = ceiling(no_of_workers),
            targeted_productivity = ifelse(targeted_productivity == 0.07, 
                                           0.70, targeted_productivity)) %>%
    mutate( targeted_productivity = print(sprintf("%02.2f", 
                                                  targeted_productivity) ) ) %>%
    replace_na( list( wip = 0) ) %>%
    mutate_if(is.character, as.factor)

# Train and test sets

set.seed(69)

n.rows <- nrow(df.model.rpart)
idx <- createDataPartition(df.model.rpart$actual_productivity, p = 0.8, list = FALSE)

train.rpart <- df.model.rpart[idx,-c(3,4,7,8)] # Excluding smv, wip, idle_
test.rpart <- df.model.rpart[-idx,-c(3,4,7,8)]
remove(n.rows)

## Training

library(rpart)

fit.rpart <- rpart(actual_productivity ~ ., data = train.rpart)
fit.rpart2 <- rpart(actual_productivity ~ . -team, data = train.rpart)

fit.rpart
fit.rpart2

library(rpart.plot)

rpart.plot(fit.rpart, digits=3, type = 3)
rpart.plot(fit.rpart2, digits=3, type = 1, tweak=1.4)

    ## Prediction

pred.rpart <- predict(fit.rpart, test.rpart)
pred.rpart2 <- predict(fit.rpart2, test.rpart)

summary(pred.rpart)
summary(test.rpart$actual_productivity)
summary(pred.rpart2)

cor(pred.rpart, test.rpart$actual_productivity)  # 0.63
cor(pred.rpart2, test.rpart$actual_productivity) # 0.66

MAE <- function(actual, predicted) {
    mean(abs(actual - predicted) )
}
MSE <- function(actual, predicted) {
    mean((actual - predicted)^2)
}
RMSE <- function(actual, predicted) {
    sqrt(mean((actual - predicted)^2))
}
R2 <- function(actual, predicted) {
    ss_res <- sum((actual - predicted)^2)
    ss_tot <- sum((actual - mean(actual))^2)
    1 - (ss_res / ss_tot)
}

R2(test.rpart$actual_productivity, pred.rpart)   # 0.32
R2(test.rpart$actual_productivity, pred.rpart2)  # 0.44

MAE(test.rpart$actual_productivity, pred.rpart)  # 0.099
MAE(test.rpart$actual_productivity, pred.rpart2) # 0.096

MSE(test.rpart$actual_productivity, pred.rpart)  # 0.020
MSE(test.rpart$actual_productivity, pred.rpart2) # 0.019

RMSE(test.rpart$actual_productivity, pred.rpart) # 0.14
RMSE(test.rpart$actual_productivity, pred.rpart2)# 0.137


#
#   Cubist model tree
#

library(Cubist)

    # Training set with log() and scale()

fit.cubist <- cubist( x = train.reg[-c(1,2,4,5,7,8,11,12,15)],
                      y = train.reg$actual_productivity,
                      rules = NA)

fit.cubist

summary(fit.cubist)

pred.cubist <- predict(fit.cubist, test.reg)

R2(test.reg$actual_productivity, pred.cubist)   # 0.39

MAE(test.reg$actual_productivity, pred.cubist)  # 0.084

MSE(test.reg$actual_productivity, pred.cubist)  # 0.017

RMSE(test.reg$actual_productivity, pred.cubist) # 0.131


