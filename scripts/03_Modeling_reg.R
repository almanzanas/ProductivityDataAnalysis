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

# Data frame with logarithmic transformation and normalization
df.model.reg <- df.model.reg %>%
    mutate( across( where(is.numeric), ~ log(. + 1) ) ) %>%
    mutate( across( where(is.numeric), scale ) )

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


# Extracting dummies for factor, J - 1

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

model.reg <- train( actual_productivity ~ . - date,
                    data = train.reg, 
                    method = "leapSeq")


train.predict <- predict(model.reg, train.reg)
    
postResample(train.predict, test.reg$actual_productivity)

plot(train.predict, residuals(model.reg))

varImp(model.reg)

hist(residuals(model.reg), main = "Histogram of Residuals")

qqnorm(residuals(model.reg))
qqline(residuals(model.reg))

car::influencePlot(model.reg$)




