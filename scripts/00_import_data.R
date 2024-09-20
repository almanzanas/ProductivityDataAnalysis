# Dataset: https://doi.org/10.24432/C51S6D

# Read data with data.table library
library(data.table)
library(tidyverse)
library(patchwork)
library(ggstatsplot)
library(GGally)
library(skimr)
library(randomForest)
library(car)
library(caret)
library(ROCR)

df <- fread(file = "data/garments_worker_productivity.csv")
df <- as.data.frame(df, stringsAsFactor = FALSE)
df.original <- df
head(df)

# Generating a key variable
vec.key <- c(paste(sprintf("%04.0f", seq(1:nrow(df)) ),
                   sprintf("%02.0f", round(df$team, 0)),
                   sep="_") )

df <- bind_cols(vec.key, df)
remove(vec.key)

colnames(df)[1] <- c("id")

# Data frame column classes

glimpse(df)
dim(df)

    ## Change date and team classes
df <- df %>% 
    mutate( team = print(sprintf("%02.0f", team)) ) %>%
    mutate_at( "date", as.Date, format = "%m/%d/%Y")

    ## Round no_of_workers and changing class
df <- df %>%
    mutate( no_of_workers = ceiling(no_of_workers) ) %>%
    mutate_at( "no_of_workers", as.integer)

table(df$targeted_productivity) 
    # there is a value '0.07' which is unlikely to
    # be correct, could be a typographical error for '0.70' 
df <- df %>% mutate( 
    targeted_productivity = ifelse(targeted_productivity==0.07, 
                                   0.70, targeted_productivity) )

