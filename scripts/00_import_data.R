# Dataset: https://doi.org/10.24432/C51S6D

# Read data with data.table library
library(data.table, quietly = TRUE)
library(tidyverse, quietly = TRUE)

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

colnames(df) <- c("id", names(df[2:ncol(df)]))

# Change date class
df$date <- as.Date(df$date, format = "%m/%d/%Y")
