#
# Before this code, execute '00_import_data.R'
#

# Enabling 'just in time' compilation
compiler::enableJIT(3)

# Check duplicates
anyDuplicated(df[,2:ncol(df)])

# Check Missing Values
anyNA(df)

for (var in colnames(df)){
    print(
        paste(var, sum(is.na(df[var]) ) )
    )
}

df.na <- df[which(is.na(df$wip)),]

df.na %>% select(team, department, date, wip, actual_productivity)
table(df$department) # Same number of rows on department-finishing

    ## Answering: department == finishing means wip == NA ?

df.dep.filt <- df %>% filter(department == "finishing")
all.equal( df.dep.filt$id, df.na$id ) # TRUE
remove(df.dep.filt)
remove(df.na)

# filling NA values with 0 (if the team finish, then no wip)

df.no.na <- df %>% replace_na( list( wip = 0) )
anyNA(df.no.na) # FALSE

table(df.no.na$idle_time)

# Checking unique values

for (var in colnames(df.no.na[,-1])){
    if( is.character(df.no.na[, var])){
        print( paste(var, unique(df.no.na[, var]) ) )
    }
} # There are 5 Quarters.

    ## The month was divided into four quarters
    ## then let's add Quarter5 to Quarter4

filt.quarter <- df.no.na %>% 
    select(id, date, quarter) %>% 
    filter(quarter == "Quarter5") 
# Days 29, 30 and 31 are labeled as Quarter5, should be Quarter4
remove(filt.quarter)

df.no.na <- df.no.na %>% 
    mutate(
        quarter = replace(quarter, quarter == "Quarter5", "Quarter4")
    )
df.no.na %>% 
    select(id, date, quarter) %>% 
    filter(quarter == "Quarter5")

# Checking outlier values by IQR

out.cols <- c("actual_productivity", "wip", "idle_time", "idle_men")
out.list <- list()
for (var in out.cols) {
    if ( is.numeric(df.no.na[, var]) ) {
        out <- boxplot.stats(df.no.na[, var])$out
        out.list[[var]] <- out
    }
}
remove(var)
remove(out)

# We assume the data is objectively recorded, then the outliers are real values
# but extreme ones. It will remain stored for modeling/transformation purposes.

# Exploring the data frame

num.cols <- NULL
for (var in colnames(df.no.na)) {
    if ( is.numeric(df.no.na[, var]) ) {
        num.cols <- c(num.cols, var)
    }
}
cat("Numeric columns:\n", num.cols, sep = ", ")

char.cols <- NULL
for (var in colnames(df.no.na)) {
    if ( is.character(df.no.na[, var]) ) {
        char.cols <- c(char.cols, var)
    }
}
cat("Numeric columns:\n", char.cols, sep = ", ")

    ## Plots: bars and Histograms

df.no.na %>% 
    ggplot() + geom_bar( aes( x = team),
                         alpha = 0.5, ) +
    scale_x_continuous(breaks = seq(min(unique(df.no.na$team)), 
                                    max(unique(df.no.na$team)), by = 1)) +
    ggtitle("Count of Teams")

df.no.na %>%
    ggplot() + geom_histogram( aes( x = no_of_workers),
                               alpha = 0.5, fill = "darkblue") +
    labs(title = "Number of Workers per Team", y = "")
    
df.no.na %>%
    ggplot() + geom_histogram( aes( x = actual_productivity),
                               alpha = 0.5, fill = "darkblue") +
    labs(title = "Actual Productivity Histogram", y = "")

df.no.na %>%
    ggplot() + geom_histogram( aes( x = wip),
                               alpha = 0.5, fill = "darkblue") +
    labs(title = "Unfinished Items Histogram", y = "")

df.no.na %>%
    ggplot() + geom_histogram( aes( x = smv),
                               alpha = 0.5, fill = "darkblue") +
    labs(title = "Standard Minute Value Histogram", y = "")

    ## Categorical Data

df.no.na %>% 
    ggplot() + geom_bar( aes( x = quarter),
                         alpha = 0.5, ) +
    ggtitle("Count of Month Quarters")

df.no.na %>% 
    ggplot() + geom_bar( aes( x = department),
                         alpha = 0.5, ) +
    ggtitle("Count of Department")

df.no.na %>% 
    ggplot() + geom_bar( aes( x = day),
                         alpha = 0.5, ) +
    ggtitle("Count of Weekdays")

df.no.na %>% 
    summary()
