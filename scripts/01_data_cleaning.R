#
# Before this code, execute '00_import_data.R'
#

# Enabling 'just in time' compilation
compiler::enableJIT(3)

# Changing scientific notion
options( scipen = 999, digits = 4 )

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

    ## Answering: department == finishing, means wip == NA ?

df.dep.filt <- df %>% filter(department == "finishing")
all.equal( df.dep.filt$id, df.na$id ) # TRUE
remove(df.dep.filt)
remove(df.na)

# filling NA values with 0 (if the team finish, then no wip)

df.clean <- df %>% replace_na( list( wip = 0) )
anyNA(df.clean) # FALSE

table(df.clean$idle_time)

# Checking unique values

for (var in colnames(df.clean[,-1])){
    if( is.character(df.clean[, var])){
        print( paste(var, unique(df.clean[, var]) ) )
    }
} # There are 5 Quarters.

    ## The month was divided into four quarters
    ## then let's add Quarter5 to Quarter4

filt.quarter <- df.clean %>% 
    select(id, date, quarter) %>% 
    filter(quarter == "Quarter5") 
# Days 29, 30 and 31 are labeled as Quarter5, should be Quarter4
remove(filt.quarter)

df.clean <- df.clean %>% 
    mutate(
        quarter = replace(quarter, quarter == "Quarter5", "Quarter4")
    )
df.clean %>% 
    select(id, date, quarter) %>% 
    filter(quarter == "Quarter5")

#
#
# Exploring the data with histograms
for (var in colnames(select_if(df.clean[,1:ncol(df.clean)], is.numeric) ) ) {
    hist( unlist( df.clean[,var]), col = "blue",
          main = paste("Histogram of", var),
          xlab = var)
}   # Target_productivity and actual_productivity have left tail
    # and actual_productivity is close to normal distribution
    # The rest of variables have right tail
    # except no_of_worker which seems to be mesokurtic


# Creating a target variable for productivity
df.clean <- df.clean %>% mutate( 
    productivity_cat = ifelse(actual_productivity >= targeted_productivity,
                              "achieved", "unsuccess")
    )

# Summary to explore the data

table( df.clean$productivity_cat)
summ <- skim(df.clean)
summ %>% 
    filter(skim_type == "numeric") %>%
    mutate( cv = numeric.sd / numeric.mean )
    # For incentive, idle_time, idle_men, no_of_style_change the p75 are near 0 or 0
        # also this variables have high CV (coefficient of variation)
    # Most of variables have high standard deviation which suggest outliers and
        # spread data
    # 875 (73.1%) achieve the target productivity 

table( df.clean$productivity_cat)


    ## Boxplots

df.clean %>%
    ggplot() +
    geom_boxplot( aes( x = actual_productivity,
                       y = productivity_cat) ) +
    labs( y = "") +
    theme_classic() +
    theme(legend.position = "none")


df.clean %>%
    ggplot() +
    geom_boxplot( aes( x = incentive,
                       y = productivity_cat) ) +
    labs( y = "") +
    theme_classic() +
    theme(legend.position = "none")


g1 <- df.clean %>%
    filter( productivity_cat == "achieved") %>%
    ggplot() +
    geom_boxplot( aes( y = reorder(team, actual_productivity),
                       x = actual_productivity,
                       fill = productivity_cat ) ) +
    labs( title = paste( "Productivity is achieved"),
          subtitle = "On Team levels",
          y = "") +
    theme_classic() +
    theme(legend.position = "none")
g2 <- df.clean %>%
    filter( productivity_cat == "unsuccessful") %>%
    ggplot() +
    geom_boxplot( aes( y = reorder(team, actual_productivity),
                       x = actual_productivity,
                       fill = productivity_cat ) ) +
    labs( title = paste( "Productivity is unsuccessful"),
          subtitle = "On Team levels",
          y = "") +
    theme_classic() +
    theme(legend.position = "none")

(g1|g2) # It can be seen differences between both groups
        # 'unsuccessful' is mode skewed but 'achieved' has outliers


df.achieve <- df.clean %>% filter(productivity_cat == "achieved")
df.unsucces <- df.clean %>% filter(productivity_cat == "unsuccessful")


g1 <- df.achieve %>%
    ggplot() +
        geom_boxplot( aes( y = reorder(targeted_productivity, actual_productivity),
                           x = actual_productivity,
                           fill = productivity_cat ) ) +
        labs( title = paste( "Productivity is achieved"),
              subtitle = "On targeted_productivity levels",
              y = "") +
        theme_classic() +
        theme(legend.position = "none")
g2 <- df.unsucces %>%
    ggplot() +
        geom_boxplot( aes( y = reorder(targeted_productivity, actual_productivity),
                           x = actual_productivity,
                           fill = productivity_cat ) ) +
        labs( title = paste( "Productivity is unsuccessful"),
              subtitle = "On targeted_productivity levels",
              y = "") +
        theme_classic() +
        theme(legend.position = "none")

(g1|g2)

df.achieve %>%
    ggplot() +
    geom_boxplot( aes( y = reorder(department, actual_productivity),
                       x = actual_productivity,
                       fill = productivity_cat ) ) +
    labs( title = paste( "Productivity is achieved"),
          subtitle = "On targeted_productivity levels",
          y = "") +
    theme_classic() +
    theme(legend.position = "none")

# Comparisons between groups on every numeric variable

for (var in colnames(select_if(df.clean[,1:ncol(df.clean)], is.numeric) ) ) {
    wilx <- wilcox.test(df.unsucces[[var]], df.achieve[[var]])
    rbis <- abs(sum(-1, (2 * wilx$statistic) / (nrow(df.achieve) * nrow(df.unsucces) ) ) )
    cat("\n===============\n",
        "\nMann-Whitney U between spam and not spam on:", var, 
        "\np-value:", sprintf("%6.4f", wilx$p.value),
        "\nr:", rbis)
}   # Variables smv, wip, over_time, incentive, no_of_workers, actual_productivity
    # will be interesting to the model


df.clean %>% 
    select(smv, wip, over_time, incentive, no_of_workers, actual_productivity) %>% 
    cor()


#
#
#   ## Team comparisons

df.clean %>% ggbarstats( y = team, x = productivity_cat, 
                         title = "Productivity achieve/unsuccess proportions by team")
    # Teams 03, 01, 06, and 04 have the highest achieve rate (>80%)
    # Teams 08, 07, 06, and 09 have a lack of achieve productivity (33-45% unsuccess)
        # Also, 07 and 08 teams have no significance, thus suggests that productivity 
        # in these two teams is not being significantly influenced by the variable “work team”.
    # Chi-square suggests that there is a relationship between productivity and teamwork in teams with p < 0.05.

df.clean <- df.clean %>%
    mutate( unproductive = ifelse(team == "07" | team == "08", "yes", "no") ) 
df.clean$unproductive <- as.factor(df.clean$unproductive)


library(ggpubr)

ggboxplot( data = df.clean, y = "actual_productivity", x = "unproductive",
           color = "unproductive", add = "jitter", shape = "unproductive") +
    stat_compare_means( comparisons = c("yes", "no") ) +
    stat_compare_means( label.y = 1.2)

g1 <- ggboxplot( data = df.clean, y = "incentive", x = "unproductive",
           color = "unproductive", add = "jitter", shape = "unproductive",
           ylim = c(0,150)) +
    stat_compare_means( comparisons = c("yes", "no") ) +
    stat_compare_means( label.y = 140, size = 5) +
    labs( title = "Incentive by teams on productiveness",
          subtitle = "The lower inventive for the teams, the lower the productivity ('unsuccess')") +
    theme(
        plot.title = element_text(size = 16), 
        plot.subtitle = element_text(size = 13), 
        axis.text = element_text(size = 12), 
        legend.text = element_text(size = 12), 
        legend.title = element_text(size = 14), 
        axis.title = element_text(size = 14), 
    ) 

wilcox.test( df.clean$incentive ~ df.clean$unproductive )
df.clean %>% group_by(unproductive) %>%
    summarize(median_incentive = median(incentive), 
              mean_incentive = mean(incentive),
              sd_incentive = sd(incentive) )

    # These results suggest that those teams (07 and 08) that have been observed
    # to be less productive by not meeting target productivity 
    # have significantly lower incentives.

ggboxplot( data = df.clean, y = "over_time", x = "unproductive",
           color = "unproductive", add = "jitter", shape = "unproductive",
           ylim = c(0,10000)) +
    stat_compare_means( comparisons = c("yes", "no") ) +
    stat_compare_means( label.y = 8000, label.x = "yes")

ggboxplot( data = df.clean, y = "no_of_workers", x = "unproductive",
           color = "unproductive", add = "jitter", shape = "unproductive",
           ylim = c(0,80)) +
    stat_compare_means( comparisons = c("yes", "no") ) +
    stat_compare_means( label.y = 75)

g2 <- ggboxplot( data = df.clean, y = "no_of_style_change", x = "unproductive",
           color = "unproductive", add = "jitter", shape = "unproductive",
           ylim = c(0,2)) +
    stat_compare_means( comparisons = c("yes", "no") ) +
    stat_compare_means( label.y = 2, size = 5 ) +
    labs( title = "Number Style Changes by teams on productiveness",
          subtitle = "With more number of changes there are\nless target productivity 'achieve'") +
    theme(
        plot.title = element_text(size = 16), #
        plot.subtitle = element_text(size = 13), #
        axis.text = element_text(size = 12), #
        legend.text = element_text(size = 12), #
        legend.title = element_text(size = 14), #
        axis.title = element_text(size = 14), #
    )

wilcox.test( df.clean$no_of_style_change ~ df.clean$unproductive )
df.clean %>% group_by(unproductive) %>%
    summarize(median_incentive = median(no_of_style_change), 
              mean_incentive = mean(no_of_style_change),
              sd_incentive = sd(no_of_style_change) )

    # No significant differences in 'over_time' or 'no_of_workers' were observed 
    # comparing the teams. However, in `no_of_style_change' significant 
    # differences are observed, being the teams with the lowest productivity 
    # the ones with the highest style changes.

cor(df.clean$actual_productivity, df.clean$no_of_style_change)

    # An inverse relationship (r= -0.207) can be seen between 
    # current productivity and 'no_of_style_change', 
    # so the more style changes the lower the productivity.

s <- (g1|g2)
ggsave("images/00.png", plot = s, width = 16, height = 9)

