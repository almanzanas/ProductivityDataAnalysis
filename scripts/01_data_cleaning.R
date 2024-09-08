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
} # The values look correct


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
# but extreme ones. It will remain stored for modeling purposes.

