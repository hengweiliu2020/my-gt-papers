library(haven)
library(psych)

# read in the data
class <- read_sas("class.sas7bdat")

summary <- function(df, my_var, classvar, decimal) {

# get the descriptive statistics
ht <- describeBy(df[[my_var]], group=df[[classvar]], mat=TRUE)

# handle the decimals
ht$n <- format(ht$n, nsamll=0)
ht$mean <- format(round(ht$mean,decimal+1), nsmall=decimal+1)
ht$sd <- format(round(ht$sd,decimal+2), nsmall=decimal+2)
ht$median <- format(round(ht$median,decimal+1), nsmall=decimal+1)
ht$min <- format(round(ht$min,decimal), nsmall=decimal)
ht$max <- format(round(ht$max,decimal), nsmall=decimal)

# create a variable minmax, and do transpose
ht$minmax <- paste(ht$min, ',', ht$max)
ht <- ht[c("n","mean","sd","median","minmax")]
ht2 <- t(ht)

# create a new column called statistics and get the final data for reporting
rownames(ht2) <- c("n","mean","sd","median","min, max")
ht3 <- data.frame(statistics=rownames(ht2), ht2)
print(ht3, row.names=FALSE)
}

summary(df=class, my_var="Height", classvar="trt", decimal=1)
summary(df=class, my_var="Weight", classvar="trt", decimal=1)
summary(df=class, my_var="Age", classvar="trt", decimal=0)
