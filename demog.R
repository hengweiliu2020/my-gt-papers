library(haven)
library(psych)
library(gt)

the_date <- as.character(Sys.Date())

# read in the data
class <- read_sas("class.sas7bdat")

# get the descriptive statistics
ht <- describeBy(class$Height, group=class$trt, mat=TRUE)
wt <- describeBy(class$Weight, group=class$trt, mat=TRUE)

# get the count and percentage
bign <- table(group=class$trt)
bign1 <- as.numeric(bign[1])
bign2 <- as.numeric(bign[2])

freq <- table(class$Sex, group=class$trt)
prop <- 100*prop.table(table(class$Sex, class$trt), 2)

# handle the decimals
ht$n <- format(ht$n, nsmall=0)
ht$mean <- format(round(ht$mean,2), nsmall=2)
ht$sd <- format(round(ht$sd,3), nsmall=3)
ht$median <- format(round(ht$median,2), nsmall=2)
ht$min <- format(round(ht$min,1), nsmall=1)
ht$max <- format(round(ht$max,1), nsmall=1)

wt$n <- format(wt$n, nsmall=0)
wt$mean <- format(round(wt$mean,2), nsmall=2)
wt$sd <- format(round(wt$sd,3), nsmall=3)
wt$median <- format(round(wt$median,2), nsmall=2)
wt$min <- format(round(wt$min,1), nsmall=1)
wt$max <- format(round(wt$max,1), nsmall=1)

# create a variable minmax, and do transpose
ht$minmax <- paste(ht$min, ',', ht$max)
ht <- ht[c("n","mean","sd","median","minmax")]
ht2 <- t(ht)

wt$minmax <- paste(wt$min, ',', wt$max)
wt <- wt[c("n","mean","sd","median","minmax")]
wt2 <- t(wt)

# combine count and percentage
X11 <- paste(freq[,1], '(', format(prop[,1], digit=3), ')')
X12 <- paste(freq[,2], '(', format(prop[,2], digit=3), ')')
sex <- cbind(X11, X12)

# create a new column called statistics and get the final data for
reporting
rownames(sex) <- c('Female','Male')
rownames(ht2)<- c("n","mean","sd","median","min, max")

sex3 <- data.frame(statistics=rownames(sex), sex)
ht3 <- data.frame(statistics=rownames(ht2), ht2)
wt3 <- data.frame(statistics=rownames(ht2), wt2)

final <- rbind(ht3, wt3, sex3)
