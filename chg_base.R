library(haven)
library(tidyr)
library(psych)
library(gt)

the_date <- as.character(Sys.Date())

# read in the data
adsl <- read_sas("adsl.sas7bdat")
advs <- read_sas("advs.sas7bdat")

# get the bign for the column header
bign <- table(group=adsl$TRT01P)

# create a function to calculate the descriptive statistics

get_stat <- function(invar, decimal, prefix){

#get the stat
ht <- describeBy(advs[[invar]], group=list(advs$TRT01P, advs$PARAM, advs$AVISIT), mat=TRUE)

# handle the decimals
ht$n <- format(ht$n, nsamll=0)
ht$mean <- format(round(ht$mean,decimal+1), nsmall=decimal+1)
ht$sd <- ifelse(is.na(ht$sd), NA, format(round(ht$sd,decimal+2), nsmall=decimal+2))
ht$median <- format(round(ht$median,decimal+1), nsmall=decimal+1)
ht$min <- format(round(ht$min,decimal), nsmall=decimal)
ht$max <- format(round(ht$max,decimal), nsmall=decimal)

# create a variable minmax
ht$minmax <- paste(ht$min, ',', ht$max)

ht_n <- ht %>%
pivot_wider(id_cols=c(group2, group3), names_from = group1, values_from = n, names_prefix = prefix)

ht_mean <- ht %>%
pivot_wider(id_cols=c(group2, group3), names_from = group1, values_from = mean, names_prefix = prefix)

ht_sd <- ht %>% 
pivot_wider(id_cols=c(group2, group3), names_from = group1, values_from =sd, names_prefix = prefix)

ht_median <- ht %>%
pivot_wider(id_cols=c(group2, group3), names_from = group1, values_from = median, names_prefix = prefix)

ht_minmax <- ht %>%
pivot_wider(id_cols=c(group2, group3), names_from = group1, values_from = minmax, names_prefix = prefix)

ht_n$stat <- "n"
ht_n$ord <- 1

ht_mean$stat <- "Mean"
ht_mean$ord <- 2

ht_sd$stat <- "Std"
ht_sd$ord <- 3

ht_median$stat <- "Median"
ht_median$ord <- 4

ht_minmax$stat <- "Min, Max"
ht_minmax$ord <- 5

ht_final <- rbind(ht_n, ht_mean, ht_sd, ht_median, ht_minmax)
ht_final <- data.frame(ht_final[order(ht_final$group2,ht_final$group3),])

return(ht_final)
}

out1 <- get_stat(invar="AVAL",decimal=1, prefix="val_")
out2 <- get_stat(invar="CHG",decimal=1, prefix="chg_")

# need to merge out1 and out2
final <- merge(out1, out2, by=c("group2", "group3", "ord", "stat"))

final$chg_trt_a <- ifelse(final$group3=="Baseline", NA, final$chg_trt_a)
final$chg_trt_b <- ifelse(final$group3=="Baseline", NA, final$chg_trt_b)

final$fdot <- !duplicated(final[c("group2", "group3")])
final$group3 <- ifelse(final$fdot==TRUE, final$group3, '')

df <- final[c("group2", "group3", "stat", "val_trt_a", "chg_trt_a","val_trt_b", "chg_trt_b")]
df %>%
gt(groupname_col="group2")
