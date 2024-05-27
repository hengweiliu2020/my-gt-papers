library(haven)
library(dplyr)
library(tidyr)
library(gt)
library(Hmisc)

the_date <- as.character(Sys.Date())

# read the data
adsl <- read_sas("C:\\efficacy\\adsl.sas7bdat")
adrs <- read_sas("C:\\efficacy\\adrs.sas7bdat")

adsl$TRT01P <- gsub(" ", "", adsl$TRT01P)
adrs <- adrs[(adrs$RSEVAL=="Independent Central Review" & adrs$PARAMCD=="CBRSP"), ]
adrs$TRT01P <- gsub(" ", "", adrs$TRT01P)

# get the big N in column headers from adsl
bign <- table(group=adsl$TRT01P)

# do the counting by TRT01P, AVALC
count0 <-
adrs %>%
group_by(TRT01P, AVALC) %>%
summarise(unique_subj = n_distinct(USUBJID))

# generate a frame data called comb
# it has all the treatment group and all response values CR, PR, SD, PD, NE

mat <- matrix(NA, nrow = 5, ncol = 2)
xy <- data.frame(mat)
xy[[1,1]] <- data.frame(X1='CR')
xy[[2,1]] <- data.frame(X1='PR')
xy[[3,1]] <- data.frame(X1='SD')
xy[[4,1]] <- data.frame(X1='PD')
xy[[5,1]] <- data.frame(X1='NE')

for (i in seq(1,5)){
xy[[i,2]] <- data.frame(X2=i)
}

for (i in seq(1:length(unique(count0$TRT01P)))){
xy$X3 <- unique(count0$TRT01P)[i]
assign(paste0('comb', i, sep=''), xy)
}

comb <- do.call("rbind", mget(sprintf("comb%d", 1:length(unique(count0$TRT01P)))))
comb$AVALC <- unlist(comb$X1)
comb$TRT01P <- unlist(comb$X3)
comb$X2 <- unlist(comb$X2)

# merge comb with count0 and calculate percentage
m_count0 <- merge(count0, comb, by=c("TRT01P", "AVALC"), all=TRUE)

m_count0$denom <- ifelse(m_count0$TRT01P=='GroupA', bign[1], ifelse(m_count0$TRT01P=='GroupB', bign[2], bign[3]))

m_count0$value <- ifelse(is.na(m_count0$unique_subj),"0", paste(m_count0$unique_subj, "(", format(round(100*m_count0$unique_subj/m_count0$denom, 1), nsmall = 1), ")"))

# do the transpose
a1 <- m_count0 %>%
pivot_wider(id_cols=c(X2, AVALC), names_from = TRT01P, values_from = value, names_prefix = "")

a1$catlabel <- a1$AVALC
a1$catlabel=ifelse(a1$AVALC=='CR','Complete Response (CR)', a1$catlabel)
a1$catlabel=ifelse(a1$AVALC=='PR','Partial Response (PR)', a1$catlabel)
a1$catlabel=ifelse(a1$AVALC=='SD','Stable Disease (SD)', a1$catlabel)
a1$catlabel=ifelse(a1$AVALC=='PD','Progressive Disease (PD)', a1$catlabel)
a1$catlabel=ifelse(a1$AVALC=='NE','Not Evaluable (NE)', a1$catlabel)

a1$block <- "Confirmed Best Overall Response"
a1 <- a1[order(a1$X2), ]

# do the ORR
mat <- matrix(NA, nrow = 1, ncol = 3)
z <- data.frame(mat)
z$GroupA <- z$X1
z$GroupB <- z$X2
z$GroupC <- z$X3
x <- table(group=adsl$TRT01P)
n <- table(group=adrs[(adrs$AVALC=='CR' | adrs$AVALC=='PR'),]$TRT01P)

# create a function to combine the proportion and confidence interval
getci <- function (grp){
grp <- {{grp}}
n[grp] <- ifelse(is.na(n[grp]), 0, n[grp])
t <- binconf(n[grp], x[grp], method="exact")
ci <- paste0(round(100*t[1], digits=1), ' (', round(100*t[2], digits=1), ',
', round(100*t[3], digits=1),')')
return(ci)
}
z$GroupA <- getci("GroupA")
z$GroupB <- getci("GroupB")
z$GroupC <- getci("GroupC")
z$catlabel <- "ORR (95% CI) [a]"
z$block <- "Objective Response Rate"

# do the CBR
mat <- matrix(NA, nrow = 1, ncol = 3)
y <- data.frame(mat)
y$GroupA <- y$X1
y$GroupB <- y$X2
y$GroupC <- y$X3
x <- table(group=adsl$TRT01P)
n <- table(group=adrs[(adrs$AVALC=='CR' | adrs$AVALC=='PR' |
adrs$AVALC=="SD"),]$TRT01P)
# create a function to combine the proportion and confidence interval
getci <- function (grp){
grp <- {{grp}}
n[grp] <- ifelse(is.na(n[grp]), 0, n[grp])
t <- binconf(n[grp], x[grp], method="exact")
ci <- paste0(round(100*t[1], digits=1), ' (', round(100*t[2], digits=1), ',
', round(100*t[3], digits=1),')')
return(ci)
}

y$GroupA <- getci("GroupA")
y$GroupB <- getci("GroupB")
y$GroupC <- getci("GroupC")
y$catlabel <- "CBR (95% CI) [a]"
y$block <- "Clinical Benefit Rate"
a1 <- a1[c("block","catlabel","GroupA", "GroupB", "GroupC")]
z <- z[c("block","catlabel","GroupA", "GroupB", "GroupC")]
y <- y[c("block","catlabel","GroupA", "GroupB", "GroupC")]
df <- rbind(a1, z, y)
df %>%
gt(groupname_col="block")
