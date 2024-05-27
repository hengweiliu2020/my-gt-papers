library(haven)
library(dplyr)
library(tidyr)
library(gt)

the_date <- as.character(Sys.Date())

adsl <- read_sas("/home/hengweiliu/Documents/adam/adsl.sas7bdat")
adae <- read_sas("/home/hengweiliu/Documents/adam/adae.sas7bdat")

# create adsl4 with treatment variable grp
adsl1 <- adsl[(adsl$TRT01P == 'Placebo' & adsl$SAFFL == 'Y'), ]
adsl1$grp <- 'grp1'
adsl2 <- adsl[(adsl$TRT01P == 'Active' & adsl$SAFFL == 'Y'), ]
adsl2$grp <- 'grp2'
adsl3 <- rbind(adsl1, adsl2)
adsl3$grp <- 'grp3'
adsl4 <- rbind(adsl1, adsl2, adsl3)

# create adae4 with treatment variable grp
adae1 <- adae[(adae$TRT01P == 'Placebo' & adae$SAFFL == 'Y' & adae$TRTEMFL == 'Y'), ]
adae1$grp <- 'grp1'
adae2 <- adae[(adae$TRT01P == 'Active' & adae$SAFFL == 'Y' & adae$TRTEMFL == 'Y'), ]
adae2$grp <- 'grp2'
adae3 <- rbind(adae1, adae2)
adae3$grp <- 'grp3'
adae4 <- rbind(adae1, adae2, adae3)

# get the big N in column headers from adsl4
bign <- table(group = adsl4$grp)

# get the number of subjects with at least one TEAE
group_by_grp <-
  adae4 %>%
  group_by(grp) %>%
  summarise(unique_subj = n_distinct(USUBJID))

# get the count by System Organ class
group_by_grp1 <-
  adae4 %>%
  group_by(grp, AEBODSYS) %>%
  summarise(unique_subj = n_distinct(USUBJID))

# get the count by preferred term
group_by_grp2 <-
  adae4 %>%
  group_by(grp, AEBODSYS, AEDECOD) %>%
  summarise(unique_subj = n_distinct(USUBJID))

# do the transpose
a1 <- spread(group_by_grp, grp, unique_subj)
a2 <- spread(group_by_grp1, grp, unique_subj)
a3 <- spread(group_by_grp2, grp, unique_subj)

a1$term <- "Subjects with at least one TEAE"
a1$AEBODSYS <- " "
a2$term <- a2$AEBODSYS
a3$term <- a3$AEDECOD

a1 <- a1[c("AEBODSYS", "term", "grp1", "grp2", "grp3")]
a2 <- a2[c("AEBODSYS", "term", "grp1", "grp2", "grp3")]
a3 <- a3[c("AEBODSYS", "term", "grp1", "grp2", "grp3")]
final <- rbind(a1, a2, a3)

#sort the ae data by AEBODSYS and descending order in the total column
df <- final[order(final$AEBODSYS, -final$grp3), ]

#create the final data df for reporting
df$perc1 <- 100 * df$grp1 / bign[1]
df$perc1 <- format(round(df$perc1, 1), nsmall = 1)
df$grp1_c <- paste(df$grp1, "(", df$perc1, ")")
df$grp1_c <- ifelse(is.na(df$grp1), "0", df$grp1_c)

df$perc2 <- 100 * df$grp2 / bign[2]
df$perc2 <- format(round(df$perc2, 1), nsmall = 1)
df$grp2_c <- paste(df$grp2, "(", df$perc2, ")")
df$grp2_c <- ifelse(is.na(df$grp2), "0", df$grp2_c)

df$perc3 <- 100 * df$grp3 / bign[3]
df$perc3 <- format(round(df$perc3, 1), nsmall = 1)
df$grp3_c <- paste(df$grp3, "(", df$perc3, ")")
df$grp3_c <- ifelse(is.na(df$grp3), "0", df$grp3_c)

df <- df[c("AEBODSYS", "term", "grp1_c", "grp2_c", "grp3_c")]

