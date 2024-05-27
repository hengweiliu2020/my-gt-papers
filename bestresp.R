
# Author: Hengwei Liu
# illustrate how to create the R function cat_stat
# the cat_stat can calculate count and percentage of a categorical variable

library(haven)
library(dplyr)
library(tidyr)
library(gt)
library(stringr)
library(psych)
source("cat_stat.R")

# create a dataframe adsl to be used for analysis

mat <- matrix(NA, nrow = 9, ncol = 3)

mat[1,1] <- '0001'
mat[2,1] <- '0002'
mat[3,1] <- '0003'
mat[4,1] <- '0004'
mat[5,1] <- '0005'
mat[6,1] <- '0006'
mat[7,1] <- '0007'
mat[8,1] <- '0008'
mat[9,1] <- '0009'

mat[1,2] <- 'WHITE'
mat[2,2] <- 'WHITE'
mat[3,2] <- 'ASIAN'
mat[4,2] <- 'BLACK OR AFRICAN AMERICAN'
mat[5,2] <- 'WHITE'
mat[6,2] <- 'ASIAN'
mat[7,2] <- 'NATIVE HAWAIIAN OR OTHER PACIFIC ISLANDER'
mat[9,2] <- 'NATIVE HAWAIIAN OR OTHER PACIFIC ISLANDER'

mat[1,3] <- 'TRT A'
mat[2,3] <- 'TRT A'
mat[3,3] <- 'TRT A'
mat[4,3] <- 'TRT B'
mat[5,3] <- 'TRT B'
mat[6,3] <- 'TRT B'
mat[7,3] <- 'TRT B'
mat[8,3] <- 'TRT C'
mat[9,3] <- 'TRT C'

adsl <- data.frame(mat)
adsl$USUBJID <- adsl$X1
adsl$RACE <- adsl$X2
adsl$TRT01A <- adsl$X3
adsl <- adsl[c("USUBJID","RACE","TRT01A")]

print(adsl)


  adsl <- adsl %>% 
    mutate(RACE = case_when(
      is.na(RACE)         ~ "MISSING", 
      !is.na(RACE)        ~ RACE
     
    ))
  


out1 <- cat_stat(df=adsl, my_var=RACE, my_label="Race n(%)",  
                 category= matrix(list("WHITE","BLACK OR AFRICAN AMERICAN","ASIAN","NATIVE HAWAIIAN OR OTHER PACIFIC ISLANDER", "AMERICAN INDIAN OR ALASKA NATIVE")), 
                 catlabel = matrix(list("White","Black or African American","Asian","Native Hawaiian or Other Pacific Islander", "American Indian or Alaska Native"))
                 )

print(out1)







