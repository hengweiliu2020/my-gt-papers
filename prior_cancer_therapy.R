# Author: Hengwei Liu
# Date: 2023-12-28
# Purpose: table for prior cancer therapy

ccUI <- function(id){
  ns <- NS(id) 
  
  tagList(
    
    column(
      width=12,  
      
      mainPanel(tableOutput(ns("dc")))
    )
  )
  
}


ccServer <- function(id) {
  moduleServer(id, 
               function(input, output, session) {
                 

library(haven)
library(dplyr)
library(tidyr)
library(gt)
library(stringr)
library(psych)


the_date <- as.character(Sys.Date())


returnTable2 <- reactive({

adsl <- read_sas("adsl.sas7bdat")
adsl <- adsl[(adsl$FASFL=='Y'),]

adsl2 <- adsl
adsl2$TRT01P <- "Total"

adsl3 <- rbind(adsl,adsl2)
adsl3$TRTP <- adsl3$TRT01P

adcm <- read_sas("adcm.sas7bdat")
adcm <- adcm[(adcm$FASFL=='Y'),]

adcm2 <- adcm
adcm2$TRTP <- "Total"

adcm3 <- rbind(adcm,adcm2)
adcm3 <- adcm3[(adcm3$CMCAT=="PRIOR CANCER SYSTEMIC THERAPY"),]

trt <- sort(unique(adsl3$TRT01P))
bign <- table(group=adsl3$TRT01P)

print(trt)
print(bign)
dim(bign)


summary <- function(df, my_var, classvar, decimal, my_label) {
  # get the descriptive statistics
  ht <- describeBy(df[[my_var]], group=df[[classvar]], mat=TRUE)
  
  # handle the decimals
  ht$n <- format(ht$n, nsamll=0)
  ht$mean <- format(round(ht$mean,decimal+1), nsmall=decimal+1)
  ht$sd <- format(round(ht$sd,decimal+2), nsmall=decimal+2)
  ht$median <- format(round(ht$median,decimal+1), nsmall=decimal+1)
  ht$min <- format(round(ht$min,decimal), nsmall=decimal)
  ht$max <- format(round(ht$max,decimal), nsmall=decimal)
  
  # create a variable minmax
  ht$minmax <- paste(ht$min, ',', ht$max) 
  
  ht1 <- ht[c("n","group1")]
  a1 <- ht1 %>%
    pivot_wider(values_from=n, names_from=group1)
  a1$statistics <- "n"
  
  ht2 <- ht[c("mean","group1")]
  a2 <- ht2 %>%
    pivot_wider(values_from=mean, names_from=group1)
  a2$statistics <- "mean"
  
  ht3 <- ht[c("sd","group1")]
  a3 <- ht3 %>%
    pivot_wider(values_from=sd, names_from=group1)
  a3$statistics <- "Standard Deviation"
  
  ht4 <- ht[c("median","group1")]
  a4 <- ht4 %>%
    pivot_wider(values_from=median, names_from=group1)
  a4$statistics <- "Median"
  
  ht5 <- ht[c("minmax","group1")]
  a5 <- ht5 %>%
    pivot_wider(values_from=minmax, names_from=group1)
  a5$statistics <- "Min, Max"
  
  a6 <- rbind(a1,a2,a3,a4,a5)
  a6$catlabel <- my_label 
  
  return(a6)
}

cat_stat2 <- function(df, my_var, my_label, myvar) {
  
  df[[myvar]] <- ifelse(df[[myvar]]=="", 'Missing', df[[myvar]])
  
  count0 <- 
    df %>%                   
    group_by(TRTP, {{my_var}}) %>%
    summarise(unique_subj = n_distinct(USUBJID))
  
  count3 <- 
    adsl3 %>%                   
    group_by(TRTP) %>%
    summarise(unique_subj = n_distinct(USUBJID))
  
  count3$n <- count3$unique_subj
  count3 <- count3[c("TRTP","n")]
  
  m_count0 <- merge(count0, count3, by=c("TRTP"), all=TRUE)
  
  m_count0$catlabel <- my_label
  
  m_count0 <- m_count0 %>%
    mutate(statistics={{my_var}})
  
  m_count0 <- m_count0[c("catlabel","statistics","TRTP","n","unique_subj")]
  
  
  m_count0$value <- ifelse(is.na(m_count0$unique_subj),"0", paste(m_count0$unique_subj, "(", format(round(100*m_count0$unique_subj/m_count0$n, 1), nsmall = 1), ")"))
  
  a1 <- m_count0 %>%
    pivot_wider(id_cols=c("catlabel","statistics"), values_from=value, names_from=TRTP)
  
  return(a1)
  
}


cat_stat1 <- function(df, my_var, my_label, myvar) {
  
  df[[myvar]] <- ifelse(df[[myvar]]=="", 'Uncoded', df[[myvar]])
  
  count0 <- 
    df %>%                   
    group_by(TRTP, {{my_var}}) %>%
    summarise(unique_subj = n_distinct(USUBJID))
  
  count3 <- 
    adsl3 %>%                   
    group_by(TRTP) %>%
    summarise(unique_subj = n_distinct(USUBJID))
  
  count3$n <- count3$unique_subj
  count3 <- count3[c("TRTP","n")]
  
  m_count0 <- merge(count0, count3, by=c("TRTP"), all=TRUE)
  
  m_count0$catlabel <- my_label
  
  m_count0 <- m_count0 %>%
    mutate(statistics={{my_var}})
  
  m_count0 <- m_count0[c("catlabel","statistics","TRTP","n","unique_subj")]
  
  
  m_count0$value <- ifelse(is.na(m_count0$unique_subj),"0", paste(m_count0$unique_subj, "(", format(round(100*m_count0$unique_subj/m_count0$n, 1), nsmall = 1), ")"))
  
  a1 <- m_count0 %>%
    pivot_wider(id_cols=c("catlabel","statistics"), values_from=value, names_from=TRTP)
  
  return(a1)
  
}




cat_stat3 <- function(df, my_var, my_label, myvar, my_var2, myvar2) {
  
  df[[myvar]] <- ifelse(df[[myvar]]=="", 'Uncoded', df[[myvar]])
  df[[myvar2]] <- ifelse(df[[myvar2]]=="", 'Uncoded', df[[myvar2]])
  
  count0 <- 
    df %>%                   
    group_by(TRTP, {{my_var}}, {{my_var2}}) %>%
    summarise(unique_subj = n_distinct(USUBJID))
  
  count3 <- 
    adsl3 %>%                   
    group_by(TRTP) %>%
    summarise(unique_subj = n_distinct(USUBJID))
  
  count3$n <- count3$unique_subj
  count3 <- count3[c("TRTP","n")]
  
  m_count0 <- merge(count0, count3, by=c("TRTP"), all=TRUE)
  
  m_count0$catlabel <- my_label
  
  m_count0 <- m_count0 %>%
    mutate(statistics={{my_var}})
  
  m_count0 <- m_count0[c("catlabel","statistics","TRTP","n","unique_subj", myvar2)]
  
  
  m_count0$value <- ifelse(is.na(m_count0$unique_subj),"0", paste(m_count0$unique_subj, "(", format(round(100*m_count0$unique_subj/m_count0$n, 1), nsmall = 1), ")"))
  
  a1 <- m_count0 %>%
    pivot_wider(id_cols=c("catlabel","statistics", myvar2), values_from=value, names_from=TRTP)
  
  return(a1)
  
}




out1 <- cat_stat2(df=adsl3, my_var=NPRESYSN, 
                  my_label="Lines of Prior Cancer Systemic Therapy for Advanced/Metastatic Cancer", myvar='NPRESYSN')

out2 <- cat_stat2(df=adsl3, my_var=NLINSTC, 
                  my_label="Number of Lines of Prior Cancer Systemic Therapy", myvar='NLINSTC')


out3 <- summary(df=adsl3, my_var='NLINST', classvar='TRT01P', decimal=1, my_label="Number of Lines of Prior Cancer Systemic Therapy")


out4 <- cat_stat2(df=adcm3, my_var=CMINDC, 
                  my_label="Intent of Prior Cancer Systemic Therapy", myvar='CMINDC')

out5 <- cat_stat2(df=adcm3, my_var=RESPONSE, 
                  my_label="Best Response to Prior Cancer Systemic Therapy", myvar='RESPONSE')

out6 <- cat_stat2(df=adcm3, my_var=REASDISC, 
                  my_label="Reason for Discontinuation of Prior Cancer Systemic Therapy", myvar='REASDISC')



out7 <- cat_stat2(df=adsl3, my_var=PRADTHYN, my_label="Prior Radiation Therapy", myvar='PRADTHYN')
out8 <- cat_stat2(df=adsl3, my_var=PRCASGYN, my_label="Prior Cancer Surgery", myvar='PRCASGYN')

out9 <- cat_stat2(df=adcm3, my_var=FASFL, 
                  my_label="Any Prior Cancer Systemic Therapy", myvar='FASFL')

out9$statistics <- out9$catlabel


out10 <- cat_stat1(df=adcm3, my_var=ATC4, 
                  my_label="Any Prior Cancer Systemic Therapy", myvar='ATC4')

out11 <- cat_stat3(df=adcm3, my_var=ATC4, 
                   my_label="Any Prior Cancer Systemic Therapy", myvar='ATC4', my_var2=CMDECOD, myvar2='CMDECOD')

out10$CMDECOD <- ' '
out12 <- rbind(out10, out11)
out13 <- out12 %>%
  arrange(catlabel,statistics,CMDECOD)


final <- rbind(out1, out2, out3, out4, out5, out6, out7, out8, out9)


final$CMDECOD <- ' '

final2 <- rbind(final, out13)

final2$fdot <- !duplicated(final2$catlabel)
final2$catlabel <- ifelse(final2$fdot==FALSE, ' ', final2$catlabel)


final2 <- final2 %>% replace(is.na(.), '0')

# use gt package to do the final reporting

final2 <- final2[c("catlabel","statistics", "CMDECOD", "Total","TRT A","TRT B")]

df <- final2 %>%
  gt()



tab_html <- df %>% 
  
  tab_header(
    title = "Table 14.1.3.1 Prior Cancer Therapy",
    subtitle = "Full Analysis Set"
  ) %>%
  
  tab_source_note(
    source_note = paste('Program Source: prior_cancer_therapy.R            Executed: (Draft)',  the_date)
  ) %>%
  
  cols_label(
    
    
    catlabel = html("Parameter"),
    statistics = html("Statistics"), 
    CMDECOD=html("CMDECOD"),
    
    
    Total = html(paste(trt[1], "<br> (N=", bign[1], ")  <br> n (%)" )),
    "TRT A" = html(paste(trt[2], "<br> (N=", bign[2], ")  <br> n (%)" )),
    "TRT B" = html(paste(trt[3], "<br> (N=", bign[3], ")  <br> n (%)" ))
    
    
    
  ) %>%
  
  tab_options(
    table.border.top.color = "white",
    heading.border.bottom.color = "black",
    table.border.bottom.color = "white",
    table_body.border.bottom.color = "black",
    table_body.hlines.color = "white", 
    row_group.border.bottom.color = "white", 
    row_group.border.top.color = "white", 
    column_labels.border.top.color = "black",
    column_labels.border.bottom.color = "black",
  ) %>%
  
  
  
  
  cols_align(
    align = "left",
    columns = c(catlabel)
  )




return(tab_html)

}
)

output$dc <- render_gt(
  
  expr = return(returnTable2()), 
  width=px(1000)
)
}
)
}

ui <- fluidPage( 
  ccUI("dc")
)

server <- function(input, output, session) {
  ccServer("dc")
}


shinyApp(ui, server)










