
# Author: Hengwei Liu
# Date: 2023-12-27
# Purpose: table for post treatment anti-cancer therapy

ccUI <- function(id){
  ns <- NS(id) 
  
  tagList(
    
    column(
      width=12,  
      
      mainPanel(tableOutput(ns("dv")))
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
adsl$TRT <- "Total"

adcm <- read_sas("adcm.sas7bdat")
adcm <- adcm[(adcm$FASFL=='Y'),]
adcm$TRT <- "Total"

adcm <- adcm[(adcm$CMCAT=="POST ANTI-CANCER TREATMENT"),]

# get the number of subjects with post anti cancer treatment

count0 <- 
  adcm %>%                   
  group_by(TRT) %>%
  summarise(unique_subj = n_distinct(USUBJID))


count0$CMSCAT <- 'Any post treatment anti-cancer therapy, n(%)'
count0$ATC2 <- ''
count0$ORD <- 1
count0$ORD2 <- 1


# get the count by CMSCAT

count1 <- 
  adcm %>%                   
  group_by(TRT, CMSCAT) %>%
  summarise(unique_subj = n_distinct(USUBJID))


count1$ATC2 <- ''
count1$ORD <- 2
count1$ORD2 <- 2


# get the count by ATC2

count2 <- 
  adcm %>%                   
  group_by(TRT, CMSCAT, ATC2) %>%
  summarise(unique_subj = n_distinct(USUBJID))

count2$ORD <- 2
count2$ORD2 <- 3


bign <- adsl %>%
  count(TRT)

big_n <- as.numeric(bign[1,2])


# merge the count data with bign
m_count0 <- merge(count0, bign, by=c("TRT"), all=TRUE)
m_count1 <- merge(count1, bign, by=c("TRT"), all=TRUE)
m_count2 <- merge(count2, bign, by=c("TRT"), all=TRUE)


m_count0 <- m_count0[c("CMSCAT","ATC2","n","unique_subj","ORD","ORD2")]
m_count1 <- m_count1[c("CMSCAT","ATC2","n","unique_subj","ORD","ORD2")]
m_count2 <- m_count2[c("CMSCAT","ATC2","n","unique_subj","ORD","ORD2")]

m_count3 <- rbind(m_count0, m_count1, m_count2)
m_count3 <- m_count3[
  with(m_count3, order(m_count3$ORD, m_count3$CMSCAT)),
]

m_count3$value <- ifelse(is.na(m_count3$unique_subj),"0", paste(m_count3$unique_subj, "(", format(round(100*m_count3$unique_subj/m_count3$n, 1), nsmall = 1), ")"))

m_count3$CMSCAT <- ifelse(m_count3$ORD2==3, '', m_count3$CMSCAT)

m_count3 <- m_count3[c("CMSCAT","ATC2","value")]

m_count3$CMSCAT <- ifelse(m_count3$CMSCAT=="Radiation", "Post treatment radiation therapy", m_count3$CMSCAT)
m_count3$CMSCAT <- ifelse(m_count3$CMSCAT=="Systemic", "Post treatment systemic therapy", m_count3$CMSCAT)


# use gt to do the reporting

df <- m_count3 %>%
  gt()



tab_html <- df %>% 
  
  tab_header(
    title = "Table 14.1.3.2 Post Treatment Anti-cancer Therapy",
    subtitle = "Full Analysis Set"
  ) %>%
  
  tab_source_note(
    source_note = paste('Program Source: post_trt_anti_cancer.R            Executed: (Draft)',  the_date)
  ) %>%
  
  cols_label(
    
    
    CMSCAT = html("Category"),
    ATC2 = html("Sub-Category"),
    value = html(paste("Total <br> (N=", big_n, ") <br> n (%)" ))
    
    
    
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
    columns = c(CMSCAT)
  )

return(tab_html)

}
)

output$dv <- render_gt(
  
  expr = return(returnTable2()), 
  width=px(1000)
)
}
)
}

ui <- fluidPage( 
  ccUI("dv")
)

server <- function(input, output, session) {
  ccServer("dv")
}


shinyApp(ui, server)












