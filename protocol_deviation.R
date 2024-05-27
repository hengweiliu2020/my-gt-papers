# Author: Hengwei Liu
# Date: 2023-12-03
# Purpose: create protocol deviation

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


the_date <- as.character(Sys.Date())


returnTable2 <- reactive({

adsl <- read_sas("adsl.sas7bdat")
addv <- read_sas("addv.sas7bdat")

adsl <- adsl[(adsl$FASFL=='Y'),]
bign <- adsl %>%
  count(FASFL)

big_n <- as.numeric(bign[1,2])

addv <- addv[(addv$DVCAT>'' & addv$DVCAT != 'NOT APPLICABLE' & addv$FASFL=='Y'),]


# do the counting by FASFL

count0 <- 
  addv %>%                   
  group_by(FASFL) %>%
  summarise(unique_subj = n_distinct(USUBJID))

count0$DVCAT <- 'Subjects with Any Important Protocol Deviation'
count0$DVDECOD <- ''
count0$ORD <- 1
count0$ORD2 <- 1

# do the counting by DVCAT

count1 <- 
  addv %>%                   
  group_by(FASFL, DVCAT) %>%
  summarise(unique_subj = n_distinct(USUBJID))

count1$DVDECOD <- ''
count1$ORD <- 2
count1$ORD2 <- 2

# do the counting by DVCAT, DVDECOD

count2 <- 
  addv %>%                   
  group_by(FASFL, DVCAT, DVDECOD) %>%
  summarise(unique_subj = n_distinct(USUBJID))

count2$ORD <- 2
count2$ORD2 <- 3

# merge the count data with bign
m_count0 <- merge(count0, bign, by=c("FASFL"), all=TRUE)
m_count1 <- merge(count1, bign, by=c("FASFL"), all=TRUE)
m_count2 <- merge(count2, bign, by=c("FASFL"), all=TRUE)

m_count0 <- m_count0[c("DVDECOD","DVCAT","n","unique_subj","ORD","ORD2")]
m_count1 <- m_count1[c("DVDECOD","DVCAT","n","unique_subj","ORD","ORD2")]
m_count2 <- m_count2[c("DVDECOD","DVCAT","n","unique_subj","ORD","ORD2")]

m_count3 <- rbind(m_count0, m_count1, m_count2)
m_count3 <- m_count3[
  with(m_count3, order(m_count3$ORD, m_count3$DVCAT)),
]

m_count3$value <- ifelse(is.na(m_count3$unique_subj),"0", paste(m_count3$unique_subj, "(", format(round(100*m_count3$unique_subj/m_count3$n, 1), nsmall = 1), ")"))

m_count3$DVCAT <- ifelse(m_count3$ORD2==3, '', m_count3$DVCAT)

m_count3 <- m_count3[c("DVCAT","DVDECOD","value")]

# use gt to do the reporting

df <- m_count3 %>%
  gt()



tab_html <- df %>% 
  
  tab_header(
    title = "Table 14.1.1.2 Important Protocol Deviations",
    subtitle = "Full Analysis Set"
  ) %>%
  
  tab_source_note(
    source_note = paste('Program Source: protcol_deviation.R            Executed: (Draft)',  the_date)
  ) %>%
  
  cols_label(
    
    
    DVCAT = html("Category"),
    DVDECOD = html("Sub-Category"),
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
    columns = c(DVCAT)
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



