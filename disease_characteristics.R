
# Author: Hengwei Liu
# Date: 2023-12-17
# Purpose: table for disease characteristics for baseline

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
trt <- sort(unique(adsl3$TRT01P))
bign <- table(group=adsl3$TRT01P)

print(trt)
print(bign)
dim(bign)

adsl3$BRNMYN <- ifelse(adsl3$BRNMYN=="",'Missing',adsl3$BRNMYN)


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
  rownames(ht2)  <- c("n","mean","sd","median","min, max")
  
  data.frame(statistics=rownames(ht2), ht2)
  
}


cat_stat <- function(df, my_var, classvar) {
  
  df[[my_var]] <- ifelse(df[[my_var]]=="", 'Missing', df[[my_var]])
  
  freq <- table(df[[my_var]], group=df[[classvar]])
  prop <- 100*prop.table(table(df[[my_var]], df[[classvar]]), 2)
  
  # combine count and percentage
  X11 <- paste(freq[,1], '(', format(prop[,1], digit=3), ')')
  X12 <- paste(freq[,2], '(', format(prop[,2], digit=3), ')')
  X13 <- paste(freq[,3], '(', format(prop[,3], digit=3), ')')
  sex <- cbind(X11, X12,X13)
  
  
  
  # create a new column called statistics and get the final data for reporting 
  rownames(sex) <- c(sort(unique(df[[my_var]])))
  
  
  data.frame(statistics=rownames(sex), sex)
  
}

out1 <- summary(df=adsl3, my_var='DURDIAG', classvar='TRT01P', decimal=1)

out2 <- cat_stat(df=adsl3, my_var='HISTGR1', classvar='TRT01P')
out3 <- cat_stat(df=adsl3, my_var='STAGESE', classvar='TRT01P')
out4 <- cat_stat(df=adsl3, my_var='TNMT', classvar='TRT01P')
out5 <- cat_stat(df=adsl3, my_var='TNMN', classvar='TRT01P')
out6 <- cat_stat(df=adsl3, my_var='TNMM', classvar='TRT01P')
out7 <- cat_stat(df=adsl3, my_var='HISTGD', classvar='TRT01P')
out8 <- cat_stat(df=adsl3, my_var='BRNMYN', classvar='TRT01P')
out9 <- cat_stat(df=adsl3, my_var='OTHMYN', classvar='TRT01P')
out10 <- cat_stat(df=adsl3, my_var='METALOC', classvar='TRT01P')

out1$catlabel <- "Time from Initial Histologic Diagnosis to First Dose Date (Days)"
out2$catlabel <- "Histology"
out3$catlabel <- "Tumor Stage at Study Entry"
out4$catlabel <- "TNM Stage at Study Entry (T)"
out5$catlabel <- "TNM Stage at Study Entry (N)"
out6$catlabel <- "TNM Stage at Study Entry (M)"

out7$catlabel <- "Histologic Grade"
out8$catlabel <- "History of Brain Metastasis"
out9$catlabel <- "History of Other Metastasis"
out10$catlabel <- "Location of Metastasis"


final <- rbind(out1, out2, out3, out4, out5, out6, out7, out8, out9, out10)

final$fdot <- !duplicated(final$catlabel)
final$catlabel <- ifelse(final$fdot==FALSE, ' ', final$catlabel)

final <- final[c("catlabel","statistics","X11","X12","X13")]

df <- final %>%
  gt()



tab_html <- df %>% 
  
  tab_header(
    title = "Table 14.1.2.2 Disease Characteristics at Baseline",
    subtitle = "Full Analysis Set"
  ) %>%
  
  tab_source_note(
    source_note = paste('Program Source: disease_characteristics.R            Executed: (Draft)',  the_date)
  ) %>%
  
  cols_label(
    
    
    catlabel = html("Parameter"),
    statistics = html("Statistics"), 
   
    
      
    X11 = html(paste(trt[1], "<br> (N=", bign[1], ")  <br> n (%)" )),
    X12 = html(paste(trt[2], "<br> (N=", bign[2], ")  <br> n (%)" )),
    X13 = html(paste(trt[3], "<br> (N=", bign[3], ")  <br> n (%)" ))
    
     
    
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





