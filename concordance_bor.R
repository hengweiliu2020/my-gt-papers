# Author: Hengwei Liu
# Date: 2023-11-25
# Purpose: create concordance table for BOR

ccUI <- function(id){
  ns <- NS(id) 
  
  tagList(
    
    column(
      width=12,  
      
      mainPanel(tableOutput(ns("concord")))
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
                 
                 # read the data
                 adrs <- read_sas("adrs.sas7bdat")
                 adrs <- adrs[(adrs$PARAMCD=='CBRSP' & adrs$TRTP>' '),]
                 ad1 <- adrs[(adrs$PARQUAL=='CENTRAL'),]
                 ad2 <- adrs[(adrs$PARQUAL=='INVESTIGATOR'),]
                 
                 ad1$bicr <- ad1$AVALC
                 ad2$inv <- ad2$AVALC
                 
                 ad3 <- merge(ad1, ad2, by=c("USUBJID","TRTP"), all=TRUE)
                 ad3 <- ad3[c("USUBJID","inv","bicr","TRTP")]
                 
                 
                 
                 ad4 <- ad3
                 ad4$inv <- "TOTAL"
                 
                 ad5 <- rbind(ad3, ad4)
                 
                 ad6 <- ad5
                 ad6$bicr <- "TOTAL"
                 
                 ad7 <- rbind(ad5, ad6)
                 
                 
                 # do the counting by TRTP, inv, bicr
                 
                 count0 <- 
                   ad7 %>%                   
                   group_by(TRTP, inv, bicr) %>%
                   summarise(unique_subj = n_distinct(USUBJID))
                 
                 
                 # generate a frame data called comb
                 # it has all the combinations of inv, bicr, TRTP
                 
                 mat1 <- matrix(NA, nrow = 7, ncol = 1)
                 mat2 <- matrix(NA, nrow = 7, ncol = 1)
                 
                 xy1 <- data.frame(mat1)
                 xy2 <- data.frame(mat2)
                 
                 xy1[[1,1]] <- data.frame(X1='CR')
                 xy1[[2,1]] <- data.frame(X1='PR')
                 xy1[[3,1]] <- data.frame(X1='SD')
                 xy1[[4,1]] <- data.frame(X1='NON-CR/NON-PD')
                 xy1[[5,1]] <- data.frame(X1='PD')
                 xy1[[6,1]] <- data.frame(X1='NE')
                 xy1[[7,1]] <- data.frame(X1='TOTAL')
                 
                
                 xy2[[1,1]] <- data.frame(X2='CR')
                 xy2[[2,1]] <- data.frame(X2='PR')
                 xy2[[3,1]] <- data.frame(X2='SD')
                 xy2[[4,1]] <- data.frame(X2='NON-CR/NON-PD')
                 xy2[[5,1]] <- data.frame(X2='PD')
                 xy2[[6,1]] <- data.frame(X2='NE')
                 xy2[[7,1]] <- data.frame(X2='TOTAL')
                 
                 
                 xy3 <- xy1 %>%
                   crossing(xy2)
                 
                 xy3$inv <- unlist(xy3$mat1)
                 xy3$bicr <- unlist(xy3$mat2)
                 
                 
                 X4 <- unique(count0$TRTP)
                 comb <- xy3 %>%
                   crossing(X4)
                 
                 
                 comb <- comb %>%
                   mutate(TRTP=X4)
                 
                 
                 # merge comb with count0
                 m_count0 <- merge(count0, comb, by=c("TRTP", "inv", "bicr"), all=TRUE)
                 
                 bign <- ad3 %>%
                   count(TRTP)
                 
                 
                 
                 # merge m_count0 with bign
                 
                 m_count1 <- merge(m_count0, bign, by=c("TRTP"), all=TRUE)
                 
                 m_count1$val <- ifelse(is.na(m_count1$unique_subj),0, m_count1$unique_subj)
                 
                 m_count1$value <- ifelse(is.na(m_count1$unique_subj),"0", paste(m_count1$unique_subj, "(", format(round(100*m_count1$unique_subj/m_count1$n, 1), nsmall = 1), ")"))
                 
               
                 
                 # do the transpose 
                 
                 
                 a1 <- m_count1 %>%
                   pivot_wider(id_cols=c(TRTP, inv), names_from = bicr, values_from = value,
                               names_prefix = "_")
                 
                 a2 <- m_count1 %>%
                   pivot_wider(id_cols=c(TRTP, inv), names_from = bicr, values_from = val,
                               names_prefix = "_")
                 
                 df <- a1 %>%
                   mutate(invn = case_when(inv=='CR' ~ 1,
                                           inv=='PR'  ~ 2,
                                           inv=='SD'  ~ 3,
                                           inv=='NON-CR/NON-PD' ~ 4,
                                           inv=='PD' ~ 5,
                                           inv=='NE' ~ 6,
                                           inv=='TOTAL' ~ 7
                   ))
                 
                 df <- df[
                   with(df, order(df$TRTP, df$invn)),
                 ]
                 
                 
                
                 
                 df2 <- a2 %>%
                   mutate(invn = case_when(inv=='CR' ~ 1,
                                           inv=='PR'  ~ 2,
                                           inv=='SD'  ~ 3,
                                           inv=='NON-CR/NON-PD' ~ 4,
                                           inv=='PD' ~ 5,
                                           inv=='NE' ~ 6,
                                           inv=='TOTAL' ~ 7
                                           
                   ))
                 
                 
                 
                 df2 <- df2[
                   with(df2, order(df2$TRTP, df2$invn)),
                 ]
                 
                 
                 df2 <-df2[c("_CR","_PR","_SD","_NON-CR/NON-PD","_PD","_NE","_TOTAL", "TRTP","invn")]
                 df <-df[c("_CR","_PR","_SD","_NON-CR/NON-PD","_PD","_NE","_TOTAL", "TRTP","invn","inv")]
                 
                
                 
                 # use df2 to calculate the concordance rate
                 
                 mat1 <- as.matrix(df2)
                 
                 
                 mat2 <- matrix(NA, nrow = length(unique(ad3$TRTP)), ncol = 3)
                 mat3 <- matrix(NA, nrow = length(unique(ad3$TRTP)), ncol = 3)
                 
                 for (i in 1:length(unique(ad3$TRTP))){
                   mat2[[i,1]] <- mat1[[i*7,8]]
                   mat2[[i,2]] <- format(round(100*(as.numeric(mat1[[1+(i-1)*7,1]])+
                                                      as.numeric(mat1[[2+(i-1)*7,2]])+
                                                      as.numeric(mat1[[3+(i-1)*7,3]])+
                                                      as.numeric(mat1[[4+(i-1)*7,4]])+
                                                      as.numeric(mat1[[5+(i-1)*7,5]])+
                                                      as.numeric(mat1[[6+(i-1)*7,6]]))/ as.numeric(mat1[[7+(i-1)*7,7]]),1), nsmall=1)
                   mat2[[i,3]] <- 7.1
                 }
                 
                 
                 for (i in 1:length(unique(ad3$TRTP))){
                   mat3[[i,1]] <- mat1[[i*7,8]]
                   mat3[[i,2]] <- format(round(100*(as.numeric(mat1[[1+(i-1)*7,1]])+as.numeric(mat1[[2+(i-1)*7,2]]))/as.numeric(mat1[[7+(i-1)*7,7]]),1), nsmall=1)
                   mat3[[i,3]] <- 7.2
                 }
                 
                 
                 df5 <- as.data.frame(mat2)
                 df6 <- as.data.frame(mat3)
                 
                 df5$TRTP <- df5$V1
                 df5$inv <- "Concordance Rate for BOR" 
                 df5$invn <- df5$V3
                 df5$"_CR" <- ' '
                 df5$"_PR" <- ' '
                 df5$"_SD" <- ' '
                 df5$"_NON-CR/NON-PD" <- ' '
                 df5$"_PD" <- ' '
                 df5$"_NE" <- ' '
                 df5$"_TOTAL" <- df5$V2
                 
                 df5 <-df5[c("_CR","_PR","_SD","_NON-CR/NON-PD","_PD","_NE","_TOTAL", "TRTP","invn","inv")]
                 
                 df6$TRTP <- df6$V1
                 df6$inv <- "Concordance Rate for ORR" 
                 df6$invn <- df6$V3
                 df6$"_CR" <- ' '
                 df6$"_PR" <- ' '
                 df6$"_SD" <- ' '
                 df6$"_NON-CR/NON-PD" <- ' '
                 df6$"_PD" <- ' '
                 df6$"_NE" <- ' '
                 df6$"_TOTAL" <- df6$V2
                 
                 df6 <-df6[c("_CR","_PR","_SD","_NON-CR/NON-PD","_PD","_NE","_TOTAL", "TRTP","invn","inv")]
                 
                 
                 df7 <- rbind(df, df5, df6)
                 
                 
                 df7 <- df7[
                   with(df7, order(df7$TRTP, df7$invn)),
                 ]
                 
                 
                 df8 <- merge(df7, bign, by=c("TRTP"), all=TRUE)
                 
                 df8$TRTP <- paste(df8$TRTP,'(N=',df8$n, ')' )
                 
                 
                 df8$TRTP <- ifelse(df8$invn==1,df8$TRTP,' ')
                 
                 df8 <- df8 %>%
                   select(-invn)
                 
                 df8 <- df8[c("TRTP", "inv", "_CR","_PR","_SD","_NON-CR/NON-PD","_PD","_NE","_TOTAL")]
                 
                 
                 
                 # use gt to do the reporting
                 
                 df <- df8 %>%
                   gt()
                 
                 #df <- df %>%
                 # gt(groupname_col="TRTP") 
                 
                 
                 tab_html <- df %>% 
                   
                   tab_header(
                     title = "Table 14.2.3. Concordance of Confirmed Best Overall Response (BOR) per RECIST v1.1 Between BICR and Investigator Assessment",
                     subtitle = "Full Analysis Set",
                     
                   ) %>%
                   tab_footnote(
                     footnote="Note: BICR = Blinded Independent Central Review; CR = Complete Response; PR = Partial Response; SD = Stable Disease; PD = Progressive Disease; NE = Not Evaluable.
                     Percentages are based on the total number of subjects in the treatment group (N). Best overall Response was based on RECIST criteria, version 1.1."
                     
                   )%>%
                   
                   tab_source_note(
                     source_note = paste('Program Source: concordance_bor.R            Executed: (Draft)',  the_date)
                   ) %>%
                   
                   cols_label(
                     
                     
                     TRTP = html("Treatment Group"),
                     inv = html("Investigator Assessment"),
                     '_CR' = html("CR <br> n (%)"), 
                     '_PR' = html("PR <br> n (%)"),
                     '_SD' = html("SD <br> n (%)"),
                     '_NON-CR/NON-PD' = html("NON-CR/NON-PD <br> n (%)"),
                     '_PD'=html("PD <br> n (%)"), 
                     '_NE'=html("NE <br> n (%)"),
                     '_TOTAL'=html("TOTAL <br> n (%)"),
                     
                     
                     
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
                     column_labels.border.bottom.color = "black"
                     
                   ) %>%
                   
                 
                   tab_spanner(
                     label = html(paste("BICR Results")),
                     columns = c("_CR","_PR","_SD","_NON-CR/NON-PD","_PD","_NE","_TOTAL")
                   ) %>%
                   
                  
                    
                   cols_align(
                     align = "left",
                     columns = c(TRTP)
                   )
                 
                 return(tab_html)
                 
               }
  )
  
  output$concord <- render_gt(
    
    expr = return(returnTable2()), 
    width=px(1000)
  )
               }
  )
}

ui <- fluidPage( 
  ccUI("concord")
)

server <- function(input, output, session) {
  ccServer("concord")
}


shinyApp(ui, server)
















