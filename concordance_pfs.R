# Author: Hengwei Liu
# Date: 2023-11-11
# Purpose: create concordance table for PFS

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
                 adtte <- read_sas("adtte.sas7bdat")
                 adtte <- adtte[(adtte$PARAMCD=='PFS' & adtte$TRTP>' '),]
                 ad1 <- adtte[(adtte$PARQUAL=='CENTRAL'),]
                 ad2 <- adtte[(adtte$PARQUAL=='INVESTIGATOR'),]
                 
                 ad1$bicr <- ifelse(ad1$CNSR==1, 'CENSORED', ad1$EVNTDESC)
                 ad2$inv <- ifelse(ad2$CNSR==1, 'CENSORED', ad2$EVNTDESC)
                 
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
                 
                 mat1 <- matrix(NA, nrow = 4, ncol = 1)
                 mat2 <- matrix(NA, nrow = 4, ncol = 1)
                 
                 xy1 <- data.frame(mat1)
                 xy2 <- data.frame(mat2)
                 
                 xy1[[1,1]] <- data.frame(X1='DEATH')
                 xy1[[2,1]] <- data.frame(X1='DISEASE PROGRESSION')
                 xy1[[3,1]] <- data.frame(X1='CENSORED')
                 xy1[[4,1]] <- data.frame(X1='TOTAL')
                 
                 xy2[[1,1]] <- data.frame(X2='DEATH')
                 xy2[[2,1]] <- data.frame(X2='DISEASE PROGRESSION')
                 xy2[[3,1]] <- data.frame(X2='CENSORED')
                 xy2[[4,1]] <- data.frame(X2='TOTAL')
                 
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
                   count(TRTP, inv)
                 
                 
                 bign2 <- ad3 %>%
                   count(TRTP)
                 
                 
                 
                 
                 # merge m_count0 with bign
                 
                 m_count1 <- merge(m_count0, bign, by=c("TRTP","inv"), all=TRUE)
                 
                 m_count1$val <- ifelse(is.na(m_count1$unique_subj),0, m_count1$unique_subj)
                 
                 m_count1$value <- ifelse(is.na(m_count1$unique_subj),"0", paste(m_count1$unique_subj, "(", format(round(100*m_count1$unique_subj/m_count1$n, 1), nsmall = 1), ")"))
                 
                 m_count1$value <- ifelse(m_count1$inv=='TOTAL', word(m_count1$value,1), m_count1$value)
                 m_count1$value <- ifelse(m_count1$bicr=='TOTAL', word(m_count1$value,1), m_count1$value)
                 
                 
                 # do the transpose 
                 
                 
                 a1 <- m_count1 %>%
                   pivot_wider(id_cols=c(TRTP, inv), names_from = bicr, values_from = value,
                               names_prefix = "_")
                 
                 a2 <- m_count1 %>%
                   pivot_wider(id_cols=c(TRTP, inv), names_from = bicr, values_from = val,
                               names_prefix = "_")
                 
                 df <- a1 %>%
                   mutate(invn = case_when(inv=='DEATH' ~ 1,
                                           inv=='DISEASE PROGRESSION'  ~ 2,
                                           inv=='CENSORED'  ~ 3,
                                           inv=='TOTAL' ~ 4
                   ))
                 
                 df <- df[
                   with(df, order(df$TRTP, df$invn)),
                 ]
                 
                 
            
                 
                 
                 df2 <- a2 %>%
                   mutate(invn = case_when(inv=='DEATH' ~ 1,
                                           inv=='DISEASE PROGRESSION'  ~ 2,
                                           inv=='CENSORED'  ~ 3,
                                           inv=='TOTAL' ~ 4
                   ))
                 
                 
                 
                 df2 <- df2[
                   with(df2, order(df2$TRTP, df2$invn)),
                 ]
                 
                 
                 df2 <-df2[c("_DEATH","_DISEASE PROGRESSION","_CENSORED","_TOTAL")]
                 
                 # use df2 to calculate the concordance rate
                 
                 mat1 <- as.matrix(df2)
                 
                 print(mat1)
                 
                 mat2 <- matrix(NA, nrow = length(unique(ad3$TRTP))*4, ncol = 1)
                 
                 
                 for (i in 1:length(unique(ad3$TRTP))){
                   
                   mat2[[i*4,1]] <- format(round(100*(mat1[[1+(i-1)*4,1]]+mat1[[2+(i-1)*4,2]]+mat1[[3+(i-1)*4,3]]+mat1[[1+(i-1)*4,2]]+mat1[[2+(i-1)*4,1]])/mat1[[4*i,4]],1), nsmall=1)
                   
                 }
                 
                 
                 
                 df5 <- as.data.frame(mat2)
                 df5$V1 <- ifelse(is.na(df5$V1),' ', df5$V1)
                 df6 <- cbind(df, df5)
                 
                 
                 print(df6)
                 print(bign2)
                 
                 
                 df6 <- df6[c("TRTP", "inv", '_DEATH','_DISEASE PROGRESSION','_CENSORED','_TOTAL','V1','invn')]
                 
                 
                 df7 <- merge(df6, bign2, by=c("TRTP"), all=TRUE)
                 
                 print(df7)
                 
                 df7$TRTP <- paste(df7$TRTP,'(N=',df7$n, ')' )
                 
                 
                 df7$TRTP <- ifelse(df7$invn==1,df7$TRTP,' ')
                 
                 
                 
                 df7 <- df7 %>%
                   select(-invn, -n)
                 
                 
                 
                 # use gt to do the reporting
                 
                 df <- df7 %>%
                   gt()
                 
                 #df <- df %>%
                 # gt(groupname_col="TRTP") 
                 
                 
                 tab_html <- df %>% 
                   
                   tab_header(
                     title = "Table 14.2.1.3. Concordance of Progression-free Survival (PFS) Status Between Investigator and BICR Assessments",
                     subtitle = "ITT Population",
                     
                   ) %>%
                   tab_footnote(
                     footnote="Note: Percentages are based on row total."
                   )%>%
                   
                   tab_source_note(
                     source_note = paste('Program Source: concordance_pfs.R            Executed: (Draft)',  the_date)
                   ) %>%
                   
                   cols_label(
                     
                     
                     TRTP = html("Treatment Group"),
                     inv = html("Investigator PFS Result"),
                     '_DEATH' = html("DEATH"), 
                     '_DISEASE PROGRESSION' = html("DISEASE PROGRESSION"),
                     '_CENSORED' = html("CENSORED"),
                     '_TOTAL' = html("TOTAL"),
                     'V1'=html("Concordance Rate")
                     
                     
                     
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
                     label = html(paste("BICR PFS Result n(%)")),
                     columns = c('_DEATH','_DISEASE PROGRESSION','_CENSORED')
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
















