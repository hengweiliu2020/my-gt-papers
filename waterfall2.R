
# Author: Hengwei Liu
# waterfall plot for oncology study


library(haven)
library(tidyr)
library(dplyr)
library(lubridate)
library(DT)
library(shiny)
library(ggplot2)
library(plotly)


# Read in the SAS data
adtr <- read_sas("adtr.sas7bdat")
data.frame(adtr)


tr <- adtr[(adtr$PARAMCD=='SUMDIAM' & adtr$CRIT1FL=='Y'),]
tr$bestpchg <- tr$PCHG
tr <- tr[c("SUBJID","bestpchg", "TRTP")]


tr$SUBJID <- factor(tr$SUBJID,                                    # Factor levels in decreasing order
               levels = tr$SUBJID[order(tr$bestpchg, decreasing = TRUE)])

tr <- tr[order(-tr$bestpchg),]

ui <- fluidPage( 
  mainPanel( plotlyOutput("wfPlot") 
  )
)

server <- function(input, output) {
  
  output$wfPlot <- renderPlotly({ 
     ggplot(tr, aes( x = SUBJID, y = bestpchg, col=TRTP,  fill=TRTP)) + 
      labs(title = "Waterfall plot for best percent change from baseline in sum of diameters", 
           x = "Patient ID", y = "Best percent change from baseline(%)") +
      theme_bw() +
      theme(axis.text.x = element_blank()) +
      geom_col( width = 0.9, show_guide=TRUE) +
      scale_color_manual(values=c("0.016 mg/kg"="red","0.032 mg/kg"="orange","0.048 mg/kg"="blue","0.072 mg/kg"="purple","0.100 mg/kg"="green", "0.150 mg/kg"="pink","0.225 mg/kg"="black")) +
      scale_fill_manual(values=c("0.016 mg/kg"="red","0.032 mg/kg"="orange","0.048 mg/kg"="blue","0.072 mg/kg"="purple","0.100 mg/kg"="green", "0.150 mg/kg"="pink","0.225 mg/kg"="black")) +
      guides(fill=guide_legend(title=NULL)) +
      scale_y_continuous(limits=c(-100, 100), breaks=c(-100,-80, -60, -40, -20, 0, 20, 40, 60, 80, 100))
      
     }) 
}

shinyApp(ui=ui, server=server)