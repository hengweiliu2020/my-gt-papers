# Author: Hengwei Liu
# spider plot for oncology studies
# with interactive feature tooltips


library(haven)
library(plotly)
library(shiny)
library(ggplot2)

# Read in the SAS data
adtr <- read_sas("adtr.sas7bdat")
data.frame(adtr)

tr <- adtr[(adtr$PARAMCD=='SUMDIAM' ),]
tr <- tr[c("ADY","PCHG","SUBJID", "TRTP","ABLFL")]

tr$x <- ifelse(tr$ABLFL=='Y', 0, tr$ADY/7)
tr$y <- tr$PCHG

ui <- fluidPage( 
  
  mainPanel( plotlyOutput("spPlot") 
  ) 
) 

server <- function(input, output) {
  output$spPlot <- renderPlotly({ 
    myPlot <- ggplot(tr, aes(x = x, y = y, group=SUBJID, fill=TRTP, col=TRTP)) + 
      labs(title = "Spider plot for percent change from baseline in sum of diameters", 
           x = "Treatment duration in weeks", y = "Percent change from baseline(%)") +
      
      geom_line(size=1, show_guide=TRUE) +
      geom_point(aes(fill=TRTP, col=TRTP),  size=2, show_guide=TRUE) +
      
      scale_color_manual(values=c("0.016 mg/kg"="red","0.032 mg/kg"="orange","0.048 mg/kg"="blue","0.072 mg/kg"="purple","0.100 mg/kg"="green", "0.150 mg/kg"="pink","0.225 mg/kg"="black")) +
      scale_fill_manual(values=c("0.016 mg/kg"="red","0.032 mg/kg"="orange","0.048 mg/kg"="blue","0.072 mg/kg"="purple","0.100 mg/kg"="green", "0.150 mg/kg"="pink","0.225 mg/kg"="black")) +
      guides(fill=guide_legend(title=NULL)) +
      scale_y_continuous(limits=c(-100, 100), breaks=c(-100,-80, -60, -40, -20, 0, 20, 40, 60, 80, 100)) +
      scale_x_continuous(limits=c(0,60), breaks=c(0,10,20,30,40,50,60))
    print(myPlot) }) 
}


shinyApp(ui=ui, server=server)