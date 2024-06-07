# Author: Hengwei Liu
# swimmer plot for oncology study


library(haven)
library(tidyr)
library(dplyr)
library(shiny)
library(ggplot2)
library(plotly)

# Read in the SAS data
adsl <- read_sas("adsl.sas7bdat")
adrs <- read_sas("adrs.sas7bdat")

data.frame(adsl)
data.frame(adrs)

adsl$TRTDURW <- (adsl$TRTEDT - adsl$TRTSDT +1)/7
adsl$DTHDY <- adsl$DTHDT - adsl$TRTSDT +1 
adrs$ADY <- adrs$ADT - adrs$TRTSDT +1
adsl <- adsl[!is.na(adsl$TRTDURW),]

ad <- adsl[c("SUBJID","TRTDURW","DTHDY","TRT01P")]

# treatment duration
ad$y <- ad$SUBJID
ad$x <- ad$TRTDURW

# response data
adrs <- adrs[( adrs$PARAMCD=='OVRLRESP'),]
adrs <- adrs[c("SUBJID","ADY","AVALC")]
adrs$y <- adrs$SUBJID
adrs$x <- adrs$ADY/7

# ongoing data
ad2 <- adsl[(adsl$EOTSTT=='ONGOING'),]
ad2 <- ad2[c("SUBJID","TRTDURW","EOTSTT")]

ad2$y <- ad2$SUBJID
ad2$x <- ad2$TRTDURW
ad2$AVALC <- ad2$EOTSTT

# death data
ad3 <- adsl[c("SUBJID","DTHDY")]
ad3$y <- ad3$SUBJID
ad3$x <- ad3$DTHDY/7
ad3$AVALC <- 'DEATH'

adrs <- adrs[c("x","y","AVALC")]
ad2 <- ad2[c("x","y","AVALC")]
ad3 <- ad3[c("x","y","AVALC")]

adrs <- rbind(adrs, ad3, ad2)

ad$y <- factor(ad$y, levels = ad$y[order(ad$x, decreasing = FALSE)])

ui <- fluidPage( 
  mainPanel( plotlyOutput("swPlot", height=800) 
  ) 
) 

server <- function(input, output) {
  output$swPlot <- renderPlotly({ 
    myPlot <- ggplot(NULL,  aes( x=x , y=y) ) +
      labs(title = "swimmer plot", 
           x = "Treatment duration in weeks", y = "Subject ID") + 
      geom_col( data=ad, width = 0.9, aes(fill=TRT01P) ) +
      geom_point(data=adrs, aes( colour=AVALC, shape=AVALC), size=2) +
      scale_shape_manual(values = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12)) +
      scale_color_manual(values=c("0.016 mg/kg"="red","0.032 mg/kg"="orange","0.048 mg/kg"="blue","0.072 mg/kg"="purple","0.100 mg/kg"="green", "0.150 mg/kg"="pink","0.225 mg/kg"="black")) +
      scale_fill_manual(values=c("0.016 mg/kg"="red","0.032 mg/kg"="orange","0.048 mg/kg"="blue","0.072 mg/kg"="purple","0.100 mg/kg"="green", "0.150 mg/kg"="pink","0.225 mg/kg"="black")) +
      guides(fill=guide_legend(title=NULL)) +
      scale_x_continuous(limits=c(0,80), breaks=c(0,10,20,30,40,50,60,70,80))    
    print(myPlot) }) 
}


shinyApp(ui=ui, server=server)