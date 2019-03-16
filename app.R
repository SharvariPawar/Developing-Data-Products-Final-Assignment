#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

setwd("C:/Users/AMIT PAWAR/Documents/Data Science Specialization/C9-Developing Data Products/WEEK-4")
library(stats) 
library(graphics)
library(shiny)
library(ggplot2)
library(ggrepel)
library(extrafont)
library(plotly)
 WDIData.Population.GreenhouseX <- read.csv("./WDIData-Population-GreenhouseX.csv",encoding = "UTF-8")
 # WDIData.Population.GreenhouseX <- read.csv("./WDIData.Population.GreenhouseX.csv",sep = ",")
# WDIData.Population.GreenhouseX <- WDIData.Population.GreenhouseX[, -1]
variableNames <- names(WDIData.Population.GreenhouseX[, -1])
variableList <- c("Forest.area....of.land.area.", "Forest.area..million.sq..km.", "Population.density..per.sq..km..", "Population..billions.", "Rural.population..billions.", "Rural.population....of.total.population." , "Urban.population..billions.", "Urban.population....of.total.", "Greenhouse.gas.emissions..million.kt.of.CO2.equivalent.")

# Define UI for application that draws a histogram
ui <- fluidPage(
   
  # Application title
  titlePanel("Realtionship of Population, Forest Area and Greenhouse gas emissions"),
  
  # Sidebar
  sidebarLayout(
    sidebarPanel(
      selectInput("variableNames1",
                  "Select X-axis:",
                  selected = variableNames[3],
                  variableNames
      ),
      selectInput("variableNames2",
                  "Select Y-axis:",
                  selected = variableNames[2],
                  variableNames
      ),
      selectInput("variableNames3",
                  " Size Scale:",
                  selected = variableNames[9],
                  variableNames
      ),
      width = 3
    ),
    mainPanel(plotOutput("myPlot"), width = 30)
  ))




# Define server logic required to draw a histogram
server <- function(input, output) {
   
  output$myPlot <- renderPlot({
    
    myinp1 <- which(variableNames %in% input$variableNames1)
    myinp2 <- which(variableNames %in% input$variableNames2)
    myinp3 <- which(variableNames %in% input$variableNames3)
    myinp1 <- variableList[myinp1]
    myinp2 <- variableList[myinp2]
    myinp3 <- variableList[myinp3]
    
    x <- WDIData.Population.GreenhouseX[, input$variableNames1]
    
    y <- WDIData.Population.GreenhouseX[, input$variableNames2]
    
    z <- WDIData.Population.GreenhouseX[, input$variableNames3]
    max <- max(x) + round(max(x)/20)
    min_x <- min(x) - round(max(x)/20)
    
    may <- max(y) + max(y)/5
    min_y <- min(y) - max(y)/5
    
    colorvalue = c("#046a2f", "#902d08", "#ce1b20", "#126db2", "#2c1d4d", "#101110","#ff9999","#111188", "#aa8888", "#229922", "#3f7f40", "#555577", "#118811","#e50d0b", "#4c3d3d", "#a80100", "#002200")
    g <- ggplot(WDIData.Population.GreenhouseX, aes(x = x, y = y, size = z, colour = as.factor(WDIData.Population.GreenhouseX$X.U.FEFF.YEAR), label = X.U.FEFF.YEAR)) + geom_point(alpha = 0.9)
    g <- g + scale_radius(range = c(5,30), name = myinp3)
    g <- g + labs(x = paste("\n",myinp1), y = paste(myinp2, "\n"), title = paste(myinp1,' vs. ',myinp2, " \n(with ", myinp3, " as Size)"))
    g <- g + theme_minimal() + theme(panel.background = element_rect(fill = "white", colour = "grey50"))
    g <- g + scale_colour_manual(values = colorvalue, guide = FALSE)
    
    g <- g + xlim(min_x, max) + ylim(min_y, may)
    g <- g + geom_text_repel(point.padding = NA, size = 3, color = "gray10")
    
    g
  }, height = 700, width = 1600, res = 140)
}



# Run the application 
shinyApp(ui = ui, server = server)

