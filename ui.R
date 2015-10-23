
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

test <- log(c(0.04, 0.09, 0.19, 0.22, 0.22, 0.24, 0.24, 0.24, 0.26, 0.32, 0.32, 0.32, 0.34, 0.37, 0.39, 0.41, 0.47, 0.49, 0.52, 0.54, 0.56, 0.58, 0.58, 0.60, 0.65, 0.67, 0.67, 0.67, 0.69, 0.73, 0.75, 0.75, 0.77, 0.80, 0.82, 0.84, 0.86, 0.86, 0.88, 0.90, 0.90, 0.97, 0.97, 1.03, 1.03, 1.08, 1.12, 1.12, 1.14, 1.18, 1.18, 1.18, 1.25, 1.31, 1.31, 1.35, 1.38, 1.51, 1.51, 1.55, 1.57, 1.57, 1.59, 1.59, 1.61, 1.61, 1.66, 1.76, 1.81, 1.89, 1.94, 1.94, 1.98, 2.00, 2.37, 2.62, 3.12, 3.59, 4.19, 4.26, 4.32, 340.71, 502.69, 604.09, 732.80, 1082.19))

library(shiny)

shinyUI(fluidPage(

  # Application title
  titlePanel("Neuraminidase Inhibitor plot (NAIplot)"),

  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      textInput("main", "Plot title"),
      helpText("Place your values here"),
      tags$textarea(id="numvalues1", rows=3, paste(test, collapse=" "), style="width:100%; resize: none;"),
      textInput("col1", "Color", "blue"),
      textInput("name1", "Legend text"),
      checkboxInput("secondPlot", "Add second set of values"),
      conditionalPanel(
        "input.secondPlot == true",
        helpText("Place your values here"),
        tags$textarea(id="numvalues2", rows=3, paste(test, collapse=" "), style="width:100%; resize: none;"),
        textInput("col2", "Color", "red"),
        textInput("name2", "Legend text")
      ),      
      
      br(),
      selectInput("legend", "Legend:",
                  c("No legend"=0,
                    "Top Left" = "topleft",
                    "Top" = "top",
                    "Top Right" = "topright",
                    "Right" = "right",
                    "Bottom Right" = "bottomright",
                    "Bottom" = "bottom",
                    "Bottom Left" = "bottomleft",
                    "Left" = "left")),
      sliderInput("lwd", "Line width", 1, 4, 1),
      checkboxInput("custAxis", "Custom axis values"),
      conditionalPanel(
        "input.custAxis == true",
        helpText("Axis values"),
        tags$textarea(id="axisval", rows=3, "", style="width:100%; resize: none;"),
        helpText("Custom axis labels"),
        tags$textarea(id="axislab", rows=3, "", style="width:100%; resize: none;")
      )
      
      
      
    ,style="overflow-y:scroll; max-height:550px"),
    

    
    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("distPlot")
    )
  )
))
