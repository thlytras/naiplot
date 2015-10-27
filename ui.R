
test1 <- c(0.04, 0.09, 0.19, 0.22, 0.22, 0.24, 0.24, 0.24, 0.26, 0.32, 0.32, 0.32, 0.34, 0.37, 0.39, 0.41, 0.47, 0.49, 0.52, 0.54, 0.56, 0.58, 0.58, 0.60, 0.65, 0.67, 0.67, 0.67, 0.69, 0.73, 0.75, 0.75, 0.77, 0.80, 0.82, 0.84, 0.86, 0.86, 0.88, 0.90, 0.90, 0.97, 0.97, 1.03, 1.03, 1.08, 1.12, 1.12, 1.14, 1.18, 1.18, 1.18, 1.25, 1.31, 1.31, 1.35, 1.38, 1.51, 1.51, 1.55, 1.57, 1.57, 1.59, 1.59, 1.61, 1.61, 1.66, 1.76, 1.81, 1.89, 1.94, 1.94, 1.98, 2.00, 2.37, 2.62, 3.12, 3.59, 4.19, 4.26, 4.32, 340.71, 502.69, 604.09, 732.80, 1082.19)
test2 <- c(0.21, 0.25, 0.31, 0.33, 0.40, 0.40, 0.42, 0.42, 0.44, 0.52, 0.62, 0.62, 0.67, 0.71, 0.73, 0.75, 0.77, 0.81, 0.92, 0.92, 0.96, 1.00, 0.52, 0.54, 0.58, 0.60, 0.60, 0.60, 0.62, 0.65, 0.67, 0.67, 0.69, 0.71, 0.73, 0.75, 0.77, 0.87, 0.77, 0.92, 0.92, 0.96, 1.00, 1.00, 1.02, 1.06, 1.06, 1.06, 1.06, 1.12, 1.17, 1.17, 1.21, 1.23, 1.23, 1.29, 1.33, 1.38, 1.38, 1.40, 1.42, 1.44, 1.48, 1.50, 1.50, 1.54, 1.56, 1.58, 1.62, 1.69, 1.73, 1.73, 1.77, 1.79, 1.83, 1.90, 1.94, 1.96, 1.98, 2.08, 2.12, 2.12, 2.31, 2.50, 2.52, 2.77)

library(shiny)

shinyUI(fluidPage(

  # Application title
  titlePanel("Neuraminidase Inhibitor plot (NAIplot)"),
  
  tabsetPanel(
    tabPanel("Plot", 
      plotOutput("distPlot")
    ),
    tabPanel("Statistics",
      verbatimTextOutput("statpanel"),
      br(), br(), br(), br(), br()
#    ),
#    tabPanel("Get help",
#      h2("This is a help page"),
#      p("This will be the help page content"),
#      br(), br(), br(), br(), br()
    )
  ),
  
  # Sidebar with a slider input for number of bins
  fluidRow(
    column(4,
      textInput("main", "Plot title:", "H1N1 strains"),
      textInput("xlab", "X-axis label:", "Fold change (log10 scale)"),
      selectInput("legend", "Legend:",
                  c("No legend"=0,
                    "Top Left" = "topleft",
                    "Top" = "top",
                    "Top Right" = "topright",
                    "Left" = "left",
                    "Right" = "right"
                    )),
      sliderInput("lwd", "Line width", 1, 4, 1, 1),
      sliderInput("cex", "Label size", 1, 2, 1.2, 0.1)
    ),
    column(4,
     textInput("col1", "Color", "blue"),
     conditionalPanel("input.legend != '0'",
       textInput("name1", "Legend text", "Oseltamivir")
     ),
     helpText("Place your values here"),
     tags$textarea(id="numvalues1", rows=3, paste(test1, collapse=" "), style="width:100%; resize: none;"),
     conditionalPanel(
       "input.secondPlot == true",
       hr(style="height: 12px; border: 0; box-shadow: inset 0 12px 12px -12px rgba(0, 0, 0, 0.5);"),
       textInput("col2", "Color", "red"),
       conditionalPanel("input.legend != '0'",
         textInput("name2", "Legend text", "Zanamivir")
       ),
       helpText("Place your values here"),
       tags$textarea(id="numvalues2", rows=3, paste(test2, collapse=" "), style="width:100%; resize: none;")
     ),
     conditionalPanel(
       "input.thirdPlot == true",
       hr(style="height: 12px; border: 0; box-shadow: inset 0 12px 12px -12px rgba(0, 0, 0, 0.5);"),
       textInput("col3", "Color", "green"),
       conditionalPanel("input.legend != '0'",
                        textInput("name3", "Legend text")
       ),
       helpText("Place your values here"),
       tags$textarea(id="numvalues3", rows=3, style="width:100%; resize: none;")
     ),
     conditionalPanel(
       "input.fourthPlot == true",
       hr(style="height: 12px; border: 0; box-shadow: inset 0 12px 12px -12px rgba(0, 0, 0, 0.5);"),
       textInput("col4", "Color", "magenta"),
       conditionalPanel("input.legend != '0'",
                        textInput("name4", "Legend text")
       ),
       helpText("Place your values here"),
       tags$textarea(id="numvalues4", rows=3, style="width:100%; resize: none;")
     )
    ),
    column(4,
      downloadButton('downloadTiff', 'Download plot as .tiff'),
      downloadButton('downloadEps', 'Download plot as .eps'),
      checkboxInput("logme", "Convert values to log10", TRUE),
      checkboxInput("secondPlot", "Add second set of values"),
      conditionalPanel(
        "input.secondPlot == true",
        checkboxInput("thirdPlot", "Add third set of values")
      ),
      conditionalPanel(
        "input.thirdPlot == true",
        checkboxInput("fourthPlot", "Add fourth set of values")
      ),
      checkboxInput("custAxis", "Custom axis values"),
      conditionalPanel(
        "input.custAxis == true",
        helpText("Axis values"),
        tags$textarea(id="axisval", rows=3, "", style="width:100%; resize: none;"),
        helpText("Custom axis labels"),
        tags$textarea(id="axislab", rows=3, "", style="width:100%; resize: none;")
      ),
      checkboxInput("boxplot", "Add horizontal boxplot(s)"),
      checkboxInput("addRI", "Add inhibition thresholds"),
      conditionalPanel(
        "input.addRI == true",
        radioButtons("RItype", NULL, c("Type A (10/100)"="a", "Type B (5/50)"="b"))
      )
    )
    
  #,style="top:75%; position:absolute; overflow-y:scroll; width:100%; height:20%"
  )
))
