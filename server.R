
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)

naiplot <- function(x, x2=NULL, col="blue", col2="red", lwd=1, xlim=NA, xlab=NA, ylab="Density", legend=NA, legendText=NA, main=NULL, axis=NA, axislabels=NA, barplot=FALSE) {
  two <- !is.null(x2) # Do we have a double plot?
  d <- density(x) # Get density for the first sample
  
  # Get y- and x- value ranges
  ry <- range(d$y); rx <- range(d$x)
  
  if (two) {
    d2 <- density(x2) # Get density estimate for the second sample
    
    # Adjust y- and x-value ranges, so that the plot fits both curves
    if (max(d2$y)>max(d$y)) ry <- range(d2$y)
    rx <- range(c(rx, range(d2$x)))
  }
  
  # Calculate plot xlim and ylim
  if (is.na(xlim)) {
    rx <- rx + c(-diff(rx)*0.04, diff(rx)*0.04)
  } else {
    rx <- xlim
  }
  ry0 <- pretty(ry); ry0 <- ry0[ry0>=0]
  ry <- range(ry) + c(-diff(range(ry))*(0.20 + 0.10*two), diff(range(ry))*(0.10+0.10*barplot*(two+1)))
  
  # Plotting the first sample
  plot(d, ylim=ry, yaxs="i", xlim=rx, yaxs="i", bty="l", 
       col=col, lwd=lwd, xlab=xlab, ylab=ylab, main=main, 
       xaxt="n", yaxt="n")
  axis(2, at=ry0, labels=ry0)
  rug(x, ticksize=0.07*((1-two)*0.5+1), col=col, lwd=lwd/2)
  
  if (two) { # Plot the second sample
    points(d2, type="l", col=col2, lwd=lwd)
    rug(x2, pos=ry[1]/2, ticksize=0.07, col=col2, lwd=lwd/2)
  }
  
  if (length(axis)==1 && is.na(axis)) axis(1) else axis(1, at=axis, label=axislabels)
  
  if (!is.na(legend)) {
    legend(legend, col=c(col, col2)[1:(two+1)], lwd=rep(lwd,two+1), legend=legendText[1:(two+1)], bty="n", inset=0.005, y.intersp=1.5)
  }
  
}


shinyServer(function(input, output) {

  output$distPlot <- renderPlot({
    
    suppressWarnings({
      values1 <- as.numeric(unlist(strsplit(input$numvalues1, "\\s*(,|\\s)\\s*")))
      values2 <- as.numeric(unlist(strsplit(input$numvalues2, "\\s*(,|\\s)\\s*")))
      axisval <- as.numeric(unlist(strsplit(input$axisval, "\\s*(,|\\s)\\s*")))
      axislab <- trimws(unlist(strsplit(input$axislab, "\\s*(,|\\s)\\s*")))
    })
    if (input$legend=="0") legend <- NA else legend <- input$legend
    if (!input$secondPlot || sum(is.na(values2)) == length(values2)) values2 <- NULL
    if (!input$custAxis || sum(is.na(axisval)) == length(axisval)) axisval <- NA

    # draw the histogram with the specified number of bins
    if (length(values1[!is.na(values1)])>0) {
      naiplot(values1, values2, legend=legend, lwd=input$lwd, 
              legendText = c(input$name1, input$name2)[1:(input$secondPlot+1)],
              col=input$col1, col2=input$col2, main=input$main,
              axis=axisval)
    }

  })

})
