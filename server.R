library(shiny)
library(diptest)
library(e1071)

# Pretty-printer for p-values
piformat <- function(x, html=FALSE) {
  res <- x
  res[which(res<0 | res>1)] <- NA
  res[which(res>0.05)] <- round(res[which(res>0.05)], 2)
  res[which(res<0.05 & res>0.001)] <- round(res[which(res<0.05 & res>0.001)], 3)
  res[which(res<0.001)] <- ifelse(html, "p&lt;0.001", "p<0.001")
  res[if (html) (res!="p&lt;0.001") else (res!="p<0.001")] <- paste("p=", res[res!="p<0.001"], sep="")
  return(res)
}

naiplot <- function(x, col=c("blue", "red", "green", "magenta"), lwd=1, 
                    xlim=NA, xlab=NA, ylab="Density", legend=NA, 
                    legendText=NA, main=NULL, axis=NA, axislabels=NA, 
                    cex=1, boxplot=FALSE, RI=NA) {
  if (!is.list(x)) x <- list(x)
  n <- length(x)
  col <- rep_len(col, n)
  
  d <- lapply(x, density)
  
  two <- (n>1)
  
  # Get y- and x- value ranges
  ry <- range(sapply(d, function(di) range(di$y)))
  rx <- range(sapply(d, function(di) range(di$x)))
  
  # Calculate plot xlim and ylim
  if (is.na(xlim)) {
    rx <- rx + c(-diff(rx)*0.04, diff(rx)*0.04)
  } else {
    rx <- xlim
  }
  ry0 <- pretty(ry); ry0 <- ry0[ry0>=0]
  ry <- range(ry) + c(-diff(range(ry))*(0.20 + 0.10*(n-1)), diff(range(ry))*(0.10+0.10*boxplot*n))
  
  rug_pos <- ry[1]/n * (1:n - 1)
  rug_ticksize <- 0.105*(2/3)^(n-1)
  
  # Plotting the first sample
  par(mar=c(5,5,4,2))
  plot(d[[1]], ylim=ry, yaxs="i", xlim=rx, yaxs="i", bty="l", 
       col=col[1], lwd=lwd, xlab=xlab, ylab=ylab, main=main, 
       xaxt="n", yaxt="n", cex.lab=cex, cex.main=cex)
  axis(2, at=ry0, labels=ry0, cex.axis=cex)
  rug(x[[1]], ticksize=rug_ticksize, col=col[1], lwd=max(1,lwd/2))
  
  if (n>1) {
    for (i in 2:n) {
      points(d[[i]], type="l", col=col[i], lwd=lwd)
      rug(x[[i]], pos=rug_pos[i], ticksize=rug_ticksize, col=col[i], lwd=max(1,lwd/2))
    }
  }
  
  if (length(axis)==1 && is.na(axis)) axis(1, cex.axis=cex) else axis(1, at=axis, label=axislabels, cex.axis=cex)
  
  if (!is.na(legend)) {
    legend(legend, col=col, lwd=rep(lwd,n), legend=legendText[1:n], 
           bty="n", inset=0.005, y.intersp=1.5, cex=cex)
  }
  
  if (!is.na(RI)) {
    if (RI=="a") {
      abline(v=log10(c(10,100)), lty=c("dotted", "dashed"), lwd=1.5*lwd, col="darkred")
    }
    if (RI=="b") {
      abline(v=log10(c(5,50)), lty=c("dotted", "dashed"), lwd=1.5*lwd, col="darkred")
    }
  }
  
  if (boxplot) {
    # 0.10*boxplot*(two+1)
    ry0 <- ry[2]/(1.1+0.10*boxplot*n)
    
    boxplot_pos <- ry0*1.03 + (ry[2]-ry0)/(n+1) * (1:n)
    for (i in 1:n) {
      boxplot(x[[i]], border=col[i], horizontal=TRUE, add=TRUE, lwd=max(1,lwd/2), outline=FALSE, axes=FALSE,
              boxwex=2/30 + n/30, at=boxplot_pos[i])
    }
  }
  
}


shinyServer(function(input, output, session) {
  cdata <- session$clientData
  
  drawPlot <- function() {
    
    suppressWarnings({
      values1 <- as.numeric(unlist(strsplit(input$numvalues1, "\\s*(,|\\s)\\s*")))
      values2 <- as.numeric(unlist(strsplit(input$numvalues2, "\\s*(,|\\s)\\s*")))
      values3 <- as.numeric(unlist(strsplit(input$numvalues3, "\\s*(,|\\s)\\s*")))
      values4 <- as.numeric(unlist(strsplit(input$numvalues4, "\\s*(,|\\s)\\s*")))
      axisval <- as.numeric(unlist(strsplit(input$axisval, "\\s*(,|\\s)\\s*")))
      axislab <- trimws(unlist(strsplit(input$axislab, "\\s*(,|\\s)\\s*")))
    })
    if (input$logme) {
      values1 <- log10(values1); values2 <- log10(values2)
      values3 <- log10(values3); values4 <- log10(values4)
    }
    if (sum(is.na(values2)) == length(values2)) values2 <- NULL
    if (sum(is.na(values3)) == length(values3)) values3 <- NULL
    if (sum(is.na(values4)) == length(values4)) values4 <- NULL
    n <- 1
    if (input$secondPlot && !is.null(values2)) n <-2
    if (n==2 && input$thirdPlot && !is.null(values3)) n <- 3
    if (n==3 && input$fourthPlot && !is.null(values4)) n <- 4
    
    if (input$legend=="0") legend <- NA else legend <- input$legend
    if (!input$custAxis || sum(is.na(axisval)) == length(axisval)) axisval <- NA
    RI <- ifelse(input$addRI, input$RItype, NA)
    
    # draw the histogram with the specified number of bins
    if (length(values1[!is.na(values1)])>0) {
      naiplot(list(values1, values2, values3, values4)[1:n],
              col = c(input$col1, input$col2, input$col3, input$col4)[1:n],
              legend=legend, lwd=input$lwd, 
              legendText = c(input$name1, input$name2, input$name3, input$name4)[1:n],
              main=input$main, xlab=input$xlab,
              axis=axisval, axislabels=axislab[1:length(axisval)], cex=input$cex,
              RI=RI, boxplot=input$boxplot)
    }
    
  }
  
  output$distPlot <- renderPlot({
    drawPlot()
  })
  
  output$downloadEps <- downloadHandler(
    filename = function() {
      paste(input$main, "eps", sep=".")
    },
    content = function(file) {
      r <- cdata$output_distPlot_width/cdata$output_distPlot_height
      postscript(file, width=7*r, height=7, paper="special", horizontal=FALSE)
      drawPlot()
      dev.off()
    }
  )
  
  output$downloadTiff <- downloadHandler(
    filename = function() {
      paste(input$main, "tiff", sep=".")
    },
    content = function(file) {
      r <- cdata$output_distPlot_width/cdata$output_distPlot_height
      tiff(file, width=3000*r, height=3000, compression="lzw", res=540)
      drawPlot()
      dev.off()
    }
  )
  
  output$statpanel <- renderText({
    outc <- textConnection("out", "w")
    sink(outc)
    suppressWarnings({
      values1 <- as.numeric(unlist(strsplit(input$numvalues1, "\\s*(,|\\s)\\s*")))
      values2 <- as.numeric(unlist(strsplit(input$numvalues2, "\\s*(,|\\s)\\s*")))
      values3 <- as.numeric(unlist(strsplit(input$numvalues3, "\\s*(,|\\s)\\s*")))
      values4 <- as.numeric(unlist(strsplit(input$numvalues4, "\\s*(,|\\s)\\s*")))
    })
    if (input$logme) {
      values1 <- log10(values1); values2 <- log10(values2)
      values3 <- log10(values3); values4 <- log10(values4)
    }
    if (sum(is.na(values2)) == length(values2)) values2 <- NULL
    if (sum(is.na(values3)) == length(values3)) values3 <- NULL
    if (sum(is.na(values4)) == length(values4)) values4 <- NULL
    
    for (i in 1:4) {
      if (!is.null(get(sprintf("values%s",i)))) {
        tmp <- sprintf("Sample %s: %s", i, input[[sprintf("name%s", i)]])
        cat(tmp, "\n")
        cat(paste(rep("-", nchar(tmp)), collapse=""), "\n")
        vals <- get(sprintf("values%s",i))
        n <- length(get(sprintf("values%s",i)))
        bmcoef <- (skewness(vals, type=2)^2 + 1) / (kurtosis(vals, type=2) + 3*(n-1)^2 / ((n-2)*(n-3)))
        cat("Summary statistics: ")
        if (input$logme) cat("(values are log10)")
        cat("\n")
        print(summary(vals))
        cat("Bimodality coefficient: ", bmcoef, "\n")
        if (bmcoef>5/9) {
          cat("   Coefficient is larger than 5/9 (≃0.555), indicating a bi- or multimodal distribution.\n")
        } else {
          cat("   Coefficient is not larger than 5/9 (≃0.555), indicating a unimodal distribution.\n")
        }
        ht <- dip.test(get(sprintf("values%s",i)))
        cat(ht$method, ": D=", ht$statistic, ", ", piformat(ht$p.value), "\n", sep="")
        if (ht$p.value<0.05) {
          cat("   Null hypothesis (unimodality) rejected (at the p<0.05 level),\n   data support the alternative hypothesis (bi- or multimodality).\n\n")
        } else {
          cat("   Null hypothesis (unimodality) not rejected (at the p<0.05 level),\n   data do not support the alternative hypothesis (bi- or multimodality).\n\n")
        }
      }
    }
    sink(); close(outc)
    return(paste(out, collapse="\n"))
    
  })
  
})
