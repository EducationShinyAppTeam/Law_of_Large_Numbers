library(shiny)
library(shinydashboard)
library(plotrix)
library(ggplot2)
library(reshape2)
library(scales)
library(stats)
library(Rlab)
library(dplyr)
library(formattable)
library(discrimARTs)
library(truncnorm)
library(shinyWidgets)

shinyServer(function(session, input, output) {
  observeEvent(input$info,{
    sendSweetAlert(
      session = session,
      title = "Instructions:",
      text = "Population Graph is used to present the overall 
              Pick a population type and see how sample averages converge which sample sums diverge from their expected value.",
      type = "info"
    )
  })
  
  
  #Go Button
  observeEvent(input$go, {
    updateTabItems(session, "tabs", "largeNumber")
  })
  
  #list all input value 
  observeEvent({
    # choose population type
    input$popDist
    
    # Left skewed
    input$leftskew
    input$leftpath
    input$leftsize
    
    # Right skewed
    input$rightskew
    input$rightpath
    input$rightsize
    
    # Symmetric
    input$inverse
    input$sympath
    input$symsize
    
    # Bimodal
    input$prop
    input$bipath
    input$bisize
    
    # Accident Rate
    input$poissonMean
    input$poissonpath
    input$poissonsize
    
    
    # Astrugluas
    input$aspath
    input$assize
    
    #ipodshuffle
    input$ptype
    input$s1
    input$s2
    input$s3
    input$s4
    input$ipodpath
    input$ipodsize
  },
  {
    ###################################################################
    ## Left skewed
    ####################################################################
    
    # Population of left skewed
    output$plotleft1 <- renderPlot({
      # plot(seq(5,0,-.001), dgamma(seq(0,5,.001), input$leftskew, input$leftskew),
      #      main="Population Graph", col="#3CA2C8", xlab="value", ylab="density", lwd = 1)
      curve(dgamma(-x, shape = input$leftskew, beta = 1),
            main="Population Graph", col="#3CA2C8", xlab="value", ylab="density",lwd = 3,
            cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5,
            xlim = c(input$leftskew-9*sqrt(input$leftskew), 0))
    })
    
    # Matrix of rgamma values
    data1 <- reactive(matrix(-rgamma(n = input$leftpath*input$leftsize, 
                                    input$leftskew, beta = 1), 
                             nrow = input$leftsize, ncol = input$leftpath))
    
    # Average of left skewed 
    output$plotleft2 <- renderPlot({
      matrix <- data1()
      # store value of mean into matrix matrix.means
      matrix.means <- matrix(0, nrow = input$leftsize, ncol = input$leftpath)
      for(j in 1:input$leftpath){
        for(i in input$leftsize:1){
          matrix.means[i, j] = mean(matrix[1:i, j])
        }
      }
      
      # define the true mean alpha*beta = 1
      true.mean = -input$leftskew
      
      # define color in different pathes
      colors = c("#3CA2C8", "#10559A","#CC6BB1", "#F9C6D7","#DB4C77")
      
      # plot average in different pathes
      for(i in 1:input$leftpath){
        if(i == 1){
          plot(1:input$leftsize, matrix.means[, i], main = "Average Graph",
               xlab = "# of trials so far", ylab = "mean", type = "l",
               cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5,
               lwd = 3, col = colors[i], ylim = c(min(matrix.means, true.mean), 
                                                  max(matrix.means, true.mean)))
        }
        if(i > 1){
          lines(1:input$leftsize, (matrix.means[, i]), col = colors[i], lwd = 3,
                ylim = c(min(matrix.means, true.mean),max(matrix.means, true.mean)))
        }
      }
      
      # plot the true mean
      abline(h = true.mean, col = "black", lty = 3, lwd = 3)
      
      # make a legend
      legend("bottomright", legend = c("True Mean"),box.lwd = 0, box.lty = 0, bg = 'transparent',
             lty=c(3), lwd=c(2.5), cex = 1.2,
             col= "black",xpd=TRUE,inset=c(0,1),horiz=TRUE)
    })
    
    # Sum of left skewed
    output$plotleft3<-renderPlot({
      matrix<- data1()
      # store value of sum into matrix matrix.sum
      matrix.sum <- matrix(0, nrow = input$leftsize, ncol = input$leftpath)
      for(j in 1:input$leftpath){
        for(i in input$leftsize:1){
          matrix.sum[i, j] = mean(matrix[1:i, j])*i + i*input$leftskew
        }
      }
      
      # define the true (sum - E(sum) = 0)
      true.sum = 0
      
      # define color in different pathes
      colors = c("#3CA2C8", "#10559A","#CC6BB1", "#F9C6D7","#DB4C77")
      
      # plot sum in different pathes
      for(i in 1:input$leftpath){
        if(i == 1){
          plot(1:input$leftsize, matrix.sum[, i], main = "Sum Graph",
               xlab = "# of trials so far", ylab = "sum - E(sum)", type = "l",
               cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5,
               lwd = 3, col = colors[i], ylim = c(min(matrix.sum, true.sum), 
                                                  max(matrix.sum, true.sum)))
        }
        if(i > 1){
          lines(1:input$leftsize, (matrix.sum[, i]), col = colors[i], lwd = 3,
                ylim = c(min(matrix.sum, true.sum),max(matrix.sum, true.sum)))
        }
      }
      
      # plot the true sum
      abline(h = true.sum, col = "black", lty = 3, lwd = 3)
      
      # make a legend
      # legend("topright", legend = c("Sum-E(Sum)"),
      #        lty=c(3), lwd=c(2.5), cex = 1.2,
      #        col= "black")
    })
    
    
    ###################################################################
    ## Right skewed
    ####################################################################
    
    # Population of right skewed
    output$plotright1<-renderPlot({
      # plot(seq(0,5,.001),dgamma(seq(0,5,.001),input$rightskew, input$rightskew),
      #      main="Population Graph", col="#3CA2C8", xlab="value", ylab="density")
      curve(dgamma(x, shape = input$rightskew, beta = 1),
            main="Population Graph", col="#3CA2C8", xlab="value", ylab="density",lwd = 3,
            cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5,
            xlim = c(0, input$rightskew+9*sqrt(input$rightskew)))
    })
    
    # Matrix of rgamma values
    data2 <- reactive(matrix(rgamma(n = input$rightpath*input$rightsize, 
                                    input$rightskew, beta = 1), 
                             nrow = input$rightsize, ncol= input$rightpath))
    
    # Average of right skewed
    output$plotright2<-renderPlot({
      matrix<-data2()
      # store value of mean into matrix matrix.means
      matrix.means = matrix(0, nrow = input$rightsize, ncol = input$rightpath)
      for(j in 1:input$rightpath){
        for(i in 1:input$rightsize){
          matrix.means[i, j] = mean(matrix[1:i, j])
        }
      }
      
      # define the true mean alpha*beta = 1
      true.mean = input$rightskew
      
      # define color in different pathes
      colors = c("#3CA2C8", "#10559A","#CC6BB1", "#F9C6D7","#DB4C77")
      
      # plot average in different pathes
      for(i in 1:input$rightpath){
        if(i == 1){
          plot(1:input$rightsize, matrix.means[, i], main = "Average Graph",
               xlab = "# of trials so far", ylab = "mean", type = "l",
               cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5,
               lwd = 3, col = colors[i], ylim = c(min(matrix.means, true.mean), 
                                                  max(matrix.means, true.mean)))
        } 
        if(i > 1){
          lines(1:input$rightsize, (matrix.means[, i]), col = colors[i], lwd = 3, 
                ylim = c(min(matrix.means, true.mean), max(matrix.means, true.mean)))
        }
      }
      
      # plot the true mean
      abline(h = true.mean, col = "black", lty = 3, lwd = 3)
      
      # make a legend
      legend("bottomright", legend = c("True Mean"),box.lwd = 0, box.lty = 0, bg = 'transparent',
             lty=c(3), lwd=c(2.5), cex = 1.2,
             col= "black",xpd=TRUE,inset=c(0,1),horiz=TRUE)
    })
    
    # Sum of right skewed 
    output$plotright3 <- renderPlot({
      matrix <- data2()
      # store value of sum into matrix matrix.sum
      matrix.sum <- matrix(0, nrow = input$rightsize, ncol = input$rightpath)
      for(j in 1:input$rightpath){
        for(i in 1:input$rightsize){
          matrix.sum[i, j] = mean(matrix[1:i, j])*i - i*input$rightskew
        }
      }
      
      # define the true (sum - E(sum) = 0)
      true.sum = 0
      
      # define color in different pathes
      colors = c("#3CA2C8", "#10559A","#CC6BB1", "#F9C6D7","#DB4C77")
      
      # plot sum in different pathes
      for(i in 1:input$rightpath){
        if(i == 1){
          plot(1:input$rightsize, matrix.sum[, i], main = "Sum Graph",
               xlab = "# of trials so far", ylab = "sum-E(sum)", type = "l",
               cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5,
               lwd = 3, col = colors[i], ylim = c(min(matrix.sum, true.sum), 
                                                  max(matrix.sum, true.sum)))
        }
        if(i > 1){
          lines(1:input$rightsize, (matrix.sum[, i]), col = colors[i], lwd = 3, 
                ylim = c(min(matrix.sum, true.sum), max(matrix.sum, true.sum)))
        }
      }
      
      #plot the true mean
      abline(h = true.sum, col = "black", lty = 3, lwd = 3)
      
      #make a legend
      # legend("topright", legend = c("Sum-E(Sum)"),
      #        lty=c(3), lwd=c(2.5), cex = 1.2,
      #        col= "black")
    })
    
    ###################################################################
    ## Symmetric skewed
    ####################################################################
    
    # Population of Symmetric skewed
    output$plotsymmetric1 <- renderPlot({
      
      x <- seq(0, 1, length = input$symsize)
      dens <- dbeta(x, shape1 = input$inverse, shape2 = input$inverse)
      
      # Dealing with peakness = 1 special case
      if(input$inverse == 1){
        plot(x, dens, type = "l", yaxs = "i", xaxs = "i", xlim=c(-0.03,1.03),
             cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5,
             xlab = "value", ylab = "density", main = "Population Graph",
             col = "red", lwd = 3)
        segments(0,0,0,1, col = "red",lwd = 3)
        segments(1,0,1,1, col = "red",lwd = 3)
        lines(x, dens, col = "red")
        
      }else{
        plot(x, dens, type = "l", yaxs = "i", xaxs = "i", xlim=c(-0.01,1.01),
             cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5,
             xlab = "value", ylab = "density", main = "Population Graph",
             col = "red", lwd = 3)
        lines(x, dens, col = "red")
      }
      # x <- seq(0, 1, length = input$symsize)
      # dens <- dbeta(x, shape1 = input$inverse, shape2 = input$inverse)
      # plot(x, dens, type = "l", yaxs = "i", xaxs = "i", xlim=c(-0.01,1.01),
      #      cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5,
      #      xlab = "value", ylab = "density", main = "Population Graph",
      #      col = "red", lwd = 3)
      # lines(x, dens, col = "red")
    })
    
    # Matrix of rbeta values
    data3 <- reactive(matrix(rbeta(input$sympath*input$symsize, 
                                   shape1 = input$inverse, shape2 = input$inverse), 
                           nrow = input$symsize, ncol = input$sympath))
    
    # Average of symmetric 
    output$plotsymmetric2 <- renderPlot({
      matrix <- data3()
      # store value of mean into matrix matrix.means
      matrix.means <- matrix(1/2, nrow = input$symsize, ncol = input$sympath)
      for(j in 1:input$sympath){
        for(i in 1:input$symsize){
          matrix.means[i, j] = mean(matrix[1:i, j])
        }
      }
      
      # define the true mean
      true.mean = 1/2
      
      # define color in different pathes
      colors = c("#3CA2C8", "#10559A","#CC6BB1", "#F9C6D7","#DB4C77")
      
      # plot average in different pathes
      for(i in 1:input$sympath){
        if(i == 1){
          plot(1:input$symsize, matrix.means[, i], main = "Average Graph",
               xlab = "# of trials so far", ylab = "mean", type = "l",
               cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5,
               lwd = 3, col = colors[i], ylim = c(min(matrix.means, true.mean), 
                                                  max(matrix.means, true.mean)))
        }
        if(i > 1){
          lines(1:input$symsize, (matrix.means[, i]), col = colors[i], lwd = 3, 
                ylim = c(min(matrix.means, true.mean), max(matrix.means, true.mean)))
        }
      }
      
      # plot the true mean
      abline(h = true.mean, col = "black", lty = 3, lwd = 3)
      
      # make a legend
      legend("bottomright", legend = c("True Mean"),box.lwd = 0, box.lty = 0, bg = 'transparent',
             lty=c(3), lwd=c(2.5), cex = 1.2,
             col= "black",xpd=TRUE,inset=c(0,1),horiz=TRUE)
    })
    
    # Sum of symmetric
    output$plotsymmetric3 <- renderPlot({
      matrix<-data3()
      # store value of sum into matrix matrix.sum
      matrix.sum = matrix(1/2, nrow = input$symsize, ncol = input$sympath)
      for(j in 1:input$sympath){
        for(i in 1:input$symsize){
          matrix.sum[i, j] = mean(matrix[1:i, j])*i - 0.5*i
        }
      }
      
      # define the true mean
      true.sum = 0
      
      # define color in different pathes
      colors = c("#3CA2C8", "#10559A","#CC6BB1", "#F9C6D7","#DB4C77")
      
      # plot sum in different pathes
      for(i in 1:input$sympath){
        if(i == 1){
          plot(1:input$symsize, matrix.sum[, i], main = "Sum Graph",
               xlab = "# of trials so far", ylab = "sum-E(sum)", type = "l",
               cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5,
               lwd = 3, col = colors[i], ylim = c(min(matrix.sum, true.sum), 
                                                  max(matrix.sum, true.sum)))
        }
        if(i > 1){
          lines(1:input$symsize, (matrix.sum[, i]), col = colors[i], lwd = 3, 
                ylim = c(min(matrix.sum, true.sum), max(matrix.sum, true.sum)))
        }
      }
      
      # plot the true mean
      abline(h = true.sum, col = "black", lty = 3, lwd = 3)
      
      # make a legend
      # legend("topright", legend = c("Sum-E(Sum)"),
      #        lty=c(3), lwd=c(2.5), cex = 1.2,
      #        col= "black")
    })
    
    ###################################################################
    ## Bimodal
    ####################################################################
    # Population for biomodel
    output$plotbiomodel1 <- renderPlot({
      a <- data4()
      t <- 5/length(a)
      y <- seq(0+t, 5, t)
      z <- seq(5-t, 0, -t)
      
      x <- seq(0, 5, by=0.005)
      leftdraw <- dgamma(z, input$leftskew, beta=1)
      rightdraw <- dgamma(y, input$rightskew, beta=1)
      Z <- input$prop*leftdraw + (1-input$prop)*rightdraw
      
      
      plot(y, Z, type="l", yaxs="i", xaxs="i",
           xlab="value", ylab="density", main="Population Graph", 
           cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5,
           col="#3CA2C8", lwd=3)
      lines(y, Z, type="l", col="#3CA2C8", xlab="",ylab="")
    })
    
    # Matrix of rgamma value
    data4 <- reactive(matrix(mix.synthetic.facing.gamma(N = input$bisize*input$bipath, mix.prob = 1-input$prop,
                                                        lower = 0, upper = 6, shape1=input$leftskew, scale1=1, 
                                                        shape2=input$rightskew, scale2=1),
                             nrow = input$bisize, ncol = input$bipath))
    
    #Average for biomodel 
    output$plotbiomodel2 <- renderPlot({
      matrix<-data4()
      # store value of mean into matrix matrix.means
      matrix.means = matrix(0, nrow = input$bisize, ncol = input$bipath)
      for(j in 1:input$bipath){
        for(i in 1:input$bisize){
          matrix.means[i, j] = mean(matrix[1:i, j])
        }
      }

      true.mean = mean(data4())
      
      # define color in different pathes
      colors = c("#3CA2C8", "#10559A","#CC6BB1", "#F9C6D7","#DB4C77")
      
      # plot average in different pathes
      for(i in 1:input$bipath){
        if(i == 1){
          plot(1:input$bisize, matrix.means[, i], main = "Average Graph",
               xlab = "# of trials so far", ylab = "mean", type = "l",
               cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5,
               lwd = 3, col = colors[i], ylim = c(min(matrix.means, true.mean), 
                                                  max(matrix.means, true.mean)))
        }
        if(i > 1){
          lines(1:input$bisize, (matrix.means[, i]), col = colors[i], lwd = 3, 
                ylim = c(min(matrix.means, true.mean), max(matrix.means, true.mean)))
        }
      }
      
      # plot the true mean
      abline(h = true.mean, col = "black", lty = 3, lwd = 3)
      
      # make a legend
      legend("bottomright", legend = c("True Mean"),box.lwd = 0, box.lty = 0, bg = 'transparent',
             lty=c(3), lwd=c(2.5), cex = 1.2,
             col= "black",xpd=TRUE,inset=c(0,1),horiz=TRUE)
    })
    
    # Sum for biomodel
    output$plotbiomodel3 <- renderPlot({
      matrix = data4()
      # store value of sum into matrix matrix.sum
      matrix.sum = matrix(0, nrow = input$bisize, ncol = input$bipath)
      for(j in 1:input$bipath){
        for(i in 1:input$bisize){
          matrix.sum[i, j] = mean(matrix[1:i, j])*i -  mean(data4())*i
        }
      }
      
      #define the true mean
      true.sum = 0
      
      # define color in different pathes
      colors = c("#3CA2C8", "#10559A","#CC6BB1", "#F9C6D7","#DB4C77")
      
      # plot sum in different pathes
      for(i in 1:input$bipath){
        if(i == 1){
          plot(1:input$bisize, matrix.sum[, i], main = "Sum Graph",
               xlab = "# of trials so far", ylab = "sum-E(sum)", type = "l",
               cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5,
               lwd = 3, col = colors[i], ylim = c(min(matrix.sum, true.sum), 
                                                  max(matrix.sum, true.sum)))
        }
        if(i > 1){
          lines(1:input$bisize, (matrix.sum[, i]), col = colors[i], lwd = 3, 
                ylim = c(min(matrix.sum, true.sum), max(matrix.sum, true.sum)))
        }
      }
      
      # plot the true mean
      abline(h = true.sum, col = "black", lty = 3, lwd = 3)
      
      # make a legend
      # legend("topright", legend = c("Sum-E(Sum)"),
      #        lty=c(3), lwd=c(2.5), cex = 1.2,
      #        col= "black")
    })
    
    
    ###################################################################
    ## Accident Rate
    ####################################################################
    
    # Population of poisson
    output$poissonpop <- renderPlot({
      N <- 10000
      x <- rpois(N, input$poissonmean)
      hist(x, 
           xlim=c(min(x),max(x)), probability = T, nclass = max(x)-min(x)+1, 
           cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5,
           col='lightblue', xlab = "# of accidents", ylab = "probability",
           main='Population Graph')
    })
    
    # Matrix of rpois values
    data5 <- reactive(matrix(rpois(input$poissonpath*input$poissonsize,
                                   input$poissonmean), 
                             nrow = input$poissonsize, ncol = input$poissonpath))
    
    # Average for poisson 
    output$plotpoisson1 <- renderPlot({
      matrix <- data5()
      # store value of mean into matrix matrix.means
      matrix.means <- matrix(0, nrow = input$poissonsize, ncol = input$poissonpath)
      for(j in 1:input$poissonpath){
        for(i in 1:input$poissonsize){
          matrix.means[i, j] = mean(matrix[1:i, j])
        }
      }
      
      # define the true mean
      true.mean = input$poissonmean
      
      # define color in different pathes
      colors = c("#3CA2C8", "#10559A","#CC6BB1", "#F9C6D7","#DB4C77")
      
      # plot average in different pathes
      for(i in 1:input$poissonpath){
        if(i == 1){
          plot(1:input$poissonsize, matrix.means[, i], main = "Average Graph",
               xlab = "# of trials so far", ylab = "mean", type = "l",
               cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5,
               lwd = 3, col = colors[i], ylim = c(min(matrix.means, true.mean), 
                                                  max(matrix.means, true.mean)))
        }
        if(i > 1){
          lines(1:input$poissonsize, (matrix.means[, i]), col = colors[i], lwd = 3, 
                ylim = c(min(matrix.means, true.mean), max(matrix.means, true.mean)))
        }
      }
      
      # plot the true mean
      abline(h = true.mean, col = "black", lty = 3, lwd = 3)
      
      # make a legend
      legend("bottomright", legend = c("True Mean"),box.lwd = 0, box.lty = 0, bg = 'transparent',
             lty=c(3), lwd=c(2.5), cex = 1.2,
             col= "black",xpd=TRUE,inset=c(0,1),horiz=TRUE)
    })
    
    # Sum for accident rate 
    output$plotpoisson2<-renderPlot({
      matrix<-data5()
      # store value of sum into matrix matrix.sum
      matrix.sum <- matrix(0, nrow = input$poissonsize, ncol = input$poissonpath)
      for(j in 1:input$poissonpath){
        for(i in 1:input$poissonsize){
          matrix.sum[i, j] = mean(matrix[1:i, j])*i-input$poissonmean*i
        }
      }
      
      # define the true (sum - E(sum) = 0)
      true.sum = 0
      
      # define color in different pathes
      colors = c("#3CA2C8", "#10559A","#CC6BB1", "#F9C6D7","#DB4C77")
      
      # plot sum in different pathes
      for(i in 1:input$poissonpath){
        if(i == 1){
          plot(1:input$poissonsize, matrix.sum[, i], main = "Sum Graph",
               xlab = "# of trials so far", ylab = "Sum-E(Sum)", type = "l",
               cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5,
               lwd = 3, col = colors[i], ylim = c(min(matrix.sum, true.sum), 
                                                  max(matrix.sum, true.sum)))
        }
        if(i > 1){
          lines(1:input$poissonsize, (matrix.sum[, i]), col = colors[i], lwd = 3, 
                ylim = c(min(matrix.sum, true.sum), max(matrix.sum, true.sum)))
        }
      }
      
      # plot the true mean
      abline(h = true.sum, col = "black", lty = 3, lwd = 3)
      
      # make a legend
      # legend("topright", legend = c("Sum-E(Sum)"),
      #        lty=c(3), lwd=c(2.5), cex = 1.2,
      #        col= "black")
    })
    
    ###################################################################
    ## Astrugluas
    ####################################################################
    
    # die results
    die<-reactive({
      die<-c(rep(1,1),rep(3,4),rep(4,4),rep(6,1))
    })
    
    # Population of Astragalus
    output$pop <- renderPlot({
      a = min(die())
      b = max(die())
      foo <- hist(x = die()+0.001,
           breaks=b-a,
           probability = T,
           xaxt="n",
           cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5,
           col='lightblue', xlab = "# on roll of Astragalus",
           ylab = "probability",
           main="Population Graph")
      axis(side=1, at=foo$mids,labels=seq(a,b))
      
      # df=data.frame(Number=die())
      # 
      # ggplot(df,aes(x=die(), y=..count..)) + geom_histogram(binwidth = 1, fill = "steelblue")+
      #   labs(title = "Population Graph", x="# on roll of Astragalus", y="probability") +
      #   scale_x_continuous(breaks = seq(0, 7, by = 1)) +
      #   theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
      #         panel.background = element_blank(), axis.title = element_text(size=15, face = "bold"),
      #         title = element_text(size=15, face = "bold"), plot.title = element_text(hjust = 0.5))
    })
    
    # Matrix of sample values
    drawAdie <- reactive(matrix(sample(die(), input$aspath*input$assize,
                                       replace = TRUE), 
                             nrow = input$assize, ncol = input$aspath))
    
    # Average of Astrugluas
    output$line2 <- renderPlot({
      matrix = drawAdie()
      # store value of mean into matrix matrix.means
      matrix.means <- matrix(0, nrow = input$assize, ncol = input$aspath)
      for(j in 1:input$aspath){
        for(i in 1:input$assize){
          matrix.means[i,j] = mean(matrix[1:i,j])
        }
      }
      
      # define the true mean 
      true.mean=3.5
      
      # define color in different pathes
      colors = c("#3CA2C8", "#10559A","#CC6BB1", "#F9C6D7","#DB4C77")
      
      # plot average in different pathes
      for(i in 1:input$aspath){
        if(i == 1){
          plot(1:input$assize, matrix.means[,i], type="l", main="Average Graph",
               cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5,
               col = "red", lwd = 3,
               xlab="# of trials so far", ylab = "mean",
               ylim = c(min(matrix.means, true.mean), 
                        max(matrix.means, true.mean)))
        }
        if(i>1){
          lines(1:input$assize, (matrix.means[,i]), col=colors[i], lwd=5,
                ylim = c(min(matrix.means, true.mean), 
                         max(matrix.means, true.mean)))       
        }}
      
      # plot the true mean
      abline(h = true.mean, col = "black", lty = 3, lwd = 3)
      
      # make a legend
      legend("bottomright", legend = c("True Mean"),box.lwd = 0, box.lty = 0, bg = 'transparent',
             lty=c(3), lwd=c(2.5), cex = 1.2,
             col= "black",xpd=TRUE,inset=c(0,1),horiz=TRUE)
    })
    
    # Sum of Astrugluas
    output$line1<-renderPlot ({ 
      matrix = drawAdie()
      matrix.sum = matrix(0, nrow=input$assize, ncol = input$aspath)
      for(j in 1:input$aspath){
        for(i in 1:input$assize){
          matrix.sum[i,j] = mean(matrix[1:i,j])*i-3.5*i
        }
      }
      
      # define the true sum 
      true.sum = 0
      
      # define color in different pathes
      colors = c("#3CA2C8", "#10559A","#CC6BB1", "#F9C6D7","#DB4C77")
      
      for(i in 1:input$aspath){
        if(i==1){
          plot(1:input$assize,matrix.sum[,i], type="l",main="Sum Graph",
               cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5,
               lwd=5, col = "red",
               xlab="# of trials so far", ylab = "sum-E(sum)",
               ylim = c(min(matrix.sum, true.sum), 
                        max(matrix.sum, true.sum)))
        }
        
        if(i>1){
          lines(1:input$assize, (matrix.sum[,i]), col=colors[i], lwd=5,
                ylim = c(min(matrix.sum, true.sum), 
                         max(matrix.sum, true.sum)))       
        }}
      # plot the true mean
      abline(h = true.sum, col = "black", lty = 3, lwd = 3)
      
      # make a legend
      # legend("topright", legend = c("Sum-E(Sum)"),
      #        lty=c(3), lwd=c(2.5),cex = 1.2,
      #        col= "black")
      
    })
   
    ###################################################################
    ## IPOD SHUFFLE
    ####################################################################
    
    #Population and Sum for IPOD
    
    # set up songs from four types  
    songs <- reactive({
      songs <- c(rep(input$s1), 
                 rep(input$s2),
                 rep(input$s3),
                 rep(input$s4)
      )
    })
    
    # average songs in the IPOD
    avg_songs<-reactive({
      mean(songs())
    })
    
    # total songs in the IPOD
    # output$songs_box <- renderPrint({ 
    #   sum(songs())
    #   
    # })
    
    # Jazz percent
    output$Jazz_percent<-renderPrint({
      cat(round(input$s1/sum(songs()),digits = 2))
    })
    
    # Rock percent 
    output$Rock_percent<-renderPrint({
      cat(round(input$s2/sum(songs()),digits = 2))
    })
    
    # Country percent 
    output$Country_percent<-renderPrint({
      cat(round(input$s3/sum(songs()),digits = 2))
    })
    
    # Hip-pop percent
    output$Hiphop_percent<-renderPrint({
      cat(round(input$s4/sum(songs()),digits = 2))
    })
    
    ############################################
    # Plot with bar plot with 4 categories songs 
    
    # Jazz population plot
    output$Plot1 <- renderPlot({
      pjazz <- input$s1/sum(songs())
      count <- c(pjazz*input$ipodsize, (1-pjazz)*input$ipodsize)
      barplot(count, main="Population Graph", xlab="Jazz vs Other music"
              ,ylab="probability",col='lightblue', space = 0.3, width = 0.1,
              cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5,
              names.arg=c("Jazz","Other music"))
      # n <- input$ipodsize
      # x <- seq(0, n, by = 1)
      # plot (x, dbinom(x, n, pjazz, log = FALSE), type = "l", xlab = "values",ylab = "density",
      #       main = "Population Graph",cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5,
      #       col="#3CA2C8", lwd=5)
    })
    
    # Rock population plot
    output$Plot2 <- renderPlot({
      prock <- input$s2/sum(songs())
      count <- c(prock*input$ipodsize, (1-prock)*input$ipodsize)
      barplot(count, main="Population Graph", xlab="Rock vs Other music"
              ,ylab="probability",col='lightblue',
              cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5,
              names.arg=c("Rock","Other music"))
      # n <- input$ipodsize
      # x <- seq(0, n, by = 1)
      # plot (x, dbinom(x, n, prock, log = FALSE), type = "l", xlab = "values",ylab = "density",
      #       main = "Population Graph",cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5,
      #       col="#3CA2C8", lwd=5)
    })
    
    # Country population plot
    output$Plot3 <- renderPlot({
      pcountry <- input$s3/sum(songs())
      count <- c(pcountry*input$ipodsize, (1-pcountry)*input$ipodsize)
      barplot(count, main="Population Graph", xlab="Country vs Other music"
              ,ylab="probability",col='lightblue',
              cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5,
              names.arg=c("Country","Other music"))
      # n <- input$ipodsize
      # x <- seq(0, n, by = 1)
      # plot (x, dbinom(x, n, pcountry, log = FALSE), type = "l", xlab = "values",ylab = "density",
      #       main = "Population Graph",cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5,
      #       col="#3CA2C8", lwd=5)
    })
    
    #Hip-pop population plot
    output$Plot4 <- renderPlot({
      phiphop <- input$s4/sum(songs())
      count <- c(phiphop*input$ipodsize, (1-phiphop)*input$ipodsize)
      barplot(count, main="Population Graph", xlab="Hip-hop vs Other music"
              ,ylab="probability",col='lightblue',
              cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5,
              names.arg=c("Hip-hop","Other music"))
      # n <- input$ipodsize
      # x <- seq(0, n, by = 1)
      # plot (x, dbinom(x, n, phiphop, log = FALSE), type = "l", xlab = "values",ylab = "density",
      #       main = "Population Graph",cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5,
      #       col="#3CA2C8", lwd=5)
    })
    
    ############################################
    # Average Plot with 4 categories songs 
    
    # Matrix of Songs from 4 types
    Jazzdata <- reactive(matrix(rbinom(input$ipodpath*input$ipodsize, size= 1, prob=input$s1/sum(songs())),
                                nrow = input$ipodsize, ncol = input$ipodpath))
    Rockdata <- reactive(matrix(rbinom(input$ipodpath*input$ipodsize, size= 1, prob=input$s2/sum(songs())),
                                nrow = input$ipodsize, ncol = input$ipodpath))
    Countrydata <- reactive(matrix(rbinom(input$ipodpath*input$ipodsize, size= 1, prob=input$s3/sum(songs())),
                                nrow = input$ipodsize, ncol = input$ipodpath))
    Hiphopdata <- reactive(matrix(rbinom(input$ipodpath*input$ipodsize, size= 1, prob=input$s4/sum(songs())),
                                nrow = input$ipodsize, ncol = input$ipodpath))
    
    # JAZZ Average Plot
    output$Plot01<- renderPlot({
      matrix <- Jazzdata()
      # store value of mean into matrix matrix.means
      matrix.means <- matrix(0, nrow = input$ipodsize, ncol = input$ipodpath)
      for(j in 1:input$ipodpath){
        for(i in 1:input$ipodsize){
          matrix.means[i, j] = mean(matrix[1:i, j])
        }
      }
      
      # define the true mean
      true.mean = input$s1/sum(songs())
      
      # define color in different pathes
      colors = c("#3CA2C8", "#10559A","#CC6BB1", "#F9C6D7","#DB4C77")
      
      # plot average in different pathes
      for(i in 1:input$ipodpath){
        if(i == 1){
          plot(1:input$ipodsize, matrix.means[, i], main = "Average Graph",
               xlab = "# of trials so far", ylab = "proportion", type = "l",
               cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5,
               lwd = 3, col = colors[i], ylim = c(min(matrix.means, true.mean), 
                                                  max(matrix.means, true.mean)))
        }
        if(i > 1){
          lines(1:input$ipodsize, (matrix.means[, i]), col = colors[i], lwd = 3)
        }
      }
      
      # plot the true mean
      abline(h = true.mean, col = "black", lty = 3, lwd = 3)
      
      # make a legend
      legend("bottomright", legend = c("True Proportion"),box.lwd = 0, box.lty = 0, bg = 'transparent',
             lty=c(3), lwd=c(2.5), cex = 1.2,
             col= "black",xpd=TRUE,inset=c(0,1),horiz=TRUE)
    })
    
    # Rock Average Plot
    output$Plot02<- renderPlot({
      matrix <- Rockdata()
      # store value of mean into matrix matrix.means
      matrix.means <- matrix(0, nrow = input$ipodsize, ncol = input$ipodpath)
      for(j in 1:input$ipodpath){
        for(i in 1:input$ipodsize){
          matrix.means[i, j] = mean(matrix[1:i, j])
        }
      }
      
      # define the true mean
      true.mean = input$s2/sum(songs())
      
      # define color in different pathes
      colors = c("#3CA2C8", "#10559A","#CC6BB1", "#F9C6D7","#DB4C77")
      
      # plot average in different pathes
      for(i in 1:input$ipodpath){
        if(i == 1){
          plot(1:input$ipodsize, matrix.means[, i], main = "Average Graph",
               xlab = "# of trials so far", ylab = "proportion", type = "l",
               cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5,
               lwd = 3, col = colors[i], ylim = c(min(matrix.means, true.mean), 
                                                  max(matrix.means, true.mean)))
        }
        if(i > 1){
          lines(1:input$ipodsize, (matrix.means[, i]), col = colors[i], lwd = 3)
        }
      }
      
      # plot the true mean
      abline(h = true.mean, col = "black", lty = 3, lwd = 3)
      
      # make a legend
      legend("bottomright", legend = c("True Proportion"),box.lwd = 0, box.lty = 0, bg = 'transparent',
             lty=c(3), lwd=c(2.5), cex = 1.2,
             col= "black",xpd=TRUE,inset=c(0,1),horiz=TRUE)
    })
    
    # Country Average Plot
    output$Plot03<- renderPlot({
      matrix <- Countrydata()
      # store value of mean into matrix matrix.means
      matrix.means <- matrix(0, nrow = input$ipodsize, ncol = input$ipodpath)
      for(j in 1:input$ipodpath){
        for(i in 1:input$ipodsize){
          matrix.means[i, j] = mean(matrix[1:i, j])
        }
      }
      
      # define the true mean
      true.mean = input$s3/sum(songs())
      
      # define color in different pathes
      colors = c("#3CA2C8", "#10559A","#CC6BB1", "#F9C6D7","#DB4C77")
      
      # plot average in different pathes
      for(i in 1:input$ipodpath){
        if(i == 1){
          plot(1:input$ipodsize, matrix.means[, i], main = "Average Graph",
               xlab = "# of trials so far", ylab = "proportion", type = "l",
               cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5,
               lwd = 3, col = colors[i], ylim = c(min(matrix.means, true.mean), 
                                                  max(matrix.means, true.mean)))
        }
        if(i > 1){
          lines(1:input$ipodsize, (matrix.means[, i]), col = colors[i], lwd = 3)
        }
      }
      
      # plot the true mean
      abline(h = true.mean, col = "black", lty = 3, lwd = 3)
      
      # make a legend
      legend("bottomright", legend = c("True Proportion"),box.lwd = 0, box.lty = 0, bg = 'transparent',
             lty=c(3), lwd=c(2.5), cex = 1.2,
             col= "black",xpd=TRUE,inset=c(0,1),horiz=TRUE)
    })
    
    # Hip-hop Average Plot
    output$Plot04<- renderPlot({
      matrix <-  Hiphopdata()
      # store value of mean into matrix matrix.means
      matrix.means <- matrix(0, nrow = input$ipodsize, ncol = input$ipodpath)
      for(j in 1:input$ipodpath){
        for(i in 1:input$ipodsize){
          matrix.means[i, j] = mean(matrix[1:i, j])
        }
      }
      
      # define the true mean
      true.mean = input$s4/sum(songs())
      
      # define color in different pathes
      colors = c("#3CA2C8", "#10559A","#CC6BB1", "#F9C6D7","#DB4C77")
      
      # plot average in different pathes
      for(i in 1:input$ipodpath){
        if(i == 1){
          plot(1:input$ipodsize, matrix.means[, i], main = "Average Graph",
               xlab = "# of trials so far", ylab = "proportion", type = "l",
               cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5,
               lwd = 3, col = colors[i], ylim = c(min(matrix.means, true.mean), 
                                                  max(matrix.means, true.mean)))
        }
        if(i > 1){
          lines(1:input$ipodsize, (matrix.means[, i]), col = colors[i], lwd = 3)
        }
      }
      
      # plot the true mean
      abline(h = true.mean, col = "black", lty = 3, lwd = 3)
      
      # make a legend
      legend("bottomright", legend = c("True Proportion"),box.lwd = 0, box.lty = 0, bg = 'transparent',
             lty=c(3), lwd=c(2.5), cex = 1.2,
             col= "black",xpd=TRUE,inset=c(0,1),horiz=TRUE)
    })
    
    
    ############################################
    # Sum Plot with 4 categories songs 
    
    # JAZZ SUM PLOT
    output$Plot10<- renderPlot({
        matrix<- Jazzdata()
        # store value of sum into matrix matrix.sum
        matrix.sum <- matrix(0, nrow = input$ipodsize, ncol = input$ipodpath)
        for(j in 1:input$ipodpath){
          for(i in 1:input$ipodsize){
            matrix.sum[i, j] = mean(matrix[1:i, j])*i - i*(input$s1/sum(songs()))
          }
        }
        
        # define the true sum
        true.sum = 0
        
        # define color in different pathes
        colors = c("#3CA2C8", "#10559A","#CC6BB1", "#F9C6D7","#DB4C77")
        
        # plot sum in different pathes
        for(i in 1:input$ipodpath){
          if(i == 1){
            plot(1:input$ipodsize, matrix.sum[, i], main = "Sum Graph",
                 xlab = "# of trials so far", ylab = "count - E(count)", type = "l",
                 cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5,
                 lwd = 3, col = colors[i], ylim = c(min(matrix.sum, true.sum), 
                                                    max(matrix.sum, true.sum)))
          }
          if(i > 1){
            lines(1:input$ipodsize, (matrix.sum[, i]), col = colors[i], lwd = 3,
                  ylim = c(min(matrix.sum, true.sum),max(matrix.sum, true.sum)))
          }
        }
        
        # plot the true sum
        abline(h = true.sum, col = "black", lty = 3, lwd = 3)
        
        # make a legend
        # legend("topright", legend = c("Sum-E(Sum)"),
        #        lty=c(3), lwd=c(2.5), cex = 1.2,
        #        col= "black")
      
    })
      
    # Rock SUM PLOT 
    output$Plot20<- renderPlot({
      matrix<- Rockdata()
      # store value of sum into matrix matrix.sum
      matrix.sum <- matrix(0, nrow = input$ipodsize, ncol = input$ipodpath)
      for(j in 1:input$ipodpath){
        for(i in 1:input$ipodsize){
          matrix.sum[i, j] = mean(matrix[1:i, j])*i - i*(input$s2/sum(songs()))
        }
      }
      
      # define the true sum
      true.sum = 0
      
      # define color in different pathes
      colors = c("#3CA2C8", "#10559A","#CC6BB1", "#F9C6D7","#DB4C77")
      
      # plot sum in different pathes
      for(i in 1:input$ipodpath){
        if(i == 1){
          plot(1:input$ipodsize, matrix.sum[, i], main = "Sum Graph",
               xlab = "# of trials so far", ylab = "count - E(count)", type = "l",
               cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5,
               lwd = 3, col = colors[i], ylim = c(min(matrix.sum, true.sum), 
                                                  max(matrix.sum, true.sum)))
        }
        if(i > 1){
          lines(1:input$ipodsize, (matrix.sum[, i]), col = colors[i], lwd = 3,
                ylim = c(min(matrix.sum, true.sum),max(matrix.sum, true.sum)))
        }
      }
      
      # plot the true sum
      abline(h = true.sum, col = "black", lty = 3, lwd = 3)
      
      # make a legend
      # legend("topright", legend = c("Sum-E(Sum)"),
      #        lty=c(3), lwd=c(2.5), cex = 1.2,
      #        col= "black")
      
    })

    # Country SUM PLOT 
    output$Plot30<- renderPlot({
      matrix<- Countrydata()
      # store value of sum into matrix matrix.sum
      matrix.sum <- matrix(0, nrow = input$ipodsize, ncol = input$ipodpath)
      for(j in 1:input$ipodpath){
        for(i in 1:input$ipodsize){
          matrix.sum[i, j] = mean(matrix[1:i, j])*i - i*(input$s3/sum(songs()))
        }
      }
      
      # define the true sum
      true.sum = 0
      
      # define color in different pathes
      colors = c("#3CA2C8", "#10559A","#CC6BB1", "#F9C6D7","#DB4C77")
      
      # plot sum in different pathes
      for(i in 1:input$ipodpath){
        if(i == 1){
          plot(1:input$ipodsize, matrix.sum[, i], main = "Sum Graph",
               xlab = "# of trials so far", ylab = "count - E(count)", type = "l",
               cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5,
               lwd = 3, col = colors[i], ylim = c(min(matrix.sum, true.sum), 
                                                  max(matrix.sum, true.sum)))
        }
        if(i > 1){
          lines(1:input$ipodsize, (matrix.sum[, i]), col = colors[i], lwd = 3,
                ylim = c(min(matrix.sum, true.sum),max(matrix.sum, true.sum)))
        }
      }
      
      # plot the true sum
      abline(h = true.sum, col = "black", lty = 3, lwd = 3)
      
      # make a legend
      # legend("topright", legend = c("Sum-E(Sum)"),
      #        lty=c(3), lwd=c(2.5), cex = 1.2,
      #        col= "black")
      
    })
    
    # Hip_Hop SUM PLOT 
    output$Plot40<- renderPlot({
      matrix<-  Hiphopdata()
      # store value of sum into matrix matrix.sum
      matrix.sum <- matrix(0, nrow = input$ipodsize, ncol = input$ipodpath)
      for(j in 1:input$ipodpath){
        for(i in 1:input$ipodsize){
          matrix.sum[i, j] = mean(matrix[1:i, j])*i - i*(input$s4/sum(songs()))
        }
      }
      
      # define the true sum
      true.sum = 0
      
      # define color in different pathes
      colors = c("#3CA2C8", "#10559A","#CC6BB1", "#F9C6D7","#DB4C77")
      
      # plot sum in different pathes
      for(i in 1:input$ipodpath){
        if(i == 1){
          plot(1:input$ipodsize, matrix.sum[, i], main = "Sum Graph",
               xlab = "# of trials so far", ylab = "count - E(count)", type = "l",
               cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5,
               lwd = 3, col = colors[i], ylim = c(min(matrix.sum, true.sum), 
                                                  max(matrix.sum, true.sum)))
        }
        if(i > 1){
          lines(1:input$ipodsize, (matrix.sum[, i]), col = colors[i], lwd = 3,
                ylim = c(min(matrix.sum, true.sum),max(matrix.sum, true.sum)))
        }
      }
      
      # plot the true sum
      abline(h = true.sum, col = "black", lty = 3, lwd = 3)
      
      # make a legend
      # legend("topright", legend = c("Sum-E(Sum)"),
      #        lty=c(3), lwd=c(2.5), cex = 1.2,
      #        col= "black")
    })
    
  })
  
})
