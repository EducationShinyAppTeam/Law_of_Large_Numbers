library(shiny)
library(shinydashboard)
library(ggplot2)
library(stats)
library(Rlab)
library(shinyWidgets)
library(dplyr)
library(boastUtils)

shinyServer(function(session, input, output) {

  # Info Button in upper corner
  observeEvent(input$info, {
    sendSweetAlert(
      session = session,
      title = "Instructions:",
      type = NULL,
      closeOnClickOutside = TRUE,
      text = "Use the controls to choose a population from which to sample.
              Then observe the means and sums plots to see if they converge
              or diverge."
    )
  })
  #Go Button
  observeEvent(input$go, {
    updateTabItems(session, "tabs", "largeNumber")
  })

  #Go Button
  observeEvent(input$gop, {
    updateTabItems(session, "tabs", "largeNumber")
  })

  # define color in different paths
  colors <-  c("#0072B2","#D55E00","#009E73","#ce77a8","#E69F00")

  # Function for matrix means
  # Input: path (numeric), size (numeric), and matrix of data (numeric matrix)
  # Output: Matrix of means
  matrixMeans<-function(path, size, matrix){
    means <- matrix(0, nrow = size, ncol = path)
    for (j in 1:path) {
      for (i in 1:size) {
        means[i, j] = mean(matrix[1:i, j])
      }
    }
    return(means)
  }

  # Function for making the sum plots
  # Input: number of paths (numeric), sample size (numeric),
  # matrix of sum values (numeric matrix), actual sum value (numeric)
  # Returns: the plot object
  makeSumPlot<-function(path, size, matrixSum, trueSum, label){
      # Set up data frame to use with ggplot
      allNames<-c("A","B","C","D","E")
      data<-as.data.frame(matrixSum)
      colnames(data)<-allNames[1:path]
      data$x<-1:size

      # Create plot
      plot<-ggplot2::ggplot(aes_string(x='x'), data= data) +
        geom_hline(aes(yintercept=trueSum, linetype="True sum"), show.legend=F,
                   size=1) +
        scale_linetype_manual(name = "", values = c("dotted"))+
        ylim(c(
          min(matrixSum, trueSum)-.01,
          max(matrixSum, trueSum)+.01
        ))+
        xlab("Number of trials so far") +
        ylab('Sum-E(sum)') +
        ggtitle("Sum")+
        theme(axis.text = element_text(size=18),
              plot.title = element_text(size=18, face="bold"),
              axis.title = element_text(size=18),
              panel.background = element_rect(fill = "white", color="black"),
              legend.position=c(.89,1.07),
              legend.text = element_text(size=14)
        )

      # Add paths
      for(i in 1:path){
        plot <- plot + geom_path(aes_string(x = 'x', y = allNames[i]),
                                 data = data,
                                 color = colors[i],
                                 size = 1.5)
      }
      plot
  }

  # Function for making the mean plots
  #Input: number of paths (numeric), sample size (numeric), matrix of sum values
  # (numeric matrix), actual sum value (numeric), optional y label
  # (default is Mean)
  #Returns: the plot object
  makeMeansPlot<-function(path, size, matrixMeans, trueMean, label="Mean"){
    # Set up dataframe to use with ggplot
    allNames<-c("A","B","C","D","E")
    data<-as.data.frame(matrixMeans)
    colnames(data)<-allNames[1:path]
    data$x<-1:size

    # Create Plot
    plot<-ggplot2::ggplot(aes_string(x='x'), data= data) +
      geom_hline(aes(yintercept=trueMean, linetype="True mean"), show.legend=T,
                 size=1) +
      scale_linetype_manual(name = "", values = c("dotted"),
                            guide = guide_legend(override.aes = list(
                              color = c("black")))) +
      ylim(c(
        min(matrixMeans, trueMean)-.01,
        max(matrixMeans, trueMean)+.01
      )) +
      xlab("Number of trials so far") +
      ylab(label) +
      ggtitle("Arithmetic Mean") +
      theme(axis.text = element_text(size=18),
            plot.title = element_text(size=18, face="bold"),
            axis.title = element_text(size=18),
            panel.background = element_rect(fill = "white", color="black"),
            legend.position=c(.89,1.07),
            legend.text = element_text(size=14)
      )
    # Add paths
    for(i in 1:path){
      plot<-plot + geom_path(aes_string(x='x', y=allNames[i]), data=data,
                             color=colors[i], size=1.5)
    }
    plot
  }

  # Function to create density plots for each group
  # Inputs: Dataframe consisting of columns x and y to define axes, limits for x
  # axis in form c(lower, upper), optional path for symmetric case
  # Output: ggplot of density
  makeDensityPlot <- function(data, xlims, path=0){
    plot<-ggplot2::ggplot(aes(x=x, y=y), data= data) +
      geom_path(color="#0072B2", size=1.5) +
      xlim(xlims) +
      xlab("Value") +
      ylab("Density") +
      ggtitle("Population Graph")+
      theme(axis.text = element_text(size=18),
            plot.title = element_text(size=18, face="bold"),
            axis.title = element_text(size=18),
            panel.background = element_rect(fill = "white", color="black")
      )
    # For case in symmetric where path is 1 causing "box" shape
    if(path ==1){
      plot<-plot+
        geom_segment(aes(x=0, y=0, xend=0, yend=1), color="#0072B2", size=1.5) +
        geom_segment(aes(x=1, y=0, xend=1, yend=1), color="#0072B2", size=1.5)
    }
    plot
  }

  # Function to create bar plots for each group
  # Inputs: x axis label (string), dataframe consisting of either column x or
  # columns x and y to define axes
  # Output: ggplot of resulting bar plot
  makeBarPlot<-function(xlab, data, levels=as.character(data$x)){
      plot<-ggplot(aes(x=factor(x, levels=levels), y=y), data= data) +
        geom_bar(stat = "identity", fill="#0072B2") +
        ylim(c(0, max(data$y)+.1*max(data$y))) +
        xlab(xlab) +
        ylab("Probability") +
        ggtitle("Population Graph") +
        theme(axis.text = element_text(size=18),
              plot.title = element_text(size=18, face="bold"),
              axis.title = element_text(size=18),
              panel.background = element_rect(fill = "white", color="black")) +
        scale_x_discrete(drop=FALSE)

    plot
  }

  ###################################################################
  ## Left skewed
  ####################################################################
  leftSkew<-reactive({11-10*input$leftskew})

  # Population of left skewed
  output$plotleft1 <- renderCachedPlot({
    # Define parameters for density plot
    x <- seq((leftSkew()) - 9 * sqrt((leftSkew())),0, length = input$symsize)
    y <- dgamma(-x, shape = (leftSkew()), beta = 1)
    data<-data.frame(x=x, y=y)

    # Make Density Plot
    makeDensityPlot(data=data, xlims = c((leftSkew()) - 9 * sqrt((leftSkew())), 0))
  },
  cacheKeyExpr = {
    list(input$leftskew)
  })

  # Matrix of rgamma values
  data1 <-
    reactive(matrix(
      -rgamma(
        n = input$leftpath * input$leftsize,
        (leftSkew()),
        beta = 1
      ),
      nrow = input$leftsize,
      ncol = input$leftpath
      ))

    # Average of left skewed
    output$plotleft2 <- renderCachedPlot({

    # Define the true mean alpha*beta = 1
    trueMean = -(leftSkew())

    # Plot average in different paths
    makeMeansPlot(input$leftpath,
                  input$leftsize,
                  matrixMeans(input$leftpath, input$leftsize, data1()),
                  trueMean)

  },
  cacheKeyExpr = {
    list(input$leftpath, input$leftsize, input$leftskew)
  })

  # Sum of left skewed
  output$plotleft3 <- renderCachedPlot({
    matrix <- data1()

    # Store value of sum into matrix matrixSum
    matrixSum <-
      matrix(0, nrow = input$leftsize, ncol = input$leftpath)
    for (j in 1:input$leftpath) {
      for (i in input$leftsize:1) {
        matrixSum[i, j] = mean(matrix[1:i, j]) * i + i * (leftSkew())
      }
    }

    # Define the true (sum - E(sum) = 0)
    trueSum = 0

    # Plot sum in different paths
    makeSumPlot(input$leftpath, input$leftsize, matrixSum, trueSum)

  },
  cacheKeyExpr = {
    list(input$leftpath, input$leftsize, input$leftskew)
  })


  ###################################################################
  ## Right skewed
  ####################################################################
  rightSkew<-reactive({11-10*input$rightskew})
  # Population of right skewed
  output$plotright1 <- renderCachedPlot({
    # Define parameters for density plot
    x <- seq(0, (rightSkew()) + 9 * sqrt(rightSkew()), length = input$symsize)
    y <- dgamma(x, shape = (rightSkew()), beta = 1)
    data<-data.frame(x=x, y=y)

    # Make the density plot
    makeDensityPlot(data=data, xlims = c(0, (rightSkew()) + 9 * sqrt((rightSkew()))))
  },
  cacheKeyExpr = {
    list(input$rightskew)
  })

  # Matrix of rgamma values
  data2 <-
    reactive(matrix(
      rgamma(
        n = input$rightpath * input$rightsize,
        (rightSkew()),
        beta = 1
      ),
      nrow = input$rightsize,
      ncol = input$rightpath
    ))

  # Average of right skewed
  output$plotright2 <- renderCachedPlot({

    # Define the true mean alpha*beta = 1
    trueMean = (rightSkew())

    # Make means plot
    print(data2())
    makeMeansPlot(input$rightpath,
                  input$rightsize,
                  matrixMeans(input$rightpath, input$rightsize, data2()),
                  trueMean)
  },
  cacheKeyExpr = {
    list(input$rightpath, input$rightsize, input$rightskew)
  })

  # Sum of right skewed
  output$plotright3 <- renderCachedPlot({
    matrix <- data2()
    # Store value of sum into matrix matrixSum
    matrixSum <-
      matrix(0,
             nrow = input$rightsize,
             ncol = input$rightpath)
      for (j in 1:input$rightpath) {
        for (i in 1:input$rightsize) {
          matrixSum[i, j] = mean(matrix[1:i, j]) * i - i * (rightSkew())
      }
    }

    # Define the true (sum - E(sum) = 0)
    trueSum = 0

    # Plot sum in different paths
    makeSumPlot(input$rightpath, input$rightsize, matrixSum, trueSum)

  }, cacheKeyExpr = {
    list(input$rightpath, input$rightsize, input$rightskew)
  })

  ###################################################################
  ## Symmetric skewed
  ####################################################################
  inverse<-reactive({round(14.6*input$inverse^3-5.7*input$inverse^2 +
                             input$inverse+.1,3)})
  # Population of Symmetric skewed
  output$plotsymmetric1 <- renderCachedPlot({
    x <- seq(0, 1, length = input$symsize)
    dens <-
      dbeta(x,
            shape1 = inverse(),
            shape2 = inverse())
    data <- data.frame(x = x, y = dens)

    # Make density plot separated by case where the peakedness is exactly 1 (causes a "box" shape)
      makeDensityPlot(data = data, xlims = c(-0.03, 1.03), path=inverse())
    },
  cacheKeyExpr = {
    list(input$symsize, input$inverse)
  })

  # Matrix of rbeta values
  data3 <- reactive(matrix(
    rbeta(
      input$sympath * input$symsize,
      shape1 = inverse(),
      shape2 = inverse()
    ),
    nrow = input$symsize,
    ncol = input$sympath
  ))

  # Average of symmetric
  output$plotsymmetric2 <- renderCachedPlot({

    # Define the true mean
    trueMean = 1 / 2

    # Make means plot
    makeMeansPlot(input$sympath,
                  input$symsize,
                  matrixMeans(input$sympath, input$symsize, data3()),
                  trueMean)
  },
  cacheKeyExpr = {
    list(input$sympath, input$symsize, input$inverse)
  })

  # Sum of symmetric
  output$plotsymmetric3 <- renderCachedPlot({
    matrix <- data3()
    # Store value of sum into matrix matrixSum
    matrixSum = matrix(1 / 2, nrow = input$symsize, ncol = input$sympath)
    for (j in 1:input$sympath) {
      for (i in 1:input$symsize) {
        matrixSum[i, j] = mean(matrix[1:i, j]) * i - 0.5 * i
      }
    }

    # Define the true mean
    trueSum = 0

    # Plot sum in different paths
    makeSumPlot(input$sympath, input$symsize, matrixSum, trueSum)

  },
  cacheKeyExpr = {
    list(input$symsize, input$sympath, input$inverse)
  })

  ###################################################################
  ## Bimodal
  ####################################################################
  # Population for bimodel
  prop<-reactive({input$prop/100})
  output$plotbiomodel1 <- renderCachedPlot({
    # Define parameters for density plot
    t <- 1 / (input$bisize * input$bipath)
    y <- seq(0, 1, t)
    z <- seq(1, 0,-t)
    leftdraw <- dbeta(z, 4,14)*.2
    rightdraw <- dbeta(y, 4,14) *.2
    data<-data.frame(x = seq(0, 5, t*5), y = prop() * leftdraw + (1 - prop()) *
                       rightdraw)

    # Make the density plot
    makeDensityPlot(data = data, xlims = c(0,5))
  },
  cacheKeyExpr = {
    list(input$prop)
  })

  # Create data for bimodel
  data4 <-
    reactive({
      # Random vector of 0s and 1s to determine which distribution each element
      # samples from
      rand<-sample(x = c(0,1),
                   size = input$bisize*input$bipath,
                   replace = TRUE,
                   prob = c(1-prop(), prop()))

      # Number of elements sampled from the right distribution (represented by 1)
      rights<-sum(rand)
      # Number of elements sampled from left distribution (represented by 0)
      lefts<-input$bisize*input$bipath-rights
      leftGammas<-rbeta(lefts, 4, 14)*5

        #rgamma(lefts, 1.25, beta = 1) # Samples left distribution
      rightGammas<-5-rbeta(rights, 4, 14)*5  # Samples right distribution

      # Loop to assign values from gamma distributions to rand
      rightIndex<-1
      leftIndex<-1
      for(x in 1:length(rand)){
        if(rand[x]==0){
          rand[x]<-leftGammas[leftIndex]
          leftIndex<-leftIndex+1
        }
        else{
          rand[x]<-rightGammas[rightIndex]
          rightIndex<-rightIndex+1
        }
      }

      # Turn vector rand into a matrix with proper dimensions
      matrix(rand, nrow=input$bisize, ncol=input$bipath)
  })

  #Average for bimodel
  output$plotbiomodel2 <- renderCachedPlot({

    # Define the true mean
    trueMean = mean(data4())

    # Plot average in different paths
    makeMeansPlot(input$bipath,
                  input$bisize,
                  matrixMeans(input$bipath, input$bisize, data4()),
                  trueMean)

  },
  cacheKeyExpr = {
    list(input$bipath, input$bisize, input$prop)
  })

  # Sum for bimodel
  output$plotbiomodel3 <- renderCachedPlot({
    matrix = data4()
    # Store value of sum into matrix matrixSum
    matrixSum = matrix(0, nrow = input$bisize, ncol = input$bipath)
    for (j in 1:input$bipath) {
      for (i in 1:input$bisize) {
        matrixSum[i, j] = mean(matrix[1:i, j]) * i -  mean(data4()) * i
      }
    }

    # Define the true sum
    trueSum = 0

    # Plot sum in different paths
    makeSumPlot(input$bipath, input$bisize, matrixSum, trueSum)

  },
  cacheKeyExpr = {
    list(input$bipath, input$bisize, input$prop)
  })


  ###################################################################
  ## Accident Rate
  ####################################################################

  # Population of Poisson
  output$poissonpop <- renderCachedPlot({
    data<-data.frame(x=0:ceiling(2*input$poissonmean+5)) # More x's than necessary
    # Get y vals for x's
    data$y<-(input$poissonmean^data$x) * exp(-input$poissonmean)/factorial(data$x)
    # Filter based on probability
    data<-rbind(data[1:2,], filter(data[-c(1,2), ], y>.0005))
    makeBarPlot(xlab= "Number of accidents", data= data)
  },
  cacheKeyExpr = {
    list(input$poissonmean)
  })

  # Matrix of rpois values
  data5 <-
    reactive(matrix(
      rpois(input$poissonpath * input$poissonsize,
            input$poissonmean),
      nrow = input$poissonsize,
      ncol = input$poissonpath
    ))

  # Average for poisson
  output$plotpoisson1 <- renderCachedPlot({

    # Define the true mean
    trueMean = input$poissonmean

    # Plot average in different paths
    makeMeansPlot(input$poissonpath,
                  input$poissonsize,
                  matrixMeans(input$poissonpath, input$poissonsize, data5()),
                  trueMean)

  },
  cacheKeyExpr = {
    list(input$poissonmean, input$poissonpath, input$poissonsize)
  })

  # Sum for accident rate
  output$plotpoisson2 <- renderCachedPlot({
    matrix <- data5()
    # Store value of sum into matrix matrixSum
    matrixSum <-
      matrix(0,
             nrow = input$poissonsize,
             ncol = input$poissonpath)
    for (j in 1:input$poissonpath) {
      for (i in 1:input$poissonsize) {
        matrixSum[i, j] = mean(matrix[1:i, j]) * i - input$poissonmean * i
      }
    }

    # Define the true sum
    trueSum = 0

    # Make plot for sum
    makeSumPlot(input$poissonpath, input$poissonsize, matrixSum, trueSum)

  },
  cacheKeyExpr = {
    list(input$poissonmean, input$poissonpath, input$poissonsize)
  })

  ###################################################################
  ## Astragalus
  ####################################################################

  # Die results
  die <- reactive({
    die <- c(rep(1, 1), rep(3, 4), rep(4, 4), rep(6, 1))
  })

  # Population of Astragalus
  output$pop <- renderPlot({
    data<-data.frame(x=c(1,3,4,6), y=c(.1,.4,.4,.1))
    makeBarPlot(xlab= "Number on roll of astragalus", data= data, levels=1:6)
  })

  # Matrix of sample values
  drawAdie <-
    reactive(matrix(
      sample(die(), input$aspath * input$assize,
             replace = TRUE),
      nrow = input$assize,
      ncol = input$aspath
    ))

  # Average of Astragalus
  output$line2 <- renderCachedPlot({

    # Define the true mean
    trueMean = 3.5

    # Plot for means
    makeMeansPlot(input$aspath,
                  input$assize,
                  matrixMeans(input$aspath, input$assize, drawAdie()),
                  trueMean)
  },
  cacheKeyExpr = {
    list(input$aspath, input$assize)
  })

  # Sum of Astragalus
  output$line1 <- renderCachedPlot ({
    matrix = drawAdie()
    matrixSum = matrix(0, nrow = input$assize, ncol = input$aspath)
    for (j in 1:input$aspath) {
      for (i in 1:input$assize) {
        matrixSum[i, j] = mean(matrix[1:i, j]) * i - 3.5 * i
      }
    }

    # Define the true sum
    trueSum = 0

    # Plot for sum
    makeSumPlot(input$aspath, input$assize, matrixSum, trueSum)

  },
  cacheKeyExpr = {
    list(input$aspath, input$assize)
  })

  ###################################################################
  ## iPOD SHUFFLE
  ####################################################################

  # Reactive expression to get the number of songs of the chosen type
  nSongs<-reactive({
    if(input$ptype=="Jazz"){
      nSongs <- input$s1
    }
    else if(input$ptype=="Rock"){
      nSongs <- input$s2
    }
    else if(input$ptype=="Country"){
      nSongs <- input$s3
    }
    else{
      nSongs <- input$s4
    }
  })

  # Set up songs from four types
  songs <- reactive({
    songs <- c(rep(input$s1),
               rep(input$s2),
               rep(input$s3),
               rep(input$s4))
  })

  # Jazz percent
  output$Jazz_percent <- renderPrint({
    cat(round(input$s1 / sum(songs()), digits = 2))
  })

  # Rock percent
  output$Rock_percent <- renderPrint({
    cat(round(input$s2 / sum(songs()), digits = 2))
  })

  # Country percent
  output$Country_percent <- renderPrint({
    cat(round(input$s3 / sum(songs()), digits = 2))
  })

  # Hip-pop percent
  output$Hiphop_percent <- renderPrint({
    cat(round(input$s4 / sum(songs()), digits = 2))
  })

  # Bar plot
  output$iPodBarPlot <- renderCachedPlot({
    # Parameters for bar plot
    p <- nSongs() / sum(songs())
    data<-data.frame(x = c("Other music (0)", paste(input$ptype,"(1)")), y=c(1-p, p))
    data$x<-factor(data$x, levels=data$x) # Done to force sorted order for bars

    # Make bar plot
    makeBarPlot(xlab= "Genre", data= data)
  },
  cacheKeyExpr = {
    list(input$s1, input$s2, input$s3, input$ptype, input$s4, input$ipodsize)
  })

  # Data for the particular genre of focus in matrix form
  genreData <-
    reactive(matrix(
      rbinom(
        input$ipodpath * input$ipodsize,
        size = 1,
        prob = nSongs() / sum(songs())
      ),
      nrow = input$ipodsize,
      ncol = input$ipodpath
    ))


  # Playlist Mean Plot
  output$PlotMeaniPod <- renderCachedPlot({

    # Define the true mean
    trueMean = nSongs() / sum(songs())

    # Plot average in different paths
    makeMeansPlot(input$ipodpath,
                  input$ipodsize,
                  matrixMeans(input$ipodpath, input$ipodsize, genreData()),
                  trueMean,
                  "Proportion")

  },
  cacheKeyExpr = {
    list(
      input$s1,
      input$s2,
      input$s3,
      input$s4,
      input$ptype,
      input$ipodsize,
      input$ipodpath
    )
  })

  #Plot playlist sum
  output$PlotSumiPod <- renderCachedPlot({

    matrix<-genreData()

    # Store value of sum into matrix matrixSum
    matrixSum <-
      matrix(0, nrow = input$ipodsize, ncol = input$ipodpath)
    for (j in 1:input$ipodpath) {
      for (i in 1:input$ipodsize) {
        matrixSum[i, j] = mean(matrix[1:i, j]) * i - i * (nSongs() / sum(songs()))
      }
    }

    # Define the true sum
    trueSum = 0

    # Plot sum in different paths
    makeSumPlot(input$ipodpath, input$ipodsize, matrixSum, trueSum)
  },
  cacheKeyExpr = {
    list(input$s1, input$s2, input$s3, input$ptype, input$s4, input$ipodsize)
  })
})
