library(shiny)
library(shinyBS)
library(shinydashboard)
library(shinyWidgets)
library(boastUtils)
library(ggplot2)
library(stats)
library(Rlab)
library(dplyr)

## App Meta Data----------------------------------------------------------------
APP_TITLE <<- "Law of Large Numbers"
APP_DESCP <<- paste(
  "This app is designed to examine the Law of Large Numbers for means and proportions.",
  "The Law of Large Numbers tells us that averages or proportions are likely to be more",
  "stable when there are more trials while sums or counts are likely to be more variable."
)
## End App Meta Data------------------------------------------------------------

# Define UI
ui <- list(
  dashboardPage(
    skin = "blue",
    dashboardHeader(
      title = "Law of Large Numbers",
      titleWidth = 250,
      tags$li(class = "dropdown", actionLink("info", icon("info"))),
      tags$li(
        class = "dropdown",
        tags$a(
          target = "_blank", icon("comments"),
          href = "https://pennstate.qualtrics.com/jfe/form/SV_7TLIkFtJEJ7fEPz?appName=Law_of_Large_Numbers"
        )
      ),
      tags$li(
        class = "dropdown",
        tags$a(
          href = "https://shinyapps.science.psu.edu/",
          icon("home")
        )
      )
    ),
    # Makes Side Panel
    dashboardSidebar(
      width = 250,
      sidebarMenu(
        id = "pages",
        menuItem("Overview", tabName = "Overview", icon = icon("tachometer-alt")),
        menuItem("Prerequisites", tabName = "Prerequisites", icon = icon("book")),
        menuItem("Explore", tabName = "largeNumber", icon = icon("wpexplorer")),
        menuItem("References", tabName = "References", icon = icon("leanpub"))
      ),
      tags$div(
        class = "sidebar-logo",
        boastUtils::psu_eberly_logo("reversed")
      )
    ),
    dashboardBody(
      tags$head(
        tags$link(
          rel = "stylesheet", type = "text/css",
          href = "https://educationshinyappteam.github.io/Style_Guide/theme/boast.css"
        )
      ),

      # Overview Tab
      tabItems(
        tabItem(
          tabName = "Overview",
          h1("Law of Large Numbers"),
          p("This app is designed to examine the Law of Large Numbers for
          means and proportions. The Law of Large Numbers tells us that the
          [arithmetic] mean and proportions become  more stable (less variable)
          than a sum or counts when there are more trials. This stability is not
          the result of some self-correcting behavior as independent trials have
          no memory of what happened before nor do they know what will happen in
          the future."),
          p("This app lets you explore the Law of Large Numbers in action for
          different sample sizes from different populations."),
          br(),
          h2("Instructions"),
          tags$ol(
            tags$li("Pick a population from one of the continuous types
                  (left-skewed; right-skewed; symmetric; or bimodal) or one of
                  the discrete examples (rolls of an astragalus/bone die; songs
                  shuffled from a playlist;or accident occurrence)."),
            tags$li("Use the sliders to adjust the parameters of the chosen
                  population model."),
            tags$li("Use the sliders to choose the sample size and how many
                  sample paths you will observe."),
            tags$li("Observe whether the plots for the averages and sums converge
                  or diverge from their expected values.")
          ),
          div(
            style = "text-align: center",
            bsButton(
              inputId = "go",
              label = "GO!",
              size = "large",
              icon = icon("bolt")
            )
          ),
          br(),
          h2("Acknowledgements"),
          p(
            "This app was originally developed and coded by Zibin Gao and Caihui
          Xiao. The app was modified and recoded by Yingjie (Chelsea) Wang in
          February 2018, by Zhiruo Wang in May 2019, and by Leah Hunt in June
          2020. Special thanks to Jingling Feng and team member Qichao Chen for
          help in the development of the app.",
            br(),
            br(),
            br(),
            div(class = "updated", "Last Update: 05/17/2021 by NJH.")
          )
        ),

        #### Set up the Prerequisites Page
        tabItem(
          tabName = "Prerequisites",
          withMathJax(),
          h2("Prerequisites"),
          p("In order to get the most out of this app, please review the following
          information that will be used in the app."),
          tags$ul(
            tags$li(
              "This app uses four distributions: right (positive) skewed,
                  left (negative) skewed, symmetric, and bimodal. While in-depth
                  understanding of these distributions is not required, you may
                  wish to review this ",
              tags$a(
                href = "https://online.stat.psu.edu/stat100/lesson/3/3.2#graphshapes",
                "Stat 100 Table of Graph Shapes."
              )
            ),
            tags$li("One of the distributions is based upon rolls of an astragalus.
                  Astragali (ankle or heel bones) of animals were used in ancient
                  times as a forerunner of modern dice. When a sheep astragalus
                  is thrown into the air it can land on one of four sides, which
                  were associated with the numbers 1, 3, 4, and 6. Two sides (the
                  3 and the 4) are wider and each come up about 40% of the time,
                  while the narrower sides (the 1 and the 6) each come up about
                  10% of the time. The following image provides four different
                  views of an astragalus."),
            tags$figure(
              align = "center",
              tags$img(
                src = "astragalus.jpg",
                width = 600,
                alt = "Picture of an astragalus (bone die)"
              ),
              tags$figcaption("Image of Astragalus by Yaan, 2007")
            ),
            tags$li("The app allows you to select the number of paths to plot. The
                  number of paths refers to the number of repetitions of the
                  entire process of taking n samples that will be done."),
            tags$li("The sample size refers to the number of times you will repeat
                  the chance process in each path.")
          ),
          div(
            style = "text-align: center",
            bsButton(
              inputId = "gop",
              label = "GO!",
              size = "large",
              icon = icon("bolt")
            )
          )
        ),

        # Explore Law of Large Numbers Tab
        tabItem(
          tabName = "largeNumber",
          h2("Law of Large Numbers"),
          p("In this section, you will have the chance to explore the law of large
          numbers. To do so, first choose a population to sample from, a number
          of samples to take, and a number of paths. Then observe the graphs of
          the means and sums to see which converge and which diverge."),
          # Layout for the population picker----
          sidebarLayout(
            sidebarPanel(
              width = 6,
              fluidRow(
                column(
                  6,
                  # Select Input for the distribution type----
                  selectInput(
                    "popDist",
                    "Population type",
                    list(
                      "Left-skewed" = "leftskewed",
                      "Right-skewed" = "rightskewed",
                      "Symmetric" = "symmetric",
                      "Bimodal" = "bimodal",
                      "Astragalus (Bone Die)" = "astragalus",
                      "Playlist" = "ipodshuffle",
                      "Accident Rate" = "poisson"
                    )
                  ),

                  # Conditional Panel for type of population distribution----
                  # Left Skewed----
                  conditionalPanel(
                    condition = "input.popDist=='leftskewed'",
                    sliderInput(
                      "leftskew",
                      " Skewness",
                      min = 0,
                      max = 1,
                      value = .5,
                      step = 0.1,
                      ticks = FALSE
                    ),
                    div(style = "position: absolute; left: 0.5em; top: 9em", "min"),
                    div(style = "position: absolute; right: 0.5em; top: 9em", "max"),
                  ),
                  # Right Skewed----
                  conditionalPanel(
                    condition = "input.popDist=='rightskewed'",
                    sliderInput(
                      "rightskew",
                      "Skewness",
                      min = 0,
                      max = 1,
                      value = .5,
                      step = .01,
                      ticks = FALSE
                    ),
                    div(style = "position: absolute; left: 0.5em; top: 9em", "min"),
                    div(style = "position: absolute; right: 0.5em; top: 9em", "max"),
                  ),
                  # Symmetric----
                  conditionalPanel(
                    condition = "input.popDist=='symmetric'",
                    sliderInput(
                      "inverse",
                      "Peakedness",
                      min = 0,
                      max = 1,
                      value = .5,
                      step = 0.01,
                      ticks = FALSE
                    ),
                    div(style = "position: absolute; left: 0.5em; top: 9em", "U"),
                    div(style = "position: absolute; left: 0.5em; top: 10em", "Shaped"),
                    div(style = "position: absolute; right: 0.5em; top: 9em", "Bell"),
                    div(style = "position: absolute; right: 0.5em; top: 10em", "Shaped"),
                  ),
                  # Bimodal----
                  conditionalPanel(
                    condition = "input.popDist=='bimodal'",
                    sliderInput(
                      "prop",
                      "Percent under right mode",
                      min = 10,
                      max = 90,
                      value = 50,
                      ticks = F,
                      post = "%",
                    )
                  ),
                  # Poisson----
                  conditionalPanel(
                    condition = "input.popDist=='poisson'",
                    sliderInput(
                      "poissonmean",
                      "Mean",
                      min = 0,
                      max = 10,
                      value = 1,
                      step = 0.1
                    ),
                    conditionalPanel(
                      condition = "input.poissonmean==0",
                      "Note: When the mean is set to 0, the number of accidents is always 0,
                  so the variance is 0."
                    )
                  ),
                  # iPod shuffle----
                  conditionalPanel(
                    condition = "input.popDist == 'ipodshuffle'",
                    column(
                      width = 7,
                      offset = 0,
                      p("Number of songs:"),
                      # Inputs for the probabilites of each music type
                      numericInput(
                        "s1",
                        "Jazz",
                        1,
                        min = 0,
                        max = 200,
                        step = 1,
                        width = "75px"
                      ),
                      numericInput(
                        "s2",
                        "Rock",
                        1,
                        min = 0,
                        max = 200,
                        step = 1,
                        width = "75px"
                      ),
                      numericInput(
                        "s3",
                        "Country",
                        1,
                        min = 0,
                        max = 200,
                        step = 1,
                        width = "75px"
                      ),
                      numericInput(
                        "s4",
                        "Hip-hop",
                        1,
                        min = 0,
                        max = 200,
                        step = 1,
                        width = "75px"
                      )
                    )
                  ) # This parenthesis ends the iPod Shuffle Conditional Panel
                ), # Ends inputs column

                # Inputs for each type:----
                column(
                  6,
                  # left skewed----
                  conditionalPanel(
                    condition = "input.popDist == 'leftskewed'",
                    # Choose number of paths
                    sliderInput(
                      "leftpath",
                      "Number of paths",
                      min = 1,
                      max = 5,
                      value = 1
                    ),
                    # Choose sample size
                    sliderInput(
                      "leftsize",
                      "Sample size (n)",
                      min = 10,
                      max = 1000,
                      value = 100
                    )
                  ),
                  # Right skewed----
                  conditionalPanel(
                    condition = "input.popDist == 'rightskewed'",
                    # Choose the number of sample means
                    sliderInput(
                      "rightpath",
                      "Number of paths",
                      min = 1,
                      max = 5,
                      value = 1
                    ),
                    # Choose the number of sample means
                    sliderInput(
                      "rightsize",
                      "Sample size (n)",
                      min = 10,
                      max = 1000,
                      value = 100
                    )
                  ),
                  # Symmetric----
                  conditionalPanel(
                    condition = "input.popDist == 'symmetric'",
                    # Choose the number of paths
                    sliderInput(
                      "sympath",
                      "Number of paths",
                      min = 1,
                      max = 5,
                      value = 1
                    ),
                    # Choose the number of sample means
                    sliderInput(
                      "symsize",
                      "Sample size (n)",
                      min = 10,
                      max = 1000,
                      value = 100
                    )
                  ),
                  # Astragalus----
                  conditionalPanel(
                    condition = "input.popDist == 'astragalus'",
                    # Choose number of paths
                    sliderInput(
                      "aspath",
                      "Number of paths",
                      min = 1,
                      max = 5,
                      value = 1
                    ),
                    # Choose sample size
                    sliderInput(
                      "assize",
                      "Number of trials",
                      min = 10,
                      max = 1000,
                      value = 100
                    )
                  ),
                  # Bimodal----
                  conditionalPanel(
                    condition = "input.popDist == 'bimodal'",
                    # Choose the number of paths
                    sliderInput(
                      "bipath",
                      "Number of paths",
                      min = 1,
                      max = 5,
                      value = 1
                    ),
                    # Choose the number of sample means
                    sliderInput(
                      "bisize",
                      "Sample size (n)",
                      min = 10,
                      max = 1000,
                      value = 100
                    )
                  ),
                  # Poisson----
                  conditionalPanel(
                    condition = "input.popDist == 'poisson'",
                    # Choose the number of paths
                    sliderInput(
                      "poissonpath",
                      "Number of paths",
                      min = 1,
                      max = 5,
                      value = 1
                    ),
                    # Choose the number of sample means
                    sliderInput(
                      "poissonsize",
                      "Sample size (n)",
                      min = 10,
                      max = 1000,
                      value = 100
                    )
                  ),
                  # Playlist----
                  conditionalPanel(
                    condition = "input.popDist == 'ipodshuffle'",
                    # Choose number of paths
                    sliderInput(
                      "ipodpath",
                      label = "Number of paths",
                      min = 1,
                      max = 5,
                      value = 1
                    ),
                    # Choose sample size
                    sliderInput(
                      "ipodsize",
                      label = "Sample size (n)",
                      min = 10,
                      max = 1000,
                      value = 100
                    ),
                    # Buttons to choose music type
                    radioButtons(
                      "ptype",
                      "Genre to track:",
                      list(
                        "Jazz",
                        "Rock",
                        "Country",
                        "Hip-hop"
                      ),
                      selected = "Jazz"
                    )
                  )
                )
              )
            ), # End of column for slider inputs

            # Population Plot----
            mainPanel(
              width = 6,
              # Plots for each distribution; either histogram or density
              conditionalPanel(
                condition = "input.popDist == 'leftskewed'",
                plotOutput("plotleft1"),
                tags$script(HTML(
                  "$(document).ready(function() {
                  document.getElementById('plotleft1').setAttribute('aria-label',
                  `The population graph shows the density curve for a process
                  which has a left (or negative) skewness, making a long tail on
                  the left. You can control the amount of skewness with the
                  skewness slider.`)
                  })"
                ))
              ),
              conditionalPanel(
                condition = "input.popDist == 'rightskewed'",
                plotOutput("plotright1"),
                tags$script(HTML(
                  "$(document).ready(function() {
                  document.getElementById('plotright1').setAttribute('aria-label',
                  `The population graph shows the density curve for a process
                  which has a right (or positive) skewness, making a long tail on
                  the right. You can control the amount of skewness with the
                  skewness slider.`)
                  })"
                ))
              ),
              conditionalPanel(
                condition = "input.popDist == 'symmetric'",
                plotOutput("plotsymmetric1"),
                tags$script(HTML(
                  "$(document).ready(function() {
                  document.getElementById('plotsymmetric1').setAttribute('aria-label',
                  `The population graph shows the density curve for a process
                  which is symmetric. You can make the process generally produce
                  values at the two extremes (a u-shape), produce them uniformly
                  (rectangular) or concentrate values in the middle (bell-shaped).`)
                  })"
                ))
              ),
              conditionalPanel(
                condition = "input.popDist == 'astragalus'",
                plotOutput("pop"),
                tags$script(HTML(
                  "$(document).ready(function() {
                  document.getElementById('pop').setAttribute('aria-label',
                  `The population graph shows the probabilities for getting each
                  particular outcome when tossign the astragalus. A 1 and 6 each
                  occur 10% of the time, while 3 and 4 both occur 40% of the time.`)
                  })"
                ))
              ),
              conditionalPanel(
                condition = "input.popDist == 'bimodal'",
                plotOutput("plotbimodal1"),
                tags$script(HTML(
                  "$(document).ready(function() {
                  document.getElementById('plotbimodal1').setAttribute('aria-label',
                  `The population graph shows the density curve for a process
                  which is bimodal. You may adjust what percentage occurs under
                  the right most mode with the slider.`)
                  })"
                ))
              ),
              conditionalPanel(
                condition = "input.popDist == 'poisson'",
                plotOutput("poissonpop"),
                tags$script(HTML(
                  "$(document).ready(function() {
                  document.getElementById('poissonpop').setAttribute('aria-label',
                  `The population graph shows the probabilities for the number
                  of accidents which occur. You may control the mean (the unit
                   rate of accidents) with the mean slider.`)
                  })"
                ))
              ),
              conditionalPanel(
                condition = "input.popDist == 'ipodshuffle'",
                plotOutput("iPodBarPlot"),
                tags$script(HTML(
                  "$(document).ready(function() {
                  document.getElementById('iPodBarPlot').setAttribute('aria-label',
                  `The population graph shows the probability for listening to a
                  song that is in the genre you're opted to track. You may set
                  how many songs of each genre there are with the jazz, rock,
                  country, and hip-hop inputs. You may then choose which genre
                  to track with the genre to track input.`)
                  })"
                ))
              )
            )
          ),
          br(),
          # Plot of Arithmetic Means---
          fluidRow(
            column(
              width = 6,
              conditionalPanel(
                condition = "input.popDist == 'leftskewed'",
                plotOutput("plotleft2"),
                tags$script(HTML(
                  "$(document).ready(function() {
                  document.getElementById('plotleft2').setAttribute('aria-label',
                  `The Arithmetic mean graph shows that as you increase the
                     number of trials (the number of observations in your sample
                     up to the value of the sample size slider), the arithmetic
                     mean of your sample will close in on the true value. You
                     may add additional runs of the process by changing the
                     number of paths slider.`)
                  })"
                ))
              ),
              conditionalPanel(
                condition = "input.popDist == 'rightskewed'",
                plotOutput("plotright2"),
                tags$script(HTML(
                  "$(document).ready(function() {
                  document.getElementById('plotright2').setAttribute('aria-label',
                  `The Arithmetic mean graph shows that as you increase the
                     number of trials (the number of observations in your sample
                     up to the value of the sample size slider), the arithmetic
                     mean of your sample will close in on the true value. You
                     may add additional runs of the process by changing the
                     number of paths slider.`)
                  })"
                ))
              ),
              conditionalPanel(
                condition = "input.popDist == 'symmetric'",
                plotOutput("plotsymmetric2"),
                tags$script(HTML(
                  "$(document).ready(function() {
                  document.getElementById('plotsymmetric2').setAttribute('aria-label',
                  `The Arithmetic mean graph shows that as you increase the
                     number of trials (the number of observations in your sample
                     up to the value of the sample size slider), the arithmetic
                     mean of your sample will close in on the true value. You
                     may add additional runs of the process by changing the
                     number of paths slider.`)
                  })"
                ))
              ),
              conditionalPanel(
                condition = "input.popDist == 'astragalus'",
                plotOutput("line2"),
                tags$script(HTML(
                  "$(document).ready(function() {
                  document.getElementById('plotline2').setAttribute('aria-label',
                  `The Arithmetic mean graph shows that as you increase the
                     number of trials (the number of observations in your sample
                     up to the value of the sample size slider), the arithmetic
                     mean of your sample will close in on the true value. You
                     may add additional runs of the process by changing the
                     number of paths slider.`)
                  })"
                ))
              ),
              conditionalPanel(
                condition = "input.popDist == 'bimodal'",
                plotOutput("plotbimodal2"),
                tags$script(HTML(
                  "$(document).ready(function() {
                  document.getElementById('plotbimodal2').setAttribute('aria-label',
                  `The Arithmetic mean graph shows that as you increase the
                     number of trials (the number of observations in your sample
                     up to the value of the sample size slider), the arithmetic
                     mean of your sample will close in on the true value. You
                     may add additional runs of the process by changing the
                     number of paths slider.`)
                  })"
                ))
              ),
              conditionalPanel(
                condition = "input.popDist == 'poisson'",
                plotOutput("plotpoisson1"),
                tags$script(HTML(
                  "$(document).ready(function() {
                  document.getElementById('plotpoisson1').setAttribute('aria-label',
                  `The Arithmetic mean graph shows that as you increase the
                     number of trials (the number of observations in your sample
                     up to the value of the sample size slider), the arithmetic
                     mean of your sample will close in on the true value. You
                     may add additional runs of the process by changing the
                     number of paths slider.`)
                  })"
                ))
              ),
              conditionalPanel(
                condition = "input.popDist =='ipodshuffle'",
                plotOutput("PlotMeaniPod"),
                tags$script(HTML(
                  "$(document).ready(function() {
                  document.getElementById('PlotMeeaniPod').setAttribute('aria-label',
                  `The Arithmetic mean graph shows that as you increase the
                     number of trials (the number of observations in your sample
                     up to the value of the sample size slider), the arithmetic
                     mean of your sample will close in on the true value. You
                     may add additional runs of the process by changing the
                     number of paths slider.`)
                  })"
                ))
              )
            ),
            # Plot of Sums----
            column(
              width = 6,
              conditionalPanel(
                condition = "input.popDist == 'leftskewed'",
                plotOutput("plotleft3"),
                tags$script(HTML(
                  "$(document).ready(function() {
                  document.getElementById('plotleft3').setAttribute('aria-label',
                  `The Sum graph shows that as you increase the number of trials
                  (the number of observations in your sample up to the value of
                  the sample size slider), the sum of your sample will not close
                  in on the true value. You may add additional runs of the
                  process by changing the number of paths slider.`)
                  })"
                ))
              ),
              conditionalPanel(
                condition = "input.popDist == 'rightskewed'",
                plotOutput("plotright3"),
                tags$script(HTML(
                  "$(document).ready(function() {
                  document.getElementById('plotright3').setAttribute('aria-label',
                  `The Sum graph shows that as you increase the number of trials
                  (the number of observations in your sample up to the value of
                  the sample size slider), the sum of your sample will not close
                  in on the true value. You may add additional runs of the
                  process by changing the number of paths slider.`)
                  })"
                ))
              ),
              conditionalPanel(
                condition = "input.popDist == 'symmetric'",
                plotOutput("plotsymmetric3"),
                tags$script(HTML(
                  "$(document).ready(function() {
                  document.getElementById('plotsymmetric3').setAttribute('aria-label',
                  `The Sum graph shows that as you increase the number of trials
                  (the number of observations in your sample up to the value of
                  the sample size slider), the sum of your sample will not close
                  in on the true value. You may add additional runs of the
                  process by changing the number of paths slider.`)
                  })"
                ))
              ),
              conditionalPanel(
                condition = "input.popDist == 'astragalus'",
                plotOutput("line1"),
                tags$script(HTML(
                  "$(document).ready(function() {
                  document.getElementById('plotline1').setAttribute('aria-label',
                  `The Sum graph shows that as you increase the number of trials
                  (the number of observations in your sample up to the value of
                  the sample size slider), the sum of your sample will not close
                  in on the true value. You may add additional runs of the
                  process by changing the number of paths slider.`)
                  })"
                ))
              ),
              conditionalPanel(
                condition = "input.popDist == 'bimodal'",
                plotOutput("plotbimodal3"),
                tags$script(HTML(
                  "$(document).ready(function() {
                  document.getElementById('plotbimodal3').setAttribute('aria-label',
                  `The Sum graph shows that as you increase the number of trials
                  (the number of observations in your sample up to the value of
                  the sample size slider), the sum of your sample will not close
                  in on the true value. You may add additional runs of the
                  process by changing the number of paths slider.`)
                  })"
                ))
              ),
              conditionalPanel(
                condition = "input.popDist == 'poisson'",
                plotOutput("plotpoisson2"),
                tags$script(HTML(
                  "$(document).ready(function() {
                  document.getElementById('plotpoisson2').setAttribute('aria-label',
                  `The Sum graph shows that as you increase the number of trials
                  (the number of observations in your sample up to the value of
                  the sample size slider), the sum of your sample will not close
                  in on the true value. You may add additional runs of the
                  process by changing the number of paths slider.`)
                  })"
                ))
              ),
              conditionalPanel(
                condition = "input.popDist =='ipodshuffle'",
                plotOutput("PlotSumiPod"),
                tags$script(HTML(
                  "$(document).ready(function() {
                  document.getElementById('PlotSumiPod').setAttribute('aria-label',
                  `The Sum graph shows that as you increase the number of trials
                  (the number of observations in your sample up to the value of
                  the sample size slider), the sum of your sample will not close
                  in on the true value. You may add additional runs of the
                  process by changing the number of paths slider.`)
                  })"
                ))
              )
            )
          )
        ),
        #### Set up the References Page----
        tabItem(
          tabName = "References",
          withMathJax(),
          h2("References"),
          p(
            class = "hangingindent",
            "Bailey, E. (2015), shinyBS: Twitter bootstrap components for shiny, R
          package. Available from https://CRAN.R-project.org/package=shinyBS"
          ),
          p(
            class = "hangingindent",
            "Boos, D. D. and Nychka, D. (2012), Rlab: Functions and Datasets
          Required for ST370 class, R package. Available from
          https://CRAN.R-project.org/package=Rlab"
          ),
          p(
            class = "hangingindent",
            "Carey, R. (2019), boastUtils: BOAST Utilities, R Package. Available
          from https://github.com/EducationShinyAppTeam/boastUtils"
          ),
          p(
            class = "hangingindent",
            "Chang, W. and Borges Ribeio, B. (2018), shinydashboard: Create
          dashboards with 'Shiny', R Package. Available from
          https://CRAN.R-project.org/package=shinydashboard"
          ),
          p(
            class = "hangingindent",
            "Chang, W., Cheng, J., Allaire, J., Xie, Y., and McPherson, J. (2019),
          shiny: Web application framework for R, R Package. Available from
          https://CRAN.R-project.org/package=shiny"
          ),
          p(
            class = "hangingindent",
            "Penn State University, 3.2 - Graphs: Displaying Measurement Data:
          STAT 100. Penn State: Statistics Online Courses. Available from
          https://online.stat.psu.edu/stat100/lesson/3/3.2"
          ),
          p(
            class = "hangingindent",
            "Perrier, V., Meyer, F., and Granjon, D. (2020), shinyWidgets: Custom
          Inputs Widgets for Shiny, R package. Available from
          https://CRAN.R-project.org/package=shinyWidgets"
          ),
          p(
            class = "hangingindent",
            "R Core Team (2020), R: A language and environment for statistical
          computing. R Foundation for Statistical Computing, Vienna, Austria,
          R package. Available from https://www.R-project.org/"
          ),
          p(
            class = "hangingindent",
            "Wickham, H., François, R., Henry L., and Müller, K. (2020), dplyr:
          A Grammar of Data Manipulation, R package. Available from
          https://CRAN.R-project.org/package=dplyr"
          ),
          p(
            class = "hangingindent",
            "Wickham, H. (2016), ggplot2: Elegant graphics for data analysis, R
          Package, New York: Springer-Verlag. Available from
          https://ggplot2.tidyverse.org"
          ),
          p(
            class = "hangingindent",
            "Yaan (2007), Shagai. Wikimedia. Available from
          https://commons.wikimedia.org/wiki/File:Shagai.jpg"
          ),
          br(),
          br(),
          br(),
          boastUtils::copyrightInfo()
        )
      )
    )
  )
)

server <- function(session, input, output) {

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
  # Go Button
  observeEvent(input$go, {
    updateTabItems(session, "pages", "largeNumber")
  })

  # Go Button
  observeEvent(input$gop, {
    updateTabItems(session, "pages", "largeNumber")
  })

  # define color in different paths
  colors <- c("#0072B2", "#D55E00", "#009E73", "#ce77a8", "#E69F00")

  # Function for matrix means
  # Input: path (numeric), size (numeric), and matrix of data (numeric matrix)
  # Output: Matrix of means
  matrixMeans <- function(path, size, matrix) {
    means <- matrix(0, nrow = size, ncol = path)
    for (j in 1:path) {
      for (i in 1:size) {
        means[i, j] <- mean(matrix[1:i, j])
      }
    }
    return(means)
  }

  # Function for making the sum plots
  # Input: number of paths (numeric), sample size (numeric),
  # matrix of sum values (numeric matrix), actual sum value (numeric)
  # Returns: the plot object
  makeSumPlot <- function(path, size, matrixSum, trueSum, label) {
    # Set up data frame to use with ggplot
    allNames <- c("A", "B", "C", "D", "E")
    data <- as.data.frame(matrixSum)
    colnames(data) <- allNames[1:path]
    data$x <- 1:size

    # Create plot
    plot <- ggplot2::ggplot(aes_string(x = "x"), data = data) +
      geom_hline(aes(yintercept = trueSum, linetype = "True sum"),
        show.legend = F,
        size = 1
      ) +
      scale_linetype_manual(name = "", values = c("dotted")) +
      ylim(c(
        min(matrixSum, trueSum) - .01,
        max(matrixSum, trueSum) + .01
      )) +
      xlab("Number of trials so far") +
      ylab("Sum-E(sum)") +
      ggtitle("Sum") +
      theme(
        axis.text = element_text(size = 18),
        plot.title = element_text(size = 18, face = "bold"),
        axis.title = element_text(size = 18),
        panel.background = element_rect(fill = "white", color = "black"),
        legend.position = c(.89, 1.07),
        legend.text = element_text(size = 14)
      )

    # Add paths
    for (i in 1:path) {
      plot <- plot + geom_path(aes_string(x = "x", y = allNames[i]),
        data = data,
        color = colors[i],
        size = 1.5
      )
    }
    plot
  }

  # Function for making the mean plots
  # Input: number of paths (numeric), sample size (numeric), matrix of sum values
  # (numeric matrix), actual sum value (numeric), optional y label
  # (default is Mean)
  # Returns: the plot object
  makeMeansPlot <- function(path, size, matrixMeans, trueMean, label = "Mean") {
    # Set up dataframe to use with ggplot
    allNames <- c("A", "B", "C", "D", "E")
    data <- as.data.frame(matrixMeans)
    colnames(data) <- allNames[1:path]
    data$x <- 1:size

    # Create Plot
    plot <- ggplot2::ggplot(aes_string(x = "x"), data = data) +
      geom_hline(aes(yintercept = trueMean, linetype = "True mean"),
        show.legend = T,
        size = 1
      ) +
      scale_linetype_manual(
        name = "", values = c("dotted"),
        guide = guide_legend(override.aes = list(
          color = c("black")
        ))
      ) +
      ylim(c(
        min(matrixMeans, trueMean) - .01,
        max(matrixMeans, trueMean) + .01
      )) +
      xlab("Number of trials so far") +
      ylab(label) +
      ggtitle("Arithmetic Mean") +
      theme(
        axis.text = element_text(size = 18),
        plot.title = element_text(size = 18, face = "bold"),
        axis.title = element_text(size = 18),
        panel.background = element_rect(fill = "white", color = "black"),
        legend.position = c(.89, 1.07),
        legend.text = element_text(size = 14)
      )
    # Add paths
    for (i in 1:path) {
      plot <- plot + geom_path(aes_string(x = "x", y = allNames[i]),
        data = data,
        color = colors[i], size = 1.5
      )
    }
    plot
  }

  # Function to create density plots for each group
  # Inputs: Dataframe consisting of columns x and y to define axes, limits for x
  # axis in form c(lower, upper), optional path for symmetric case
  # Output: ggplot of density
  makeDensityPlot <- function(data, xlims, path = 0) {
    plot <- ggplot2::ggplot(aes(x = x, y = y), data = data) +
      geom_path(color = "#0072B2", size = 1.5) +
      xlim(xlims) +
      xlab("Value") +
      ylab("Density") +
      ggtitle("Population Graph") +
      theme(
        axis.text = element_text(size = 18),
        plot.title = element_text(size = 18, face = "bold"),
        axis.title = element_text(size = 18),
        panel.background = element_rect(fill = "white", color = "black")
      )
    # For case in symmetric where path is 1 causing "box" shape
    if (path == 1) {
      plot <- plot +
        geom_segment(aes(x = 0, y = 0, xend = 0, yend = 1), color = "#0072B2", size = 1.5) +
        geom_segment(aes(x = 1, y = 0, xend = 1, yend = 1), color = "#0072B2", size = 1.5)
    }
    plot
  }

  # Function to create bar plots for each group
  # Inputs: x axis label (string), dataframe consisting of either column x or
  # columns x and y to define axes
  # Output: ggplot of resulting bar plot
  makeBarPlot <- function(xlab, data, levels = as.character(data$x)) {
    plot <- ggplot(aes(x = factor(x, levels = levels), y = y), data = data) +
      geom_bar(stat = "identity", fill = "#0072B2") +
      ylim(c(0, max(data$y) + .1 * max(data$y))) +
      xlab(xlab) +
      ylab("Probability") +
      ggtitle("Population Graph") +
      theme(
        axis.text = element_text(size = 18),
        plot.title = element_text(size = 18, face = "bold"),
        axis.title = element_text(size = 18),
        panel.background = element_rect(fill = "white", color = "black")
      ) +
      scale_x_discrete(drop = FALSE)

    plot
  }

  ###################################################################
  ## Left skewed
  ####################################################################
  leftSkew <- reactive({
    11 - 10 * input$leftskew
  })

  # Population of left skewed
  output$plotleft1 <- renderCachedPlot(
    {
      # Define parameters for density plot
      x <- seq((leftSkew()) - 9 * sqrt((leftSkew())), 0, length = input$symsize)
      y <- dgamma(-x, shape = (leftSkew()), beta = 1)
      data <- data.frame(x = x, y = y)

      # Make Density Plot
      makeDensityPlot(data = data, xlims = c((leftSkew()) - 9 * sqrt((leftSkew())), 0))
    },
    cacheKeyExpr = {
      list(input$leftskew)
    }
  )

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
  output$plotleft2 <- renderCachedPlot(
    {

      # Define the true mean alpha*beta = 1
      trueMean <- -(leftSkew())

      # Plot average in different paths
      makeMeansPlot(
        input$leftpath,
        input$leftsize,
        matrixMeans(input$leftpath, input$leftsize, data1()),
        trueMean
      )
    },
    cacheKeyExpr = {
      list(input$leftpath, input$leftsize, input$leftskew)
    }
  )

  # Sum of left skewed
  output$plotleft3 <- renderCachedPlot(
    {
      matrix <- data1()

      # Store value of sum into matrix matrixSum
      matrixSum <-
        matrix(0, nrow = input$leftsize, ncol = input$leftpath)
      for (j in 1:input$leftpath) {
        for (i in input$leftsize:1) {
          matrixSum[i, j] <- mean(matrix[1:i, j]) * i + i * (leftSkew())
        }
      }

      # Define the true (sum - E(sum) = 0)
      trueSum <- 0

      # Plot sum in different paths
      makeSumPlot(input$leftpath, input$leftsize, matrixSum, trueSum)
    },
    cacheKeyExpr = {
      list(input$leftpath, input$leftsize, input$leftskew)
    }
  )


  ###################################################################
  ## Right skewed
  ####################################################################
  rightSkew <- reactive({
    11 - 10 * input$rightskew
  })
  # Population of right skewed
  output$plotright1 <- renderCachedPlot(
    {
      # Define parameters for density plot
      x <- seq(0, (rightSkew()) + 9 * sqrt(rightSkew()), length = input$symsize)
      y <- dgamma(x, shape = (rightSkew()), beta = 1)
      data <- data.frame(x = x, y = y)

      # Make the density plot
      makeDensityPlot(data = data, xlims = c(0, (rightSkew()) + 9 * sqrt((rightSkew()))))
    },
    cacheKeyExpr = {
      list(input$rightskew)
    }
  )

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
  output$plotright2 <- renderCachedPlot(
    {

      # Define the true mean alpha*beta = 1
      trueMean <- (rightSkew())

      # Make means plot
      makeMeansPlot(
        input$rightpath,
        input$rightsize,
        matrixMeans(input$rightpath, input$rightsize, data2()),
        trueMean
      )
    },
    cacheKeyExpr = {
      list(input$rightpath, input$rightsize, input$rightskew)
    }
  )

  # Sum of right skewed
  output$plotright3 <- renderCachedPlot(
    {
      matrix <- data2()
      # Store value of sum into matrix matrixSum
      matrixSum <-
        matrix(0,
          nrow = input$rightsize,
          ncol = input$rightpath
        )
      for (j in 1:input$rightpath) {
        for (i in 1:input$rightsize) {
          matrixSum[i, j] <- mean(matrix[1:i, j]) * i - i * (rightSkew())
        }
      }

      # Define the true (sum - E(sum) = 0)
      trueSum <- 0

      # Plot sum in different paths
      makeSumPlot(input$rightpath, input$rightsize, matrixSum, trueSum)
    },
    cacheKeyExpr = {
      list(input$rightpath, input$rightsize, input$rightskew)
    }
  )

  ###################################################################
  ## Symmetric skewed
  ####################################################################
  inverse <- reactive({
    round(14.6 * input$inverse^3 - 5.7 * input$inverse^2 +
      input$inverse + .1, 3)
  })
  # Population of Symmetric skewed
  output$plotsymmetric1 <- renderCachedPlot(
    {
      x <- seq(0, 1, length = input$symsize)
      dens <-
        dbeta(x,
          shape1 = inverse(),
          shape2 = inverse()
        )
      data <- data.frame(x = x, y = dens)

      # Make density plot separated by case where the peakedness is exactly 1 (causes a "box" shape)
      makeDensityPlot(data = data, xlims = c(-0.03, 1.03), path = inverse())
    },
    cacheKeyExpr = {
      list(input$symsize, input$inverse)
    }
  )

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
  output$plotsymmetric2 <- renderCachedPlot(
    {

      # Define the true mean
      trueMean <- 1 / 2

      # Make means plot
      makeMeansPlot(
        input$sympath,
        input$symsize,
        matrixMeans(input$sympath, input$symsize, data3()),
        trueMean
      )
    },
    cacheKeyExpr = {
      list(input$sympath, input$symsize, input$inverse)
    }
  )

  # Sum of symmetric
  output$plotsymmetric3 <- renderCachedPlot(
    {
      matrix <- data3()
      # Store value of sum into matrix matrixSum
      matrixSum <- matrix(1 / 2, nrow = input$symsize, ncol = input$sympath)
      for (j in 1:input$sympath) {
        for (i in 1:input$symsize) {
          matrixSum[i, j] <- mean(matrix[1:i, j]) * i - 0.5 * i
        }
      }

      # Define the true mean
      trueSum <- 0

      # Plot sum in different paths
      makeSumPlot(input$sympath, input$symsize, matrixSum, trueSum)
    },
    cacheKeyExpr = {
      list(input$symsize, input$sympath, input$inverse)
    }
  )

  ###################################################################
  ## Bimodal
  ####################################################################
  # Population for bimodel
  prop <- reactive({
    input$prop / 100
  })
  output$plotbimodal1 <- renderCachedPlot(
    {
      # Define parameters for density plot
      t <- 1 / (input$bisize * input$bipath)
      y <- seq(0, 1, t)
      z <- seq(1, 0, -t)
      leftdraw <- dbeta(z, 4, 14) * .2
      rightdraw <- dbeta(y, 4, 14) * .2
      data <- data.frame(x = seq(0, 5, t * 5), y = prop() * leftdraw + (1 - prop()) *
        rightdraw)

      # Make the density plot
      makeDensityPlot(data = data, xlims = c(0, 5))
    },
    cacheKeyExpr = {
      list(input$prop)
    }
  )

  # Create data for bimodel
  data4 <-
    reactive({
      # Random vector of 0s and 1s to determine which distribution each element
      # samples from
      rand <- sample(
        x = c(0, 1),
        size = input$bisize * input$bipath,
        replace = TRUE,
        prob = c(1 - prop(), prop())
      )

      # Number of elements sampled from the right distribution (represented by 1)
      rights <- sum(rand)
      # Number of elements sampled from left distribution (represented by 0)
      lefts <- input$bisize * input$bipath - rights
      leftGammas <- rbeta(lefts, 4, 14) * 5

      # rgamma(lefts, 1.25, beta = 1) # Samples left distribution
      rightGammas <- 5 - rbeta(rights, 4, 14) * 5 # Samples right distribution

      # Loop to assign values from gamma distributions to rand
      rightIndex <- 1
      leftIndex <- 1
      for (x in 1:length(rand)) {
        if (rand[x] == 0) {
          rand[x] <- leftGammas[leftIndex]
          leftIndex <- leftIndex + 1
        }
        else {
          rand[x] <- rightGammas[rightIndex]
          rightIndex <- rightIndex + 1
        }
      }

      # Turn vector rand into a matrix with proper dimensions
      matrix(rand, nrow = input$bisize, ncol = input$bipath)
    })

  # Average for bimodel
  output$plotbimodal2 <- renderCachedPlot(
    {

      # Define the true mean
      trueMean <- mean(data4())

      # Plot average in different paths
      makeMeansPlot(
        input$bipath,
        input$bisize,
        matrixMeans(input$bipath, input$bisize, data4()),
        trueMean
      )
    },
    cacheKeyExpr = {
      list(input$bipath, input$bisize, input$prop)
    }
  )

  # Sum for bimodel
  output$plotbimodal3 <- renderCachedPlot(
    {
      matrix <- data4()
      # Store value of sum into matrix matrixSum
      matrixSum <- matrix(0, nrow = input$bisize, ncol = input$bipath)
      for (j in 1:input$bipath) {
        for (i in 1:input$bisize) {
          matrixSum[i, j] <- mean(matrix[1:i, j]) * i - mean(data4()) * i
        }
      }

      # Define the true sum
      trueSum <- 0

      # Plot sum in different paths
      makeSumPlot(input$bipath, input$bisize, matrixSum, trueSum)
    },
    cacheKeyExpr = {
      list(input$bipath, input$bisize, input$prop)
    }
  )


  ###################################################################
  ## Accident Rate
  ####################################################################

  # Population of Poisson
  output$poissonpop <- renderCachedPlot(
    {
      data <- data.frame(x = 0:ceiling(2 * input$poissonmean + 5)) # More x's than necessary
      # Get y vals for x's
      data$y <- (input$poissonmean^data$x) * exp(-input$poissonmean) / factorial(data$x)
      # Filter based on probability
      data <- rbind(data[1:2, ], filter(data[-c(1, 2), ], y > .0005))
      makeBarPlot(xlab = "Number of accidents", data = data)
    },
    cacheKeyExpr = {
      list(input$poissonmean)
    }
  )

  # Matrix of rpois values
  data5 <-
    reactive(matrix(
      rpois(
        input$poissonpath * input$poissonsize,
        input$poissonmean
      ),
      nrow = input$poissonsize,
      ncol = input$poissonpath
    ))

  # Average for poisson
  output$plotpoisson1 <- renderCachedPlot(
    {

      # Define the true mean
      trueMean <- input$poissonmean

      # Plot average in different paths
      makeMeansPlot(
        input$poissonpath,
        input$poissonsize,
        matrixMeans(input$poissonpath, input$poissonsize, data5()),
        trueMean
      )
    },
    cacheKeyExpr = {
      list(input$poissonmean, input$poissonpath, input$poissonsize)
    }
  )

  # Sum for accident rate
  output$plotpoisson2 <- renderCachedPlot(
    {
      matrix <- data5()
      # Store value of sum into matrix matrixSum
      matrixSum <-
        matrix(0,
          nrow = input$poissonsize,
          ncol = input$poissonpath
        )
      for (j in 1:input$poissonpath) {
        for (i in 1:input$poissonsize) {
          matrixSum[i, j] <- mean(matrix[1:i, j]) * i - input$poissonmean * i
        }
      }

      # Define the true sum
      trueSum <- 0

      # Make plot for sum
      makeSumPlot(input$poissonpath, input$poissonsize, matrixSum, trueSum)
    },
    cacheKeyExpr = {
      list(input$poissonmean, input$poissonpath, input$poissonsize)
    }
  )

  ###################################################################
  ## Astragalus
  ####################################################################

  # Die results
  die <- reactive({
    die <- c(rep(1, 1), rep(3, 4), rep(4, 4), rep(6, 1))
  })

  # Population of Astragalus
  output$pop <- renderPlot({
    data <- data.frame(x = c(1, 3, 4, 6), y = c(.1, .4, .4, .1))
    makeBarPlot(xlab = "Number on roll of astragalus", data = data, levels = 1:6)
  })

  # Matrix of sample values
  drawAdie <-
    reactive(matrix(
      sample(die(), input$aspath * input$assize,
        replace = TRUE
      ),
      nrow = input$assize,
      ncol = input$aspath
    ))

  # Average of Astragalus
  output$line2 <- renderCachedPlot(
    {

      # Define the true mean
      trueMean <- 3.5

      # Plot for means
      makeMeansPlot(
        input$aspath,
        input$assize,
        matrixMeans(input$aspath, input$assize, drawAdie()),
        trueMean
      )
    },
    cacheKeyExpr = {
      list(input$aspath, input$assize)
    }
  )

  # Sum of Astragalus
  output$line1 <- renderCachedPlot(
    {
      matrix <- drawAdie()
      matrixSum <- matrix(0, nrow = input$assize, ncol = input$aspath)
      for (j in 1:input$aspath) {
        for (i in 1:input$assize) {
          matrixSum[i, j] <- mean(matrix[1:i, j]) * i - 3.5 * i
        }
      }

      # Define the true sum
      trueSum <- 0

      # Plot for sum
      makeSumPlot(input$aspath, input$assize, matrixSum, trueSum)
    },
    cacheKeyExpr = {
      list(input$aspath, input$assize)
    }
  )

  ###################################################################
  ## iPOD SHUFFLE
  ####################################################################

  # Reactive expression to get the number of songs of the chosen type
  nSongs <- reactive({
    if (input$ptype == "Jazz") {
      nSongs <- input$s1
    }
    else if (input$ptype == "Rock") {
      nSongs <- input$s2
    }
    else if (input$ptype == "Country") {
      nSongs <- input$s3
    }
    else {
      nSongs <- input$s4
    }
  })

  # Set up songs from four types
  songs <- reactive({
    songs <- c(
      rep(input$s1),
      rep(input$s2),
      rep(input$s3),
      rep(input$s4)
    )
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
  output$iPodBarPlot <- renderCachedPlot(
    {
      # Parameters for bar plot
      p <- nSongs() / sum(songs())
      data <- data.frame(x = c("Other music (0)", paste(input$ptype, "(1)")), y = c(1 - p, p))
      data$x <- factor(data$x, levels = data$x) # Done to force sorted order for bars

      # Make bar plot
      makeBarPlot(xlab = "Genre", data = data)
    },
    cacheKeyExpr = {
      list(input$s1, input$s2, input$s3, input$ptype, input$s4, input$ipodsize)
    }
  )

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
  output$PlotMeaniPod <- renderCachedPlot(
    {

      # Define the true mean
      trueMean <- nSongs() / sum(songs())

      # Plot average in different paths
      makeMeansPlot(
        input$ipodpath,
        input$ipodsize,
        matrixMeans(input$ipodpath, input$ipodsize, genreData()),
        trueMean,
        "Proportion"
      )
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
    }
  )

  # Plot playlist sum
  output$PlotSumiPod <- renderCachedPlot(
    {
      matrix <- genreData()

      # Store value of sum into matrix matrixSum
      matrixSum <-
        matrix(0, nrow = input$ipodsize, ncol = input$ipodpath)
      for (j in 1:input$ipodpath) {
        for (i in 1:input$ipodsize) {
          matrixSum[i, j] <- mean(matrix[1:i, j]) * i - i * (nSongs() / sum(songs()))
        }
      }

      # Define the true sum
      trueSum <- 0

      # Plot sum in different paths
      makeSumPlot(input$ipodpath, input$ipodsize, matrixSum, trueSum)
    },
    cacheKeyExpr = {
      list(
        input$s1, input$s2, input$s3, input$ptype,
        input$s4, input$ipodsize, input$ipodpath
      )
    }
  )
}

boastUtils::boastApp(ui = ui, server = server)
