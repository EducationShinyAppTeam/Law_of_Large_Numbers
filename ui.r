library(shiny)
library(shinydashboard)
library(shinyBS)

# Define UI 
shinyUI(dashboardPage(
  skin = "blue",
  dashboardHeader(
    title = 'Law of Large Numbers',
    titleWidth = 300,
    tags$li(
      class = "dropdown",
      tags$a(href = 'https://shinyapps.science.psu.edu/', icon("home"))
    ),
    tags$li(class = "dropdown", actionLink("info", icon("info", class =
                                                          "myClass")))
  ),
  # Makes Side Panel
  dashboardSidebar(
    width=300,
    sidebarMenu(
    id = "tabs",
    menuItem("Overview", tabName = "Overview", icon = icon("tachometer-alt")),
    menuItem("Prerequisites", tabName = "Prerequisites", icon = icon("book")),
    menuItem("Law of Large Numbers",icon = icon("wpexplorer"),tabName = "largeNumber"),
    menuItem("References", tabName = "References", icon = icon("leanpub")
    )
  )),
  dashboardBody(
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "https://educationshinyappteam.github.io/Style_Guide/theme/boast.css")
    ),
    
    # Overview Tab
    tabItems(
      tabItem(
        tabName = "Overview",
        tags$a(
          href = 'http://stat.psu.edu/',
          tags$img(src = 'PS-HOR-RGB-2C.png', align = "left", width = 180)
        ),
        br(),
        br(),
        br(),
        
        fluidPage(
          h3(tags$b("About:")),
          
            "This app is designed to examine the Law of Large Numbers for means and proportions.
                                 The Law of Large Numbers tells us that averages or proportions are likely
                                 to be more stable when there are more trials while sums or counts are likely to be more variable.
                                 This does not happen by compensation for a bad run of luck since independent trials have no memory."
          ,
          
            "This app lets students see the Law of Large Numbers in action for different sample sizes from different populations."
          ,
          br(),
          h3(tags$b("Instructions:")),
          
            tags$li(
              "Pick a population from one of the continuous types (left-skewed; right-skewed; symmetric; or bimodal) or one of the discrete examples (rolls of an astragalus/bone die; songs shuffled from a playlist;
                                 or accident occurrence)."
            
          ),
        
            tags$li(
              "Use the sliders to adjust the parameters of the chosen population model."
            ),
          
            tags$li(
              "Use the sliders to choose the sample size and how many sample paths you will observe."
          ),
          
            tags$li(
              "Observe whether the plots for the averages and sums converge or diverge from their expected values."
          ),
          
          div(style = "text-align: center",
              bsButton("go", "Explore", icon("bolt"), class =
                         "circle grow")),
          br(),
          h3(tags$b("Acknowledgements:")),
          
          "This app was originally developed and coded by Zibin Gao and Caihui Xiao. 
          The app was modified and recoded by Yingjie(Chelsea) Wang in February 2018, by Zhiruo Wang in May 2019,
          and by Leah Hunt in June 2020. 
          Special thanks to Jingling Feng and team member Qichao Chen for help in the development of the app."
          
        )
      ),
      
      #### Set up the Prerequisites Page
      tabItem(
        tabName = "Prerequisites",
        withMathJax(),
        h2("Prerequisites"),
        p("In order to get the most out of this app, please review the
            following information that will be used in the app."),

        #https://online.stat.psu.edu/stat100/lesson/3/3.2
        #"https://online.stat.psu.edu/stat200/lesson/2/2.2/2.2.3"
        tags$ul(
          tags$li("This app uses four distributions: right (positive) skewed, left (negative) skewed, symmetric, and bimodal.
          While in depth understanding of these distributions is not required, you may wish to review this ",
                  tags$a(href="https://online.stat.psu.edu/stat100/lesson/3/3.2#graphshapes", "Stat 100 Table of Graph Shapes"),
                  ".",),
          tags$li("One of the distributions is based upon rolls of an astragalus. The astragalus (ankle or heel bone) of animals were used in ancient times as a forerunner of modern dice. 
                  When a sheep astragalus is thrown into the air it can land on one of four sides, which were associated with the numbers 
                  1, 3, 4, and 6. Two sides (the 3 and the 4) are wider and each come up about 40% of the time, while the narrower sides 
                  (the 1 and the 6) each come up about 10% of the time. An image of an astralagus is shown below.")
        ),
        HTML('<center><figure><img src="astragalus.jpg" alt="Picture of an astragalus" width="600"><figcaption>image by Yaan, 2007</figcaption></figure></center>'),
        tags$ul(
          tags$li("The app allows the user to select the number of paths to plot. The number of paths refers to the number of repetitions of
                  the experiment that will be done.")
          
        ),
        tags$ul(
          tags$li("The sample size refers to the number of times you will repeat the chance process in each path.")
          
        )
      ),
      
      
      # Explore Law of Large Numbers Tab
      tabItem(
        tabName = "largeNumber",
        
        tags$head(tags$style(
          HTML("input[type=\"number\"] {width: 60px;}")
        )),
        
        # Layout for the population picker
        sidebarLayout(
          sidebarPanel(
          width=6,
          fluidRow(
            column(
              6,
              # Select Input for the distribution type
              selectInput(
                "popDist",
                "Population type",
                list(
                  "Left-skewed" = "leftskewed",
                  "Right-skewed" = "rightskewed",
                  "Symmetric" = "symmetric",
                  "Bimodal" = "bimodal",
                  "Astragalus (Bone Die)" =
                    "astragalus",
                  "Playlist" =
                    "ipodshuffle",
                  "Accident Rate" = "poisson"
                )
              ),
              
              # Conditional Panel for type of population distribution
              
              # Left Skewed
              conditionalPanel(
                condition = "input.popDist=='leftskewed'",
                sliderInput(
                  "leftskew",
                  " Skewness",
                  min = 1,
                  max = 10,
                  value = 1,
                  step = 0.1
                ),
                "Where 1 represents the greatest skew and 10 the least skew"
              )
              ,
              
              # Right Skewed
              conditionalPanel(
                condition = "input.popDist=='rightskewed'",
                sliderInput(
                  "rightskew",
                  "Skewness",
                  min = 1,
                  max = 10,
                  value = 1,
                  step = 0.1
                ),
                "Where 1 represents the greatest skew and 10 the least skew"
              ),
              
              #Symmetric
              conditionalPanel(
                condition = "input.popDist=='symmetric'",
                sliderInput(
                  "inverse",
                  "Peakedness",
                  min = 0.5,
                  max = 10,
                  value = 1,
                  step = 0.1
                )
              ),
              
              # Bimodal
              conditionalPanel(
                condition = "input.popDist=='bimodal'",
                
                sliderInput(
                  "prop",
                  "% under right mode",
                  min = .1,
                  max = .9,
                  value = 0.5
                  #interval = 1
                )
              ),
              
              # Poisson
              conditionalPanel(
                condition = "input.popDist=='poisson'",
                
                sliderInput(
                  "poissonmean",
                  "Mean",
                  min = 0.1,
                  max = 10,
                  value = 1,
                  step = 0.1
                )
              ),
              
              #iPod shuffle
              conditionalPanel(
                condition = "input.popDist == 'ipodshuffle'",
                column(
                  width = 7,
                  offset = 0,
                  
                  p("Number of songs:"),
                  column(
                    4,
                    offset = 0,
                    
                    # Inputs for the probabilites of each music type
                    numericInput(
                      "s1",
                      "Jazz",
                      1,
                      min = 0,
                      max = 200,
                      step = 1
                    ),
                    numericInput(
                      "s2",
                      "Rock",
                      1,
                      min = 0,
                      max = 200,
                      step = 1
                    ),
                    numericInput(
                      "s3",
                      "Country",
                      1,
                      min = 0,
                      max = 200,
                      step = 1
                    ),
                    numericInput(
                      "s4",
                      "Hip-hop",
                      1,
                      min = 0,
                      max = 200,
                      step = 1
                    )
                  )
                ),

              ) #This parenthesis ends the iPod Shuffle Conditional Panel
              
            ), #Ends inputs column
            
            # Inputs for each type:
            
            column(
              6,
              
              #left skewed
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
              
              # Right skewed
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
              
              # Symmetric
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
              # Astragulus
              conditionalPanel(
                condition = "input.popDist == 'astragalus'",
                # Choose number of paths
                sliderInput(
                  "aspath",
                  'Number of paths',
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
              
              # Bimodal
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
              
              # Poisson
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
              
              # Playlist
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
                  list("Jazz",
                       "Rock",
                       "Country",
                       "Hip-hop"),
                  selected = "Jazz"
                )
              )
            )
            )
          ), #End of column for slider inputs
            
          mainPanel(
            width = 6,
            # Plots for each distribution; either histogram or density
              conditionalPanel(condition = "input.popDist == 'leftskewed'",
                               plotOutput('plotleft1')),
              conditionalPanel(condition = "input.popDist == 'rightskewed'",
                               plotOutput('plotright1')),
              conditionalPanel(condition = "input.popDist == 'symmetric'",
                               plotOutput('plotsymmetric1')),
              conditionalPanel(condition = "input.popDist == 'astragalus'",
                               plotOutput("pop")),
              conditionalPanel(condition = "input.popDist == 'bimodal'",
                               plotOutput('plotbiomodel1')),
              conditionalPanel(condition = "input.popDist == 'poisson'",
                               plotOutput('poissonpop')),
              conditionalPanel(condition = "input.popDist == 'ipodshuffle'",
                               plotOutput("iPodBarPlot")
            )
          )
          ),
        br(),
        
        # Plot of Arithmetic Means
        fluidRow(
          column(
            6,
            conditionalPanel(condition = "input.popDist == 'leftskewed'",
                             plotOutput('plotleft2')),
            conditionalPanel(condition = "input.popDist == 'rightskewed'",
                             plotOutput('plotright2')),
            conditionalPanel(condition = "input.popDist == 'symmetric'",
                             plotOutput('plotsymmetric2')),
            conditionalPanel(condition = "input.popDist == 'astragalus'",
                             plotOutput("line2")),
            conditionalPanel(condition = "input.popDist == 'bimodal'",
                             plotOutput('plotbiomodel2')),
            conditionalPanel(condition = "input.popDist == 'poisson'",
                             plotOutput('plotpoisson1')),
            conditionalPanel(condition = "input.popDist =='ipodshuffle'",
                             plotOutput("PlotMeaniPod")
            )
          ),
          
          # Plot of Sums
          column(
            6,
            conditionalPanel(condition = "input.popDist == 'leftskewed'",
                             plotOutput('plotleft3')),
            conditionalPanel(condition = "input.popDist == 'rightskewed'",
                             plotOutput('plotright3')),
            conditionalPanel(condition = "input.popDist == 'symmetric'",
                             plotOutput('plotsymmetric3')),
            conditionalPanel(condition = "input.popDist == 'astragalus'",
                             plotOutput("line1")),
            conditionalPanel(condition = "input.popDist == 'bimodal'",
                             plotOutput('plotbiomodel3')),
            conditionalPanel(condition = "input.popDist == 'poisson'",
                             plotOutput('plotpoisson2')),
            conditionalPanel(
              condition = "input.popDist =='ipodshuffle'",
              plotOutput('PlotSumiPod')
              
            )
          )
        )
      ),
      #### Set up the References Page
      tabItem(
        tabName = "References",
        withMathJax(),
        h2("References"),
        p(
          class = "hangingindent",
          "Bailey, E. (2015). shinyBS: Twitter bootstrap components for shiny.
            (v0.61). [R package]. Available from
            https://CRAN.R-project.org/package=shinyBS"
        ),
        p(
          class = "hangingindent",
          "Carey, R. (2019). boastUtils: BOAST Utilities. (v0.1.0).
            [R Package]. Available from
            https://github.com/EducationShinyAppTeam/boastUtils"
        ),
        p(
          class = "hangingindent",
          "Chang, W. and Borges Ribeio, B. (2018). shinydashboard: Create
            dashboards with 'Shiny'. (v0.7.1) [R Package]. Available from
            https://CRAN.R-project.org/package=shinydashboard"
        ),
        p(
          class = "hangingindent",
          "Chang, W., Cheng, J., Allaire, J., Xie, Y., and McPherson, J.
            (2019). shiny: Web application framework for R. (v1.4.0)
            [R Package]. Available from https://CRAN.R-project.org/package=shiny"
        ),
        p(
          class = "hangingindent",
          "Dennis D. Boos and Douglas Nychka (2012). Rlab: Functions and Datasets Required for
  ST370 class. R package version 2.15.1. https://CRAN.R-project.org/package=Rlab"
        ),
        p(
          class="hangingindent",
          "J. Mark Rowland, Clifford Qualls and Christian Gunning (2013). discrimARTs:
  Discrimination of Alternative Reproductive Tactics (ARTs).. R package version 0.2.
  https://CRAN.R-project.org/package=discrimARTs"
        ),
        p(
          class= "hangingindent",
          "Penn State University. 3.2 - Graphs: Displaying Measurement Data: STAT 100. 
          Penn State: Statistics Online Courses. Available at https://online.stat.psu.edu/stat100/lesson/3/3.2."
        ),
        p(
          class = "hangingindent",
          "R Core Team (2020). R: A language and environment for statistical computing. R
          Foundation for Statistical Computing, Vienna, Austria. URL https://www.R-project.org/."
        )
        ,
        
        p(
          class="hangingindent",
          "Victor Perrier, Fanny Meyer and David Granjon (2020). shinyWidgets: Custom Inputs
  Widgets for Shiny. R package version 0.5.2.
  https://CRAN.R-project.org/package=shinyWidgets"
        ),
        p(
          class="hangingindent",
          "Yaan. (2007). Shagai. Wikimedia. Available at https://commons.wikimedia.org/wiki/File:Shagai.jpg."
        )
        
      )
    )
  )
))
