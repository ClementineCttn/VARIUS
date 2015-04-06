library(shiny)

shinyUI(fluidPage(
  tags$head(tags$link(rel="shortcut icon", href="favicon.png")),
  titlePanel("VARIUS"),
  titlePanel(h4("Visualising empirical and simulated Agglomerations of (Imperial) Russia and the Soviet Union")),
  
  navlistPanel(
    "The MARIUS Project",
    tabPanel("Presentation",
             fluidRow(
               column(3, img(src = "mariusLogo.png",class="img-responsive")),
               column(9, h2("The MARIUS Project"))),
             tags$p(class="text-justify",
               'MARIUS is an incremental agent-based model of systems of cities. It 
             has been built to reproduce the trajectories of cities in the Former Soviet Union.
             The rules of the model are designed to range from generic interurban
             interactions to specific interactions between Soviet cities and their
             geographical environment. The structure of the model is modular enough
             to allow various sets of mechanisms playing together or alternatively.
             Our aim is to evaluate the degree of genericity of Soviet urban
             trajectories by growing articial systems of cities, from the most
             generic ones to the most particular. The fitness of the model is evaluated
             against urban data, gathered in the DARIUS Database.')
             #img(src = "incremental-modeling-EN.png", height = 500, width = 500)
    ),
    "What Happened ?",
    tabPanel( "Census Data", 
              h3("Observed distribution of city sizes with DARIUS Database, 1897-2010"),
                fluidRow(
                  column(6,
                         selectInput("Census_year",
                                     label = "Census year",
                                     choices = c("Pop2010", "Pop2002","Pop1989",
                                                 "Pop1979", "Pop1970", "Pop1959",
                                                 "Pop1939", "Pop1926","Pop1897"),
                                     selected = "Pop1959")),
                  column(6,
                         sliderInput("sizefactor1",
                                     label = "Population size factor",
                                     min = 0.01, max = 0.3,
                                     value = 0.1, step = 0.01))
                ),
              plotOutput("map1"),
              h3("Inter-census urban growth"),
              fluidRow(
                column(6,
                selectInput("Census_year1", label = "First Census year",
                            choices = c("Pop2002","Pop1989", "Pop1979",
                                        "Pop1970", "Pop1959", "Pop1939",
                                        "Pop1926", "Pop1897"),
                            selected = "Pop2002")),
                column(6,
                       selectInput("Census_year2", label = "Second Census year",
                            choices = c("Pop2010", "Pop2002","Pop1989",
                                        "Pop1979", "Pop1970","Pop1959",
                                        "Pop1939", "Pop1926"),
                            selected = "Pop2010"))
              ),
              plotOutput("map1bis"),
              h3("Urban Statuses & Attributes"),
              fluidRow(
                selectInput("attribute", label = "Urban Attribute",
                            choices = c("Regional Capital","National Capital", "Mono Industry", 
                                        "Oil&Gas located", "Coal Located", "Airport", "East/West"), 
                            selected = "Regional Capital")
              ),
              plotOutput("map1ter")
    ),
    
    "How to simulate it ?",
    tabPanel("Model Structures Analysis",
             h3("Contribution of mechanisms to the quality of simulation (closeness to data)"),
             fluidRow(
               column(6,
                      sliderInput("pvalue", label = "Statistical Significance (p-value, %)",
                           min = 0, max = 10, value = 1, step = 0.1))
             ),
             tags$p(class="text-justify", HTML('Models with different combinaison of mechanisms have been calibrated
              intensively against empirical data, using generic algorithms for as many
              generation as is defined by the slider "number of generations". 
              This plot shows the result of a regression of their closeness to data.
              Each bar represents the value of the estimated
              <span class="mark" title="Significance is given with a probability of error of the chosen percentage">
              coefficient</span> for each activated mechanism, in comparison with the
              same model structure without this mechanism, everything else being equal.')),
             plotOutput("graph1")
    ),
    
    tabPanel("Choose Mechanisms' combination", 
             h3("Optimal model after multi-calibration"),
             fluidRow(
               column(6,
                      sliderInput("ntrait", label = "Number of mechanisms",
                           min = 0, max = 5, value = 4, step = 1)),
               column(6,
                      selectInput("period", label = "Period",
                           choices = c("1959-1989", "1989-2010"), selected = "1959-1989"))
               
             ),
             tags$p("This gives a profile of the best performing model,
                    given the number of mechanisms actived at the chosen period."),
             h3("Evaluation Criteria:"),
             dataTableOutput("print1"),
             h3("Mechanisms' combination:"),
             dataTableOutput("print1bis"),
             h3("Parameter values:"),
             dataTableOutput("print1ter")
    ),
    "Simulate it",
    tabPanel("Run a MARIUS model",
             wellPanel(
             h3("Model structures and parameterization"),
             fluidRow(
               column(4,
               selectInput("choice_run", label = "Model to run",
                           choices = c("Best [Calibrated] Model", "Run [Customised] Model"), 
                           selected = "Best [Calibrated] Model")),
               column(4,
               selectInput("period2", label = "Period",
                           choices = c("1959-1989", "1989-2010"), selected = "1959-1989")),
               column(4,
                      checkboxGroupInput("mechanisms", 
                                  label = "Selected mechanisms", 
                                  choices = list("Bonus", "Fixed Costs", "Resources",
                                                 "Redistribution", "Urban Transition"),
                                  selected = c("Bonus", "Fixed Costs"))))),
             # Custom Model
             conditionalPanel(condition = "input.choice_run == 'Run [Customised] Model'",
                                h3("Generic parameters"),
                                fluidRow(
                                  column(4, sliderInput("populationToWealthExponent",
                                                        label = "populationToWealthExponent",
                                                        min = 1, max = 5, value = 1.1, step = 0.0001)),
                                  column(4, sliderInput("economicMultiplier",
                                                        label = "economicMultiplier",
                                                        min = 0, max = 1, value = 0.5, step = 0.001)),
                                  column(4, sliderInput("sizeEffectOnSupply",
                                                        label = "sizeEffectOnSupply",
                                                        min = 1, max = 2, value = 1.01, step = 0.0001))
                                ),
                                fluidRow(
                                  column(4, sliderInput("sizeEffectOnDemand",
                                                        label = "sizeEffectOnDemand",
                                                        min = 1, max = 2, value = 1.01, step = 0.0001)),
                                  column(4, sliderInput("distanceDecay",
                                                        label = "distanceDecay",
                                                        min = 0, max = 10, value = 0.5, step = 0.001)),
                                  column(4, sliderInput("wealthToPopulationExponent",
                                                        label = "wealthToPopulationExponent", 
                                                        min = 0, max = 2, value = 0.4, step = 0.0001))
                                ),     
                              # Bonus mechanism settings
                              #"For models with the bonus mechanism, choose parameter value",
                              conditionalPanel(condition = 'input.mechanisms.indexOf("Bonus") != -1',
                                               h3("Bonus parameter"),
                                               sliderInput("bonusMultiplier", label = "bonusMultiplier", 
                                                           min = 0, max = 100, value = 50, step = 0.001)
                              ),
                              # Fixed costs mechanism setitings
                              #"For models with the fixed costs mechanism, choose parameter value",
                              conditionalPanel(condition = 'input.mechanisms.indexOf("Fixed Costs") != -1',
                                               h3("Fixed Costs Transaction parameter"),
                                               sliderInput("fixedCost", label = "fixedCost",
                                                           min = 0, max = 100, value = 0.4, step = 0.01)
                              ),
                              # Ressources mechanism settings
                              #"For models with the resources mechanism, choose parameter values",
                              conditionalPanel(condition = 'input.mechanisms.indexOf("Resources") != -1',
                                               h3("SubSurface Resource parameters"),
                                               fluidRow(
                                                 column(6,
                                                        sliderInput("oilAndGazEffect",
                                                                    label = "oilAndGazEffect", 
                                                                    min = -1, max = 1,
                                                                    value = 0.3, step = 0.0001)
                                                 ),
                                                 column(6,
                                                        sliderInput("coalEffect",
                                                                    label = "coalEffect", 
                                                                    min = -1, max = 1,
                                                                    value = -0.05, step = 0.0001)
                                                 )
                                               )),
                              # Redistribution mechanism settings 
                              # "For models with the redistribution mechanism, choose parameter values",
                              conditionalPanel(condition = 'input.mechanisms.indexOf("Redistribution") != -1',
                                               h3("Double Redistribution parameters"),
                                               fluidRow(
                                                 column(6, 
                                                        sliderInput("territorialTaxes",
                                                                    label = "territorialTaxes", 
                                                                    min = 0, max = 1,
                                                                    value = 0.4, step = 0.0001)
                                                 ),
                                                 column(6,
                                                        sliderInput("capitalShareOfTaxes",
                                                                    label = "capitalShareOfTaxes", 
                                                                    min = 0, max = 1,
                                                                    value = 0.2, step = 0.0001)
                                                 )
                                               )
                              ),
                              # Urban transition mechanism settings  
                              #"For models with the urban transition mechanism, choose parameter value",
                              conditionalPanel(condition = 'input.mechanisms.indexOf("Urban Transition") != -1',
                                               h3("Urban transition parameter"),
                                               sliderInput("ruralMultiplier",
                                                           label = "ruralMultiplier", 
                                                           min = 0, max = 1,
                                                           value = 0.02, step = 0.0001)
                              )  
                              
                              
                              
                              
             ),
             
            hr(),
             "To execute the model with your parameterization, press ",
             inputPanel(
               actionButton("go", "Run The Model", icon = NULL)
               ),
            conditionalPanel(condition = 'input.go >= 1',
                 verbatimTextOutput("cppst"))
            
    ),
            
    "How close are we ?",
    tabPanel("Macro Analysis",
             h3("Macro-geographic analysis"),
             # verbatimTextOutput("modelcombi"),
             tags$p(class="text-justify",
                    "This rank-size representation is common to study the hierarchical
                    structure of systems of cities and their evolution towards equalisation
                    or differenciation of city sizes. Blue dots indicate simulated cities
                    over time, in comparison with empirical observations (in grey)"),
             plotOutput("graph2"),
             verbatimTextOutput("modelcombi")
    ),
    tabPanel("Residual trajectories",
             h3("Residuals Trajectories"),
             # verbatimTextOutput("modelcombi"),
             tags$p(class="text-justify",
                    "Residuals represent the difference between observed and simulated population for each city (in logs).
             Positive residuals mean that cities grew faster in reality than what we were able to simulate, 
             wheareas negative residuals indicate that we overestimated the growth of such cities.
             Residuals help trigger how to improve the model."),
             fluidRow(
               column(6,
                      selectInput("year_sim", label = "Simulated Year",
                                  choices = c("Pop2010","Pop2002","Pop1989",
                                              "Pop1979", "Pop1970"), 
                                  selected = "Pop1989")    
                      ),
               column(6,
                      sliderInput("cutoff", label = "Residual absolute cut-off",
                                  min = 0, max = 5, value = 0.5, step = 0.05))
             ),
             plotOutput("map2"),
             h3("Most Positive Residual trajectories"),
             dataTableOutput("table_pos_res"),
             h3("Most Negative Residual trajectories"),
             dataTableOutput("table_neg_res"),
             verbatimTextOutput("modelcombi2")
    ),
    tabPanel("Profiles of Residual cities",
             h3("Profiles of Residual cities"),
             tags$p(class="text-justify",HTML('Profiles of residual cities are obtained after a regression
                  on the value of residual. We plot the
                  <span title="Significance is given with a probability of error of 1%."
                  class="mark">coefficient values</span>
                  of some available urban attributes (status of capital, resources, location and past growth)')),
             fluidRow(
               column(6,
                      selectInput("year_sim2", label = "Simulated Year",
                                  choices = c("Pop2010","Pop2002","Pop1989", "Pop1979", "Pop1970", "Pop1959"),
                                  selected = "Pop1989")
                      )),
             plotOutput("graph3"),
             verbatimTextOutput("modelcombi3")
    ),
    "-----",
    tabPanel("About", h2("About"),
             fluidRow(
               column(4,
                  img(src = "mariusLogo.png", class="img-responsive")
                ),
               column(8,
                      h3("Who ?"),
                      tags$li( strong("DARIUS"),": Clémentine Cottineau"),
                      tags$li( strong("MARIUS"),": Paul Chapron, Guillaume Chérel, Clémentine Cottineau, Denise Pumain, Romain Reuillon"),
                      tags$li(strong("VARIUS"),": Clémentine Cottineau, Robin Cura, Romain Reuillon")
                      )
               ),
             hr(),   
             h3("Institutional Supports : "),
             fluidRow(
               column(2, img(src = "ERCLogo.jpg", class="img-responsive")),
               column(2, img(src = "geocitesLogo.jpg", class="img-responsive")),
               column(2, img(src = "iscpifLogo.png", class="img-responsive")),
               column(2, img(src = "CNRSLogo.jpg", class="img-responsive")),
               column(2,  img(src = "paris1Logo.jpg", class="img-responsive"))
               ),
             hr(),
             h3("About the MARIUS model : "),
             "Clémentine Cottineau, Paul Chapron, Romain Reuillon, 2014, An incremental method for building and evaluating agent-based models of systems of cities. ",
             a("https://hal.inria.fr/OPENAIRE/halshs-01093426v1",
               href="https://hal.inria.fr/OPENAIRE/halshs-01093426v1"),
             hr(),
             h3("Model code : "),
             a("https://github.com/ISCPIF/marius-method",
               href="https://github.com/ISCPIF/marius-method"), 
             hr(),
             h3("About DARIUS data : "),
             "Clémentine Cottineau, 2014, DARIUS Database,", em("figshare"), 
             a("http://dx.doi.org/10.6084/m9.figshare.1108081",
               href = "http://dx.doi.org/10.6084/m9.figshare.1108081"),
             br(),
             "Clémentine Cottineau, 2015, Urban DARIUS Shape Files, ", em("figshare"), 
             a("http://dx.doi.org/10.6084/m9.figshare.1348297",
               href= "http://dx.doi.org/10.6084/m9.figshare.1348297"),
             br(),
             "Clémentine Cottineau, 2015, Regional DARIUS Shape Files, ", em("figshare"), 
             a("http://dx.doi.org/10.6084/m9.figshare.1348298",
               href="http://dx.doi.org/10.6084/m9.figshare.1348298"))
  )
))
