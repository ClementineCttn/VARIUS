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
              'MARIUS is an incremental family of agent-based models of systems of cities. It 
             was built to simulate the demographic trajectories of cities in the Former Soviet Union.',
              br(),  br(), '
             The rules of the models are designed to range from generic interurban
             interactions to specific mechanisms characterizing Soviet cities and their
             geographical environment. The structure of the model family is modular enough
             to allow various sets of mechanisms playing together or alternatively.',
             br(), 
             column(8, img(src = "MARIUS_Approach.png",class="img-responsive")),
              'The aim is to evaluate the degree of genericity of Soviet urban
             trajectories by growing articial systems of cities, from the most
             generic ones to the most particular ones.',
            br(),  br(),'The fitness of the model is evaluated
             against historical urban data, gathered within DARIUS Database.')
             #img(src = "incremental-modeling-EN.png", height = 500, width = 500)
    ),
    "What Happened ?",
    tabPanel( "Census Data", 
              h3("Observed distribution of city sizes with DARIUS Database, 1897-2010"),
                fluidRow(
                  column(6,
                         selectInput("Census_year",
                                     label = "Census year",
                                     choices = c("2010" = "Pop2010", "2002"="Pop2002",
                                                "1989"="Pop1989", "1979"="Pop1979", 
                                                "1970"="Pop1970", "1959"="Pop1959",
                                                 "1939"="Pop1939", "1926"= "Pop1926",
                                                "1897"="Pop1897"),
                                     selected = "Pop1959")),
                  column(6,
                         sliderInput("sizefactor1",
                                     label = "Population size factor",
                                     min = 0.01, max = 0.3,
                                     value = 0.1, step = 0.01))
                ),
              plotOutput("map1"),
              h3("Inter-census urban growth"),
              "Urban growth corresponds to the average annual growth rate between the two selected dates.",
              fluidRow(
                column(6,
                selectInput("Census_year1", label = "First Census year",
                            choices = c("2002"="Pop2002",
                                        "1989"="Pop1989", "1979"="Pop1979", 
                                        "1970"="Pop1970", "1959"="Pop1959",
                                        "1939"="Pop1939", "1926"= "Pop1926",
                                        "1897"="Pop1897"),
                            selected = "Pop2002")),
                column(6,
                       selectInput("Census_year2", label = "Second Census year",
                            choices = c("2010" = "Pop2010", "2002"="Pop2002",
                                        "1989"="Pop1989", "1979"="Pop1979", 
                                        "1970"="Pop1970", "1959"="Pop1959",
                                        "1939"="Pop1939", "1926"= "Pop1926"),
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
    tabPanel("Selected mechanisms",

               tags$p(class="text-justify",
                      'MARIUS is built as a modular family of models. Each model contains the 
                      cornerstone assumptions about city growth (the cornerstone model), as 
                      well as a combination of supplementary mechanisms corresponding to 
                      different hypotheses regarding the explanation of the diversity of urban
                       trajectories in the modelled system of cities.'),
               h3("The Cornerstone model"),
               column(12, img(src = "mech_0.png",class="img-responsive")),
               tags$p(class="text-justify",
'Generic mechanisms included in the cornerstone model consist in scaling laws relating
city population and city wealth, supply and demand, as well as a gravity model of
interurban interactions. Cities are supposed to generate larger economic output per capita
as they grow in size, and to interact more intensely with the cities that are larger
and closer to them.'),
               h3("Bonified Interurban Exchanges"),
               column(12, img(src = "mech_bonus.png",class="img-responsive")),
               tags$p(class="text-justify",
                      'To account for spillover effects in interurban exchanges,
                      we model an interaction bonus for cities exchanging with many
                      partners and large flows.'),
               h3("Costly Interactions"),
               column(12, img(src = "mech_cost.png",class="img-responsive")),
               tags$p(class="text-justify",
                      'Transaction costs and entry costs are supposed to reduce the pool of 
                      potential partners to those with which cities can expect
                      profitable transactions. This mechanism aims at eliminating
                      interactions between very distant and very small cities.'),
               h3("Environmental resources"),
               column(12, img(src = "mech_resource.png",class="img-responsive")),
               tags$p(class="text-justify",
                      'Because of their location in an anysotropic environment,
                      cities benefit from uneven opportunities of local resource extraction 
                      (for example : coal mining or oil extraction). The meaning of resource
                       here can also include negative resources affecting city growth 
                      (polluted sites, etc.).'),
               h3("Redistributive regions"),
               column(12, img(src = "mech_redistribution.png",class="img-responsive")),
               tags$p(class="text-justify",
                      'Regions as political and administrative territories can involve redistribution 
                      of wealth among cities. This mechanism consists in a mutualisation of wealth 
                      at the regional and national levels, a possible capture of some of this resource by the capital city,
                      and the redistribution of the rest to every city according to its needs, measured by total population.'),
               h3("Time-lags in regional transitions"),
               column(12, img(src = "mech_transition.png",class="img-responsive")),
               tags$p(class="text-justify",
                      'As the different parts of the territory differ in their position 
                      in the urban transition, this mechanism takes into account the time lag of regions
                       in terms of potential rural migration to cities.'))
             ,
    tabPanel("Model Structures Analysis",
             h3("Contribution of mechanisms to the quality of simulation (closeness to data)"),
             fluidRow(
               column(6,
                      sliderInput("pvalue", label = "Statistical Significance (% of error)",
                           min = 0, max = 10, value = 1, step = 0.1))
             ),
             tags$p(class="text-justify", HTML('Models with different combination of mechanisms have been calibrated
              intensively against empirical data, using generic algorithms for more than 100000 generations. 
              This plot shows the results of a regression explaining one measure of the quality of 
models (a small difference between simulated and empirical urban trajectories) by their mechanisms composition 
(the fact that any of the supplementary mechanisms is activated or not"
              Each bar represents the value of the estimated 
              coefficient for each activated mechanism, in comparison with the
              same model structure without this mechanism, everything else being equal.')),
             plotOutput("graph1"),
             h3("Contribution of mechanisms' interactions to the quality of simulation (closeness to data)"),
             "This graph allows to explore the interaction of two mechanisms in reducing the distance
              between empirical and simulated trajectories of cities in the Former Soviet Union.",
             fluidRow(
               column(6,
                      selectInput("mechanismInteractions", 
                                         label = "Mechanism 1", 
                                         choices = list("Bonus", "Fixed Costs", "Resources",
                                                        "Redistribution", "Urban Transition"),
                                         selected = "Bonus")),
               column(6,
                      selectInput("mechanismsInteractions", 
                                         label = "Mechanism 2", 
                                         choices = list("Bonus", "Fixed Costs", "Resources",
                                                        "Redistribution", "Urban Transition"),
                                         selected = c("Fixed Costs")))
             ),
              plotOutput("graphinteraction")
    ),
    
    tabPanel("Choose Mechanisms' combination", 
             h3("Optimal model after multi-calibration"),
             tags$p("Given the calibration of all model structures against the same data and
                     evaluation criteria, these tables describe the best calibrated model 
                    for a given period and a selected level of parsimony (the number of mechanisms 
                    included in the model structure)."),
             fluidRow(
               column(6,
                      sliderInput("ntrait", label = "Number of mechanisms",
                           min = 0, max = 5, value = 1, step = 1)),
               column(6,
                      selectInput("period", label = "Period",
                           choices = c("1959-1989", "1989-2010"), selected = "1959-1989"))
               
             ),
              h3("Mechanisms' combination:"),
             "This is the best set of mechanisms for the given period to minimize the distance 
             between simulated and observed urban trajectories in the Former Soviet Union.",
             dataTableOutput("print1bis"),
             h3("Parameter values:"),
             "Given the structure of the model, these are the values of parameter that perform best 
             to reproduce the empirical urban evolution.",             
             dataTableOutput("print1ter"),
             h3("Evaluation Criteria:"),
             "These are the three measures used to evaluate the quality of the model (to be minimized).",
             dataTableOutput("print1")
             
    ),
    "Simulate it",
    tabPanel("Run a MARIUS model",
             "To execute a model, either select one of the best calibrated models for a given period of time and 
             mechanisms combination, or customize the parameter values yourself [model to run | customised model].",
             wellPanel(
             h3("Model structures and parameterization"),
             fluidRow(
               column(4,
               selectInput("choice_run", label = "Model to run",
                           choices = c("Best Calibrated Model", "Customised Model"), 
                           selected = "Best Calibrated Model")),
               column(4,
               selectInput("runingperiod", label = "Period",
                           choices = c("1959-1989", "1989-2010"), selected = "1959-1989")),
               column(4,
                      checkboxGroupInput("mechanisms", 
                                  label = "Selected mechanisms", 
                                  choices = list("Bonus", "Fixed Costs", "Resources",
                                                 "Redistribution", "Urban Transition"),
                                  selected = c("Bonus", "Fixed Costs"))))),
             # Custom Model
             conditionalPanel(condition = "input.choice_run == 'Customised Model'",
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
            "And wait a few seconds until the message 'Done' appears.",
            conditionalPanel(condition = 'input.go >= 1',
                 verbatimTextOutput("cppst"))
            
    ),
            
    "How close are we ?", 
     tabPanel("Macro Analysis",
             h3("Macro-geographic analysis"),
             tags$p(class="text-justify",
                    "This rank-size representation is common to study the hierarchical
                    structure of systems of cities and their evolution towards equalisation
                    or differenciation of city sizes. Blue dots indicate simulated cities
                    over time, in comparison with empirical observations (in grey)."),
             plotOutput("graph2"),
             tags$p(class="text-justify",
                    "In this graph represent the value of each city's population observed (x axis)
                    and simulated (y axis) at the last date of the simulation. A perfect model would
                    exhibit a straight line and a slope equal to 1."),
             plotOutput("graphObsSim"),
             verbatimTextOutput("modelcombi")
    ),
    tabPanel("Residual trajectories",
             h3("Residuals Trajectories"),
             tags$p(class="text-justify",
                    "Residuals represent the difference between observed and simulated population for each city (in logs).
             Positive residuals mean that cities grew faster in reality than what we were able to simulate, 
             whereas negative residuals indicate that we over-estimated the growth of such cities.
             Residuals help trigger how and where model needs to be improved."),
             fluidRow(
               column(6,
                      sliderInput("cutoff", label = "Residual absolute cut-off",
                                  min = 0, max = 5, value = 0.5, step = 0.05)),
               conditionalPanel(condition = "input.period2 == '1959-1989'",
                                
                                column(6,
                                       selectInput("year_sima", label = "Simulated Year",
                                                   choices = c("1989"="Pop1989", "1979"="Pop1979", 
                                                               "1970"="Pop1970", "1959"="Pop1959"), 
                                                   selected = "Pop1989")    
                                )),
               conditionalPanel(condition = "input.period2 == '1989-2010'",
                                
                                column(6,
                                       selectInput("year_simb", label = "Simulated Year",
                                                   choices = c("2010" = "Pop2010", "2002"="Pop2002",
                                                               "1989"="Pop1989"), 
                                                   selected = "Pop2010")    
                                ))
               
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
                  on the value of residual population. We plot the
                  <span title="Significance is given with a probability of error of 1%."
                  class="mark">coefficient values</span>
                  of some available urban attributes (status of capital, resources, location and past growth). 
                  This regression helps profiling the type of cities most over- and under-estimated by the model, that 
                                              is the topical areas where the model needs to be improved.')),
             fluidRow(
               conditionalPanel(condition = "input.period2 == '1959-1989'",
                     column(6,
                     selectInput("year_sim2a", label = "Simulated Year",
                     choices = c("1989"="Pop1989", "1979"="Pop1979", 
                                 "1970"="Pop1970", "1959"="Pop1959"),
                     selected = "Pop1989"))),                     
               conditionalPanel(condition = "input.period2 == '1989-2010'",
                                column(6,
                                       selectInput("year_sim2b", label = "Simulated Year",
                                                   choices = c("2010" = "Pop2010", "2002"="Pop2002",
                                                               "1989"="Pop1989"),
                                                   selected = "Pop2010")))
             ),
             plotOutput("graph3"),
             h3("Contribution of attributes' interactions to the variation of residuals"),
             fluidRow(
               column(6,
                      selectInput("Attribute1", 
                                  label = "Attribute 1", 
                                  choices = list("Location", "Status", "Oil&Gas",
                                                 "Coal", "Specialisation", "Size"),
                                  selected = "Status")),
               column(6,
                      selectInput("Attribute2", 
                                  label = "Attribute 2", 
                                  choices = list("Location", "Status", "Oil&Gas",
                                                 "Coal", "Specialisation", "Size"),
                                  selected = c("Size")))
             ),
             plotOutput("graph3interaction"),
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
