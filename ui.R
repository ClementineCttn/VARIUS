library(shiny)

shinyUI(fluidPage(
  tags$head(tags$link(rel="shortcut icon", href="favicon.png")),
  titlePanel("VARIUS"),
  titlePanel(h4("Visualising empirical and simulated Agglomerations of (Imperial) Russia and the Soviet Union")),
  
  navlistPanel(
    "The MARIUS Project",
    tabPanel("Presentation", h2("The MARIUS Project"),
             img(src = "mariusLogo.png", height = 80, width = 100),
             "MARIUS is an incremental agent-based model of systems of cities. It 
             has been built to reproduce the trajectories of cities in the Former Soviet Union.
             The rules of the model are designed to range from generic interurban
             interactions to specific interactions between Soviet cities and their
             geographical environment. The structure of the model is modular enough
             to allow various sets of mechanisms playing together or alternatively.
             Our aim is to evaluate the degree of genericity of Soviet urban
             trajectories by growing articial systems of cities, from the most
             generic ones to the most particular. The fitness of the model is evaluated
             against urban data, gathered in the DARIUS Database. ",
             br(),
             br(),
            
             br()
    #img(src = "incremental-modeling-EN.png", height = 500, width = 500)
    ),
  
    "What Happened ?",
    tabPanel( "Census Data", 
              h3("Observed distribution of city sizes with DARIUS Database, 1897-2010"),
              inputPanel( 
             selectInput("Census_year", label = "Census year",
                         choices = c("Pop2010", "Pop2002","Pop1989", "Pop1979", "Pop1970", "Pop1959",
                                     "Pop1939", "Pop1926","Pop1897"), selected = "Pop1959"),
             sliderInput("sizefactor1", label = "Population size factor",
                         min = 0.01, max = 0.3, value = 0.1, step = 0.01)
              ),
    plotOutput("map1"),
    h3("Inter-census urban growth"),
    inputPanel( 
      selectInput("Census_year1", label = "First Census year",
                                    choices = c("Pop2002","Pop1989", "Pop1979", "Pop1970", "Pop1959", "Pop1939", "Pop1926",
                                                "Pop1897"), selected = "Pop2002"),
      selectInput("Census_year2", label = "Second Census year",
                  choices = c("Pop2010", "Pop2002","Pop1989", "Pop1979", "Pop1970","Pop1959", "Pop1939", 
                              "Pop1926"), selected = "Pop2010")
    ),
    plotOutput("map1bis"),
    h3("Urban Statuses & Attributes"),
    inputPanel(
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
             inputPanel(
               sliderInput("pvalue", label = "Statistical Significance",
                           min = 0, max = 10, value = 1, step = 0.1)
             ),
            "Models with different combinaison of mechaisms have been calibrated intensively against empirical data,
using generic algorithms for as many generation as is defined by the slider 'number of generations'. 
                This plot shows the result of a regression of their closeness to data. Each bar represent the
                value of the estimated coefficient* for each activatesd mechanism, in comparison with the same 
                model structure without this mechanism, everything else being equal.",
             plotOutput("graph1"),
             "* Significance is given with a probability of error of the chosen percentage."
             ),
    
    tabPanel("Choose Mechanisms' combination", 
             h3("Optimal model after multi-calibration"),
             inputPanel(
               sliderInput("ntrait", label = "Number of mechanisms",
                           min = 0, max = 5, value = 4, step = 1),
               selectInput("period", label = "Period",
                           choices = c("1959-1989", "1989-2010"), selected = "1959-1989")
               
             ),
             "This gives a profile of the best performing model, given the number of mechanisms actived at the chosen period.",
             h3("Evalutation Criteria:"),
             dataTableOutput("print1"),
             h3("Mechanisms' combination:"),
             dataTableOutput("print1bis"),
             h3("Parameter values:"),
             dataTableOutput("print1ter")
    ),
    "Simulate it",
    tabPanel("Run a MARIUS model",
             h3("Model structures and parameterization"),
             inputPanel(
               selectInput("choice_run", label = "Model to run",
                           choices = c("Best [Calibrated] Model", "Run [Customised] Model"), 
                           selected = "Best [Calibrated] Model"),
               selectInput("period2", label = "Period",
                           choices = c("1959-1989", "1989-2010"), selected = "1959-1989"),
                checkboxGroupInput("mechanisms", 
                                  label = "Selected mechanisms", 
                                  choices = list("Bonus", "Fixed Costs", "Resources", "Redistribution", "Urban Transition"),
                                  selected = c("Bonus", "Fixed Costs"))),
             "For customized models, choose parameter values",
             inputPanel("Generic parameters",
             
               sliderInput("populationToWealthExponent", label = "populationToWealthExponent",
                           min = 1, max = 5, value = 1.1, step = 0.0001),
               sliderInput("economicMultiplier", label = "economicMultiplier",
                           min = 0, max = 1, value = 0.5, step = 0.001),
               sliderInput("sizeEffectOnSupply", label = "sizeEffectOnSupply",
                           min = 1, max = 2, value = 1.01, step = 0.0001),
               sliderInput("sizeEffectOnDemand", label = "sizeEffectOnDemand",
                           min = 1, max = 2, value = 1.01, step = 0.0001),
               sliderInput("distanceDecay", label = "distanceDecay",
                           min = 0, max = 10, value = 0.5, step = 0.001),
             sliderInput("wealthToPopulationExponent", label = "wealthToPopulationExponent", 
                         min = 0, max = 2, value = 0.4, step = 0.0001)),
             "For models with the bonus mechanism, choose parameter value",
             inputPanel("Bonus parameter",
                        sliderInput("bonusMultiplier", label = "bonusMultiplier", 
                                    min = 0, max = 100, value = 50, step = 0.001)),
             "For models with the fixed costs mechanism, choose parameter value",
             inputPanel("Fixed Costs Transaction parameter",
                        sliderInput("fixedCost", label = "fixedCost",
                                    min = 0, max = 100, value = 0.4, step = 0.01)),
             "For models with the resources mechanism, choose parameter values",
             inputPanel("SubSurface Resource parameters",
               sliderInput("oilAndGazEffect", label = "oilAndGazEffect", 
                           min = -1, max = 1, value = 0.3, step = 0.0001),
               sliderInput("coalEffect", label = "coalEffect", 
                           min = -1, max = 1, value = -0.05, step = 0.0001)
               ),
             "For models with the redistribution mechanism, choose parameter values",
             inputPanel("Double Redistribution parameters",
               sliderInput("territorialTaxes", label = "territorialTaxes", 
                           min = 0, max = 1, value = 0.4, step = 0.0001),
               sliderInput("capitalShareOfTaxes", label = "capitalShareOfTaxes", 
                           min = 0, max = 1, value = 0.2, step = 0.0001)
               ),
             "For models with the urban transition mechanism, choose parameter value",
             inputPanel("Urban transition parameter",
               sliderInput("ruralMultiplier", label = "ruralMultiplier", 
                           min = 0, max = 1, value = 0.02, step = 0.0001)),
             "To execute the model with your parameterization, press ",
             inputPanel(actionButton("go", "Run The Model", icon = NULL)
             ),
             verbatimTextOutput("cppst")),
   "How close are we ?",
    tabPanel("Macro Analysis",
             h3("Macro-geographic analysis"),
            # verbatimTextOutput("modelcombi"),
             "This rank-size representation is common to study the hierarchical structure of systems of cities 
             and their evolution towards equalisation or differenciation of city sizes.
             Blue dots indicate simulated cities over time, in comparison with empirical observations (in grey)",
             plotOutput("graph2"),
            verbatimTextOutput("modelcombi")
             ),
    tabPanel("Residual trajectories",
             h3("Residuals Trajectories"),
            # verbatimTextOutput("modelcombi"),
             "Residuals represent the difference between observed and simulated population for each city (in logs).
             Positive residuals mean that cities grew faster in reality than what we were able to simulate, 
             wheareas negative residuals indicate that we overestimated the growth of such cities.
             Residuals help trigger how to improve the model.",
             inputPanel(
               selectInput("year_sim", label = "Simulated Year",
                           choices = c("Pop2010","Pop2002","Pop1989", "Pop1979", "Pop1970"), 
                           selected = "Pop1989"),
               sliderInput("cutoff", label = "Residual absolute cut-off",
                           min = 0, max = 5, value = 0.5, step = 0.05)
             ),
             plotOutput("map2"),
             h3("Most Positive Residual trajectories"),
            dataTableOutput("table_pos_res"),
             h3("Most Negative Residual trajectories"),
            dataTableOutput("table_neg_res"),
            verbatimTextOutput("modelcombi2")
             ),
    tabPanel("Profiles of Residual cities", h3("Profiles of Residual cities"),
             "Profiles of residual cities are obtained after a regression on the value of residual. We plot 
the coefficient values* of some available urban attributes (status of capital, resources, location and past growth)",
             inputPanel(
               selectInput("year_sim2", label = "Simulated Year",
                           choices = c("Pop2010","Pop2002","Pop1989", "Pop1979", "Pop1970", "Pop1959"),
                           selected = "Pop1989")
             ),
             plotOutput("graph3"),
             "* Significance is given with a probability of error of 1%.",
             verbatimTextOutput("modelcombi3")
             ),
    "-----",
    tabPanel("About", h2("About"),  img(src = "mariusLogo.png", height = 80, width = 100), 
             h3("Who ?"),
             br(),  strong("DARIUS"),": Clémentine Cottineau",
             br(),  strong("MARIUS"),": Paul Chapron, Guillaume Chérel, Clémentine Cottineau, Denise Pumain, Romain Reuillon",
             br(),  strong("VARIUS"),": Clémentine Cottineau, Robin Cura, Romain Reuillon",
             br(),   
             h3("Institutional Supports : "),
             br(),          
            img(src = "ERCLogo.jpg", height = 80, width = 100), 
            img(src = "geocitesLogo.jpg", height = 80, width = 75), 
            img(src = "iscpifLogo.png", height = 80, width = 110), 
            img(src = "CNRSLogo.jpg", height = 80, width = 85),
            img(src = "paris1Logo.jpg", height = 80, width = 120),
            br(),
            h3("About the MARIUS model : "),
            br(),
            "Clémentine Cottineau, Paul Chapron, Romain Reuillon, 2014, An incremental method for building and evaluating agent-based models of systems of cities. ",
            a("https://hal.inria.fr/OPENAIRE/halshs-01093426v1"),  br(),
            h3("Model code : "),
            br(),
            a("https://github.com/ISCPIF/marius-method"), 
            br(),
            h3("About DARIUS data : "),
            br(),
            "Clémentine Cottineau, 2014, DARIUS Database,", em("figshare"), 
            a("http://dx.doi.org/10.6084/m9.figshare.1108081"),
            br(),
             "Clémentine Cottineau, 2015, Urban DARIUS Shape Files, ", em("figshare"), 
            a("http://dx.doi.org/10.6084/m9.figshare.1348297"
              ), br(),
          "Clémentine Cottineau, 2015, Regional DARIUS Shape Files, ", em("figshare"), 
           a("http://dx.doi.org/10.6084/m9.figshare.1348298"))
  )
))
