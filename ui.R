library(shiny)
library(shinythemes)
library(shinycssloaders)
library(spasm)
library(tmbstan)
library(shinydashboard)
library(FishLife)
library(taxize)
library(here)
library(shinyalert)

viable_fishlife_list <- readRDS(here("data","viable_fishlife_list.rds"))
  
function(input,output){
  
  
  header  <-  dashboardHeader(title = "simMPA")
  
  sidebar <- dashboardSidebar(
    sidebarMenu(
      id = "tabs",
      menuItem("Description", tabName = "description", icon = icon("book-reader")),
      menuItem("Inputs",tabName = "inputs", icon = icon("dashboard")),
      menuItem("Results",tabName = "results", icon = icon("chart-line"))
      
      
    )) # close dashboardSidebar
  
  body <-  dashboardBody(
    useShinyalert(),
    
    tabItems(
      # input tab
      tabItem(tabName = "description",
              h1("simMPA"),
              "simMPA is a web application for simulating the effects of Marine Protected Areas.",
              h2("What is it for?"),
              "simMPA is meant to help visualize the effects of MPAs on conservation and fishery outcomes under a wide range of biological and economic scenarios.
     Users interested using the underlying model to for example run and compare many different scenarios
        should use the underlying package `spasm` (github.com/danovndo/spasm). simMPA is not intended to design specific MPAs. 
              Local marine spatial planning exercises should carefully consider whether the limited assumptions of this model adequately represent their reality.",
              h2("How does it work?"),
              "simMPA is built on a single-species age structured bio-economic model (SPASM) that can be found at https://github.com/DanOvando/spasm. 
              It simulates a closed system (picture a population and fishery surrounding an island). You setup your fishery, and then simulate the effects of an MPA on that fishery. 
              The model performs one run without MPAs, and then another with MPAs holding everything else constant. You can then compare the results to see how things like
              biomass and catches changed as a result of the MPA",
              h2("How do I use it?"),
              
              "Click on the 'Inputs' tab to select life history, fleet, and MPA design options, then click 'Run MPA Simulation' to see the results."),
      tabItem(tabName = "inputs",
              actionButton("go","Run MPA Simulations"),
              br(),
              br(),
              box(title = "Life History",
                  solidHeader = TRUE,
                  collapsible = TRUE,
                  status = "primary",
                  "Select life history options",
                  br(),
                  selectizeInput("sciname",label = 'Common Name', choices = viable_fishlife_list, selected = "brownsnout spookfish"),
                  sliderInput("steepness", label = "Beverton-Holt Steepness",
                              0.6,0.99,.8,step = 0.01),
                  sliderInput("sigma_r", label = "Recruitment Variation (CV)",
                              0,0.3,0,step = 0.01),
                  sliderInput("rec_ac", label = "Recruitment Autocorrelation",
                              0,1,0,step = 0.01),
                  selectizeInput("dd_form","Timing of Density Dependence",
                                 choices = list(
                                   "Local density dependence: Density dependence occurs independently in each patch, and recruits then disperse to nearby patches" = 1,
                                   "Global density dependence: Density dependence is a function of the sum of spawning biomass across all patches, and recruits are then distributed according to habitat quality" = 2,
                                   "Post-dispersal density dependence: Larvae are distributed throughout the system, and then density dependence occurs based on the density of adult biomass at the destination patch" = 3
                                   ),
                                 selected = 1),
                  sliderInput("adult_movement", label = "Upper % patches moved by adults",
                              0,40,0, post = "%"),
                  sliderInput("dd_adult_movement", label = "Sensitivity of adult movement to density",
                              0,1,0,step = 0.01),
                  sliderInput("larval_movement", label = "Upper % patches moved by larvae",
                              0,100,0, post = "%")),
              box(title = "Fishing Fleet",
                  solidHeader = TRUE,
                  collapsible = TRUE,
                  status = "warning",
                  "Select fishery options",
                  numericInput("f_v_m", label = "Initial fishing mortality relative to natural mortality",
                              value = 1, min = 0, max = 10, step = 0.001),
                  sliderInput("sel", label = "% of mature length first selected by fishery",
                              min = 0, max = 4, value = 0.25, step = 0.01),
                  selectizeInput("fleet_model","Fleet Model:", 
                                 list("Constant Effort" = "constant-effort",
                                      "Constant Catch" = "constant-catch",
                                      "Open Access" = "open-access")),
                  conditionalPanel(
                    condition = "input.fleet_model == 'open-access'",
                    sliderInput("max_cr_ratio",
                                "Max. Cost to Revenue Ratio. Higher = Less Fishing",
                                min = 0,
                                max = 1,
                                value = 0.8,
                                step = 0.01),
                    sliderInput("price_slope",
                                "Annual percent change in price",
                                min = 0,
                                max = 100,
                                value = 0,
                                step = 0.01,
                                post = '%')),
                  selectizeInput("spatial_allocation","Spatial effort allocation strategy", 
                                 list("Spread evenly in open areas" = "simple",
                                      "Concentrated around fishable biomass" = "gravity",
                                      "Concentrated around profit per unit effort" = "profit-gravity")),
                  selectizeInput("mpa_reaction","Initial fleet reaction to MPA:", list("Leave the fishery" = "leave",
                                                                                       "Concentrate outside MPA" = "concentrate"))
                  ), # close fishing fleet box,
              box(title = "MPA Design",
                  solidHeader = TRUE,
                  collapsible = TRUE,
                  status = "info",
                  "Design your MPA",
                  sliderInput("mpa_size", label = "% patches inside MPA",
                              0,100,25, post = "%"),
                  sliderInput("year_mpa", label = "Years into fishery to start MPA",
                              0,49,25),
                  sliderInput("min_size","MPA Patchiness",0,1,.1, value = 0),
                  sliderInput("mpa_habfactor","MPA habitat multiplier",1,4,.1, value = 1),
                  checkboxInput("random_mpas", "Randomly Place MPAs?"),
                  checkboxInput("sprinkler", "Place MPA in larval source?")
              
                  )
              )# close mpa design box
      ,# close inputs,
      
    tabItem(
      tabName = "results",
    fluidRow(
      box(withSpinner(plotOutput("summary_plot")), title = "Percent change in metrics relative to values just before the MPA"),
      box(withSpinner(plotOutput("static_doughnut")), title = "Spatial distribution of biomass and effort at equilibrium"),
      box(withSpinner(plotOutput("raw_summary_plot")), title = "Absolute values of metrics over time")
    ) # close mainPanel
  ) # close plot tab
  ) # close tabItems
  ) # close dashboardBody
  
  
  dashboardPage(
    header,
    sidebar,
    body
    ) # close dashboard Padte

} # close server function 