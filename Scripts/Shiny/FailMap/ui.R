library(shiny)
library(shinythemes)
library(leaflet)

# Define UI for application that draws a histogram
shinyUI(navbarPage("Hydraulic Bridge Failures", id="nav",
                   theme = shinytheme("flatly"),
                   tabPanel("Interactive map",
                            div(class="outer"),
                            sidebarLayout(position = "right",
                                          sidebarPanel(h2("Bridge explorer"),
                                                       helpText("Failed bridges plot as colored circles scaled to drainage
                                                  area. Linked gauges plot as black circles, also scaled to
                                                  drainage area."),
                                                       checkboxGroupInput("plotTypes",
                                                                          label = "Plot",
                                                                          choices = list("BRIDGES" = "BRIDGES",
                                                                                         "GAUGES" = "GAUGES"),
                                                                          selected = c("BRIDGES","GAUGES")),
                                                       # conditionalPanel("input.plotTypes == ")
                                                       selectInput("Popups",
                                                                   label = "Show data popup for",
                                                                   choices = list("BRIDGE" = "BRIDGE",
                                                                                  "GAUGE"   = "GAUGE",
                                                                                  "NONE"    = "NONE"),
                                                                   selected = "BRIDGES"),
                                                       sliderInput("drainArea",
                                                                   "Drainage Area (square km):",
                                                                   min = 0,
                                                                   max = 8500,
                                                                   value = c(10,8200),
                                                                   round = TRUE,
                                                                   step = 500),
                                                       
                                                       checkboxGroupInput("FailCause",
                                                                          label = "Failure causes",
                                                                          choices = list("FLOOD" = "FLOOD",
                                                                                         "SCOUR" = "SCOUR",
                                                                                         "HURRICANE" = "HURRICANE",
                                                                                         "OTHER" = "OTHER"),
                                                                          selected = c("FLOOD","SCOUR","HURRICANE","OTHER")),
                                                       plotOutput("histTfail", height = 200)
                                                            
                                                          ),
                             mainPanel(
                              leafletOutput("map"),
                              tags$div(id="cite",
                                       'Data compiled for ', tags$em('Historical Analysis of Hydraulic Bridge Failures in the Continental United States'), ' by Madeleine Flint (2016).'
                              ),
                              em("Disclaimer: there are approximately 504,000 bridges over water in the U.S.,
                               and bridge failures are fortunately very rare. The 35 totally or partially
                               collapsed structures shown on the map
                                are not geographically representative of the frequency of failure across the U.S.
                                They were obtained from a database that has more data for some states than for others,
                                and were selected because they had a stream gauge on or near the bridge at the
                                time of failure. Due to various factors, gauge data was more likely to be
                               available in the New England and Mid-Atlantic
                                regions."),
                              br(),
                              br(),
                              p(a("R code and source data available in the Virginia Tech git repository.",     
                                  href="https://git.it.vt.edu/mflint/Flint.et.al.2016.Historical.Hydraulic.Bridge.Failures", target = "_blank")
                               )
                             )
                             )
                            
#                             absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
#                                           draggable = TRUE, top = 60, left = "auto", right = 20, bottom = "auto",
#                                           width = 250, height = "auto",
                                          
                                  
                            
                   ),
                   tabPanel("Data explorer",
                            fluidRow(
                              column(3,
                                     selectInput("data", "Data type", list(BRIDGE = "Failed Bridge",
                                                                           NBI = "Linked National Bridge Inventory", 
                                                                           USGS = "Linked USGS Gauge", 
                                                                           ANALYSIS = "Flow and return period analysis"),
                                                 selected = "Failed Bridge")
                              )#,
#                               column(3,
#                                      conditionalPanel("input.states",
#                                                       selectInput("cities", "Cities", c("All cities"=""), multiple=TRUE)
#                                      )
#                               ),
#                               column(3,
#                                      conditionalPanel("input.states",
#                                                       selectInput("zipcodes", "Zipcodes", c("All zipcodes"=""), multiple=TRUE)
#                                      )
#                               )
                            ),
#                             fluidRow(
#                               column(1,
#                                      numericInput("minScore", "Min score", min=0, max=100, value=0)
#                               ),
#                               column(1,
#                                      numericInput("maxScore", "Max score", min=0, max=100, value=100)
#                               )
#                             ),
                            hr(),
                            dataTableOutput("table")
                            ),

        #fluidPage(theme = shinytheme("flatly"),
  
  # Application title
#   titlePanel("Hydraulic Bridge Failures in the Continental U.S."),
#   
#   # Sidebar with a slider input for the number of bins
#   sidebarLayout(position = "right",
#                 sidebarPanel(
#                   helpText("Map historical hydraulic bridge failures with control over:
#                            range of drainage area and failure causes shown; whether
#                            popups show data related to failed bridge or linked gauge
#                            (or neither). Note: you may need to click multiple times
#                            to show popup."),
#                   sliderInput("drainArea",
#                               "Drainage Area (km2):",
#                               min = 0,
#                               max = 8500,
#                               value = c(10,8200),
#                               round = TRUE,
#                               step = 500),
#                   
#                   checkboxGroupInput("FailCause",
#                                      label = "Failure causes",
#                                      choices = list("FLOOD" = "FLOOD",
#                                                     "SCOUR" = "SCOUR",
#                                                     "HURRICANE" = "HURRICANE",
#                                                     "OTHER" = "OTHER"),
#                                      selected = c("FLOOD","SCOUR","HURRICANE","OTHER")),
#                   selectInput("Popups",
#                                 label = "Show popup for",
#                                 choices = list("BRIDGE" = "BRIDGE",
#                                                "GAUGE"   = "GAUGE",
#                                                "NONE"    = "NONE"),
#                                 selected = "BRIDGES")
#                   ),
#                 
#                 # Show map
#                 mainPanel(
#                   p("Visualization created by Madeleine Flint, Virginia Tech, 2016."),
                  # plotOutput("distPlot"),
                  # leafletOutput("map") ,
#                   em("Disclaimer: there are approximately 504,000 bridges over water in the U.S.,
#         and bridge failures are fortunately very rare. The 35 totally or partially
#        collapsed structures shown on the map
#         are not geographically representative of the frequency of failure across the U.S.
#         They were obtained from a database that has more data for some states than for others,
#         and were selected because they had a stream gauge on or near the bridge at the
#         time of failure. Due to various factors, gauge data was more likely to be
#        available in the New England and Mid-Atlantic
#         regions."),
#                   br(),
#                   br(),
#                   p(a("R code and source data available in the Virginia Tech git repository.",     href="https://git.it.vt.edu/mflint/Flint.et.al.2016.Historical.Hydraulic.Bridge.Failures", target = "_blank")
#                   )
#                 )

conditionalPanel("false", icon("crosshair"))
))