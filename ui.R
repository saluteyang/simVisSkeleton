library(shiny)
library(shinydashboard)
library(shinythemes)
library(plotly)
# library(shinyFiles)

shinyUI(navbarPage(theme = shinytheme("cerulean"),
                   "Risk Scenario Simulation",
                   tabPanel("Ad Hoc Forward Curve Simulations",
                            sidebarPanel(
                              helpText("Select from main portfolio curves and simulation parameters;
                                       Note that number of simulations below 1000 has noticeable non-convergence to forwards."),
                              sliderInput("numsimslider", label = h4("Number of Sims"),
                                          min = 200, max = 2000, step = 200, value = 500),
                              dateRangeInput("curvedaterange", label = h4("Historical Range of Forward Curve Dates"),
                                             start = '2017-07-01', end = '2017-10-31'),
                              helpText("Make sure the ending curve date selected is not a weekend or holiday. 
                                       This is used to calculate correlation matrix."),
                              dateRangeInput("simrangemonth", label = h4("Range of Forward Months to Simulate"),
                                             start = '2018-01-01', end = '2018-12-01'),
                              selectInput("mktcomp", "Market-Component", c('ERCOT-ZONE N', 'PJM-WESTRT'),
                                          selected = 'ERCOT-ZONE N'),
                              checkboxInput("visfwd", "include plotting of forward price history", value = TRUE),
                              conditionalPanel(
                                condition = "input.visfwd == true",
                                selectInput("window", label = "Select backward looking window length for correlation heatmap",
                                            choices = c(10, 20, 30, 60, 90), selected = 30),
                                helpText("After clicking Go, click on the forward chart to select the end date of the window.
                                         Make sure the lookback window doesn't fall out of the curve date range specified.
                                         Note that correlation for simulation will always use the full range of curve dates."),
                                selectInput("pwrseg", label = "Select on or off peak for power forward price visualization",
                                            choices = c('On Peak', 'Off Peak'), selected = 'On Peak')
                              ),
                              checkboxInput("tblout", "include table with select percentiles of simulated prices", value = FALSE),
                              checkboxInput("aggreg", "include custom range aggregation", value = FALSE),
                              conditionalPanel(
                                condition = "input.aggreg == true",
                                dateRangeInput("aggrangemonth", label = h4("Select strip of aggregation (optional)"),
                                               start = '2018-07-01', end = '2018-08-01'),
                                helpText("Make sure the aggregation period is included in the simulation period
                                         selected above")
                              ),
                              checkboxInput("spreadopt", "include valuation of dummy spread option", value = FALSE),
                              conditionalPanel(
                                condition = "input.spreadopt == true",
                                sliderInput("hr", label = h4("Heat rate for spread option"),
                                            min = 4, max = 15, step = 0.5, value = 10),
                                sliderInput("vom", label = h4("VOM for spread option"),
                                            min = 0, max = 10, step = 0.5, value = 2)
                              ),

                              actionButton("goButton1", "Start"),
                              downloadButton('downloadPct', 'Download Percentiles'),
                              downloadButton('downloadSim', 'Download All Simulations'),
                              downloadButton('downloadAgg', 'Download Strip Simulations'),
                              downloadButton('downloadSprd', 'Download Spread Option Simulations')
                              ),
                            mainPanel(
                              tabsetPanel(
                                tabPanel("plots of forward curves",
                                         fluidRow(
                                           column(6, plotlyOutput("p1")),
                                           column(6, plotlyOutput("p2")),
                                           column(12, plotlyOutput("correlation"))
                                         )
                                         ),
                                tabPanel("plots of simulated curves",
                                         plotlyOutput("pricePlot"),
                                         plotlyOutput("distPlot1"),
                                         plotlyOutput("distPlot2"),
                                         plotlyOutput("distPlot3")),
                                tabPanel("table with select percentiles", dataTableOutput("pctileTbl")),
                                tabPanel("distribution of prices aggregated by strip",
                                         plotOutput("aggDistPlot"),
                                         dataTableOutput("aggregTbl")),
                                tabPanel("distibution of custom spread option",
                                         dataTableOutput('spreadTbl'),
                                         plotOutput('spreadPeriodPlot'))
                              )
                            )
                    )
                   )
                  )