#################################################################################################################################################
#
# LGBTSurvey2017 USER INTERFACE (See server.R file)
#
#
# VERSION:
#   1.0
#
# AUTHOR:
#   T. Dougall
#
# WRITTEN:
#   October 2018
#
#
# *UPDATES*
#
# VERSION:
#
# UPDATED BY:
#
# DATE:
#
# DETAILS:
#
#################################################################################################################################################

options(stringsAsFactors = FALSE)

require(shiny)
library(devtools)
library(shinyBS)
require(grDevices)
require(plyr)

require(data.table)
require(dplyr)




shinyUI(fluidPage(theme="bootstrap.css",


                  

  # tags$style(type="text/css",
  #            ".shiny-output-error { visibility: hidden; }",
  #            ".shiny-output-error:before { visibility: hidden; }"
  # ),
  
  navbarPage("LGBTSurvey2017 v1.0",theme = "bootstrap.css", inverse = FALSE,
          #   tabPanel("Intro",
           #           wellPanel("I'm gonna write loads of cool info to help users")),
             tabPanel("Analyse",
  titlePanel("LGBT Survey 2017"),
  
  fluidRow(
    column(3,
           bsCollapse(id = "Panels", open = list("Question","Demographic","Filter 1","Filter 2"), multiple = TRUE,
                      # bsCollapsePanel(title = "Question",
                      #                 uiOutput("chooseQuestion"),
                      #                 style = "primary"),
                      bsCollapsePanel(title = "Demographic",
                                      uiOutput("chooseDemographic"),
                                      style = "info")),
                      bsCollapsePanel(title = "Filter 1",
                                      uiOutput("chooseFilter1"),
                                      uiOutput("chooseFilter1Option"),
                                      style = "success"),
                      bsCollapsePanel(title = "Filter 2",
                                      uiOutput("chooseFilter2"),
                                      uiOutput("chooseFilter2Option"),
                                      style = "success"),
           downloadButton('downloadData', 'Download .csv of Results')),
      column(8,
             wellPanel(
             uiOutput("NoData"),
             uiOutput("Title")),
             plotOutput("stackedplot"),
             br(),
             wellPanel(
             uiOutput("table")),
             conditionalPanel(condition="!is.null(table)",
                              wellPanel(uiOutput("Notes")))

))), tabPanel("Help",
               wellPanel("I'm gonna write loads of cool info to help users"))
, tabPanel("Test",
           sidebarLayout(
             sidebarPanel(
               h4(strong("Local Authority (LA) level exclusions")),
               br(),
               uiOutput("chooseQuestion"),
               h5(strong("Instructions")),
               "From the dropdown menus below, please select the area and exclusion type of interest. Then use the chart and table to see how exclusion figures have changed over time for the coverage selected.",
               br(),
               "The rate or number radio buttons can be used to change between exclusion rates and number of exclusions respectively. A comparison to regional and national figures figures can also be seen by clicking the appropriate tab.", 
               hr(),
               h5(strong("1. Pick an area")),
               
               h5(strong("2. Pick an exclusion category")),
               
               hr(),
               
               br(),
               
               br(),
               
               br()
             ),
             mainPanel(tabsetPanel(
               tabPanel(
                 'Trend',
                 
                 br(),
                 
                 br(),
                 
                 br()),
               tabPanel(
                 'Comparison to region and national',
                 br(),
                 strong("State-funded primary, secondary and special schools"),
                 br(),
                 br(),
                 
                 br(),
                 
                 br(),
                 
                 br())))
           ),
           hr())  
)
))




