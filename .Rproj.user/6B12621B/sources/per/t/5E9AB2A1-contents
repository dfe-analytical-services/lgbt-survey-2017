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
           wellPanel(
             h5(strong("Choose question"),br(),"Choose what question you would like to look at, this will appear as the rows in the table"),
             uiOutput("chooseQuestion"),
             h5(strong("Choose Demographic"),br(),"Choose what demographic of respondents you would like to look at, this will appear as the columns in the table"),
             uiOutput("chooseDemographic"),
             br(),br(),
             h5(strong("Filter Respondents (1)"),br(),"Here you can choose to subset the respondents in the table."),
             uiOutput("chooseFilter1"),
             uiOutput("chooseFilter1Option"),
             h5(strong("Filter Respondents (2)"),br(),"Here you can choose a further subset of the respondents in the table."),
             uiOutput("chooseFilter2"),
             uiOutput("chooseFilter2Option"),
              
             # uiOutput("chooseFilter1"),
             # uiOutput("chooseFilter1Option"),
             
             # uiOutput("chooseFilter2"),
             # uiOutput("chooseFilter2Option"),
             br(),
             h5(strong("Download Table"),br(),"Here you can download the table you have created. It will download as a .csv file"),
             downloadButton('downloadData', 'Download .csv of Results'))),
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
)
))




