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

shinyUI(fluidPage(theme="shiny.css",
 

  # tags$style(type="text/css",
  #            ".shiny-output-error { visibility: hidden; }",
  #            ".shiny-output-error:before { visibility: hidden; }"
  # ),
  
  navbarPage("NationalLGBTSurvey2017 alpha build",theme = "shiny.css", inverse = FALSE,
             tabPanel("Home",
                      wellPanel("Welcome to the LGBT Survey 2017 App. Contained here is over half a million tables to choose from.")),
             tabPanel("Terminology",
                      wellPanel("x - suppresed", 
                                br(),"'-' - 0 respondents")),
             tabPanel("Analyse",
  titlePanel("National LGBT Survey 2017"),
  
  fluidRow(
    column(12,
           conditionalPanel(  
             condition = "input.chooseFilter3Option == null",
             wellPanel(h5(strong("Please wait while data is loaded into the app, initial loading can take a few min."))
             ))
    ),
    column(3,
           wellPanel(
             h5(strong("Choose chapter"),br(),"In this panel you will choose which part of the survey you would like to view.",br(),"First, pick a chapter of the survey,
                most chapters are then split into sections followed by multiple questions."),
             br(),
             h5(strong("Chapter")),
             uiOutput("chooseChapter"),
             h5(strong("Section")),
             uiOutput("chooseSection"),
             h5(strong("Choose Question")),
             uiOutput("chooseQuestion")),
           wellPanel(
             h5(strong("All / Trans / Cisgender respondents")),
             uiOutput("chooseFilter1Option"),
             h5(strong("Demographic"),br(),"Choose respondents demographic"),
             uiOutput("chooseDemographic")),
           wellPanel(
             h5(strong("Subset the data"),br(),"You can now filter the respondents for who will appear in the table."),
             br(),
             h5(strong("Filter Respondents 1")),
             uiOutput("chooseFilter2"),
             uiOutput("chooseFilter2Option"),
             h5(strong("Filter Respondents 2")),
             uiOutput("chooseFilter3"),
             uiOutput("chooseFilter3Option"))),

      column(8,
             fluidRow(
               column(6,
                 conditionalPanel(  
                   condition = "input.chooseFilter3Option != null",
                   wellPanel(h5(strong("Generate Table"),br(),"Once you have chosen your table options, click to generate"),
                   actionButton("do", "Analyse"))
                 )
               ),
               column(6,
                      conditionalPanel(  
                        condition = "input.chooseFilter3Option != null",
                        wellPanel(h5(strong("Download Table"),br(),"Here you can download the table you have created. It will download as a .csv file"),
                   downloadButton('downloadData', 'Download .csv of Results'))
                 )
               )
             ),
             fluidRow(     
               conditionalPanel(  
                 condition = "input.do > 0",
                 wellPanel(uiOutput("NoData"),
                 uiOutput("Title")
                 )),
             plotOutput("stackedplot"),
             br(),
             conditionalPanel(  
               condition = "input.do > 0",
               wellPanel( uiOutput("table")
               )),
             conditionalPanel(
               condition = "input.do > 0",
               wellPanel(uiOutput("Notes"))))

))), tabPanel("Help",
               wellPanel("Please contact Tom for help"))
)
))




