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


shinyUI(fluidPage(theme="shiny.css",
 

  # tags$style(type="text/css",
  #            ".shiny-output-error { visibility: hidden; }",
  #            ".shiny-output-error:before { visibility: hidden; }"
  # ),
  
  navbarPage("NationalLGBTSurvey2017 alpha build",theme = "shiny.css", inverse = FALSE,
             tabPanel("Home",
                      wellPanel(
HTML(
"<h3>Home Page</h3>
<br>
<h4>Welcome to the National LGBT Survey 2017 Data Viewer.</h4>
<p> The Data Viewer allows you to find information about the different experiences of LGBT people in the UK who responded to the survey in 2017. You can look at the results on the themes of safety, education, health, and employment for different LGBT groups.,
</p>
<br>
<h4>To know before your start </h4>
<p> The survey received 108,100 responses from people who self-identified as having a minority sexual orientation or gender identity, or self-identified as intersex; were 16 or above; and lived in the United Kingdom. As such, the dataset represents a self-selected sample and is not representative of all LGBT people in the UK.,
</p>
<p> Some data won't appear because small cell sizes have been suppressed to prevent disclosure (marked 'x' in tables), or there was no respondents (marked '-' in tables)., 
</p>
<p> Charts and base numbers have also been rounded to the nearest 10 to prevent disclosure, and some for some questions, respondents were able to tick as many answer categories as applied. As a result, some totals may not add up to 100%.,
</p>
<br>
<h4> How to use the Data Viewer </h4>
<p> The Data Viewer gives you access to over half a million tables from the survey dataset.,
</p>
<p> To generate tables, you will be asked to first to a theme, sub-theme and the relevant question you are interested in.,
</p>
<p> You will then need to choose whether you would like to look at responses from all survey respondents, cisgender respondents or trans respondents. Only responses provided by cisgender respondents or trans respondents can be disaggregated further.,
</p>
<p> If you choose the look at responses from cisgender respondents or trans respondents, you will then be able to choose between different filters and explore different sub-section of these groups.
</p>
"))),
             tabPanel("Terminology",
                      wellPanel(HTML(
                        
"
<h3> Terminology </h3>
<br>
<p> For the purposes of the Data Viewer, the following terms are being used: </p>
<ul>
<li> <b> 'Women' </b> : refers to all self-identified women and girls, where they indicated that they were assigned female at birth in the survey. </li>
<li> <b> 'Men' </b> : refers to all self-identified men and boys, where they indicated that they were assigned male at birth in the survey. </li>
<li> <b> 'Cisgender' </b> : umbrella term that includes all men and women as defined above. </li>
<li> <b> 'Trans women' </b> : refers to self-identified trans women and trans girls, or respondents who self-identified as 'woman/girl' but were assigned male at birth. </li>
<li> <b> 'Trans men' </b> : refers to self-identified trans men and trans boys, or respondents who self-identified as 'man/boy' but were assigned female at birth. </li>
<li> <b> 'Non-binary' </b> : umbrella term that includes all respondents who self-identified as non-binary, genderqueer, agender or gender fluid in the survey. </li>
<li> <b> 'Trans' </b> : umbrella term that includes all trans women, trans men and non-binary respondents as defined above. </li>
<li> <b> 'LGBT' </b> : umbrella term to describe people who self-identify as lesbian, gay, bisexual, transgender or as having any other minority sexual orientation or gender identity, or as intersex. </li></ul>"
                                ))),
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
             h5(strong("Choose theme"),br(),"In this panel you will choose which part of the survey you would like to view.",br(),"First, pick a chapter of the survey,
                most chapters are then split into sections followed by multiple questions."),
             br(),
             h5(strong("Theme")),
             uiOutput("chooseChapter"),
             h5(strong("Sub-theme")),
             uiOutput("chooseSection"),
             h5(strong("Choose Question")),
             uiOutput("chooseQuestion")),
           wellPanel(
             h5(strong("All / Trans / Cisgender respondents")),
             uiOutput("chooseFilter1Option"),
             h5(strong("Category"),br(),"Choose a category of respondents"),
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
               
               column(12,
                      
               conditionalPanel(  
                 condition = "input.do > 0",
                 uiOutput("NoData"),
                 uiOutput("Title")
                 ),
               br(),
             plotOutput("stackedplot"),
             br(),
             column(3,
                    conditionalPanel(  
                      condition = "input.do > 0",
             wellPanel(actionButton("GRAPH","Reset / Update Graph")))),
             br(),
             br(),
             conditionalPanel(  
               condition = "input.do > 0",
              DT::dataTableOutput("table")
               ),
             br(),
             conditionalPanel(
               condition = "input.do > 0",
              uiOutput("Notes")),
             br(),
             br())
             )

))), 
tabPanel("Question Index",
              wellPanel(uiOutput("glossary"))),
tabPanel("Help",
               wellPanel(HTML(
"<h3> Help Tab </h3>
<br>
<p> <b> Why can't I select other options if I select 'All respondents'? </b>
<br> Tables produced for all respondents cannot be disaggregated further. You need to select whether you would like to focus on cisgender respondents or trans respondents if you wish to disaggregate the data further.  
</p>
<p> <b> Why does this '-' appears in my table? </b>
<br> This sign means that no respondents have selected this response.
</p>
<p> <b> What does this 'x' appears in my table? </b>
<br> This sign means that the number has been suppressed due to containing 5 or fewer respondents. 
</p>
<p> <b> Why does my total not add up to 100%? </b>
<br> Some totals may not add up to 100% due to rounding or because for some questions, respondents were able to tick as many answer categories as applied.
</p>
<br>
<p> Can't find the help you are looking for? Please contact <b> lgbt.app@mailbox.gov.uk </b>
</p>")))
)
))




