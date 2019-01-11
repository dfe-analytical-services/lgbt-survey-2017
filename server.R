#################################################################################################################################################
#
# PROGRAM LGBT_Survey
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
# DETAILS:
#
# REFERENCES:
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

options(bitmapType='cairo')
args(png)
getOption('bitmapType')

library(readr)
library(dplyr)
library(ggplot2)
library(stringr)

#data <- readRDS("C:/Users/tdougall/OneDrive - Department for Education/Documents/shiny/GEO/LGBTSurvey2017/lgbt-survey-2017 test/data/datatest")

data <- readRDS("data/Data")
#data <- data %>% mutate_at(vars(), function(x){gsub('[^ -~]', '', x)})
data$value <- as.character(data$value)


vlookup <- read.csv("data/vlookup.csv",na.strings = "",header = TRUE)

tables <- unique(data[,c("Chapter","Section","Question","Demographic","Filter1","Filter1Options","Filter2","Filter2Options","Filter3","Filter3Options")])

reduce <- function(x){
  print(x)
  #print(paste0(substr(x, start = 0, stop= which(strsplit(x, "")[[1]]==".")+1)," %"))
  paste0(substr(x, start = 0, stop= which(strsplit(x, "")[[1]]==".")+1)," %")
  }

# reduce <- function(x){
#   print(x)
#   #print(paste0(substr(x, start = 0, stop= which(strsplit(x, "")[[1]]==".")+1)," %"))
#   if(grepl(".",x)){
#     paste0(substr(x, start = 0, stop= which(strsplit(x, "")[[1]]==".")+1)," %")
#   }else if(grepl("x",x) || grepl("-",x)){
#     return(x)
#   } else{paste0(x," %",sep="")
#   }
# }
  
  



### Start of Shiny server
shinyServer(function(input, output, session) {
  
  output$fileLoaded <- reactive({
    return(!is.null(data))
  })
  
  
  output$chooseChapter <- renderUI({
    selectInput("chooseChapter",label = NULL, choices = unique(tables$Chapter))
  })

  output$chooseSection <- renderUI({
    selectInput("chooseSection",label = NULL, choices = unique(tables$Section[tables$Chapter == input$chooseChapter]))
                
  })
   
  output$chooseQuestion <- renderUI({
    selectInput("chooseQuestion",label = NULL, choices = NULL)
  })
  
   
  observeEvent(input$chooseSection,{
    updateSelectInput(session, "chooseQuestion", choices = unique(tables$Question[tables$Chapter == input$chooseChapter &
                                                                                tables$Section == input$chooseSection ]))
  })
  


  # CHOOSE FILTER 1
  
  output$chooseFilter1 <- renderUI({
    selectInput("chooseFilter1",label=NULL, choices =  NULL)
    
  })
  
  observeEvent(input$chooseQuestion, {
    
    updateSelectizeInput(session,'chooseFilter1',
                         choices = unique(tables$Filter1[tables$Chapter == input$chooseChapter &
                           tables$Section == input$chooseSection &
                                                       tables$Question == input$chooseQuestion]))
  })
  

  
  # CHOOSE FILTER 1 OPTION
  
  observe({
    updateSelectizeInput(session,'chooseFilter1Option',
                         choices = unique(tables$Filter1Options[tables$Chapter == input$chooseChapter &
                           tables$Section == input$chooseSection &
                                                              tables$Question == input$chooseQuestion]))
  })
  
  output$chooseFilter1Option <- renderUI({
    selectizeInput(inputId = "chooseFilter1Option",label=NULL, selected = "None", choices = NULL)
  })
  

  
  
  
  
  output$chooseDemographic <- renderUI({
    selectInput("chooseDemographic",label=NULL, choices = unique(tables$Demographic[tables$Chapter == input$chooseChapter &
      tables$Section == input$chooseSection &
                                                                             tables$Question == input$chooseQuestion &
                                                                             tables$Filter1Options == input$chooseFilter1Option]))
  })
  
  

  # CHOOSE FILTER 2
  
  observe({
    updateSelectizeInput(session,'chooseFilter2',
                         choices = unique(tables$Filter2[tables$Chapter == input$chooseChapter &
                           tables$Section == input$chooseSection &
                                                         tables$Question == input$chooseQuestion &
                                                         tables$Filter1Options == input$chooseFilter1Option &
                                                         tables$Demographic == input$chooseDemographic]))
  })
  
  
  
  
  output$chooseFilter2 <- renderUI({
      selectInput("chooseFilter2",label=NULL, selected = "None", choices = NULL)
    })

  
  # CHOOSE FILTER 2 OPTION

  output$chooseFilter2Option <- renderUI({
      selectizeInput(inputId = "chooseFilter2Option",label=NULL, choices = NULL)
    })

  
  
  observe({
    updateSelectizeInput(session,'chooseFilter2Option',
                         choices = unique(tables$Filter2Options[tables$Chapter == input$chooseChapter &
                           tables$Section == input$chooseSection &
                                                               tables$Question == input$chooseQuestion &
                                                               tables$Filter1Options == input$chooseFilter1Option &
                                                               tables$Demographic == input$chooseDemographic &
                                                               tables$Filter2 == input$chooseFilter2]))
  })
 
  
  
  
  
  # CHOOSE FILTER 3
  
  observe({
    updateSelectizeInput(session,'chooseFilter3',
                         choices = unique(tables$Filter3[tables$Chapter == input$chooseChapter &
                           tables$Section == input$chooseSection &
                                                         tables$Question == input$chooseQuestion &
                                                         tables$Filter1Options == input$chooseFilter1Option &
                                                         tables$Demographic == input$chooseDemographic &
                                                         tables$Filter2 == input$chooseFilter2 &
                                                         tables$Filter2Options == input$chooseFilter2Option]))
  })
  
  
  
  
  output$chooseFilter3 <- renderUI({
    selectInput("chooseFilter3",label=NULL, selected = "None", choices = NULL)
  })
  
  
  # CHOOSE FILTER 3 OPTION
  
  output$chooseFilter3Option <- renderUI({
    selectizeInput(inputId = "chooseFilter3Option",label=NULL, choices = NULL)
  })
  
  observe({
    updateSelectizeInput(session,'chooseFilter3Option',
                         choices = unique(tables$Filter3Options[tables$Chapter == input$chooseChapter &
                           tables$Section == input$chooseSection &
                                                               tables$Question == input$chooseQuestion &
                                                               tables$Filter1Options == input$chooseFilter1Option &
                                                               tables$Demographic == input$chooseDemographic &
                                                               tables$Filter2 == input$chooseFilter2 &
                                                               tables$Filter2Options == input$chooseFilter2Option &
                                                               tables$Filter3 == input$chooseFilter3]))
  })
  

  output$table <-  renderTable({
    if(is.null(input$do) || input$do == 0 ){return(NULL)}
    print(outputlist()$dataout)
  },
  include.rownames=FALSE
  )
  
  output$stackedplot <- renderPlot({
    if(is.null(input$do) || input$do == 0 ){return(NULL)}
    print(outputlist()$stackedplot)
  })
  
  
  output$Title<- renderText({
    if(is.null(input$do) || input$do == 0 ){return(NULL)}
     description()
           })
  
  description <-  eventReactive(input$do,{
    description <- paste0("<h5> <b> ",input$chooseChapter," - ",input$chooseSection,"</b> <br>",
           "This question covers <b>",input$chooseQuestion,"</b> by the <b>",input$chooseDemographic, "</b> demographic",
           "<p> 
           <br>
           You have chosen to include <b>",input$chooseFilter1Option,"</b> respondents. <br> 
           The data has been subsetted to only include: <b>", input$chooseFilter2," - ", input$chooseFilter2Option,"</b> <br>
           and <b>", input$chooseFilter3," - ", input$chooseFilter3Option,"</b>",
           br(),
           br(),
           "Original wording of the question given to the respondents:
           <b>",br(),vlookup[vlookup$Section == input$chooseSection & vlookup$VariableName == input$chooseQuestion,"VariableDescription"],"</b>")
    return(description)
    })
  
  output$Notes<- renderText({
    if(is.null(input$do) || input$do == 0 ){return(NULL)}
    if(outputlist()$datatest == FALSE){return(NULL)}
    HTML("<h4> Notes </h4>
         <p> Here is important information to consider when interpretating the analysis</p>
         <br>
         <li>", vlookup[vlookup$Section == input$chooseSection & vlookup$VariableName == input$chooseQuestion,"Notes1"], "</li>
         <li>", vlookup[vlookup$Section == input$chooseSection & vlookup$VariableName == input$chooseQuestion,"Notes2"], "</li>
         <li>", vlookup[vlookup$Section == input$chooseSection & vlookup$VariableName == input$chooseQuestion,"Notes3"], "</li>"
         )})
  
  
  output$NoData <- renderText({
    if(is.null(input$do) || input$do == 0 ){return(NULL)}
    if(outputlist()$datatest != "No Data"){return(NULL)}
    HTML("<h4> No data available </h4>
         <br>
         <p> There were no respondents to the survey for the options you have chosen.</p>
         <br>")})

  ##### RESULTS FILE FOR DOWNLOAD #####
  
  ## CSV Download
  
  output$csvtitle <- renderText( {
    HTML("<h4> Download .csv file </h4>
         <br>
         <p> This is a .csv file which can be edited and have data extracted.</p>
         <br>")})
  
  
  output$downloadData <-  downloadHandler(filename = function() {
    paste('LGBTSurvey - ',input$chooseFilter1Option,' - ',input$chooseFilter2Option,' - ', Sys.time(), '.csv', sep="")
  },
  content = function(file) {
    write.csv(print(outputlist()$dataout, quote=FALSE,right=TRUE), file)}
  )
  
  


  outputlist  <- eventReactive(input$do,{
        w.data <-  data[data$Chapter   == input$chooseChapter &
                   data$Section        == input$chooseSection &
                   data$Question       == input$chooseQuestion       &
                   data$Demographic    == input$chooseDemographic    &
                   data$Filter1Options == input$chooseFilter1Option  &
                   data$Filter2        == input$chooseFilter2        &
                   data$Filter2Options == input$chooseFilter2Option  &
                   data$Filter3        == input$chooseFilter3        &
                   data$Filter3Options == input$chooseFilter3Option,]
    
        
        
        
    if(w.data$value[1] == "NA" || is.na(w.data$value[1])){
        return(list(datatest=FALSE, dataout=NULL, stackedplot=NULL,download=NULL))}
   
    if(w.data$value[1] == "No Data available"){
      return(list(datatest="No Data", dataout=NULL, stackedplot=NULL,download=NULL))}
      
  
    
   colorder <- unique(w.data$Columns)
   roworder <- unique(w.data$Rows)
    
   datatest <- TRUE   
    
   table <- dcast(w.data, Rows ~ Columns, value.var="value")
   table <- select(.data = table, Rows, colorder)
   table <- arrange(.data = table, order(roworder))
   download <- table
   
   table[(1:nrow(table)-1),2:ncol(table)] <- sapply(table[1:nrow(table)-1,2:ncol(table)],reduce)
   
   #table[(1:nrow(table)-1),2:ncol(table)] %>% mutate_all(funs(reduce))
   
   w.data$Rows <- factor(w.data$Rows, levels = rev(roworder))
   w.data$Columns <- factor(w.data$Columns , levels = colorder)  
   
   stackedplot <- ggplot(data = w.data[w.data$Rows != "Totals",], aes(x = Columns, y = as.numeric(value), fill = Rows)) + geom_bar(stat='identity') +
    xlab(input$chooseDemographic) +
    ylab("Percentage (%)")+
     theme(axis.text=element_text(size=12),
           axis.title=element_text(size=14,face="bold"),
           legend.text=element_text(size=12),
           legend.title=element_text(size=14,face="bold"))
   
   
   return(list(datatest=datatest, dataout=table, stackedplot=stackedplot,download=download))
  
   
   })
  
  #stop app running when closed in browser
  session$onSessionEnded(function() { stopApp() })

})
