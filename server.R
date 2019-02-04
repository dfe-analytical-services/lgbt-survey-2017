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
library(data.table)
library(DT)

# start.time <- Sys.time()
# 
# setwd("C:/Users/tdougall/OneDrive - Department for Education/Documents/shiny/GEO/LGBTSurvey2017/lgbt-survey-2017/")
# 
# data <- readRDS("data/DummyData")
# 
# data <- data %>% mutate_at(vars(), function(x){gsub('[^ -~]', '', x)})
# object.size(data)/(1024*1024)
# vlookup <- read.csv("data/vlookup.csv",na.strings = "",header = TRUE)
# 
# tables <- distinct(data[,c("Chapter","Section","Question","Demographic","Filter1","Filter1Options","Filter2","Filter2Options","Filter3","Filter3Options")])
# 
# print(Sys.time() - start.time)

start.time <- Sys.time()

db      <- DBI::dbConnect(RSQLite::SQLite(), dbname = "C:/Users/tdougall/OneDrive - Department for Education/Documents/shiny/GEO/LGBTSurvey2017/lgbt-survey-2017 test/data/SQLDB.sqlite")
data    <- tbl(db, "allsqldata")
tables  <- tbl(db, "tables")
tables  <- tables %>% collect()

vlookup <- read.csv("data/vlookup.csv",na.strings = "",header = TRUE)

print(Sys.time() - start.time)

reduce <- function(b){
  sapply(b, FUN = function(a) {
    if(!(a %in% c("x","-"))){      a <- as.character(round(as.numeric(a),1))}
    if(grepl("\\.",a) == TRUE){
      paste0(substr(a, start = 0, stop= which(strsplit(a, "")[[1]]==".")+1)," %")
    }else if(grepl("x",a)  == TRUE | grepl("-",a)  == TRUE){
      return(a)
    } else{paste0(a," %",sep="")}})
}
  



### Start of Shiny server
shinyServer(function(input, output, session) {
  
  output$fileLoaded <- reactive({
    return(!is.null(data))
  })
  
  
  output$chooseChapter <- renderUI({
    selectInput("chooseChapter",label = NULL, choices = unique(tables$Theme))
  })

  output$chooseSection <- renderUI({
    selectInput("chooseSection",label = NULL, choices = unique(tables$`Sub-theme`[tables$Theme == input$chooseChapter]))
                
  })
   
  output$chooseQuestion <- renderUI({
    selectInput("chooseQuestion",label = NULL, choices = NULL)
  })
  
   
  observeEvent(input$chooseSection,{
    updateSelectInput(session, "chooseQuestion", choices = unique(tables$Question[tables$Theme == input$chooseChapter &
                                                                                tables$`Sub-theme` == input$chooseSection ]))
  })
  


  # CHOOSE FILTER 1
  
  output$chooseFilter1 <- renderUI({
    selectInput("chooseFilter1",label=NULL, choices =  NULL)
    
  })
  
  observeEvent(input$chooseQuestion, {
    
    updateSelectizeInput(session,'chooseFilter1',
                         choices = unique(tables$Filter1[tables$Theme == input$chooseChapter &
                           tables$`Sub-theme` == input$chooseSection &
                                                       tables$Question == input$chooseQuestion]))
  })
  

  
  # CHOOSE FILTER 1 OPTION
  
  observe({
    updateSelectizeInput(session,'chooseFilter1Option',
                         choices = unique(tables$Filter1Options[tables$Theme == input$chooseChapter &
                           tables$`Sub-theme` == input$chooseSection &
                                                              tables$Question == input$chooseQuestion]))
  })
  
  output$chooseFilter1Option <- renderUI({
    selectizeInput(inputId = "chooseFilter1Option",label=NULL, selected = "None", choices = NULL)
  })
  

  
  
  
  
  output$chooseDemographic <- renderUI({
    selectInput("chooseDemographic",label=NULL, choices = unique(tables$`Category of respondents`[tables$Theme == input$chooseChapter &
      tables$`Sub-theme` == input$chooseSection &
                                                                             tables$Question == input$chooseQuestion &
                                                                             tables$Filter1Options == input$chooseFilter1Option]))
  })
  
  

  # CHOOSE FILTER 2
  
  observe({
    updateSelectizeInput(session,'chooseFilter2',
                         choices = unique(tables$Filter2[tables$Theme == input$chooseChapter &
                           tables$`Sub-theme` == input$chooseSection &
                                                         tables$Question == input$chooseQuestion &
                                                         tables$Filter1Options == input$chooseFilter1Option &
                                                         tables$`Category of respondents` == input$chooseDemographic]))
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
                         choices = unique(tables$Filter2Options[tables$Theme == input$chooseChapter &
                           tables$`Sub-theme` == input$chooseSection &
                                                               tables$Question == input$chooseQuestion &
                                                               tables$Filter1Options == input$chooseFilter1Option &
                                                               tables$`Category of respondents` == input$chooseDemographic &
                                                               tables$Filter2 == input$chooseFilter2]))
  })
 
  
  
  
  
  # CHOOSE FILTER 3
  
  observe({
    updateSelectizeInput(session,'chooseFilter3',
                         choices = unique(tables$Filter3[tables$Theme == input$chooseChapter &
                           tables$`Sub-theme` == input$chooseSection &
                                                         tables$Question == input$chooseQuestion &
                                                         tables$Filter1Options == input$chooseFilter1Option &
                                                         tables$`Category of respondents` == input$chooseDemographic &
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
                         choices = unique(tables$Filter3Options[tables$Theme == input$chooseChapter &
                           tables$`Sub-theme` == input$chooseSection &
                                                               tables$Question == input$chooseQuestion &
                                                               tables$Filter1Options == input$chooseFilter1Option &
                                                               tables$`Category of respondents` == input$chooseDemographic &
                                                               tables$Filter2 == input$chooseFilter2 &
                                                               tables$Filter2Options == input$chooseFilter2Option &
                                                               tables$Filter3 == input$chooseFilter3]))
  })
  

  output$table <-  DT::renderDataTable(server = FALSE,options = list(pageLength = 30, dom='t',ordering=FALSE),rownames=FALSE,{
    if(is.null(input$do) || input$do == 0 ){return(NULL)}
    print(outputlist()$dataout)
  })
  
  output$glossary <- renderTable({Glossary})
  
  
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
    notes()
  })
    

  notes<-  eventReactive(input$do,{ 
      
    if(vlookup[vlookup$Section == input$chooseSection & vlookup$VariableName == input$chooseQuestion,"Notes1"] == "No notes"){
        Notes1 <- ""} else{ Notes1 <- paste("<li>",vlookup[vlookup$Section == input$chooseSection & vlookup$VariableName == input$chooseQuestion,"Notes1"],"</li>")}
      
    if(vlookup[vlookup$Section == input$chooseSection & vlookup$VariableName == input$chooseQuestion,"Notes2"] == "No notes"){
        Notes2 <- ""} else{ Notes2 <- paste("<li>",vlookup[vlookup$Section == input$chooseSection & vlookup$VariableName == input$chooseQuestion,"Notes2"],"</li>")}
      
    if(vlookup[vlookup$Section == input$chooseSection & vlookup$VariableName == input$chooseQuestion,"Notes3"] == "No notes"){
        Notes3 <- ""} else{ Notes3 <- paste("<li>",vlookup[vlookup$Section == input$chooseSection & vlookup$VariableName == input$chooseQuestion,"Notes3"],"</li>")}

    
    if(Notes1 == "" & Notes2 == "" & Notes3 == ""){
      notes <- HTML("<h4> Notes </h4>
         <p> There are no notes for the chosen options</p>")} else{
      notes <- HTML(paste0("<h4> Notes </h4>
              <p> Here is important information to consider when interpretating the analysis</p>
              <br>",
              Notes1,
              "<br>",
              Notes2,
              "<br>",
              Notes3))}
      return(notes)
      
           })
  
  
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

       w.data <- data %>%
          filter(Theme        == input$chooseChapter)       %>%
          filter(`Sub-theme`        == input$chooseSection)       %>%
          filter(Question       == input$chooseQuestion)      %>%
          filter(`Category of respondents`    == input$chooseDemographic)   %>%
          filter(Filter1Options == input$chooseFilter1Option) %>%
          filter(Filter2        == input$chooseFilter2)       %>%
          filter(Filter2Options == input$chooseFilter2Option) %>%
          filter(Filter3        == input$chooseFilter3)       %>%
          filter(Filter3Options == input$chooseFilter3Option)

          w.data <- w.data %>% collect()

          print(w.data)
          
          print(w.data$value[1])
          
    if(w.data$value[1] == "NA" || is.na(w.data$value[1])){
        return(list(datatest=FALSE, dataout=NULL, stackedplot=NULL,download=NULL))}
   
    if(w.data$value[1] == "No Data available"){
      return(list(datatest="No Data", dataout=NULL, stackedplot=NULL,download=NULL))}
     
          colorder <- unique(w.data$Columns)
          roworder <- unique(w.data$Rows)
   
   
   datatest <- TRUE   
   
   table <- dcast(w.data, Rows ~ Columns, value.var="value")
   table <- table[match(roworder, table$Rows),]
   table <- table[, c("Rows",colorder)]
   download <- table
   
   table[(1:nrow(table)-1),2:ncol(table)] <- table[(1:nrow(table)-1),2:ncol(table)] %>% mutate_all(funs(reduce))

   Demographic <- input$chooseDemographic
   
   return(list(datatest=datatest, stackedplot = TRUE, dataout=table,download=download, w.data=w.data, Demographic=Demographic))
   
   
   })
  

  proxy = dataTableProxy('table')
  observeEvent(input$do, {
    selectRows(proxy, NULL)
  })
 
  output$stackedplot <- renderPlot({
    plot()
  })
  
   plot <- eventReactive(
     c(input$GRAPH,
     input$do),{  
    if(is.null(input$do) || input$do == 0 || is.null(outputlist()$w.data)){return(NULL)}
  
  w.data <- outputlist()$w.data  
  

  colorder <- unique(w.data$Columns)
  roworder <- unique(w.data$Rows)
    
  w.data$Rows <- factor(w.data$Rows, levels = rev(roworder))
  w.data$Columns <- factor(w.data$Columns , levels = colorder) 
  
  w.data$value[w.data$value == "x"] <- "0"
  w.data$value[w.data$value == "-"] <- "0"
  
  # pallete <- rev(lgbt_pallete[1:(nrow(table)-1)])
  
  s = input$table_rows_selected
  categories <- outputlist()$dataout
  categories <- categories[s,c("Rows")]
  
  if(length(s) > 0){w.data <- w.data[w.data$Rows %in% categories,]}
  w.data <- w.data[w.data$Rows != "Totals",]
    
  pallete_no <- length(unique(as.character(w.data$Rows)))
  pallete <- get(paste0("lgbt_pallete_",pallete_no))
  
  stackedplot <- ggplot(data = w.data, aes(x = Columns, y = as.numeric(value), fill = Rows)) +
    geom_bar(stat='identity') +
    xlab(outputlist()$Demographic) +
    ylab("Percentage (%)")+
    scale_fill_manual(values = pallete )+
    guides(fill = guide_legend(reverse = TRUE)) +
    theme(axis.text=element_text(size=12, angle = 45, h=1),
          axis.title=element_text(size=14,face="bold"),
          legend.text=element_text(size=12),
          legend.title=element_text(size=14,face="bold")) 

    
  print(stackedplot)
  
  })

  
  
  
  #stop app running when closed in browser
  #session$onSessionEnded(function() { stopApp() })

})

lgbt_pallete <- rev(c(
  rgb(231, 211, 218, maxColorValue = 255),
  rgb(219, 189, 200, maxColorValue = 255),
  rgb(207, 167, 181, maxColorValue = 255),
  rgb(184, 123, 143, maxColorValue = 255),
  rgb(160, 79, 106, maxColorValue = 255),
  rgb(136, 35, 69, maxColorValue = 255),
  rgb(204, 212, 224, maxColorValue = 255),
  rgb(188, 190, 208, maxColorValue = 255),
  rgb(153, 168, 193, maxColorValue = 255),
  rgb(102, 125, 162, maxColorValue = 255),
  rgb(77, 103, 147, maxColorValue = 255),
  rgb(51, 81, 132, maxColorValue = 255),
  rgb(0, 38, 100, maxColorValue = 255)))


lgbt_pallete_1 <- c(rgb(204, 212, 224, maxColorValue = 255))


lgbt_pallete_2 <- c(rgb(204,212,224, maxColorValue = 255),
                    rgb(0, 38, 100, maxColorValue = 255))

lgbt_pallete_3 <- c(rgb(204,212,224, maxColorValue = 255),
                    rgb(102, 125, 162, maxColorValue = 255),
                    rgb(0, 38, 100, maxColorValue = 255))

lgbt_pallete_4 <- c(rgb(204, 212, 224, maxColorValue = 255),
                    rgb(153, 168, 193, maxColorValue = 255),
                    rgb(102, 125, 162, maxColorValue = 255),
                    rgb(0, 38, 100, maxColorValue = 255))

lgbt_pallete_5 <- c(rgb(204, 212, 224, maxColorValue = 255),
                    rgb(153, 168, 193, maxColorValue = 255),
                    rgb(102, 125, 162, maxColorValue = 255),
                    rgb(51, 81, 131, maxColorValue = 255),
                    rgb(0, 38, 100, maxColorValue = 255))

lgbt_pallete_6 <- c(rgb(136, 35, 69, maxColorValue = 255),
                    rgb(204, 212, 224, maxColorValue = 255),
                    rgb(153, 168, 193, maxColorValue = 255),
                    rgb(102, 125, 162, maxColorValue = 255),
                    rgb(51, 81, 131, maxColorValue = 255),
                    rgb(0, 38, 100, maxColorValue = 255))

lgbt_pallete_7 <- c(
  rgb(231, 211, 218, maxColorValue = 255),
  rgb(184, 123, 143, maxColorValue = 255),
  rgb(136, 35, 69, maxColorValue = 255),
  rgb(204, 212, 224, maxColorValue = 255),
  rgb(153, 168, 193, maxColorValue = 255),
  rgb(102, 125, 162, maxColorValue = 255),
  rgb(0, 38, 100, maxColorValue = 255))

lgbt_pallete_8 <- c(
  rgb(231, 211, 218, maxColorValue = 255),
  rgb(207, 167, 181, maxColorValue = 255),
  rgb(184, 123, 143, maxColorValue = 255),
  rgb(136, 35, 69, maxColorValue = 255),
  rgb(204, 212, 224, maxColorValue = 255),
  rgb(153, 168, 193, maxColorValue = 255),
  rgb(102, 125, 162, maxColorValue = 255),
  rgb(0, 38, 100, maxColorValue = 255))

lgbt_pallete_9 <- c(
  rgb(231, 211, 218, maxColorValue = 255),
  rgb(207, 167, 181, maxColorValue = 255),
  rgb(184, 123, 143, maxColorValue = 255),
  rgb(136, 35, 69, maxColorValue = 255),
  rgb(204, 212, 224, maxColorValue = 255),
  rgb(153, 168, 193, maxColorValue = 255),
  rgb(102, 125, 162, maxColorValue = 255),
  rgb(51, 81, 132, maxColorValue = 255),
  rgb(0, 38, 100, maxColorValue = 255))

lgbt_pallete_10 <- c(
  rgb(231, 211, 218, maxColorValue = 255),
  rgb(207, 167, 181, maxColorValue = 255),
  rgb(184, 123, 143, maxColorValue = 255),
  rgb(160, 79, 106, maxColorValue = 255),
  rgb(136, 35, 69, maxColorValue = 255),
  rgb(204, 212, 224, maxColorValue = 255),
  rgb(153, 168, 193, maxColorValue = 255),
  rgb(102, 125, 162, maxColorValue = 255),
  rgb(51, 81, 132, maxColorValue = 255),
  rgb(0, 38, 100, maxColorValue = 255))

lgbt_pallete_11 <- c(
  rgb(207, 167, 181, maxColorValue = 255),
  rgb(184, 123, 143, maxColorValue = 255),
  rgb(160, 79, 106, maxColorValue = 255),
  rgb(136, 35, 69, maxColorValue = 255),
  rgb(204, 212, 224, maxColorValue = 255),
  rgb(188, 190, 208, maxColorValue = 255),
  rgb(153, 168, 193, maxColorValue = 255),
  rgb(102, 125, 162, maxColorValue = 255),
  rgb(77, 103, 147, maxColorValue = 255),
  rgb(51, 81, 132, maxColorValue = 255),
  rgb(0, 38, 100, maxColorValue = 255))

lgbt_pallete_12 <- c(
  rgb(219, 189, 200, maxColorValue = 255),
  rgb(207, 167, 181, maxColorValue = 255),
  rgb(184, 123, 143, maxColorValue = 255),
  rgb(160, 79, 106, maxColorValue = 255),
  rgb(136, 35, 69, maxColorValue = 255),
  rgb(204, 212, 224, maxColorValue = 255),
  rgb(188, 190, 208, maxColorValue = 255),
  rgb(153, 168, 193, maxColorValue = 255),
  rgb(102, 125, 162, maxColorValue = 255),
  rgb(77, 103, 147, maxColorValue = 255),
  rgb(51, 81, 132, maxColorValue = 255),
  rgb(0, 38, 100, maxColorValue = 255))

lgbt_pallete_13 <- c(
  rgb(231, 211, 218, maxColorValue = 255),
  rgb(219, 189, 200, maxColorValue = 255),
  rgb(207, 167, 181, maxColorValue = 255),
  rgb(184, 123, 143, maxColorValue = 255),
  rgb(160, 79, 106, maxColorValue = 255),
  rgb(136, 35, 69, maxColorValue = 255),
  rgb(204, 212, 224, maxColorValue = 255),
  rgb(188, 190, 208, maxColorValue = 255),
  rgb(153, 168, 193, maxColorValue = 255),
  rgb(102, 125, 162, maxColorValue = 255),
  rgb(77, 103, 147, maxColorValue = 255),
  rgb(51, 81, 132, maxColorValue = 255),
  rgb(0, 38, 100, maxColorValue = 255))


lgbt_pallete_14 <- c(
  rgb(237,151, 75, maxColorValue = 255),
  rgb(232,125,30, maxColorValue = 255),
  rgb(231, 211, 218, maxColorValue = 255),
  rgb(219, 189, 200, maxColorValue = 255),
  rgb(207, 167, 181, maxColorValue = 255),
  rgb(184, 123, 143, maxColorValue = 255),
  rgb(160, 79, 106, maxColorValue = 255),
  rgb(136, 35, 69, maxColorValue = 255),
  rgb(204, 212, 224, maxColorValue = 255),
  rgb(188, 190, 208, maxColorValue = 255),
  rgb(153, 168, 193, maxColorValue = 255),
  rgb(102, 125, 162, maxColorValue = 255),
  rgb(77, 103, 147, maxColorValue = 255),
  rgb(51, 81, 132, maxColorValue = 255),
  rgb(0, 38, 100, maxColorValue = 255))

lgbt_pallete_15 <- c(
  rgb(241,177,120, maxColorValue = 255),
  rgb(237,151, 75, maxColorValue = 255),
  rgb(232,125,30, maxColorValue = 255),
  rgb(231, 211, 218, maxColorValue = 255),
  rgb(219, 189, 200, maxColorValue = 255),
  rgb(207, 167, 181, maxColorValue = 255),
  rgb(184, 123, 143, maxColorValue = 255),
  rgb(160, 79, 106, maxColorValue = 255),
  rgb(136, 35, 69, maxColorValue = 255),
  rgb(204, 212, 224, maxColorValue = 255),
  rgb(188, 190, 208, maxColorValue = 255),
  rgb(153, 168, 193, maxColorValue = 255),
  rgb(102, 125, 162, maxColorValue = 255),
  rgb(77, 103, 147, maxColorValue = 255),
  rgb(51, 81, 132, maxColorValue = 255),
  rgb(0, 38, 100, maxColorValue = 255))

lgbt_pallete_16 <- c(
  rgb(246,203,165, maxColorValue = 255),
  rgb(241,177,120, maxColorValue = 255),
  rgb(237,151, 75, maxColorValue = 255),
  rgb(232,125,30, maxColorValue = 255),
  rgb(231, 211, 218, maxColorValue = 255),
  rgb(219, 189, 200, maxColorValue = 255),
  rgb(207, 167, 181, maxColorValue = 255),
  rgb(184, 123, 143, maxColorValue = 255),
  rgb(160, 79, 106, maxColorValue = 255),
  rgb(136, 35, 69, maxColorValue = 255),
  rgb(204, 212, 224, maxColorValue = 255),
  rgb(188, 190, 208, maxColorValue = 255),
  rgb(153, 168, 193, maxColorValue = 255),
  rgb(102, 125, 162, maxColorValue = 255),
  rgb(77, 103, 147, maxColorValue = 255),
  rgb(51, 81, 132, maxColorValue = 255),
  rgb(0, 38, 100, maxColorValue = 255))

lgbt_pallete_17 <- rev(c(
  rgb(250,229,210, maxColorValue = 255),
  rgb(246,203,165, maxColorValue = 255),
  rgb(241,177,120, maxColorValue = 255),
  rgb(237,151, 75, maxColorValue = 255),
  rgb(232,125,30, maxColorValue = 255),
  rgb(231, 211, 218, maxColorValue = 255),
  rgb(219, 189, 200, maxColorValue = 255),
  rgb(207, 167, 181, maxColorValue = 255),
  rgb(184, 123, 143, maxColorValue = 255),
  rgb(160, 79, 106, maxColorValue = 255),
  rgb(136, 35, 69, maxColorValue = 255),
  rgb(204, 212, 224, maxColorValue = 255),
  rgb(188, 190, 208, maxColorValue = 255),
  rgb(153, 168, 193, maxColorValue = 255),
  rgb(102, 125, 162, maxColorValue = 255),
  rgb(77, 103, 147, maxColorValue = 255),
  rgb(51, 81, 132, maxColorValue = 255),
  rgb(0, 38, 100, maxColorValue = 255)))

Glossary <- data.frame(matrix(
  c("Characteristics","Characteristics","Age","How old are you?",
    "","","Intersex status","Do you identify as intersex?",
    "","","Relationship status","What is your current relationship status?",
    "","","Ethnicity","What is your ethnic group?",
    "","","Religion/Belief","What is your religion/belief? ",
    "","","Disability","Do you consider yourself to have a disability?",
    "","","Education level","What is the furthest level of education you have completed? ",
    "","","Personal income","What is your personal annual income (before tax)?",
    "","","Region","Which area of the United Kingdom do you live in? ",
    "","","Sexual orientation","This question is about your sexual orientation. What do you identify as?",
    "","","In work or education","0",
    "Education","Coverage of LGBT issues at school","Discussion(s) at school","Were sexual orientation and gender identity discussed at school in lessons, assemblies or in any other part of your schooling",
    "","","Helpfulness of discussion(s)","How well did the discussion of sexual orientation or gender identity at school prepare you for later life as an LGBT person?",
    "","","School staff understanding","How understanding were your teachers and other staff of issues facing transgender, gender fluid and non-binary pupils in general?",
    "","Experiences in education","Most serious incident","Think about the most serious incident in the past 12 months. Which of the following happened to you?",
    "","","Reaction(s) to being LGBT","In the last academic year, how did others at your educational institution react to you being LGBT or because they thought you were LGBT?",
    "","","Incident(s)","In the past 12 months, did you experience any of the following from someone you lived with for any reason?",
    "","","Perpetrator(s) of incidents","Who was the perpetrator(s) of this most serious incident?",
    "","","Reporting of incidents","Did you or anyone else report this most serious incident? ",
    "","","Reason(s) for non-reporting","Why did you not report this most serious incident to the police?",
    "","","Organisation(s) reported to","Who was this most serious incident reported to?",
    "","","Helpfulness of teaching staff","How helpful or unhelpful was your teacher, lecturer, tutor or other teaching staff in handling this most serious incident?",
    "","","Helpfulness of non-teaching staff","How helpful or unhelpful were non-teaching staff in handling this most serious incident?",
    "","","Helpfulness of student representatives","How helpful or unhelpful was the student union, school council or class representative in handling this most serious incident?",
    "","","Helpfulness of parents/guardians","How helpful or unhelpful was your parent/guardian in handling this most serious incident?",
    "","","Helpfulness of police","How helpful or unhelpful were the police when reporting this most serious incident?",
    "","","Helpfulness of LGBT organisation","How helpful or unhelpful was an LGBT organisation or charity when you reported this most serious incident?",
    "","","Helpfulness of other organisation","How helpful or unhelpful was another external organisation or charity in handling this most serious incident?",
    "","","Helpfulness of other organisation","How helpful or unhelpful was another external organisation or charity in handling this most serious incident?",
    "","Openness in education","Classmates or other students","In the last academic year, how many people at your educational institution, if any, were you open with about being LGBT?",
    "","","Teaching staff","In the last academic year, how many people at your educational institution, if any, were you open with about being LGBT?",
    "","","Non-teaching staff","In the last academic year, how many people at your educational institution, if any, were you open with about being LGBT?",
    "","Respondents in education","Previous academic year","In the last academic year (beginning September 2016), were you in education at any time?",
    "","","Educational institution(s)","In the last academic year, which of the following educational institutions did you attend?",
    "Gender identity","Gender Identity Services","Access","In the past 12 months, did you access, or try to access, any specialist gender identity services in the UK for support in relation to your gender identity? For example, a Gender Identity Clinic (GIC).",
    "","","Ease of access","On a scale of 1 to 5, how easy was it for you to access mental health services in the past 12 months? If you would prefer not to answer, please leave blank.",
    "","","Barriers to access","In the past 12 months, why was accessing mental health services difficult?",
    "","","Experiences of services","Overall, how would you rate the mental health services you used in the past 12 months?",
    "","Gender recognition","Applied for a GRC","Have you previously applied, or are you currently applying, for a Gender Recognition Certificate (GRC)?",
    "","","GRC ownership","Do have a Gender Recognition Certificate (GRC)?",
    "","","Awareness of legal process","Are you aware of the UK's legal gender recognition process that allows people to apply for a Gender Recognition Certificate (GRC) in order to change their legal gender? ",
    "","","Perceived legal requirements","Which of the following, if any, do you think are requirements of the UK's legal gender recognition process?",
    "","","Help with application","Did you receive help in making the application?",
    "","","Who helped with application","Who helped you apply for a Gender Recognition Certificate (GRC)?",
    "","","Barriers to application","Why have you not applied for a Gender Recognition Certificate (GRC)?",
    "","Gender transition","Age at the start of transition","At what age did you start transitioning? Transitioning refers to the steps a person may take to live in the gender with which they identify.",
    "","","Duration of transition","To the nearest year, how long did it take to complete your transition, or are you still transitioning?",
    "","","At school","Were you transitioning while you were at school?",
    "","","School support during transition","How supportive was your school of your specific needs while you were transitioning? For example, by making toilets and changing rooms accessible, helping you to change your name, or using your pronoun of choice.",
    "","","Transitioning status","At what age did you start transitioning? Transitioning refers to the steps a person may take to live in the gender with which they identify. (Q17); To the nearest year, how long did it take to complete your transition, or are you still transitioning? (Q18)",
    "","Transitioning treatment abroad","Access","In the past 12 months, did you access, or try to access, any public healthcare services? For example, any services provided by the National Health Service (NHS) in England, Scotland and Wales, or Health and Social Care (HSC) in Northern Ireland. This includes in relation to your physical, mental or sexual health, or your gender identity, and includes routine appointments with your GP (General Practitioner).",
    "","","Reasons for accessing ","Why did you use or pay for healthcare services or medical treatment outside the UK to support your transition?",
    "Healthcare","Mental health services","Access","In the past 12 months, did you access, or try to access, any public healthcare services? For example, any services provided by the National Health Service (NHS) in England, Scotland and Wales, or Health and Social Care (HSC) in Northern Ireland. This includes in relation to your physical, mental or sexual health, or your gender identity, and includes routine appointments with your GP (General Practitioner).",
    "","","Ease of access","On a scale of 1 to 5, how easy was it for you to access mental health services in the past 12 months? If you would prefer not to answer, please leave blank.",
    "","","Barriers to access","In the past 12 months, why was accessing mental health services difficult?",
    "","","Experiences of services","Overall, how would you rate the mental health services you used in the past 12 months?",
    "","Public healthcare","Access","In the past 12 months, did you access, or try to access, any public healthcare services? For example, any services provided by the National Health Service (NHS) in England, Scotland and Wales, or Health and Social Care (HSC) in Northern Ireland. This includes in relation to your physical, mental or sexual health, or your gender identity, and includes routine appointments with your GP (General Practitioner).",
    "","","Disclosing sexual orientation to staff","In the past 12 months, how often did you discuss or disclose your sexual orientation with healthcare staff?",
    "","","Impact of disclosure to staff","In the past 12 months, did being open about your sexual orientation with healthcare staff have an effect on your care?",
    "","","Reasons for non-disclosure to staff","In the past 12 months, why did you not discuss your sexual orientation with all healthcare staff?",
    "","","Incidents due to sexual orientation","In the past 12 months, did you experience any of the following when using or trying to access healthcare services because of your sexual orientation?",
    "","","Incidents due to gender identity","In the past 12 months, did you experience any of the following when using or trying to access healthcare services because of your transgender status or gender identity?",
    "","","Incidents due to intersex status","In the past 12 months, did you experience any of the following when using or trying to access healthcare services because of being intersex?",
    "","Respondents in care","Previous year","In the past 12 months, have you lived in a care home or been in any other form of institutional care?",
    "","","Openness in care","In the past 12 months, how often were you open about being LGBT with care staff and other residents?",
    "","","Reasons for non-disclosure of LGBT status","In the past 12 months, why were you not always open about being LGBT with care staff and other residents?",
    "","","Impact of disclosure of LGBT status","In the past 12 months, has being open about being LGBT with some care staff and other residents affected your experience?",
    "","Sexual health services","Access","In the past 12 months, did you access, or try to access, any public healthcare services? For example, any services provided by the National Health Service (NHS) in England, Scotland and Wales, or Health and Social Care (HSC) in Northern Ireland. This includes in relation to your physical, mental or sexual health, or your gender identity, and includes routine appointments with your GP (General Practitioner).",
    "","","Ease of access","On a scale of 1 to 5, how easy was it for you to access mental health services in the past 12 months? If you would prefer not to answer, please leave blank.",
    "","","Barriers to access","In the past 12 months, why was accessing mental health services difficult?",
    "","","Experiences of services","Overall, how would you rate the mental health services you used in the past 12 months?",
    "Life in the UK","Life in the UK","Comfort being LGBT in the UK","On a scale of 1 to 5, how comfortable do you feel being an LGBT person in the UK? If you would prefer not to answer, please leave blank. ",
    "","","Life satisfaction","Overall, on a scale of 1 to 10, how satisfied are you with your life nowadays?",
    "Safety","Avoidance behaviours","Holding hands in public","Do you ever avoid holding hands in public with a same-sex partner for fear of a negative reaction from others?",
    "","","Related to sexual orientation","Do you ever avoid being open about your sexual orientation for fear of a negative reaction from others?",
    "","","Related to sexual orientation - place","Where do you avoid being open about your sexual orientation for fear of a negative reaction from others?'",
    "","","Related to gender identity","Do you ever avoid expressing your gender identity for fear of a negative reaction from others? For example, through your physical appearance or clothing.'",
    "","","Related to gender identity - place","Where do you avoid expressing your gender identity or your preferred gender) for fear of a negative reaction from others?'",
    "","Conversion therapy","Experienced or offered","Have you ever had so-called 'conversion' or 'reparative' therapy in an attempt to 'cure' you of being LGBT? Have you ever been offered this so-called 'conversion' or 'reparative' therapy?",
    "","","People/organisation who offered ","Who offered you this so-called 'conversion' or 'reparative' therapy?",
    "","","People/organisation who conducted","Who conducted this so-called 'conversion' or 'reparative' therapy?",
    "","In the home","Most serious incident","Think about the most serious incident in the past 12 months. Which of the following happened to you?",
    "","","Helpfulness of police","How helpful or unhelpful were the police when reporting this most serious incident?",
    "","","Satisfaction with police","How satisfied or unsatisfied were you with how the police handled this most serious incident?",
    "","","Helpfulness of LGBT organisation","How helpful or unhelpful was an LGBT organisation or charity when you reported this most serious incident?",
    "","","Satisfaction with LGBT organisation","How satisfied or unsatisfied were you with how an LGBT organisation or charity handled this most serious incident?",
    "","","Helpfulness of victim support organisation","How helpful or unhelpful was another victim support organisation or charity when you reported this most serious incident?",
    "","","Satisfaction with other victim support organisation","How satisfied or unsatisfied were you with how another victim support organisation or charity handled this?",
    "","","Helpfulness of state/national institution","How helpful or unhelpful was a state or national institution (such as an Equality Body) when you reported this most serious incident?",
    "","","Satisfaction with state/national institution","How satisfied or unsatisfied were you with how a state or national institution (such as an Equality Body) handled this most serious incident?",
    "","","Helpfulness of hospital/healthcare service","How helpful or unhelpful was a hospital or other healthcare service when you reported this most serious incident?",
    "","","Satisfaction with hospital/healthcare service","How satisfied or unsatisfied were you with how a hospital or other healthcare service handled this most serious incident?",
    "","","Helpfulness of rape crisis centre","How helpful or unhelpful was a rape crisis centre when you reported this most serious incident?",
    "","","Satisfaction with rape crisis centre","How satisfied or unsatisfied were you with how a rape crisis centre handled this most serious incident?",
    "","","Helpfulness of True Vision website","How helpful or unhelpful was the True Vision website when you reported this most serious incident?",
    "","","Satisfaction with True Vision website","How satisfied or unsatisfied were you with how the True Vision website handled this most serious incident?",
    "","","Incident(s)","In the past 12 months, did you experience any of the following from someone you lived with for any reason?",
    "","","Perpetrator(s) of incidents","Who was the perpetrator(s) of this most serious incident?",
    "","","Reporting of incidents","Did you or anyone else report this most serious incident? ",
    "","","Organisation(s) reported to","Who was this most serious incident reported to?",
    "","","Reason(s) for non-reporting","Why did you not report this most serious incident to the police?",
    "","Openess in personal life","Friends","In the past 12 months, how many people in the following groups, if any, were you open with about being LGBT - Friends?",
    "","","Neighbours ","In the past 12 months, how many people in the following groups, if any, were you open with about being LGBT - Neighbours?",
    "","","Family not lived with","In the past 12 months, how many people in the following groups, if any, were you open with about being LGBT - Family members you were not living with?",
    "","","Family members lived with","In the past 12 months, how many people you lived with, if any, were you open with about being LGBT?",
    "","","Other people lived with","In the past 12 months, how many people you lived with, if any, were you open with about being LGBT?",
    "","Outside the home","Most serious incident","Think about the most serious incident in the past 12 months. Which of the following happened to you?",
    "","","Incident(s)","In the past 12 months, did you experience any of the following from someone you lived with for any reason?",
    "","","Perpetrator(s) of incidents","Who was the perpetrator(s) of this most serious incident?",
    "","","Reporting of incidents","Did you or anyone else report this most serious incident? ",
    "","","Organisation(s) reported to","Who was this most serious incident reported to?",
    "","","Reason(s) for non-reporting","Why did you not report this most serious incident to the police?",
    "","","Helpfulness of police","How helpful or unhelpful were the police when reporting this most serious incident?",
    "","","Satisfaction with police","How satisfied or unsatisfied were you with how the police handled this most serious incident?",
    "","","Helpfulness of LGBT organisation","How helpful or unhelpful was an LGBT organisation or charity when you reported this most serious incident?",
    "","","Satisfaction with LGBT organisation","How satisfied or unsatisfied were you with how an LGBT organisation or charity handled this most serious incident?",
    "","","Helpfulness of victim support organisation","How helpful or unhelpful was another victim support organisation or charity when you reported this most serious incident?",
    "","","Satisfaction with other victim support organisation","How satisfied or unsatisfied were you with how another victim support organisation or charity handled this?",
    "","","Helpfulness of state/national institution","How helpful or unhelpful was a state or national institution (such as an Equality Body) when you reported this most serious incident?",
    "","","Satisfaction with state/national institution","How satisfied or unsatisfied were you with how a state or national institution (such as an Equality Body) handled this most serious incident?",
    "","","Helpfulness of hospital/healthcare service","How helpful or unhelpful was a hospital or other healthcare service when you reported this most serious incident?",
    "","","Satisfaction with hospital/healthcare service","How satisfied or unsatisfied were you with how a hospital or other healthcare service handled this most serious incident?",
    "","","Helpfulness of rape crisis centre","How helpful or unhelpful was a rape crisis centre when you reported this most serious incident?",
    "","","Satisfaction with rape crisis centre","How satisfied or unsatisfied were you with how a rape crisis centre handled this most serious incident?",
    "","","Helpfulness of True Vision website","How helpful or unhelpful was the True Vision website when you reported this most serious incident?",
    "","","Satisfaction with True Vision website","How satisfied or unsatisfied were you with how the True Vision website handled this most serious incident?",
    "","Sharing of private sexual content","Incident(s)","In the past 12 months, did you experience any of the following from someone you lived with for any reason?",
    "","","Reporting of incidents","Did you or anyone else report this most serious incident? ",
    "","","Organisation(s) reported to","Who was this most serious incident reported to?",
    "","","Satisfaction with police","How satisfied or unsatisfied were you with how the police handled this most serious incident?",
    "","","Satisfaction with website/app/messaging service","How satisfied or unsatisfied were you with how the website, app or messaging service involved handled this?",
    "","","Satisfaction with LGBT organisation","How satisfied or unsatisfied were you with how an LGBT organisation or charity handled this most serious incident?",
    "","","Satisfaction with other victim support organisation","How satisfied or unsatisfied were you with how another victim support organisation or charity handled this?",
    "Workplace","Experiences in the workplace","Reaction(s) to being LGBT","In the last academic year, how did others at your educational institution react to you being LGBT or because they thought you were LGBT?",
    "","","Incident(s)","In the past 12 months, did you experience any of the following from someone you lived with for any reason?",
    "","","Perpetrator(s) of incidents","Who was the perpetrator(s) of this most serious incident?",
    "","","Reporting of incidents","Did you or anyone else report this most serious incident? ",
    "","","Reason(s) for non-reporting","Why did you not report this most serious incident to the police?",
    "","","Organisation(s) reported to","Who was this most serious incident reported to?",
    "","","Helpfulness of manager","How helpful or unhelpful was your line manager, immediate manager, or supervisor in handling this most serious incident?",
    "","","Helpfulness of higher management","How helpful or unhelpful was higher management in handling this most serious incident?",
    "","","Helpfulness of human resources","How helpful or unhelpful were human resources (HR) in handling this most serious incident?",
    "","","Helpfulness of trade union","How helpful or unhelpful was the trade union in handling this most serious incident?",
    "","","Helpfulness of diversity/staff network","How helpful or unhelpful was the diversity or other staff network in handling this most serious incident?",
    "","","Helpfulness of police","How helpful or unhelpful were the police when reporting this most serious incident?",
    "","","Helpfulness of LGBT organisation","How helpful or unhelpful was an LGBT organisation or charity when you reported this most serious incident?",
    "","","Helpfulness of other organisation","How helpful or unhelpful was another external organisation or charity in handling this most serious incident?",
    "","","Impact of reporting","After you reported this most serious incident, did the negative comments or conduct stop?",
    "","","Most serious incident","Think about the most serious incident in the past 12 months. Which of the following happened to you?",
    "","Openness at work","Senior staff","In the past 12 months, how many people in your workplace, if any, were you open with about being LGBT?",
    "","","Colleagues","In the past 12 months, how many people in your workplace, if any, were you open with about being LGBT?",
    "","","Customers or clients","In the past 12 months, how many people in your workplace, if any, were you open with about being LGBT?",
    "","Respondents in employment","In the previous year","In the past 12 months, have you had a paid job at any time?",
    "","","Work sector","Which of the following sectors does your current or most recent job fall into?"),
    ncol=4,byrow = TRUE))
colnames(Glossary) <- c("Theme","Sub-theme","Question","Question Wording")
