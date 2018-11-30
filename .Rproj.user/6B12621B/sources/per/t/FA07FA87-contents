#################################################################################################################################################
#
# PROGRAM LGBT_Survey EDITS MADE
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

library(readr)
library(dplyr)
library(ggplot2)
library(stringr)

#data <- read_csv("data/Data.csv", col_types = cols(.default = "c", value = "c"))

data <- readRDS("data/testdata")



data %>%
  mutate_at(vars(), function(x){gsub('[^ -~]', '', x)})

### Start of Shiny server
shinyServer(function(input, output, session) {
  
 # outputOptions(output,"chooseFilter1",priority = 10)
  
  VersionNo <- "1.0"
  
  output$chooseQuestion <- renderUI({
    selectInput("chooseQuestion",label = NULL, choices = unique(data$Row))
  })
  
  
  output$chooseDemographic <- renderUI({
    selectInput("chooseDemographic",label=NULL, choices = unique(data$Column))
  })
  
  
  
  # CHOOSE FILTER 1
  
  observe({
  updateSelectizeInput(session,'chooseFilter1',
                       choices = data$Filter1[data$Row == input$chooseQuestion &
                                              data$Column == input$chooseDemographic])
  })  

  
  output$chooseFilter1 <- renderUI({
  selectInput("chooseFilter1",label=NULL, choices =  NULL)
    
  })
  
  # CHOOSE FILTER 1 OPTION
  
  observe({
    updateSelectizeInput(session,'chooseFilter1Option',
                         choices = data$Filter1Option[data$Row == input$chooseQuestion &
                                                        data$Column == input$chooseDemographic &
                                                        data$Filter1 == input$chooseFilter1])
  })
  
  
  output$chooseFilter1Option <- renderUI({
  selectizeInput(inputId = "chooseFilter1Option",label=NULL, selected = "None", choices = NULL)
  })

  # CHOOSE FILTER 2
  
  observe({
    updateSelectizeInput(session,'chooseFilter2',
                         choices = data$Filter2[data$Row == input$chooseQuestion &
                                                  data$Column == input$chooseDemographic &
                                                  data$Filter1 == input$chooseFilter1 &
                                                  data$Filter1Option == input$chooseFilter1Option])
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
                         choices = data$Filter2Option[data$Row == input$chooseQuestion &
                                                        data$Column == input$chooseDemographic &
                                                        data$Filter1 == input$chooseFilter1 &
                                                        data$Filter1Option == input$chooseFilter1Option &
                                                        data$Filter2 == input$chooseFilter2])
  })
 

  # output$chooseFilter1 <- renderUI({
  #   selectInput("chooseFilter1",label=NULL, selected = "None", choices = unique(data$Filter1))
  # })
  # 
  # 
  # output$chooseFilter1Option <- renderUI({
  #   if(is.null(input$chooseFilter1)){return(NULL)}
  #   selectInput("chooseFilter1Option",label=NULL, choices = unique(data[data$Filter1 == input$chooseFilter1,]$Filter1Option))
  # })

  
  
  # output$chooseFilter2 <- renderUI({
  #   selectInput("chooseFilter2",label=NULL, selected = "None", choices = unique(data[data$Filter1       == input$chooseFilter1     &
  #                                                           data$Filter1Option == input$chooseFilter1Option,]$Filter2))
  # })
  # 
  # 
  # output$chooseFilter2Option <- renderUI({
  #   if(is.null(input$chooseFilter2)){return(NULL)}
  #   selectInput("chooseFilter2Option",label=NULL, choices = unique(data[data$Filter1       == input$chooseFilter1     &
  #                                                                   data$Filter1Option == input$chooseFilter1Option &
  #                                                                   data$Filter2       == input$chooseFilter2,]$Filter2Option))
  # })



  output$table <-  renderTable({
    print(outputlist()$dataout)
  },
  include.rownames=FALSE
  )
  
  output$stackedplot <- renderPlot({
    if(is.null(outputlist())){return(NULL)}
    print(outputlist()$stackedplot)
  })
  
  
  output$Title<- renderText({
    if(is.null(input$chooseFilter2Option)){return(NULL)}
    if(outputlist()$datatest == FALSE){return(NULL)}
    paste0("<h5> This question covers ",input$chooseQuestion," by this ",input$chooseDemographic, " demographic",
           "<p> 
            <br>
            You have chosen to filter the respondents by ",input$chooseFilter1Option," and by ", input$chooseFilter2Option,
           "<p>
            <br>
            The question was originally worded ....")})
  
  output$Notes<- renderText({
    if(outputlist()$datatest == FALSE){return(NULL)}
    HTML("<h4> Notes to go here</h4>
         <br>
         <p> Explanation of restrictions of what is shown.</p>
         <br>")})
  
  
  output$NoData <- renderText({
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
  
  


  outputlist  <- reactive({
  
    w.data <- data[data$Row           == input$chooseQuestion       & 
                   data$Column        == input$chooseDemographic    &
                   data$Filter1       == input$chooseFilter1        &
                   data$Filter1Option == input$chooseFilter1Option  &
                   data$Filter2       == input$chooseFilter2        & 
                   data$Filter2Option == input$chooseFilter2Option,]    
    
    
    print(w.data$value[1])
    
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
   
   table[1:nrow(table)-1,2:ncol(table)] <- sapply(table[1:nrow(table)-1,2:ncol(table)],reduce)

   
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
  
reduce <-function(x){paste0(substr(x, start = 0, stop= which(strsplit(x, "")[[1]]==".")+1)," %")}

args(png)
getOption('bitmapType')
