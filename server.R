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

library(readr)
library(dplyr)
library(ggplot2)

data <- read_csv("data/Data.csv", col_types = cols(.default = "c", value = "c"))

data %>%
  mutate_at(vars(), function(x){gsub('[^ -~]', '', x)})

### Start of Shiny server
shinyServer(function(input, output, session) {
  
  VersionNo <- "1.0"
  
  output$chooseQuestion <- renderUI({
    selectInput("chooseQuestion",label = NULL, choices = unique(data$Row))
  })
  
  
  output$chooseDemographic <- renderUI({
    radioButtons("chooseDemographic",label=NULL, choices = unique(data$Column))
  })
  
  output$chooseFilter1 <- renderUI({
    radioButtons("chooseFilter1", selected = "None",
                label = h5("Filter"), choices = unique(data$Filter1))
  })
  
  
  output$chooseFilter1Option <- renderUI({
    if(is.null(input$chooseFilter1)){return(NULL)}
    radioButtons("chooseFilter1Option",
                label = h5("Filter Options"), choices = unique(data[data$Filter1 == input$chooseFilter1,]$Filter1Option))
  })  

  output$chooseFilter2 <- renderUI({
    radioButtons("chooseFilter2", selected = "None",
                label = h5("Filter"), choices = unique(data[data$Filter1       == input$chooseFilter1     &
                                                            data$Filter1Option == input$chooseFilter1Option,]$Filter2))
  })


  output$chooseFilter2Option <- renderUI({
    if(is.null(input$chooseFilter2)){return(NULL)}
    radioButtons("chooseFilter2Option",
                label = h5("Filter Options"), choices = unique(data[data$Filter1       == input$chooseFilter1     &
                                                                    data$Filter1Option == input$chooseFilter1Option &
                                                                    data$Filter2       == input$chooseFilter2,]$Filter2Option))
  })



  output$table <-  renderTable({
    if((length(input$chooseFilter1Option) == 0 && !is.null(input$chooseFilter1))){return(NULL)}
    if((length(input$chooseFilter2Option) == 0 && !is.null(input$chooseFilter2))){return(NULL)}
    if(outputlist()$datatest == FALSE){return(NULL)}
    print(outputlist()$dataout)
  },
  include.rownames=FALSE
  )
  
  output$stackedplot <- renderPlot({
    if((length(input$chooseFilter1Option) == 0 && !is.null(input$chooseFilter1))){return(NULL)}
    if((length(input$chooseFilter2Option) == 0 && !is.null(input$chooseFilter2))){return(NULL)}
    if(outputlist()$datatest == FALSE){return(NULL)}
    print(outputlist()$stackedplot)
  })
  
  
  output$Title<- renderText({
    if(outputlist()$datatest == FALSE){return(NULL)}
    paste0("<h4> This question covers ",input$chooseQuestion," by this ",input$chooseDemographic, " demographic",
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
    if(outputlist()$datatest == TRUE){return(NULL)}
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
    
    if(is.null(input$chooseFilter1)){
        Filter1       <- "None"
        Filter1Option <- "None"} else {
        Filter1       <- input$chooseFilter1
        Filter1Option <- input$chooseFilter1Option}  
        
    if(is.null(input$chooseFilter2)){
        Filter2       <- "None"
        Filter2Option <- "None"} else {
        Filter2       <- input$chooseFilter2
        Filter2Option <- input$chooseFilter2Option} 
    
    w.data <- data[data$Row           == input$chooseQuestion        & 
                   data$Column        == input$chooseDemographic     &
                   data$Filter1       == Filter1       &
                   data$Filter1Option == Filter1Option  &
                   data$Filter2       == Filter2        & 
                   data$Filter2Option == Filter2Option,]    
    

    
   colorder <- unique(w.data$Columns)
   roworder <- unique(w.data$Rows)
    
   
   if(w.data$value[1] == "No Data available"){
   datatest <- FALSE 
   table <- NULL
   stackedplot <- NULL
   }else{
   
   datatest <- TRUE   
     
   table <- dcast(w.data, Rows ~ Columns, value.var="value")
   table <- select(.data = table, Rows, colorder)
   table <- arrange(.data = table, order(roworder))
   

   
   w.data$Rows <- factor(w.data$Rows, levels = rev(roworder))
   w.data$Columns <- factor(w.data$Columns , levels = colorder)  
   

   
   stackedplot <- ggplot(data = w.data[w.data$Rows != "Totals",], aes(x = Columns, y = as.numeric(value), fill = Rows)) + geom_bar(stat='identity') +
    xlab(input$chooseFilter2) +
    ylab(input$chooseFilter1)
   }
      

   
   return(list(datatest=datatest, dataout=table, stackedplot=stackedplot))
  
   
   })
  
  #stop app running when closed in browser
  session$onSessionEnded(function() { stopApp() })

})
  

args(png)
getOption('bitmapType')
