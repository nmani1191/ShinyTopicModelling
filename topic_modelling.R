
run_topic_model <- function()
{
  #Package Loading
  library(shinydashboard)
  library(shiny)
  library(dplyr)
  library(DT)
  library(stringr)
  library(ggplot2)
  library(RTextTools)
  library(topicmodels)
  library(tm)
  
  #Function Loading
  ###################################################
  Unique_NA_counts <- function(input_data)
  {
    df1 <- data.frame(character(),character(),integer(),integer(),integer(),numeric())
    
    for (name in colnames(input_data)) {
      
      df1 <- rbind(df1,data.frame(ColName=name,Datatype=class(input_data[,name]),Total_Records=nrow(input_data),
                                  Unique_Counts=length(unique(input_data[,name])),
                                  NA_Counts=sum(is.na(input_data[,name])),
                                  NA_Percent=round(sum(is.na(input_data[,name]))/nrow(input_data),2)))
      
    }
    
    df1 <- as.data.frame(df1 %>% arrange(-NA_Counts))
    
    return(df1)
  }
  
  #######################################################
  Text_Processing<- function (FreeText, wordstoRemove)
  {
    FreeText <- iconv(FreeText, "latin1", "ASCII", sub="")
    wordstoRemove <- iconv(wordstoRemove,"latin1", "ASCII", sub = "")
    
    FreeText <- as.character(FreeText)
    wordstoRemove <- tolower(wordstoRemove)
    FreeText <- gsub("[^ -z]", " ", FreeText)
    FreeText <- gsub("[0-9]", " ", FreeText)
    FreeText <- gsub("[34-47]", " ", FreeText)#Spl character
    FreeText <- stringi::stri_trim(FreeText, side = c("both"))
    FreeText <- tolower(FreeText)
    FreeText <- gsub("[[:punct:]]", " ", FreeText)
    FreeText <- gsub("\\s+", " ", str_trim(FreeText))
    FreeText <- paste0("wordstart ", FreeText, " wordend")
    for (i in 1:length(wordstoRemove)) {
      FreeText <- gsub(paste("*\\b", wordstoRemove[i], "\\b*"), 
                       " ", FreeText)
    }
    FreeText <- gsub("\\s+", " ", str_trim(FreeText))
    FreeText <- substr(FreeText, 10, stringi::stri_length(FreeText) - 
                         8)
    FreeText <- str_trim(FreeText)
    
    #Remove Invalid Characters
    analysisdata <- iconv(FreeText, "latin1", "ASCII", sub="")
    
    #Replace the NA documents with blank value ""
    Modifiedanalysisdata <- data.frame(analysisdata)
    Modifiedanalysisdata$analysisdata <- as.character(Modifiedanalysisdata$analysisdata)
    Modifiedanalysisdata[is.na(Modifiedanalysisdata)] <- ""
    FreeText <- as.character(Modifiedanalysisdata$analysisdata)
    
    return(FreeText)
  }
  #######################################################
  get_dtm <- function(text_data)
  {
    #Create a Document Term Matrix
    dtm_matrix= create_matrix(text_data, 
                              language="english",removeSparseTerms=0)
    
    dtm_matrix <- as.matrix(dtm_matrix)
    
    return(dtm_matrix)
  }
  #######################################################
  get_optimal_topics <- function(matrix,maxtopics)
  {
    best.model <- lapply(seq(2,maxtopics, by=1), function(k){LDA(matrix,k)})
    
    best_model<- as.data.frame(as.matrix(lapply(best.model, logLik)))
    
    final_best_model <- data.frame(topics=c(seq(2,maxtopics, by=1)), 
                                   log_likelihood=as.numeric(as.matrix(best_model)))
    
    chart <- with(final_best_model,qplot(topics,log_likelihood,color="red"))
    
    #Based on the graph, we can choose the best model
    k=final_best_model[which.max(final_best_model$log_likelihood),1]
    
    return(list(graph=chart,finalTopic=k))
    
  }
  #######################################################
  run_TopicModel <- function(topicCount,matrix)
  {
    #Building model on train data
    model.lda <- LDA(matrix,topicCount)
    
    return(model.lda)
    
  }
  #######################################################
  append_Topics <- function(model,data)
  {
    model.topics <- topics(model)
    data$Topics <- model.topics
    
    return(data)
  }
  #######################################################
  get_terms_model <- function(model,terms.count)
  {
    term_result <- get_terms(model,terms.count)
    
    return(term_result)
  }
  #######################################################
  
  options(shiny.maxRequestSize = 1024*1024^2)
  
  shinyApp (
    
    ui= dashboardPage(
      dashboardHeader(title = "Topic Modeling",titleWidth=300),
      dashboardSidebar(width = 300,
                       conditionalPanel(condition="input.conditionedPanels==1",
                                        fileInput('dataset', 'Choose CSV File',accept=c('.csv')),
                                        actionButton("Validate","validate"),
                                        tags$hr(),
                                        uiOutput("TextVar"),
                                        uiOutput("MaxTopicVar"),
                                        uiOutput("Key"),
                                        uiOutput("runbutton")),
                       conditionalPanel(condition="input.conditionedPanels==2",
                                        uiOutput("OptimalTopicVar"),
                                        uiOutput("TopPatternVar"),
                                        uiOutput("runmodelbutton"))
      ),
      dashboardBody(
        
        tabsetPanel(id="conditionedPanels",
                    tabPanel("Model Building",value = 1,
                             fluidRow(column(width=12,h4("Data Summary",style="color:darkgreen"),DT::dataTableOutput("dataSummary"))),
                             uiOutput("dataSummary_download"),
                             tags$hr(),
                             fluidRow(h4("Liklywood of different Topics",style="color:darkgreen"),plotOutput("topic_liklywood"))),
                    tabPanel("Model Output",value = 2,
                             uiOutput("Topic_download"),
                             tags$hr(),
                             fluidRow(h4("Top Terms in Topics",style="color:darkgreen"),DT::dataTableOutput("topic_terms")))
        )
      )
    ),
    server= function(input, output, session) {
      
      observeEvent(input$Validate,{
        
        input$Validate # Re-run when button is clicked
        
        withProgress(message = 'Validating...', value = 0, {
          
          incProgress(0.25, detail = " 25%")
          
          inFile <- input$dataset
          ins_data_set <- read.csv(inFile$datapath,header = T,strip.white = T,
                                   na.strings = c(""," ","NA","NULL","na","null"),fileEncoding = "latin1")
          Column_names <- colnames(ins_data_set)
          
          output$TextVar <- renderUI({
            selectInput("textField", label = "Select Text Field for Topic Model:",  c("--select--", Column_names))
          })
          
          output$MaxTopicVar <- renderUI({
            sliderInput("maxTopicVar", label = "Select Maximum topics to choose optimum one:", min = 1,max = 100,value =50)
          })
          
          output$Key <- renderUI({
            fileInput('keyword', 'Choose Keyword CSV File',accept=c('.csv'))
          })
          
          output$runbutton <- renderUI({
            actionButton("run","Run")
          })
          
          incProgress(0.5, detail = " 50%")
          
          dataSummary <- reactive({
            
            validate(
              need(input$Validate != 0, "Please Upload Date & Validate")
            )
            
            isolate({
              summary<- Unique_NA_counts(ins_data_set)
              return(summary)
            })
          })
          
          incProgress(0.75, detail = " 75%")
          
          output$dataSummary<- DT::renderDataTable((datatable(dataSummary())),filter='top',options=list(autoWidth=TRUE))
          
          output$dataSummary_download <- renderUI({
            fluidRow(
              column(width=6,h4("Download Data Summary Table",style="color:darkgreen")),
              column(width=3,downloadButton('dataSummary_downloader',"Download Table"))
            )
          })
          
          output$dataSummary_downloader <- downloadHandler(
            filename = "DataSummary.csv",
            content = function(file) {
              write.csv(dataSummary(), file,row.names = F) })
          
          incProgress(1, detail = " 100%")
        })
      })
      
      observeEvent(input$run,{
        
        input$run
        
        withProgress(message = 'Processing...', value = 0, {
          
          incProgress(0.05, detail = " 5%")
          
          inFile <- input$dataset
          ins_data_set <- read.csv(inFile$datapath,header = T,strip.white = T,
                                   na.strings = c(""," ","NA","NULL","na","null"),fileEncoding = "latin1")
          Column_names <- colnames(ins_data_set)
          
          incProgress(0.1, detail = " 10%")
          
          output$runmodelbutton <- renderUI({
            actionButton("runmodel","RunModel")
          })
          
          output$OptimalTopicVar <- renderUI({
            sliderInput("OptTopicVar", label = "Choose optimum topic number:", min = 1,max = 100,value =30)
          })
          
          output$TopPatternVar <- renderUI({
            sliderInput("TopTerms", label = "Choose Top Terms you wish to see in topics:", min = 1,max = 200,value =30)
          })
          
          model_building <- reactive({
            
            validate(
              need(input$run != 0 & (!input$textField %in% c("")),"Please Upload Data & Run it")
            )
            isolate({
              withProgress(message = 'Processing...', value = 0, {
                
                inFileKey <- input$keyword
                if(!is.null(inFileKey))
                {
                  keywords <- read.csv(inFileKey$datapath,header = T,strip.white = T,fileEncoding = "latin1")
                  keywords <- as.character( keywords[,1])
                  keywords <- c(stopwords("english"), keywords)
                  Keywordstoremove <- as.data.frame(keywords)
                  colnames(Keywordstoremove) <- "Keyword"
                }
                else
                {
                  Keywordstoremove <- as.data.frame(stopwords("english"))
                  colnames(Keywordstoremove ) <- c("Keyword")
                }
                
                incProgress(0.1, detail = " 10%")
                total_rows <- nrow(ins_data_set)
                
                Textcolumn <- input$textField
                preproccessdata <- Text_Processing(ins_data_set[,which(colnames(ins_data_set)==Textcolumn)],Keywordstoremove$Keyword)
                
                incProgress(0.25, detail = " 25%")
                
                dtm_matrix <- get_dtm(preproccessdata)
                
                incProgress(0.5, detail = " 50%")
                
                topic_liklywood_chart <- get_optimal_topics(dtm_matrix[rowSums(dtm_matrix) > 0, ],input$maxTopicVar)
                
                incProgress(0.75, detail = " 75%")
                
                return(list(chart=topic_liklywood_chart$graph))
                
                incProgress(1, detail = " 100%")
                
              })
            })
          })
          
          output$topic_liklywood <- renderPlot({model_building()$chart})
          incProgress(1, detail = " 100%")
        })
      })
      
      observeEvent(input$runmodel,{
        
        input$runmodel
        
        withProgress(message = 'Processing...', value = 0, {
          
          incProgress(0.05, detail = " 5%")
          
          inFile <- input$dataset
          ins_data_set <- read.csv(inFile$datapath,header = T,strip.white = T,
                                   na.strings = c(""," ","NA","NULL","na","null"),fileEncoding = "latin1")
          Column_names <- colnames(ins_data_set)
          
          incProgress(0.1, detail = " 10%")
          
          
          output$Topic_download <- renderUI({
            fluidRow(
              column(width=6,h4("Download Data with Topic File",style="color:darkgreen")),
              column(width=3,downloadButton('dataTopic_download',"Download Data"))
            )
          })
          
          incProgress(1, detail = " 100%")
          
          model_running <- reactive({
            
            validate(
              need(input$runmodel != 0 & (!input$textField %in% c("")),"Please Upload Data & Run it")
            )
            isolate({
              withProgress(message = 'Processing...', value = 0, {
                
                inFileKey <- input$keyword
                if(!is.null(inFileKey))
                {
                  keywords <- read.csv(inFileKey$datapath,header = T,strip.white = T,fileEncoding = "latin1")
                  keywords <- as.character( keywords[,1])
                  keywords <- c(stopwords("english"), keywords)
                  Keywordstoremove <- as.data.frame(keywords)
                  colnames(Keywordstoremove) <- "Keyword"
                }
                else
                {
                  Keywordstoremove <- as.data.frame(stopwords("english"))
                  colnames(Keywordstoremove ) <- c("Keyword")
                }
                
                incProgress(0.1, detail = " 10%")
                total_rows <- nrow(ins_data_set)
                
                Textcolumn <- input$textField
                preproccessdata <- Text_Processing(ins_data_set[,which(colnames(ins_data_set)==Textcolumn)],Keywordstoremove$Keyword)
                
                incProgress(0.25, detail = " 25%")
                
                dtm_matrix <- get_dtm(preproccessdata)
                ins_data_set$TotalTerms <- rowSums(dtm_matrix)
                ins_data_set <- ins_data_set[ins_data_set$TotalTerms > 0,]
                
                incProgress(0.5, detail = " 50%")
                
                final_model <- run_TopicModel(input$OptTopicVar,dtm_matrix[rowSums(dtm_matrix) > 0,])
                
                incProgress(0.75, detail = " 75%")
                
                dataWithTopic <- append_Topics(final_model,ins_data_set)
                top_terms <- get_terms_model(final_model,input$TopTerms)
                top_terms <- as.data.frame(top_terms)
                
                return(list(dataset_topics=dataWithTopic,topic_top_terms=top_terms))
                
                incProgress(1, detail = " 100%")
                
              })
            })
          })
          
          output$dataTopic_download <- downloadHandler(
            filename = "DataWithTopics.csv",
            content = function(file) {
              write.csv(model_running()$dataset_topics, file,row.names = F) })
          
          output$topic_terms<- DT::renderDataTable((datatable(model_running()$topic_top_terms)),filter='top',options=list(scrollX = TRUE))
          
        })
      })
      
    }
  )
}