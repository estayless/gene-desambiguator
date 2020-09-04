library(shiny)
library(tidyverse)
library(DT)
library(shinydashboardPlus)
library(shinydashboard)
library(shinyWidgets)
source("indexConn.R")
source("dictConn.R")
source("mammaliaSynonyms.R")
source("addTable.R")

header <- dashboardHeader(
  title = span(tagList(icon("dna"),icon("edit"), "Gene Disambiguator")),
  titleWidth = 300
)
sidebar <- dashboardSidebar(
  disable = TRUE
)
body <- dashboardBody(
  
  fluidRow(
    column(width = 3,
      box(width = 12,
        status = "primary",
        # Inpur: Select a specie
        selectInput(inputId = "specie", label = "Select a specie: ", choices = NULL),
        actionButton(inputId = "getSpecie",
                   label = "Get the specie db",
                   class = "btn-block",
                   icon = icon("cloud-download")
        ),
        tags$br(),
        tags$strong(textOutput("totalDefinitions")),
        
        # Horizontal line ----
        tags$hr(),      
        
        # Input: Select a file ----
        fileInput("file", "Choose a File",
                  multiple = FALSE,
                  accept = c("text/csv",
                             "text/comma-separated-values,text/plain",
                             ".csv")),

        p(strong("Header")),
        # Input: Checkbox if file has header ----
        materialSwitch(inputId = "header",
                       label = "",
                       value = TRUE,
                       right = TRUE,
                       status = "primary"),
        
        # Input: Select separator ----
        prettyRadioButtons(inputId = "sep",
                           label = "Separator",
                           choices = c(Comma = ",",
                                       Semicolon = ";",
                                       Tab = "\t"),
                           selected = ",",
                           status = "primary",
                           fill = TRUE),
        
        # Input: Select quotes ----
        prettyRadioButtons(inputId = "quote",
                           label = "Quote",
                           choices = c(None = "",
                                       "Double Quote" = '"',
                                       "Single Quote" = "'"),
                           selected = '"',
                           status = "primary",
                           fill = TRUE),
        
        tags$hr(),
        actionButton(inputId = "synonyms",
                   label = "Search synonyms",
                   class = "btn-block btn-outline-primary",
                   icon = icon("search")
                   ),
        tags$br(),tags$br(),
        tags$strong(textOutput("singleSynonyms")),
        
        tags$br(),tags$br(),
        actionButton(inputId = "disambiguate",
                   label = "Disambiguate",
                   class = "btn-block",
                   style="color: #fff; background-color: #3c8dbc; border-color: #2e6da4",
                   icon = icon("edit")
        ),
        tags$br(),
        downloadButton("download", label = "Download", class = "btn-block")
        
      )
    ),
    
    column(width = 9,
       box(width = 12,
         title = span(icon("table"), "File content"),
         status = "primary",
         collapsible = TRUE, collapsed = FALSE,
         div(style = 'overflow-x: scroll', DT::dataTableOutput("genesList")),
       ),
       
       box(width = 12,
           title = span(icon("edit"), "Disambiguated content"),
           status = "primary", solidHeader = TRUE,
           collapsible = TRUE, collapsed = FALSE,
           div(style = 'overflow-x: scroll', DT::dataTableOutput("ultimateList")),
       )
       
    )
  ),
  
  fluidRow(
    column(width = 12,
           tags$div(id = "placeholder"),
    )
  )
  
  
  
)



ui <- dashboardPage(
  #skin = "black",
  header = header,
  sidebar = sidebar,
  body = body
)

server <- function(input, output, session){
  
  
  indexList <- reactive({
    indexConnection <- indexConn()
    indexList <- indexConnection$find()
  })
  
  species <- reactive({
    speciesDb <- indexList()$tax_id
    speciesNames <- indexList()$tax_name
    names(speciesDb) <- speciesNames
    speciesDb
  })
  observe({
    showModal(modalDialog(title="Downloading available species", size = "s", footer=NULL))
    updateSelectInput(session=session, inputId="specie", choices = species())
    removeModal()
  })
  
  clusterNumber <- reactive({
    req(input$specie)
    tax <- subset(indexList(), tax_id %in% input$specie)
    tax$cluster_N
  })
  
  taxonInt <-reactive({
    taxonInt <- as.integer(input$specie)
  })
  
  specieTaxon <- reactive({
    collConn <- dictConn(clusterNumber())
    query <- paste0('[{"$match" : {"tax_id" :', taxonInt(),'}}]')
    specie <- as_tibble(collConn$aggregate(query))
  })
  
  numbers <- reactiveValues()
  
  observe({
    req(genes()$Gene)
    numbers[["ofGenes"]] <- length(genes()$Gene)
  })
  
  observeEvent(input$getSpecie,{
    showModal(modalDialog(title="Downloading gene definitions", size = "s", footer=NULL))
    numbers[["totalDefinitions"]]<-length(specieTaxon()$tax_id)
    removeModal()
  })
  
  output$totalDefinitions <- renderText({
    req(numbers$totalDefinitions)
    paste0(numbers$totalDefinitions," definitions found.")
  })
  
  genes <- reactive({
    req(input$file)
    tryCatch(
      {
        df <- read.delim(input$file$datapath,
                         header = input$header,
                         sep = input$sep,
                         quote = input$quote,
                         fill = TRUE)
      },
      error = function(e) {
        stop(safeError(e))
      }
    )
    names(df)[1] <- "Gene"
    i <- 1
    for(gene in df$Gene){
      cleanGene <- strsplit(gene, "!|#|%|$|&|(|)|=|?|¿|¡|°|¨|~|+|*|[|]|^|`|_")
      cleanGene <- sapply(cleanGene, paste, collapse = "")
      df[[1]][[i]] = cleanGene
      i<-i+1
    }
    df
  })
  
  output$genesList <- DT::renderDataTable({
    genes()
  })
  
  synonyms <- eventReactive(input$synonyms, {
    req(numbers$ofGenes)
    synonymsList <- vector(mode = "list", length = numbers$ofGenes)
    names(synonymsList) <- genes()$Gene
    numbers[["synonyms"]] <- 0
    
    # Create a Progress object
    progress <- shiny::Progress$new()
    # Make sure it closes when we exit this reactive, even if there's an error
    on.exit(progress$close())
    progress$set(message = "Searching synonyms for ", value = 0)
    
    for(gene in genes()$Gene){
      syns <- mammaliaSynonyms(gene,specieTaxon())
      synonymsList[[gene]] <- syns
      if(length(syns$GeneID) != 0){
        numbers$synonyms <- numbers$synonyms + 1
      }
      # Make progress
      progress$inc(1/numbers$ofGenes, detail = gene)
    }
    synonymsList
  })
  
  observe({
    synonyms()
  })
  
  output$singleSynonyms <- renderText({
    req(numbers$synonyms, numbers$ofGenes)
    paste0(numbers$synonyms, " out of ", numbers$ofGenes, " genes with synonyms.")
  })
  
  rv <- reactiveValues()
  
  observeEvent(synonyms(), {
    req(synonyms())
    id <- 1
    for(gene in genes()$Gene){
      addTable(gene, id, synonyms(), rv, output, input)
      id <- id+1
    }
  })
  
  observeEvent(c(genes(), input$specie, input$getSpecie), {
    rvList <- reactiveValuesToList(rv)
    for (i in 1:length(rvList)){
      dt <- paste0("DT",i)
      removeUI(selector = paste0("#", dt))
      rv[[dt]] <- NULL
    }
  })
  
  synDis <- eventReactive(input$disambiguate, {
    genesList <- vector(mode = "list", length = numbers$ofGenes)
    names(genesList) <- genes()$Gene
    id <- 1
    for(gene in genes()$Gene){
      wrd <- as.name(paste0("input$DT",id,"_rows_selected"))  
      data <- synonyms()[[gene]]
      genesList[[gene]] <- (data[eval(parse(text=wrd)), ]$Symbol)
      id<-id+1
    }
    genesList
  })
  
  observe({
    synDis()
  })
  
  ultimateList<-reactive({
    req(synDis())
    genesList <- vector(mode = "list", length = numbers$ofGenes)
    names(genesList) <- genes()$Gene
    genesOut <- genes()
    i <- 1
    for(gene in genes()$Gene){
      if(length(synDis()[[gene]]) != 0){
        if(!is.na(synDis()[[gene]])){
          genesOut[[1]][[i]] = synDis()[[gene]]
        }
        else{
          genesOut[[1]][[i]] = gene
        }
      }
      else{
        genesOut[[1]][[i]]=gene
      }
      i <- i+1
    }
    genesOut
  })
  
  output$download <- downloadHandler(
    filename = function() {
      paste("data-", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      write.csv(ultimateList(), file, row.names=FALSE)
    }
  )

  output$ultimateList<- DT::renderDataTable({
    ultimateList()
  })

  
}

shinyApp(ui, server)