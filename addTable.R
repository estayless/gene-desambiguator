addTable<-function(gene,id , synonyms, rv, output, input){
  dtID <- paste0("DT",id)
  btnID <- paste0("rmv",id)
  if(length(synonyms[[gene]]$Symbol) == 0){
    stts <- "danger"
    mssg<-paste0("There are no synonyms for ",gene,".")
  }else{
    stts <- "success"
    mssg<-paste0("Synonyms for ",gene, ", select one.")
  }
  
  if (is.null(rv[[dtID]])) {
    
    insertUI(
      selector = "#placeholder",
      ui = tags$div(id = dtID,
            box(
              width = 12,
              title=span(icon("edit"), mssg),
              status = stts,
              collapsible = TRUE, collapsed = TRUE,
              style = 'overflow-x: scroll',
              DT::dataTableOutput(dtID),
              hr()
            )

            
          )
    )
    
    output[[dtID]] <- DT::renderDataTable(synonyms[[gene]],
                                           colnames = c('GeneID', 'Symbol', 'Synonyms', 'Description', 'Symbol FNA', 'Full name FNA', 'Other designations'),
                                           selection = list( mode = 'single'))
                                           #selection = list( mode = 'single', selected = 1 ))
    
    rv[[dtID]] <- TRUE
    
    observeEvent(input[[btnID]], {
      removeUI(selector = paste0("#", dtID))
      
      rv[[dtID]] <- NULL
      
    }, ignoreInit = TRUE, once = TRUE)
    
  } else {
    message("The table has already been created!")
  }
}
