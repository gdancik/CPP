

# updates the selectInput with given id, choices, and selected;
# if choices is NULL, disable and set choices to "NONE"
setSelectInput <- function(session, id, choices, selected) {
  if (is.null(choices)) {
    updateSelectInput(session, id, choices = "NONE")
    shinyjs::disable(id)
  } else {
    updateSelectInput(session, id, choices = choices,
                      selected = selected)
    shinyjs::enable(id)
  }
  
}

####################################################################################################
# calls 'query_function' to query database, then sets the input
####################################################################################################
getSummaries <- function(msg, con, query_function, pmids, session, resTable, selectID = NULL, ...) {
  #cat("getSummaries, msg: ", msg, "\n")
  #scan(what=character())
  shinyjs::html("bar-text", paste0("Retrieving ", msg, ", please wait..."))
  resTable$dat <- query_function(pmids, con, ...)
  
  isolate({
    # cat("done\n")
    choices <- resTable$selectedID
    if (!is.null(choices)) {
      names(choices) <- resTable$selectedTerm
    }
    if (!is.null(selectID)) {  
        setSelectInput(session, selectID, choices, resTable$selectedID)
    }
  })
}


# sets unique disease results and updates drop down
setGeneResults <-function(session, res, geneSummary) {
  #cat("setting gene results...")
  
  isolate({
    geneSummary$dat <- res
   # cat("done\n")
  
    setSelectInput(session, "filterGenes", geneSummary$selectedTerm, geneSummary$selectedTerm)
    
  })
}
