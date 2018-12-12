#######################################################################################
# queries DB for summaries and sets the drop down results
#######################################################################################

####################################################################################################
# calls 'query_function' to query database
####################################################################################################
getSummaries <- function(msg, con, query_function, pmids, session, resTable, selectID = NULL, ...) {
  #cat("getSummaries, msg: ", msg, "\n")
  #scan(what=character())
  shinyjs::html("bar-text", paste0("Retrieving ", msg, ", please wait..."))
  resTable$dat <- query_function(pmids, con, ...)

}

# updates the selectInput with given id, choices (ids), and terms;
# if choices is NULL, disable and set choices to "NONE"
setSelectInput <- function(session, id, choices, terms) {
  if (is.null(choices)) {
    updateSelectInput(session, id, choices = "NONE")
    shinyjs::disable(id)
  } else {
    if (!is.null(terms)) {
      names(choices) <- terms
    }
    
    updateSelectInput(session, id, choices = choices,
                      selected = choices)
    shinyjs::enable(id)
  }
  
}

# sets unique disease results and updates drop down
setGeneResults <-function(session, res, geneSummary) {
  cat("setting gene results...")
  
  isolate({
    geneSummary$dat <- res
    setSelectInput(session, "filterGenes", geneSummary$selectedTerm, geneSummary$selectedTerm)
    
  })
}
