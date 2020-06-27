#######################################################################################
# queries DB for summaries and sets the drop down results
#######################################################################################

source('sql_functions.R')
####################################################################################################
# calls 'query_function' to query database
####################################################################################################
getSummaries <- function(msg, con, query_function, pmids, session, resTable, selectID = NULL, ...) {
  #cat("getSummaries, msg: ", msg, "\n")
  #scan(what=character())
  
  shinyjs::html("bar-text", paste0("Retrieving ", msg, ", please wait..."))
  shinyjs::show('progress-bar-results')
  resTable$dat <- query_function(pmids, con, ...)
  shinyjs::hide('progress-bar-results')
}





