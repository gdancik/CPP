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


# gets stat summaries for given resTable with res_id, and
# sets resTable$stat to result including Score, etc
getStatSummaries <- function(con, resTable, res_id, n, dbTableName) {

  catn("in getStatSummaries...")
  
  if (is.null(resTable$dat)) {
    resTable$stat <- NULL
    return(NULL)
  } else if (nrow(resTable$dat) == 0) {
    resTable$stat <- data.frame(lapply(1:11, function(x)as.integer()))
    colnames(resTable$stat) <- c("Score", "P.Value", "FDR",
                     "Frequency", "n", "Proportion",
                     colnames(resTable$dat)[2:3],
                     "Frequency_N", "N", "Proportion_N")  
    return(NULL)
  }
  
  catn("call query...")
  qry <- paste0('select * from ', dbTableName, ' where ', res_id, ' in ', cleanseList(resTable$dat[[res_id]]))
  
  # catn(qry)
  # wait()

  res <- dbGetQuery(con, qry)
  
  if (is.null(res) || nrow(res) == 0) {
    return(NULL)
  }
  
  
  catn("match dat with ", res_id)
  m <- match(resTable$dat[[res_id]], res[[res_id]])
  
  catn("done...")
  
  r <- resTable$dat %>% mutate(Frequency = as.integer(Frequency), n = n, 
                               Frequency_N = as.integer(res$Count[m]),
                           Proportion_N = Frequency_N / TOTAL_ARTICLES,
                           Proportion = Frequency / n,
                           N = TOTAL_ARTICLES)
  
  catn("done mutating...")
  b <- n - r$Frequency
  c <- TOTAL_ARTICLES - r$Frequency_N
  p <- rep(1, length(b))
  
  for (i in 1:length(p)) {
    p[i] <- phyper(r$Frequency[i]-1, n, c[i], r$Frequency_N[i], lower.tail = FALSE)
  }

  resTable$stat <- r %>% mutate(Score = Proportion/Proportion_N,
                                Proportion = round(Proportion,3), 
                                Proportion_N = round(Proportion_N, 3),
                                P.Value = round(p,2),
                                FDR = round(p.adjust(p, method = 'fdr'),2)
                              ) %>% mutate(Score = round(Score, 2))
  
  resTable$stat <- resTable$stat[,c(9,10, 11, 1,4,7,2,3,5,8,6)] %>% 
    arrange(P.Value, FDR, desc(Score))
  
  resTable$dat <- resTable$stat[,c(1,2,4,7:8)]
  
}



