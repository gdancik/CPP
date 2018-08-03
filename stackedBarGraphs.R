##################################################################
# update chemical stacked bar graph
##################################################################

source("abbreviate.R", local = TRUE)

getStackedResults <- function(sql_function, group) {

  if (is.null(diseaseSummary$dat)) {
    return(NULL)
  }

  con = dbConnect(MariaDB(), group = "CPP")
  
  cat("warning: NO FILTERS ARE APPLIED to Chemical stacked bar graph\n")
  res <- sql_function(pmidList$pmids$PMID, con)
  dbDisconnect(con)
  
  if (nrow(res) == 0) {
    return(NULL)
  }
  
  
  
  # keep results for most frequent diseases
  t <- sort(table(res$Disease), decreasing = TRUE)
  numDiseases <- min(10, length(t))
  res2 <- filter(res, Disease %in% names(t)[1:numDiseases])
  
  res2$Frequency <- as.double(res2$Frequency)
  
  # keep results for most frequent groups
  s <- split(res2$Frequency, res2[[group]])
  s <- sapply(s, sum)
  s <- sort(s, decreasing = TRUE)
  
  numGroups <- min(15, length(s))
  keepThese <- names(s)[1:numGroups]
  res2[[group]][!res2[[group]]%in%keepThese] <- NA
  
  res2 <- filter(res2, res2[[group]] %in% keepThese)
  
  
  #res2[[group]] <- abbreviate(res2[[group]], minlength = 10)
  
  #re-order bars
  sr <- split(res2$Frequency, res2$Disease)
  st <- sort(sapply(sr, sum))
  res2$Disease <- factor(res2$Disease, levels = names(st)[order(st)])
  res2
  
}


observe({
  cat("in chemical stacked bar observe...\n")
   res2 <- getStackedResults(getChemByDiseaseContingency, "Chemical")
    
   if (is.null(res2)) {
     return()
   }
   
   output$chemGraph <- renderPlotly({
      stackedBarGraph(res2, "Disease", "Frequency", "Chemical", "Distribution of drug mentions by cancer (max 10 cancers and 15 drugs)",
                      "Number of chemical mentions")
    })
})


observe({
  cat("in mut stacked bar observe...\n")
  res2 <- getStackedResults(getMutByDiseaseContingency, "Mutation")
  
  if (is.null(res2)) {
    return()
  }
  
  output$mutGraph <- renderPlotly({
    stackedBarGraph(res2, "Disease", "Frequency", "Mutation", "Distribution of mutations by cancer (max 10 cancers and 15 drugs)",
                    "Number of mutation mentions", abbreviate = FALSE)
  })
})






# generates a stacked bar graph using ggplotly with specified data frame (res2)
# res2 must be data.frame with columns corresponding to x,fill, and y
stackedBarGraph <- function(res2, xname, yname, fillName, title, ylab, abbreviate = TRUE) {
  
  str <- paste0("gg <- ggplot(res2, aes(x=", xname, ", y = ", yname, ", fill = ", fillName, "))")
  eval(parse(text = str))
  
  #gg <- ggplot(res2, aes(x=x, y = y, fill = fill)) + 
  gg <- gg +  geom_bar(stat = "identity") + coord_flip() +
    theme(plot.title = element_text(face = "bold"),  
          axis.title = element_text(face = "bold"),
          axis.text = element_text(face = "bold")) +
    ggtitle(title) +
    xlab("") + ylab(ylab) +
    theme_linedraw() + scale_x_discrete(label=function(x) abbreviate2(x, 2))
  
  #scale_x_discrete(labels = function(x) str_wrap(x, width = 8)) 
  
  p <- ggplotly(gg, tooltip = c("x", "fill", "y"))
  #p <- ggplotly(gg, tooltip = "all")
  return(p %>% config(displayModeBar = F) %>% layout(xaxis=list(fixedrange=TRUE)) %>% layout(yaxis=list(fixedrange=TRUE)))
}


