library(shiny)
library(jsonlite)
library(tm)
library(wordcloud2)

shinyServer(function(input, output) {

  
  ######################################
  # Error message function
  ######################################
  printError <-function(msg, files) {
    print(msg)
    print(files)
  }
  
  #############################################################
  # Error handling function to parse documents using fromJSON
  # Errors result in a returned value of NA
  #############################################################
  tryJSON <- function(x) {
    t = try(fromJSON(x), silent = TRUE)
    if (class(t) == "try-error") {
      return (NA)
    }
    return (t)
  } 
  
  #############################################################
  # remove pure numeric strings from a vector
  #############################################################
  removePureNumbers <- function(x) {
    g = grep("^[+-]*[0-9]+$", x)
    if (length(g) == 0) {
      return(x)
    }
    x[-g]
  }
  
  #############################################################
  # Process files from file picker, stage for analysis
  #############################################################
  processFiles <- function(x) {
    
    # read in each file using scan
    docs = sapply(x$datapath, scan, what = "character", sep = "\n")
    
    # remove docs with "Service unavailable" errors
    errors = sapply(docs, length) > 1
    w=which(errors)
    docs = docs[-w]
    readnames = x$name[w]

    
    # convert each JSON formatted file to a list
    l = lapply(docs, tryJSON)
    
    # remove and report docs to DOM that could not be processed #
    keep = !is.na(l)
    l = l[keep]
    w = which(!keep)
    procnames = x$name[w]
    
    output$console = renderPrint({
      cat("Warning, the following files could not be read:", sep='\n')
      cat(readnames, sep = '\n')
      if(length(procnames) > 0){
        cat("Warning, the following files could not be processed:", sep='\n')
        cat(procnames, sep='\n')
      }
    })
    return(l)
  }
  
  #############################################################
  # Analyze processed files and display wordcloud
  #############################################################
  analyzeFiles <- function(l){
    # extract the abstract ('text') element to get a list of abstracts
    abstracts = sapply(l, function(x)x$text)
    
    # get a list, with each element a vector of words from an abstract
    words = lapply(abstracts, removePunctuation,preserve_intra_word_dashes = TRUE )
    words = lapply(words, stripWhitespace)
    words = lapply(words, tolower)
    words = unlist(words)
    words = strsplit(words, " ")
    words = lapply(words, unlist)
    words = lapply(words, removePureNumbers)
    
    # get a list, with each element a vector of stem words for each abstract
    #stems = lapply(words, hunspell_stem)
    stems = lapply(words, stemDocument)
    stems = lapply(stems, unique) 
    
    # summarize the stem words across all abstracts via a frequency table
    stem.summary = sort(table(unlist(stems)), decreasing = TRUE)
    head(stem.summary) # this includes 'stop words'
    
    # read in stop words, and remove these from the results #
    stopwords = as.character(read.delim("stopwords.txt")[[1]])
    keep = !names(stem.summary)%in%stopwords
    
    # label and display results
    results <- as.data.frame(stem.summary[keep])
    colnames(results) <- c("word", "freq")
    head(results)
    
    # generate wordcloud, using top 30 results
    m = min(30, nrow(results))
    
    output$wordCloud <- renderWordcloud2({
      wordcloud2(results[1:m,], size = input$size)
      
    })
  }
  
  #reactive value to grab return value from event observer
  processedFiles = reactiveValues(dat = NULL)
  #event observer listening to fileInput state change, calls processing function
  observeEvent(input$fileSelect, processedFiles$dat <- processFiles(input$fileSelect))
  #event observer listening to "Analyze" button, calls analysis function
  observeEvent(input$analyzeFiles, analyzeFiles(processedFiles$dat))
  
  
})
