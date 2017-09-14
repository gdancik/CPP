library(shiny)
library(jsonlite)
library(tm)
library(wordcloud2)

AUTO.READ <- "/Users/dancikg/RESEARCH/research_easternct/work/GenePubViewer/DCAST/Github/DCAST/doc/"
#AUTO.READ = NULL

if (!exists("AUTO.READ")) {
  AUTO.READ = NULL
}

shinyServer(function(input, output, session) {

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
    readnames = integer(0)
    if (length(w) > 0) {
        docs = docs[-w]
        readnames = x$name[w]
    }
    
    # convert each JSON formatted file to a list
    l = lapply(docs, tryJSON)
    
    # remove and report docs to DOM that could not be processed #
    keep = !is.na(l)
    l = l[keep]
    w = which(!keep)
    procnames = x$name[w]
    
    output$console = renderPrint({
      
      if (!is.null(AUTO.READ)) {
        cat("files automatically read from: ")
        cat(AUTO.READ, "\n")
      }
      
      if(length(readnames) > 0){
        cat("Warning, the following files could not be read:", sep='\n')
        cat(readnames, sep = '\n')
      }
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
  analyzeFiles <- function(){
    print("In analyzeFiles")
    l <- processedFiles$dat
    save(l, file = "l.RData")
    
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
   
    print ("got words") 
    # get a list, with each element a vector of stem words for each abstract
    #stems = lapply(words, hunspell_stem)
    stems = lapply(words, stemDocument)
    stems = lapply(stems, unique) 
    
    print ("got stems")
    # summarize the stem words across all abstracts via a frequency table
    stem.summary = sort(table(unlist(stems)), decreasing = TRUE)
    head(stem.summary) # this includes 'stop words'
    
    print("read stop words")
    # read in stop words, and remove these from the results #
    stopwords = as.character(read.delim("stopwords.txt")[[1]])
    keep = !names(stem.summary)%in%stopwords
    
    # label and display results
    results <- as.data.frame(stem.summary[keep])
    save(results,stems, stopwords, file = "results.RData")

    colnames(results) <- c("word", "freq")
    print(head(results))
    
    # generate wordcloud, using top 30 results
    m = min(30, nrow(results))
    
    # create term document matrix, after getting stems
    x = sapply(stems, paste0, collapse = " ")
    v = VectorSource(x)
    corpus = SimpleCorpus(v)
    
    dm = DocumentTermMatrix(corpus, control = list(stopwords = stopwords))
    
    # keep terms appearing in <10% of documents (we don't care about rare words)
    dm = removeSparseTerms(dm, 0.9)
    
    # convert to matrix to display top results
    m = as.matrix(dm)
    
    # find the top words
    counts = colSums(m)
    topwords = sort(counts, decreasing =  TRUE)
    topwords[1:3]
    
    # generate distance matrix
    d_m = dist(m, method = "euclidean")
    
    #convert dist matrix to hierarchical cluster
    hc_m = hclust(d_m, method = "complete")
    groups = cutree(hc_m, k=2)
    
    #find top words for each cluster
    cluster1 = groups[groups == 1]
    cluster2 = groups[groups == 2]
    
    m1 = m[names(cluster1), , drop = FALSE]
    m2 = m[names(cluster2), , drop = FALSE]
    
    
    save(m1, m2, file = "results.RData")
    
    # documents are in rows, and terms are in columns, so count up column by column to get the
    # frequency for each word
    get.top.words <-function(x) {
      counts = colSums(x)
      counts = sort(counts, decreasing = TRUE)
      counts
    }
    
    topwords1 = get.top.words(m1)
    topwords2 = get.top.words(m2)
    
    topwords1 = data.frame(topwords1)
    topwords2 = data.frame(topwords2)
    
    ###################################################
    ## old code
    #topwords1 = apply(m1, 1, which.max)
    #topwords1 = sort(topwords1, decreasing = TRUE)
    #topwords2 = apply(m2, 1, which.max)
    #topwords2 = sort(topwords2, decreasing = TRUE)
    #names(topwords1) = colnames(m1[names(topwords1), topwords1])
    #names(topwords2) = colnames(m2[names(topwords2), topwords2])
    
    #topwords1 = as.table(topwords1)
    #topwords2 = as.table(topwords2)
    ###################################################
    
    print("got topwords 1 and 2")
  
      
    #plot cluster tables
    output$cluster1Table <- renderTable(topwords1, rownames = TRUE)
    output$cluster2Table <- renderTable(topwords2, rownames = TRUE)
    
    #plot hclust with cutree borders
    output$clusters <- renderPlot({
      plot(hc_m, labels = FALSE)
      groups = cutree(hc_m, k=2)
      rect.hclust(hc_m, k=2, border="red")
    })
    
    shinyjs::enable("btnViewClusters")
    toggleModal(session, "clusterModal")
    
  }
  
  #reactive value to grab return value from event observer
  processedFiles = reactiveValues(dat = NULL)
  #event observer listening to fileInput state change, calls processing function
  
  observe({
  
    if (!is.null(AUTO.READ)) {
      files = list(datapath = Sys.glob(paste0(AUTO.READ, "/*")))
      processedFiles$dat <- processFiles(files)
    }  
    
  })
  
    observeEvent(input$fileSelect, {
      processedFiles$dat <- processFiles(input$fileSelect)
        AUTO.READ = NULL
    })
    
    #event observer listening to "Analyze" button, calls analysis function
    observeEvent(input$analyzeFiles, analyzeFiles())
  
  
})
