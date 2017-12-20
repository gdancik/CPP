######################################################################################
# gets the associations between terms and clusters for given document-term 
# matrix (dm) and cluster vector containing cluster numbers
# returns a data.frame with terms as row names and the following columns: 
#      p.value - the p-value from the statistical analysis
#      cluster - the cluster most associated with the term
#      proportion - the proportion of documents in this cluster containing the term
######################################################################################
getAssociations <- function(dm, groups) {
  
  ## make sure group names match (probably not necessary)
  m = match(names(groups), rownames(dm))
  dm = dm[m,]
  
  # convert document matrix to a matrix
  m = as.matrix(dm)
  
  # some words occur multiple times in an abstract, we only care if the word occurs
  # or the word does not
  m = m > 0
  
  # convert document matrix to a data.frame (needed for splitting by cluster)
  m = as.data.frame(m)
  
  # create list of data.frames for each cluster
  dm_by_cluster = split(m, groups)
  
  # get the number of documents in each cluster
  n = sapply(dm_by_cluster, nrow)
  
  counts_by_cluster = lapply(dm_by_cluster, colSums)
  
  # creates matrix of word counts per cluster, with terms in 
  # rows and clusters in columns
  wordCountsByCluster = sapply(counts_by_cluster, function(x)x)
  
  # get association information for each term
  associations1 = apply(wordCountsByCluster, 1, clusterAssociation, n = n)
  
  # convert to a data.frame with terms in rows
  associations = NULL
  for (a in associations1) {
    associations = rbind(associations, a)
  }
  rownames(associations) = names(associations1)
  
  return(associations)
}


######################################################################################
# determine whether a term is associated with one or more clusters for x = vector 
# of word counts for each cluster and n = vector of total documents / cluster
# returns a data.frame with
#      p.value - the p-value from the statistical analysis
#      cluster - the cluster most associated with the term
#      proportion - the proportion of documents in this cluster containing the term
######################################################################################
clusterAssociation <- function(x, n) {
  
  x=rbind(x,n-x)
  
  props = prop.table(x, 2)[1,]
  
  # get the p-value from chi-square test
  p.value = fisher.test(x)$p.value
  
  # which cluster is the term associated with?
  w = which.max(props)
  maxCluster = as.character(names(w))
  data.frame(p.value = p.value, cluster = maxCluster, proportion = props[w], stringsAsFactors = FALSE)
}


######################################################################################
# Small function to return a list of flattened TreeIDs for retrieving neoplasms by site.
# It accepts a list of lists of TreeIDs strsplit by ".", and returns a list of the first
# three elements joined by ".".
######################################################################################
flattenTree <- function(x){
  if(length(x) > 2){
    x = paste(x[1:3], collapse = ".")
  }
  return(x)
}

######################################################################################
# Function to FOIL (generate each branch combination) MeSH Tree IDs. This ensures that
# all Tree IDs are enumerated if there are orphan branches. Accepts a vector.
######################################################################################
foilID <- function(x){
  v = NULL
  for(i in 1:length(x)){
    id = paste0(x[1:i], collapse = ".")
    v = c(v, id)
  }
  return(v)
}

######################################################################################
# Function that accepts a vector of pre-formatted MeSH Tree IDs from formatMesh() and
# returns each vector indented with a non-blank-space for each branch.
######################################################################################
indentMesh <- function(x){
  indent = rep("&nbsp;&nbsp;", length(x)-1) %>% paste0(collapse = " ")
  
  litem = paste0(x, collapse = ".")
  litem = paste0(indent, litem, collapse="")
  
  return(litem)
}

######################################################################################
# Function that accepts a vector of list items and formats them into an HTML unordered
# list. Returns an HTML string.
######################################################################################
listMesh <- function(x){
  litems = paste0("<li>", x, "</li>", collapse = "")
  ulist = paste0("<ul>", litems, "</ul>")
  
  return(ulist)
}


######################################################################################
# Function to format MeSH Tree IDs for display. Accepts a vector of Tree IDs, and their count, returns
# a vector of Tree IDs by count.
# Behavior: split list by period, generate each possible branch combination, append to
#           vector, generate table, format with count for each branch.
######################################################################################
displayMesh <- function(x, y){
  
  # Add correct amount of TreeID strings specified by the count in the MeSH
  # table.
  x = mapply(rep, x, y) %>% unlist()
  
  splitIDs = strsplit(x, "\\.")
  
  idTable = lapply(splitIDs, foilID) %>%
    unlist() %>% table()
  
  # query the Tree IDs (columns of idTable) to get the MesH terms and
  # add terms to the output
  
  idCount = paste0("(", idTable, ") ", names(idTable))
  
  idIndent = strsplit(idCount, "\\.") %>% 
    lapply(indentMesh) %>% unlist()
  
  idDisplay = listMesh(idIndent)
  
  return(idDisplay)
  
}