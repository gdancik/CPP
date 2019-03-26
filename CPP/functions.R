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
  return(indent)
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
  
  return(paste0(x, "</br>"))
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
  
  con = dbConnect(MariaDB(), dbname = "dcast", user = "root", password = "password")
  
  ids <- paste0("'",names(idTable), "'",collapse = ",")
  query = paste0("SELECT * from MeshTerms WHERE TreeID in (", ids, ")")
  
  res <- dbGetQuery(con, query)
  dbDisconnect(con)
  
  spaces = strsplit(names(idTable), "\\.") %>% 
    lapply(indentMesh) %>% unlist()
  
  m <- match(names(idTable), res$TreeID)
  idDisplay <- listMesh(paste0(spaces, "(", idTable, ")", res$Term[m], "(", names(idTable), ")"))
  
  return(idDisplay)
  
}
