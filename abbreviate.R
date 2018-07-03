library(syuzhet)

## functions for abbreviating Mesh labels

# combines elements of a vector into a single string
# if vector is of length 2 (e.g., following a split based 
# on a comma), then reverse the 1st and 2nd element
switch12 <- function(x) {
  if (length(x) == 1) {
    return(x)
  } else if (length(x) == 2) {
    return(paste(x[2], x[1]))
  }
  return(paste(x, collapse = ", "))
}


useInitials <- function(x, threshold) {
  t <- get_tokens(x)
  if (length(t) > threshold) {
    t <- strsplit(t, "")
    x <- paste0(toupper(sapply(t, function(x)x[1])), collapse = "")
  }
  x
}

abbreviate2 <- function(x, threshold) {
  
  x <- gsub("^Neoplasm[,]|^Neoplasms[,]|Neoplasms$|Syndromes$", "", x)
  x <- gsub("Leukemia, Myeloid, Acute", "AML", x)
  
  s <- strsplit(x, ", ")
  
  s <- sapply(s, switch12)
  s <- sapply(s, useInitials, threshold)
  s
  
}

