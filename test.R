library(jsonlite)
library(hunspell)
library(wordcloud2)

files = Sys.glob("doc/file_*")

# read in each file using scan
docs = sapply(files, scan, what = "character", sep = "\n")

# convert each JSON formatted file to a list
l = lapply(docs, fromJSON)

# extract the abstract ('text') element to get a list of abstracts
abstracts = sapply(l, function(x)x$text)

# get a list, with each element a vector of words from an abstract
words = lapply(abstracts, hunspell_parse)
words = lapply(words, unique)
words = lapply(words, unlist)

# get a list, with each element a vector of stem words for each abstract
stems = lapply(words, hunspell_stem)
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
w = wordcloud2(results[1:m,], size = .5)
print(w)
