# initializeGeneTable


GeneTable <- read.csv("data/human_genes.csv")
GeneTable$SYMBOL <- as.character(GeneTable$SYMBOL)

dups <- (GeneTable %>% count(SYMBOL) %>% filter(n > 1))$SYMBOL
GeneTable <- GeneTable %>% filter(!SYMBOL %in% dups)
rownames(GeneTable) <- GeneTable$SYMBOL

geneSymbolToID <- function(symbols, GeneTable) {
  symbols <- gsub("\r", "", symbols)
  m <- match(symbols, GeneTable$SYMBOL)
  data.frame(Symbol = as.character(symbols), ID = as.character(GeneTable$GeneID)[m],
             stringsAsFactors = FALSE)
}


geneIDs <- GeneTable$GeneID
names(geneIDs) <- GeneTable$SYMBOL

updateSelectizeInput(session, "geneInput", choices = geneIDs, selected = 178, server = TRUE)
