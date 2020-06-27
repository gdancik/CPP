# initializeGeneTable


GeneTable <- read.csv("data/human_genes.csv")
GeneTable$SYMBOL <- as.character(GeneTable$SYMBOL)
GeneTable$lower <- tolower(GeneTable$SYMBOL)
dups <- (GeneTable %>% count(SYMBOL) %>% filter(n > 1))$SYMBOL
GeneTable <- GeneTable %>% filter(!SYMBOL %in% dups)
rownames(GeneTable) <- GeneTable$SYMBOL

o <- order(GeneTable$SYMBOL)
GeneTable <- GeneTable[o,]

geneSymbolToID <- function(symbols, GeneTable) {
  m <- match(symbols, GeneTable$SYMBOL)
  data.frame(Symbol = as.character(symbols), ID = as.character(GeneTable$GeneID)[m],
             stringsAsFactors = FALSE)
}

geneIDToSymbol <- function(id, GeneTable) {
    m <- match(id, GeneTable$GeneID)
    return(GeneTable$SYMBOL[m])
}

geneIDs <- GeneTable$GeneID
names(geneIDs) <- GeneTable$SYMBOL


id <- geneSymbolToID(CONFIG$DEFAULT.GENE, GeneTable)$ID

if (is.na(id)) {
   msg <- paste0("invalid default gene: ", CONFIG$DEFAULT.GENE)
   alert(msg)
   id <- 178
}

CONFIG$DEFAULT.GENE <- id
updateSelectizeInput(session, "geneInput", choices = geneIDs, 
                                          selected = id, server = TRUE)

