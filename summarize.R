library(RMySQL)


con = dbConnect(MySQL(), group = "CPP")

# count number of articles in CPP
numArticles <- dbGetQuery(con, "select count(distinct PMID) from PubGene;")
numGenes <- dbGetQuery(con, "select count(distinct GeneID) from PubGene;")
numCancerArticles <- dbGetQuery(con, "select count(distinct PMID) from PubMesh inner join MeshTerms ON PubMesh.MeshID = MeshTerms.MeshID where
	MeshTerms.TreeID LIKE \"C04.%\";")

numCancerGenes <- dbGetQuery(con, "select count(distinct PubGene.GeneID) from PubGene inner join 
                                    ( select distinct (PMID) from PubMesh inner join 
                                  MeshTerms ON PubMesh.MeshID = MeshTerms.MeshID 
                                  where MeshTerms.TreeID LIKE \"C04.%\") as TT ON PubGene.PMID = TT.PMID;")


plotCounts <- function(geneCounts) {

  colnames(geneCounts) <- c("Symbol", "GeneID", "Frequency")
  geneCounts$Symbol <- gsub("\r", "", geneCounts$Symbol)

  levels <- geneCounts$Symbol
  geneCounts$Symbol <- factor(geneCounts$Symbol, levels = levels)

  ggplot(data = geneCounts, aes(Symbol, Frequency, fill = Symbol)) + 
    geom_col() + #coord_flip() + 
    theme_linedraw() +
    theme(legend.position = "none", axis.text.x = element_text(face = "bold", angle = 45, hjust = 1), 
    axis.text.y = element_text(face = "bold"), axis.title = element_text(face = "bold")) +
    ylab("Number of articles") + xlab("")
}

# plot the top 20 genes
qry <- paste("select Genes.SYMBOL, PubGene.GeneID, count(PubGene.GeneID) from PubGene", 
             "inner join Genes ON Genes.GeneID = PubGene.GeneID",
             "Group By GeneID order by count(GeneID) desc limit 20;")

geneCounts <- dbGetQuery(con, qry)
plotCounts(geneCounts)

# plot the top 20 cancer-related genes


qry <- paste("select Genes.SYMBOL, PubGene.GeneID, count(PubGene.GeneID) from PubGene",
             "inner join",
             "( select distinct (PMID) from PubMesh inner join MeshTerms ON PubMesh.MeshID = MeshTerms.MeshID where",
             "MeshTerms.TreeID LIKE \"C04.%\") as TT ON PubGene.PMID = TT.PMID",
             "inner join Genes ON Genes.GeneID = PubGene.GeneID",
             "Group By GeneID order by count(GeneID) desc limit 20;")
geneCancerCounts <- dbGetQuery(con, qry)
plotCounts(geneCancerCounts)

dbDisconnect(con)
