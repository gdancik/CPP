
# queries mesh terms - currently stores results in 
# diseaseSummary$dat and diseaseSummary$uniqueDat
retrieveDiseases <- function(meshID, limit) {
  con = dbConnect(MySQL(), dbname = "dcast", user = "root", password = "password")
  
  # query MeSH terms
  query = paste("SELECT count(MeshTerms.MeshID), MeshTerms.MeshID, MeshTerms.Term, MeshTerms.TreeID from MeshTerms")
  
  query <- paste(query, "INNER JOIN PubMesh ON MeshTerms.MeshID = PubMesh.MeshID",
                 "INNER JOIN PubGene ON PubMesh.PMID = PubGene.PMID",
                 "WHERE PubGene.GeneID = ", input$geneInput)
  
  if (!is.null(meshID)) {
    query <- paste0(query, " AND MeshTerms.MeshID = ", meshID)
  }
  
  if (limit == "cancer") {
    query <- paste(query, "AND TreeID LIKE 'C04.%'")
  } else if (limit != "none") {
    stop("limit of ", limit, " is not implemented")
  }
  
  query <- paste(query,  "GROUP BY MeshTerms.MeshID, MeshTerms.Term, MeshTerms.TreeID",
                 "ORDER BY count(MeshTerms.MeshID) desc")
  print(query)
  diseaseSummary$dat <- dbGetQuery(con, query)
  dbDisconnect(con)
  
  if (!is.null(diseaseSummary$dat)) {
    colnames(diseaseSummary$dat)[1] = "Frequency"
  }
  
  rownames(diseaseSummary$dat) = NULL
  
  setDiseaseResults(session, unique(diseaseSummary$dat[,1:3]), diseaseSummary)
  
}

retrieveChemicals <- function(meshID) {
  
  cat("\n\nGETTING CHEMICALS!!\n\n")
  con = dbConnect(MySQL(), dbname = "dcast", user = "root", password = "password")
  
  # query MeSH terms
  query = paste("SELECT count(MeshTerms.MeshID), MeshTerms.MeshID, MeshTerms.Term, MeshTerms.TreeID from MeshTerms")
  
  query <- paste(query, "INNER JOIN PubChem ON MeshTerms.MeshID = PubChem.MeshID",
                 "INNER JOIN PubGene ON PubChem.PMID = PubGene.PMID",
                 "WHERE PubGene.GeneID = ", input$geneInput)
  
  if (!is.null(meshID)) {
    query <- paste0(query, " AND MeshTerms.MeshID = ", meshID)
  }
  
  query <- paste(query,  "GROUP BY MeshTerms.MeshID, MeshTerms.Term, MeshTerms.TreeID",
                 "ORDER BY count(MeshTerms.MeshID) desc")
  print(query)
  chemSummary$dat <- dbGetQuery(con, query)
  dbDisconnect(con)
  
  if (!is.null(chemSummary$dat)) {
    colnames(chemSummary$dat)[1] = "Frequency"
  }
  
  rownames(chemSummary$dat) = NULL
  
  setChemResults(session, unique(chemSummary$dat[,1:3]), chemSummary)
  
  
} # end retrieveChemicals




# queries mesh terms - currently stores results in 
# geneSummary$dat 
retrieveGenes <- function() {
  con = dbConnect(MySQL(), dbname = "dcast", user = "root", password = "password")
  
  # query MeSH terms
  query = paste("SELECT count(MeshTerms.MeshID), MeshTerms.MeshID, MeshTerms.Term, MeshTerms.TreeID from MeshTerms")
  
  query <- paste("select count(g.SYMBOL) as Freqency, g.SYMBOL as Symbol from Genes g",
                 "INNER JOIN PubGene p on g.GeneID = p.GeneID",
                 "INNER JOIN PubGene p2 ON p.PMID = p2.PMID",
                 "where p2.GeneID = ", input$geneInput, 
                 "group by g.SYMBOL",
                 "order by count(g.SYMBOL) desc;")
  
  print(query)
  geneSummary$dat <- dbGetQuery(con, query)
  setGeneResults(session, geneSummary$dat, geneSummary)
  
  dbDisconnect(con)
  
}



# retrieve articles based on diseaseMeshID OR chemMeshID OR geneID2 (only 1 will be applied)
# if no IDs are specified we do not retreive articles
retrieveArticles <- function(diseaseMeshID = NULL, chemMeshID = NULL, geneID2 = NULL) {
  
  cat("\nRETREIVING ARTICLES ---->\n\n")
  
  if (is.null(input$geneInput) | input$geneInput=="") {
    return()
  }
  con = dbConnect(MySQL(), dbname = "dcast", user = "root", password = "password")
  
  # gene selection: retrieve articles containing both genes
  if (!is.null(geneID2)) {
    
    query <- paste("SELECT PMID from PubGene",
                   "where GeneID= ", input$geneInput, " or GeneID = ", geneID2, 
                   "GROUP BY PMID HAVING count(PMID) > 1"
    )
  } 
  # if meshID selected, retreive all articles with selected gene and meshID
  else if (!is.null(diseaseMeshID)) {
    
    # query PMIDs
    query = paste("SELECT distinct(PubGene.PMID) from PubGene", 
                  "INNER JOIN PubMesh ON PubGene.PMID = PubMesh.PMID",
                  "WHERE PubGene.GeneID = ", input$geneInput)
    query <- paste0(query, " AND MeshID = ", diseaseMeshID)
  }
  else if (!is.null(chemMeshID)) {
    
    query = paste("SELECT distinct(PubGene.PMID) from PubGene", 
                  "INNER JOIN PubChem ON PubGene.PMID = PubChem.PMID",
                  "WHERE PubGene.GeneID = ", input$geneInput)
    query <- paste0(query, " AND MeshID = ", chemMeshID)
  } else {
    # return; but we could retrieve all articles with no filters
    dbDisconnect(con)
    return()
  }
  
  query <- paste(query, " ORDER BY PubGene.PMID DESC;")
  
  
  print("pmid query: ")
  print(query)
  pmids <- dbGetQuery(con, query)
  dbDisconnect(con)
  rownames(pmids) <- NULL
  pmidList$pmids <- pmids
  
}



# returns a vector of PMIDs from where table.idType matches all ids.
# optionally, we can constrain analysis to vector of pmids

getPMIDs <- function(tblName, idType, ids, con, pmids = NULL) {
  
  tblPMID <- paste0(tblName,".PMID")
  
  str <- paste0("SELECT ", tblPMID, " FROM ", tblName, "\nWHERE ")
  if (!is.null(pmids)) {
    str <- paste0(str, tblName,".PMID IN ", 
                  "(", paste0("'",pmids,"'", collapse = ","), ") AND ")
  }              
                
  str <- paste0(str, tblName,".",idType)

  if (length(ids) == 1) {
    str <- paste0(str, " = ", paste0("'",ids,"'"), ";")
  } else {
    str <- paste0(str, " IN ",
         paste0("(",paste0("'",ids,"'", collapse = ","), ")\n"),
         "GROUP BY ", tblPMID, " having COUNT(", tblPMID, ") >= ", length(ids), ";")
  }
  
  #cat("submit the query: \n", str, "\n")
  
  dbGetQuery(con, str)
         
}


# returns Mesh Summary table for vector of pmids
getMeshSummaryByPMIDs <- function(pmids, con) {
  
  pmids <- paste0("'",pmids,"'", collapse = ",")
  
  str <- paste0("select count(TT.MeshID) as Frequency, TT.MeshID, TT.Term from\n",
  "(select PubMesh.PMID, MeshTerms.MeshID, MeshTerms.Term from PubMesh\n",
    "inner join MeshTerms ON MeshTerms.MeshID = PubMesh.MeshID\n",
    "where PubMesh.PMID IN ", paste0("(", pmids, ")\n"),
    "group by PubMesh.PMID, MeshTerms.MeshID, MeshTerms.Term) as TT\n",
  "group by TT.MeshID, TT.Term ORDER BY count(TT.MeshID) desc;")
  
  dbGetQuery(con, str)

}



# returns Chem Summary table for vector of pmids
getChemSummaryByPMIDs <- function(pmids, con) {
  
  pmids <- paste0("'",pmids,"'", collapse = ",")
  
  str <- paste0("select count(TT.MeshID) as Frequency, TT.MeshID, TT.Term from\n",
                "(select PubChem.PMID, MeshTerms.MeshID, MeshTerms.Term from PubChem\n",
                "inner join MeshTerms ON MeshTerms.MeshID = PubChem.MeshID\n",
                "where PubChem.PMID IN ", paste0("(", pmids, ")\n"),
                "group by PubChem.PMID, MeshTerms.MeshID, MeshTerms.Term) as TT\n",
                "group by TT.MeshID, TT.Term ORDER BY count(TT.MeshID) desc;")
  
  dbGetQuery(con, str)
}

# returns gene summary table for vector of pmids
getGeneSummaryByPMIDs <- function(pmids, con) {
  
  pmids <- paste0("'",pmids,"'", collapse = ",")
  
  str <- paste0("select count(TT.GeneID) as Frequency, TT.SYMBOL as Symbol FROM\n",
  "(select PubGene.PMID, Genes.GeneID, Genes.SYMBOL from Genes\n",
    "inner join PubGene ON PubGene.GeneID = Genes.GeneID\n",
    "where PubGene.PMID IN ", paste0("(", pmids, ")\n"),
    "group by PubGene.PMID, Genes.GeneID, Genes.SYMBOL) as TT\n",
  "group by TT.GeneID, TT.SYMBOL order by count(TT.GeneID) desc;")
  
  dbGetQuery(con, str)
  
}
