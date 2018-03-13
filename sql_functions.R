
# returns a vector of PMIDs from the table (type) matching all IDs 
getPMIDs <- function(tblName, idType, ids, con) {
  
  tblPMID <- paste0(tblName,".PMID")
  
  str <- paste0("SELECT ", tblPMID, " FROM ", tblName, "\nWHERE ",
         tblName,".",idType)

  if (length(ids) == 1) {
    str <- paste0(str, " = ", paste0("'",ids,"'"), ";")
  } else {
    str <- paste0(str, " IN ",
         paste0("(",paste0("'",ids,"'", collapse = ","), ")\n"),
         "GROUP BY ", tblPMID, " having COUNT(", tblPMID, ") >= ", length(ids), ";")
  }
  
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
