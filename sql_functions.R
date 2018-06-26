# prints out query information
catQuery <- function(desc, query, file = NULL) {
  cat(desc, "\n", query, "\n")
  updateLog(logFile, desc, "\n", query)
  if (!is.null(file)) {
    write(query, file = file)
  }
}

# run and time query, with optional output
# if desc is not null, will output to screen and to file if 
# file is not NULL
runQuery <- function(con, str, desc = NULL, file = NULL) {
  cat("in run query..\n")
  if (!is.null(desc)) {
    catQuery(desc, str, file)
  }
  t1 <- Sys.time()
  res <- dbGetQuery(con, str)
  t2 <- Sys.time()
  print(t2-t1)
  x <- t2-t1
  updateLog(logFile, paste0("Time difference of ", round(x,3), " ", units(x)), "\n")

  res
}

cleanse <-function(x) {
  paste(sqlInterpolate(ANSI(), "?value", value = x))
}

# returns cleansed list of form ('1','2','3')
cleanseList <-function(x) {
  if (is.null(x)) {
    return(NULL)
  }
  paste0("(", paste0(sapply(x, cleanse), collapse = ","), ")")
}


# get PMIDs corresponding to cancer articles, 
# GeneID has been cleansed
getCancerPMIDs <- function(con, GeneID) {
  
  str <- paste0("select distinct PubGene.PMID from PubGene\n",
                "inner join PubMesh ON PubMesh.PMID = PubGene.PMID\n",
                "inner join MeshTerms ON PubMesh.MeshID = MeshTerms.MeshID\n",
                "where GeneID = ", GeneID, " and MeshTerms.TreeID LIKE 'C04.%'");
  
  runQuery(con, str, "get cancer PMIDs with query")
  
}

# returns a vector of PMIDs from tblName where table.idType matches ids.
# optionally, we can constrain analysis to vector of pmids
# ids are in SQL format, e.g., ('ID1', 'ID2', 'ID3')
getPMIDs <- function(tblName, idType, ids, con, pmids) {
  
  tblPMID <- paste0(tblName,".PMID")
  
  str <- paste0("SELECT ", tblPMID, " FROM ", tblName, "\nWHERE ")
  if (!is.null(pmids)) {
    str <- paste0(str, tblName,".PMID IN ", 
                  "(", paste0("'",pmids,"'", collapse = ","), 
                  ") AND ")
  }              
                
  str <- paste0(str, tblName,".",idType)

  if (length(ids) == 1) {
    #str <- paste0(str, " = ", paste0("'",ids,"'"), ";")
    str <- paste0(str, " = ", cleanse(ids), ";")
  } else {
    str <- paste0(str, " IN ",
         #paste0("(",paste0("'",ids,"'", collapse = ","), ")\n"),
         cleanseList(ids), "\n",
         "GROUP BY ", tblPMID, " having COUNT(", tblPMID, ") >= ", length(ids), ";")
  }
  
  runQuery(con, str, "get PMIDS query")
  
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
  
  runQuery(con, str, "Mesh summary query:", file = "mesh.sql")
  
}


# returns Chem Summary table for vector of pmids
getChemSummaryByPMIDs <- function(pmids, con, pa = FALSE) {
  
  pmids <- paste0("'",pmids,"'", collapse = ",")
  
  str <- paste0("select count(TT.MeshID) as Frequency, TT.MeshID, TT.Term from\n",
                "(select PubChem.PMID, MeshTerms.MeshID, MeshTerms.Term from PubChem\n",
                "inner join MeshTerms ON MeshTerms.MeshID = PubChem.MeshID\n",
                "where PubChem.PMID IN ", paste0("(", pmids, ")\n"),
                "group by PubChem.PMID, MeshTerms.MeshID, MeshTerms.Term) as TT\n",
                "group by TT.MeshID, TT.Term ORDER BY count(TT.MeshID) desc;")
  
  if (pa) {
    str <- gsub("MeshTerms", "PharmActionTerms", str)
  }
  
  runQuery(con, str, "Chem/PA summary query:")
  
}

# returns Chem Summary table for vector of pmids
getMutationSummaryByPMIDs <- function(pmids, con) {
  
  pmids <- paste0("'",pmids,"'", collapse = ",")
  
  str <- paste0("select count(MutID) as Frequency, MutID from\n",
                "PubMut where PubMut.PMID IN ", paste0("(", pmids, ")\n"),
                "group by PubMut.MutID ORDER BY count(PubMut.MutID) desc;")

  runQuery(con, str, "Mut summary query:")
  
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
  
  runQuery(con, str, "Gene summary query:", file = "genes.sql")
  
  
}
