source("treeIDfunctions.R", local = TRUE)

# prints out query information
catQuery <- function(desc, query, file = NULL) {
  #return()
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

removeParentalMeshIDs <- function(con, ids) {
  if (length(ids) == 1) {
    return(ids)
  }
  ids <- paste0("'", ids, "'", collapse = ",")
  str <- paste0("select * from MeshTerms where MeshID in (", ids, ");")
  res <- dbGetQuery(con, str)
  s <- split(res$TreeID, res$MeshID)
  keep <- NULL
  len <- length(s)
  for (i in 1:len) {
    current = s[i]
    look <- s[-i]
    g <- grep(paste0(current[[1]], collapse = "|"),
            unlist(look))
      if (length(g) ==0) {
        keep <- c(keep, names(current))
    }
  }
  keep
}
# for SINGLE MeshID, get all list of that MeshID including children
getChildMeshIDs <- function(con, MeshID) {
  if (is.null(MeshID)) {
    return(NULL)
  }
  
  # This get all the treeIDs that associated with the MeshID that the user selected but only MeshID related to cancer
  qry <- paste0("Select TreeID from dcast.meshterms where MeshID = '",MeshID,"' and treeid like 'C04%';")
  
  treeIDlist <- dbGetQuery(con, qry)
  # This treeIDlist will have to be formated to work with next select statement.
  # To formating, first I have to add % at the end of each treeID to do a wildcard query.
  # Then it has to repeat 'treeid like' and then the treeID with wildcard
  # An example of the select statement after formating the treeIDlist with muliple treeID trees:
  # qry <- paste0("select MeshID from dcast.meshterms where treeid like 'c04.557.337%' or treeid like 'C04.588.322.894%' or treeid like'C04.588.443.915%' ;")
  
  treeIDpaste <- paste0("TreeID like '", treeIDlist$TreeID, '%\'', collapse=" or ")
  cat("limit by treeID: ", treeIDpaste)
  #This select statement gets all the MeshIDs from the TreeIDs
  qry <- paste0("select distinct(MeshID) from dcast.meshterms where ",treeIDpaste,";")
  meshIDlist <- dbGetQuery(con, qry)
  #cleanseList(meshIDlist$MeshID)
  meshIDlist$MeshID
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


# get PMIDs corresponding to cancer articles with user cancer selection
getCancerPMIDsbyMeshID <- function(con, GeneID, MeshID) {
  
  str <- paste0("select distinct PubGene.PMID from PubGene\n",
                "inner join PubMesh ON PubMesh.PMID = PubGene.PMID\n",
                "inner join MeshTerms ON PubMesh.MeshID = MeshTerms.MeshID\n",
                "where GeneID = ", GeneID, " and PubMesh.MeshID IN" , MeshID);
  
  runQuery(con, str, "get cancer PMIDs with query limited to user selection of cancer types")
  
}


# getPMIDS - 
# basic query is: select PMIDs from tblName where PMIDs in (pmids) AND
#   tblName.idType in (ids)
# if ids.AND is TRUE, then use group by and count to require that all
#     (ids) are found (e.g., if looking for articles mentioning both 
#     breast and lung tumors). Otherwise all matches to (ids) will
#     be returned
# ids must already be in MySQL format ('id1','id2',etc)

getPMIDs <- function(tblName, idType, ids, con, pmids, ids.AND = TRUE) {
  
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
         cleanseList(ids))
    if (ids.AND) {
      str <- paste0(str, "\nGROUP BY ", tblPMID, 
                    " having COUNT(", tblPMID, ") >= ", length(ids), ";")
    }
      
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
  
  str <- paste0("SELECT count(TT.MeshID) as Frequency,
     TT.MeshID, TT.Term, TT.TreeID
     FROM
     (SELECT
       PubMesh.PMID,
       MeshTerms.MeshID,
       MeshTerms.Term,
       MeshTerms.TreeID
       FROM
       PubMesh
       INNER JOIN MeshTerms ON MeshTerms.MeshID = PubMesh.MeshID
       WHERE
       PubMesh.PMID IN (", pmids, ")
       GROUP BY PubMesh.PMID , MeshTerms.MeshID , MeshTerms.Term , MeshTerms.TreeID) AS TT
#       WHERE (TT.TreeID = 'C04'
#            OR TT.TreeID LIKE 'C04.%')
       WHERE TT.TreeID LIKE 'C04.%'
       GROUP BY TT.MeshID , TT.Term , TT.TreeID;")
  
  
  res <- runQuery(con, str, "Mesh summary query:")
#save(res, file = "res.RData")
  a<-countChildTreeIDs(res)
  a
  
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


# returns Mesh Summary table for vector of pmids
getCancerTermSummaryByPMIDs <- function(pmids, con) {
  
  pmids <- paste0("'",pmids,"'", collapse = ",")
  
  str <- paste0("select count(TT.TermID) as Frequency, TT.TermID, TT.Term from\n",
                "(select PubCancerTerms.PMID, CancerTerms.TermID, CancerTerms.Term from PubCancerTerms\n",
                "inner join CancerTerms ON CancerTerms.TermID = PubCancerTerms.TermID\n",
                "where PubCancerTerms.PMID IN ", paste0("(", pmids, ")\n"),
                "group by PubCancerTerms.PMID, CancerTerms.TermID, CancerTerms.Term) as TT\n",
                "group by TT.TermID, TT.Term ORDER BY count(TT.TermID) desc;")
  
  runQuery(con, str, "CancerTerm summary query:")
  
}
