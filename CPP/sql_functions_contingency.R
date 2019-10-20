
##################################################################
# Functions that create contingency table queries for a given
# set of pmids and given set of meshIDs (m)
##################################################################

makeChemByDiseaseContingencyQuery <- function(pmids, m) {
  paste0(
    "SELECT 
    PharmActionTerms.Term as Term, count(distinct PubChem.PMID) as Frequency
    FROM PubChem
    INNER JOIN PubMesh ON (PubChem.PMID = PubMesh.PMID)
    INNER JOIN PharmActionTerms ON (PubChem.MeshID = PharmActionTerms.MeshID)
    WHERE PubMesh.MeshID IN (",m,")
    AND 
    PubChem.PMID IN (",pmids, ")
    group by Term;")
}

makeCancerTermsByDiseaseContingencyQuery <- function(pmids, m) {
  paste0(
    "SELECT 
    CancerTerms.Term as Term, count(distinct PubCancerTerms.PMID) as Frequency
    FROM PubCancerTerms
    INNER JOIN PubMesh ON (PubCancerTerms.PMID = PubMesh.PMID)
    INNER JOIN CancerTerms ON (PubCancerTerms.TermID = CancerTerms.TermID)
    WHERE PubMesh.MeshID IN (",m,")
    AND 
    PubCancerTerms.PMID IN (",pmids, ")
    group by CancerTerms.Term;")
}

makeGenesByDiseaseContingencyQuery <- function(pmids, m) {
  paste0(
    "SELECT 
    GENES.Symbol as Gene, count(distinct PubGene.PMID) as Frequency
    FROM PubGene
    INNER JOIN PubMesh ON (PubGene.PMID = PubMesh.PMID)
    INNER JOIN GENES ON (PubGene.GeneID = GENES.GeneID)
    WHERE PubMesh.MeshID IN (",m,")
    AND 
    PubGene.PMID IN (",pmids, ")
    group by Gene;")
}



makeMutByDiseaseContingencyQuery <- function(pmids,m) {
  paste0(
    "SELECT 
    MutID as Mutation, count(distinct PubMut.PMID) as Frequency
    FROM PubMut
    INNER JOIN PubMesh ON (PubMut.PMID = PubMesh.PMID)
    WHERE PubMesh.MeshID IN (",m,")
    AND 
    PubMut.PMID IN (",pmids, ")
    group by MutID;")
}

contingencyQueryByDisease <- function(pmids, con, meshIDs, diseases, makeQueryFunction) {
  
  t1 <- Sys.time()
  
  pmids <- paste0("'",pmids,"'", collapse = ",")
  
  all_res <- NULL
  for (i in seq_along(meshIDs)) {
    m <- meshIDs[i]
    m <- getChildMeshIDs(con, m)
    m <- paste0("'",m,"'", collapse = ",")
    
    qry <- makeQueryFunction(pmids, m)
    
    catn("Getting contingency for disease: ", diseases[i])
    
    res<- dbGetQuery(con, qry)
    if (nrow(res) > 0) {
      res <- cbind(Disease = diseases[i], res)
      all_res <- rbind(all_res, res)
    }
    
  }
  t2 <- Sys.time()
  print(t2-t1)
  all_res
}

####################################################################
# functions to query the database
####################################################################
getChemByDiseaseContingency2 <- function(pmids, con, meshIDs, diseases) {
  contingencyQueryByDisease(pmids, con, meshIDs, diseases, makeChemByDiseaseContingencyQuery) 
}

getMutByDiseaseContingency2 <- function(pmids, con, meshIDs, diseases) {
  contingencyQueryByDisease(pmids, con, meshIDs, diseases, makeMutByDiseaseContingencyQuery) 
}


getCancerTermsByDiseaseContingency2 <- function(pmids, con, meshIDs, diseases) {
  contingencyQueryByDisease(pmids, con, meshIDs, diseases, makeCancerTermsByDiseaseContingencyQuery) 
}

getGenesByDiseaseContingency2 <- function(pmids, con, meshIDs, diseases) {
  contingencyQueryByDisease(pmids, con, meshIDs, diseases, makeGenesByDiseaseContingencyQuery) 
}
