

# Gets chemical counts for each disease
getChemByDiseaseContingency <- function(pmids, con) {
  pmids <- paste0("'",pmids,"'", collapse = ",")
  
  qry <- paste0("SELECT 
  distinct DT.Term AS Disease, CT.Term as Chemical, Frequency
  FROM
  (SELECT 
  TT.Disease AS Disease,
  TT.Chem AS Chem,
  COUNT(TT.Chem) AS Frequency
  FROM
  (SELECT 
  PubChem.PMID AS PMID,
  PubChem.MeshID AS Chem,
  PubMesh.MeshID AS Disease
  FROM
  PubChem
  INNER JOIN PubMesh ON PubMesh.PMID = PubChem.PMID
  WHERE
  PubChem.PMID IN (", pmids,  ")) AS TT
  GROUP BY TT.Disease, TT.Chem) AS R
  inner join MeshTerms as DT ON DT.MeshID = R.Disease
  inner join MeshTerms as CT ON CT.MeshID = R.Chem
  where DT.TreeID like 'C04.%';")
  
  runQuery(con, qry, "Chem by Disease query:")
  
}


#mutations by disease (cancer only)
getMutByDiseaseContingency <- function(pmids, con) {
  pmids <- paste0("'",pmids,"'", collapse = ",")
  qry <- paste0("SELECT 
distinct DT.Term AS Disease, R.Mutation as Mutation, Frequency
FROM
(SELECT 
  TT.Disease AS Disease,
  TT.Mutation AS Mutation,
  COUNT(TT.Mutation) AS Frequency
  FROM
  (SELECT 
    PubMut.PMID AS PMID,
    PubMut.MutID AS Mutation,
    PubMesh.MeshID AS Disease
    FROM
    PubMut
    INNER JOIN PubMesh ON PubMesh.PMID = PubMut.PMID
    WHERE
    PubMut.PMID IN (", pmids,  ")) AS TT
  GROUP BY TT.Disease , TT.Mutation) AS R
inner join MeshTerms as DT ON DT.MeshID = R.Disease
where DT.TreeID like 'C04.%';")
  runQuery(con, qry, "Chem by Disease query:")
}