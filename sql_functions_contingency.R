

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

