# for id = ABC.DEF count number of unique articles
# with ABC.DEF or ABC.DEF.*
# res columns are PMID, MeshID, Term, and TreeID
# example query to get res is:
# str <- paste0("SELECT
#   TT.PMID, TT.MeshID, TT.Term, TT.TreeID
#   FROM
#   (SELECT
#     PubMesh.PMID,
#     MeshTerms.MeshID,
#     MeshTerms.Term,
#     MeshTerms.TreeID
#     FROM
#     PubMesh
#     INNER JOIN MeshTerms ON MeshTerms.MeshID = PubMesh.MeshID
#     WHERE
#     PubMesh.PMID IN (", pmids, ")
#     AND (MeshTerms.TreeID = 'C04'
#          OR MeshTerms.TreeID LIKE 'C04.%')
#     GROUP BY PubMesh.PMID , MeshTerms.MeshID , MeshTerms.Term , MeshTerms.TreeID) AS TT;")
countUniqueArticlesByTreeID <- function(res) {

  # get articles with each Tree ID, order so TreeIDs
  # are from lowest to highest
  s <- split(res$PMID, res$TreeID)
  o <- order(nchar(names(s)), decreasing = FALSE)
  s<-s[o]
  
  counts <- rep(0, length(s))
  i <- 1
  idList <- names(s)
  for (id in idList) {
    qry <- paste0("^", id, "$|", id, ".")
    g <- grep(qry, names(s))
    counts[i] <- length(unique(unlist(s[g])))
    i <- i + 1
    
    # remove since these articles will not be found
    # at a higher level
    s <- s[-1]
  }
  
  m <- match(idList, res$TreeID)
  res2 <- unique(cbind(Frequency = counts, res[m,-1])[,1:3])
  res2 <- arrange(res2, desc(Frequency))
  res2
}


# res is frequency table with Frequency, MeshID, Term, TreeID, e.g. from
# str <- paste0("SELECT
#   TT.MeshID, TT.Term, TT.TreeID
#   FROM
#   (SELECT
#     MeshTerms.MeshID,
#     MeshTerms.Term,
#     MeshTerms.TreeID
#     FROM
#     PubMesh
#     INNER JOIN MeshTerms ON MeshTerms.MeshID = PubMesh.MeshID
#     WHERE
#     PubMesh.PMID IN (", pmids, ")
#     AND (MeshTerms.TreeID = 'C04'
#          OR MeshTerms.TreeID LIKE 'C04.%')
#     GROUP BYMeshTerms.MeshID , MeshTerms.Term , MeshTerms.TreeID) AS TT;")
# Update frequencies to include child IDs
countChildTreeIDs <- function(res) {

  res$Frequency <- as.double(res$Frequency)
  s <- split(res$Frequency, res$TreeID)
  o <- order(nchar(names(s)), decreasing = FALSE)
  s<-s[o]

  counts <- rep(0, length(s))
  i <- 1
  idList <- names(s)
  for (id in idList) {
    qry <- paste0("^", id, "$|", id, ".")
    g <- grep(qry, names(s))
    counts[i] <- sum(unlist(s[g]))
    i <- i + 1
  
    # remove since these ids we are going "down" the tree
    s <- s[-1]
  }

  m <- match(idList, res$TreeID)
  res2 <- unique(cbind(Frequency = counts, res[m,-1])[,1:3])
  res2 <- arrange(res2, desc(Frequency))
  
  # remove duplicates
  dups <- duplicated(res2$MeshID)
  res2[!dups,]
}
