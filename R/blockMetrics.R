#' blockMetrics
#' 
#' Reports block metrics to assess quality of blocking. If known matches are null then 
#' 
#' @param dfA dataframe A 
#' @param dfB dataframe B 
#' @param blocks Dataframe of two columns representing pairs to be checked post blocking
#' @param known.matches Dataframe of two columns representing indices of known matches 
#' where the left index refers to a record index in dfA and 
#' the right index refers to a record index in dfB
#' 
#' @return List of metrics
#' \item{original.comparisons}{Number of original comparisons}
#' \item{remaining.comparisons}{Number of comparisons remaining post blocking}
#' \item{pairs.reduction}{Proportion of original comparisons remaining post blocking}
#' \item{total.known.matches}{If known.matches was provided, number of total known matches}
#' \item{found.matches}{Number of known matches found in the remaning pairs}
#' \item{pairs.completeness}{Percentage of known matches found in remaining pairs}
#' 
#' @export
blockMetrics <- function(dfA, dfB, blocks, known.matches = NULL) {
  original.comparisons <- nrow(dfA) * nrow(dfB)
  remaining.comparisons <- nrow(blocks)
  pairs.reduction <- (remaining.comparisons*100) / original.comparisons
  
  cat('\n********************************\n')
  cat('Original Comparisons Needed: ', original.comparisons)
  cat('\nComparisons Remaining Post-Blocking: ', remaining.comparisons, '( ', pairs.reduction, '% )')   
  
  found.matches <- NULL 
  pairs.completeness <- NULL 
  
  if (!is.null(known.matches)) {
    known.matches <- data.table::as.data.table(known.matches)
    colnames(known.matches) <- colnames(blocks)
    found <- data.table::fintersect(known.matches, blocks)
    pairs.completeness <- nrow(found) /nrow(known.matches)
    cat('\nMatches Found: ', nrow(found))
    cat('\nPairs Completeness: ', round(pairs.completeness * 100,2), '%')
  }
  else {found <- NA}
  return(list('original.comparisons' = original.comparisons, 
              'remaining.comparisons' = remaining.comparisons, 
              'pairs.reduction' = pairs.reduction, 
              'total.known.matches' = nrow(known.matches),
              'found.matches' = found,
              'pairs.completeness' = pairs.completeness))
}
