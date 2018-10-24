#' compare
#' 
#' Generate comparison matrix for pairs of records 
#' 
#' @param dfA datafarme 
#' @param dfB dataframe
#' @param blocks block indices 
#' @param compare.string.encoder string encoder cols 
#' @param encoder.model.path Path to encoder model 
#' @param compare.string.sim string similiarity cols 
#' @param string.sim.method Jaro Winkler vs. cosine 
#' @param compare.numeric numeric compare field 
#' @param compare.exact exact compare field 
#' @param n.cores Cores to parallelize over 
#' 
#' @return List comparison matrix
#' @export
compare <- function(dfA, dfB, blocks,
                    compare.string.encoder = NULL,
                    encoder.model.path = NULL, 
                    compare.string.sim = NULL, 
                    string.sim.method = 'jw', 
                    compare.numeric = NULL, 
                    compare.exact = NULL, 
                    n.cores = parallel::detectCores()-1) {
  # Check data types and vector lengths 
  all.comparisons <- list(compare.string.encoder, compare.string.sim, compare.numeric, compare.exact)
  
  # Check that vars in in each list are the same length for A and B 
  for (var in all.comparisons) {
    ifelse(!is.null(var) & length(var[['A']]) != length(var[['B']]), 
           stop('List names must be `A` and `B` with equal length values.'), next)
  }
  
  # Check if comparing using encoder, that encoder model path is specified 
  if (is.null(encoder.model.path) && !(is.null(compare.string.encoder))) {
    cat('\nNeed an encoder model path to compare by encoding.')
    return()
  }
  
  comparisons <- vector(mode = 'list', length = length(compare.string.encoder[['A']]) + 
                          length(compare.string.sim[['A']]) + 
                          length(compare.numeric[['A']]) + 
                          length(compare.exact[['A']]))  
  
  colnames(blocks) <- c('V1', 'V2')
  compareVecs <- function(i, compare.list, func) {
    A <- dfA[compare.list[['A']][i]][blocks$V1,]
    B <- dfB[compare.list[['B']][i]][blocks$V2,]
    func(A, B)
  }
  
  i <- 1
  cols <- c(compare.exact[['A']], compare.numeric[['A']], compare.string.sim[['A']], compare.string.encoder[['A']])
  
  if (!is.null(compare.exact)) {
    exact.list <- parallel::mclapply(1:length(compare.exact[['A']]), compareVecs, 
                                                    compare.list = compare.exact,
                                                    func = recordlinkR::exactCompare, 
                                                    mc.cores = n.cores)
    for (j in 1:length(exact.list)) {
      comparisons[[i]] <- exact.list[[j]]
      i <- i + 1 
    }
    rm(exact.list)
  }
  
  if (!is.null(compare.numeric)) {
    numeric.list <- parallel::mclapply(1:length(compare.numeric[['A']]), compareVecs,
                                                      compare.list = compare.numeric,
                                                      func = recordlinkR::numCompare,
                                                      mc.cores = n.cores)
    for (j in 1:length(numeric.list)) {
      comparisons[[i]] <- numeric.list[[j]]
      i <- i + 1 
    }
    rm(numeric.list)
  }
  
  if (!is.null(compare.string.sim)) {
    string.list <- data.table::data.table(parallel::mclapply(1:length(compare.string.sim[['A']]), compareVecs,
                                                     compare.list = compare.string.sim,
                                                     func = recordlinkR::stringCompare,
                                                     mc.cores = n.cores))
    for (j in 1:length(string.list)) {
      comparisons[[i]] <- string.list[[j]]
      i <- i + 1 
    }
    rm(string.list)
  }
  comparisons <- data.frame(comparisons)
  colnames(comparisons) <- cols
  return(comparisons)
}


#' exactCompare
#' 
#' @param vec.A exact vector from dfA 
#' @param vec.B exact vector from dfB
#' 
#' @return vector 1 for exact match, 0 for no match
#' @export
exactCompare <- function(vec.A, vec.B) {
  as.integer(vec.A == vec.B)
}


#' numCompare
#' 
#' @param vec.A exact vector from dfA 
#' @param vec.B exact vector from dfB
#' 
#' @return vector comparisons
#' @export 
numCompare <- function(vec.A, vec.B) {
  diff <- vec.A - vec.B
  max.val <- max(diff) 
  min.val <- min(diff)
  range <- max.val - min.val  
  unlist(parallel::mclapply(diff, function(x) {(x-min.val) / range}, mc.cores=parallel::detectCores()-1))
}

#' stringCompare
#' 
#' @param vec.A exact vector from dfA 
#' @param vec.B exact vector from dfB
#' @param method Jaro Winkler
#' 
#' @return dataframe difference
#' @export 
stringCompare <- function(vec.A, vec.B, method = 'jw') {
  sim <- stringdist::stringsim(vec.A, vec.B, method = method)
}

#' encodeCompare 
#' 
#' @param vec.A exact vector from dfA 
#' @param vec.B exact vector from dfB
#' @param encoder.model.path model path
#' 
#' @return vector comparisons
#' @export
encodeCompare <- function(vec.A, vec.B, encoder.model.path = NULL) {
  encoder <- keras::load_model_hdf5(encoder.model.path)
  max.length <- keras::get_layer(encoder, 'input_1')$input_shape[[2]]
  cat('\nEncoder has been loaded from ', encoder.model.path) 
}

