library(keras)
library(purrr)
library(data.table)
library(stats) 
library(parallel)

#' block 
#' 
#' Block data to reduce the number of comparisons needed for linkage
#' 
#' 
#' @param dfA Dataframe to be linked to \code{dfB}
#' @param dfB Dataframe to be linked to \code{dfA}, if doing deduplication this is \code{dfA}
#' @param cols.exact List of exact match columns 
#' @param cols.numeric List of numeric range columns
#' @param numeric.range Range of numeric 
#' @param cols.encoder List of encoder columns
#' @param encoder.model.path Path to encoder model 
#' @param encoder.trainA Vector of names to train
#' @param encoder.trainB Vector of matching names to train
#' @param encoder.block.method binary or cluster
#' @param encoder.nclusters Number of cluster is encoding by cluster 
#' @param encoder.maxiter Max iterations in kmeans clustering
#' @param known.matches Dataframe of known matches with first column having indices of matches 
#' from dfA and second column having indices of known matches from dfB
#' @param dim.latent Number of latent dimensions
#' @param dim.encode Number of encoding dimensions
#' @param dim.decode Number of decoding dimensions
#' @param max.length Maximum length of characters 
#' @param num.encode.layers Encode layers
#' @param num.decode.layers Decoder layers
#' @param batch.size Training batch size 
#' @param epochs Number of training epochs
#' @param lr Learning rate
#' @param validation.split Validation 
#' @param save.dir save directory path 
#' @param reconstruct Whether or not show reconstructions 
#' @param reconstruct.n How many reconstructions to show
#' @param reconstruct.display After many epochs to show reconstructions
#' @param earlystop TRUE if stopping early when validation loss is no longer decreasing,
#'  if FALSE then train for all epochs 
#' @param earlystop.patience Number of epochs to wait while validation loss does not
#'  decrease before stopping training early 
#' @param tensorboard TRUE if tensorboard metrics are to be recorded. 
#' Logs are recorded in the /tmp/ directory
#' @param tensorboard.runid Unique identifier for the run to separate tensorboard logs
#' @param verbose Verbosity level for training output, 0 = silence, 1 = minimal, 2 = verboses
#' @param n.cores cores to parallelize over
#' 
#' @return Nested list of blocks where each block is one encoding. 
#' Sub-lists are of indices of namesA and namesB belonging to each block
#' 
#' @export 
block <- function(dfA, dfB, 
                  cols.exact = NULL, 
                  cols.numeric = NULL, numeric.range = NULL, 
                  cols.encoder = NULL, encoder.model.path = NULL, 
                  encoder.trainA = NULL, encoder.trainB = NULL,  
                  encoder.block.method = 'binary', 
                  encoder.nclusters = 5,
                  encoder.maxiter = 1000,
                  known.matches = NULL, 
                  dim.latent = 8, dim.encode = 64, dim.decode = 64, 
                  max.length = 12, 
                  num.encode.layers = 2, num.decode.layers = 2, 
                  batch.size = 32,
                  epochs = 500, 
                  lr = 5e-4,
                  validation.split = .2, 
                  save.dir = '~/blocking_models/', 
                  reconstruct = TRUE, reconstruct.n = 5, reconstruct.display = 20, 
                  earlystop = FALSE, earlystop.patience = 10, 
                  tensorboard = FALSE, tensorboard.runid = as.character(Sys.time()), verbose=2, 
                  n.cores = parallel::detectCores()-1) {

  if (is.null(cols.exact) & is.null(cols.numeric) & is.null(cols.encoder)) {
    cat('\n WARNING: NO BLOCKING COLUMNS SELECTED.')
    return()
  }
  
  # Check that every column in A has a comparison column in B 
  for (col in list(c(cols.exact, cols.numeric, cols.encoder))) {
      ifelse(!is.null(col) & 
               names(col) != c('A', 'B') & 
               length(col[['A']]) == length(col[['B']]), 
             stop('List names must be `A` and `B` with equal length values.'), next)
  }
  
  # Check that if blocking on numeric columns, numeric range is defined 
  if ((!is.null(cols.numeric)) & (is.null(numeric.range))) {
    stop('Numeric range must be defined if blocking on numeric columns.')
  }
  
  # Check if blocking on encoder, encoder block methods are defined 
  if ((!is.null(cols.encoder)) & (!(encoder.block.method %in% c('binary', 'cluster')))) {
    stop('Encoder block method must be `binary` or `cluster`.')
  }
  
  # Check column lists are defined properly in parameters 
  cat('*****BLOCKING ON VARIABLES*****\n\n')
  if (!is.null(cols.exact)) {
    cat('EXACT: \n')
    cat('A: ', unlist(c(cols.exact[['A']])))
    cat('   B: ', unlist(c(cols.exact[['B']])))
  }
  
  if (!is.null(cols.numeric)) {
    cat('\n\nNUMERIC:\n')
    cat('A: ', unlist(c(cols.numeric[['A']])))
    cat('   B: ', unlist(c(cols.numeric[['B']])))
  }
  
  if (!is.null(cols.encoder)) {
    cat('\n\nENCODER: \n')
    cat('A: ', unlist(c(cols.encoder[['A']])))
    cat('   B: ', unlist(c(cols.encoder[['B']])))
  }
  cat('\n******************************')
  cat('\nUsing', n.cores, 'cores.')
  blocks <- vector(mode = 'list', length = length(cols.exact[['A']]) + 
                     length(cols.numeric[['A']]) + 
                     length(cols.encoder[['A']]))
  i <- 1
  if (!is.null(cols.exact)) {
    blocks[[i]] <- blockExact(dfA, dfB, cols.exact, n.cores = n.cores)
    i <- i + 1 
  }
  
  if (!is.null(cols.numeric)) {
    numeric.blocks <- blockNumeric(dfA, dfB, cols.numeric, numeric.range, n.cores = n.cores)
    blocks[[i]] <- numeric.blocks
    i <- i + 1 
  }
  if (!is.null(cols.encoder)) {
    # Encode
    encoder <- NULL
    encoded.A <- NULL 
    encoded.B <- NULL
    
    encoded <- encode(dfA = dfA, dfB = dfB, cols.encoder = cols.encoder, 
                      encoder.model.path = encoder.model.path,
                      encoder.trainA = encoder.trainA, encoder.trainB = encoder.trainB,
                      dim.latent = dim.latent, dim.encode = dim.encode, dim.decode = dim.decode, 
                      max.length = max.length, 
                      num.encode.layers = num.encode.layers, num.decode.layers = num.decode.layers, 
                      batch.size = batch.size,
                      epochs = epochs, 
                      lr = lr,
                      validation.split = validation.split, 
                      save.dir = save.dir, 
                      reconstruct = reconstruct, reconstruct.n = reconstruct.n, reconstruct.display = reconstruct.display, 
                      earlystop = earlystop, earlystop.patience = earlystop.patience, 
                      tensorboard = tensorboard, tensorboard.runid = tensorboard.runid, 
                      verbose = verbose) 
    encoder <- encoded[['encoder']]
    encoded.A <- encoded[['encoded.A']]
    encoded.B <- encoded[['encoded.B']]
    dfA <- encoded[['dfA']]
    dfB <- encoded[['dfB']]
    
    # Then block based on blocking type 
    if (encoder.block.method == 'binary') {
      encoder.blocks <- blockBinaryEncoder(encoded.A = encoded.A, encoded.B = encoded.B, n.cores)
    }
    else if (encoder.block.method == 'cluster') {
      encoder.blocks <- blockClusterEncoder(encoded.A, encoded.B, n.cores, 
                                            encoder.nclusters, encoder.maxiter)
    }
    else {
      stop('Encoder block method not recognized.')
    }
    blocks[[i]] <- encoder.blocks
  }
  
  start.time <- Sys.time() 
  blocks <- Reduce(data.table::fintersect, blocks)
  
  cat('\nBlocks combined in ', difftime(Sys.time(), start.time, units = 'secs'), 'seconds.')
  block.metrics <- recordlinkR::blockMetrics(dfA, dfB, blocks, known.matches = known.matches)
  
  blocks <- list('dfA' = dfA, 
              'dfB' = dfB,
              'blocks' = blocks, 
              'block.metrics' = block.metrics, 
              'encoder' = encoder, 
              'encoded.A' = encoded.A, 
              'encoded.B' = encoded.B) 
  attr(blocks, "class") <- 'blocklist'
  return(blocks)
}


#' getIndex 
#' 
#' @param vector Vector to get indices from 
#' @param x Vector of values to be matched
#' 
#' @return Indicies of vector which match values in x 
#' 
#' @export
getIndex <- function(x, vector) {
  return(which(vector %in% x))
}


#' blockExact 
#' 
#' Block on exact column values 
#' 
#' @param dfA Dataframe to be linked to `dfB`
#' @param dfB Dataframe to be linked to `dfA`
#' @param cols.exact List with columns in dfA to be exactly matched to columns in dfB
#' @param n.cores cores
#' 
#' @return Nested list where each list corresponds to one column in cols.exact. Each sublist 
#' contains a vector of indices named `A` and a vector of indices named `B` for indices in `dfA`
#' and `dfB` which correspond to the same block value 
#' 
#' @export
blockExact <- function(dfA, dfB, cols.exact, n.cores) {
  start.time <- Sys.time()
  
  A <- dfA[cols.exact[['A']]]
  B <- dfB[cols.exact[['B']]]

  # Convert all columns to character for comparison
  A <- sapply(A, as.character)
  B <- sapply(B, as.character)
  
  colBlock <- function(i) {
    keys <- intersect(unique(A[,i]), unique(B[,i]))
    idx.A <- sapply(keys, getIndex, vector = A[,i], USE.NAMES = T)
    idx.B <- sapply(keys, getIndex, vector = B[,i], USE.NAMES = T)
    data.table::rbindlist(parallel::mclapply(keys, function(key) {data.table::CJ(idx.A[[key]], idx.B[[key]])}, mc.cores = n.cores))
  }
  exact.blocks <- lapply(1:length(colnames(A)), colBlock)

  cat('\nExact blocks created in', difftime(Sys.time(), start.time, units = 'secs'), 'seconds.')
  exact.blocks <- Reduce(data.table::fintersect, exact.blocks)
}


#' blockNumeric
#' 
#' @param dfA Dataframe to be linked to `dfB`
#' @param dfB Dataframe to be linked to `dfA`
#' @param n.cores Cores
#' @param cols.numeric Named list
#' @param numeric.range Window 
#' 
#' @export
blockNumeric <- function(dfA, dfB, n.cores, 
                         cols.numeric, numeric.range = 1) {
  start.time <- Sys.time()
  A <- dfA[cols.numeric[['A']]]
  B <- dfB[cols.numeric[['B']]]
  
  A <- sapply(A, as.numeric)
  B <- sapply(B, as.numeric)

  numeric.blocks <- vector(mode = 'list', length = length(colnames(A)))
  
  for (i in 1:length(colnames(A))) {
    keys <- unique(A[,i]) 
    
    idx.A <- lapply(keys, getIndex, vector = A[,i])
    seq.from.keys <- lapply(keys, function(x) {seq(from=x-numeric.range, to=x+numeric.range, by=1)})
    idx.B <- lapply(seq.from.keys, getIndex, vector = B[,i])
    
    keys <- sapply(keys, as.character)
    names(idx.A) <- keys
    names(idx.B) <- keys
    
    matchNumericKeys <- function(key) {
      if (length(idx.B[[key]] >= 1)) {
        data.table::CJ(idx.A[[key]], idx.B[[key]])
        }
    }
    numeric.blocks[[i]] <- data.table::rbindlist(parallel::mclapply(keys, matchNumericKeys, mc.cores = n.cores))
  }
  cat('\nNumeric blocks created in', difftime(Sys.time(), start.time, units = 'secs'), 'seconds.') 
  numeric.blocks <- Reduce(data.table::fintersect, numeric.blocks)
  return(numeric.blocks)
}

#' encode
#' 
#' Adds encoded columns to dataframes to be linked 
#' 
#' @param dfA DataframeA 
#' @param dfB DataframeB
#' @param cols.encoder Encoder cols 
#' @param encoder.model.path Model path if no train
#' @param encoder.trainA trainA 
#' @param encoder.trainB trainB
#' @param dim.latent Dim latent
#' @param dim.encode Dim encode 
#' @param dim.decode Dim decode 
#' @param max.length Max length 
#' @param num.encode.layers encode layers 
#' @param num.decode.layers decode layers 
#' @param batch.size batch size 
#' @param epochs Epochs 
#' @param lr Learning rate
#' @param validation.split validation split
#' @param save.dir Save directory 
#' @param reconstruct reconstruct 
#' @param reconstruct.n How many reconstructions to show
#' @param reconstruct.display After many epochs to show reconstructions
#' @param earlystop TRUE if stopping early when validation loss is no longer decreasing,
#'  if FALSE then train for all epochs 
#' @param earlystop.patience Number of epochs to wait while validation loss does not
#'  decrease before stopping training early 
#' @param tensorboard TRUE if tensorboard metrics are to be recorded. 
#' Logs are recorded in the /tmp/ directory
#' @param tensorboard.runid Unique identifier for the run to separate tensorboard logs
#' @param verbose Verbosity level for training output, 0 = silence, 1 = minimal, 2 = verboses
#' 
#' @export
encode <- function(dfA, dfB, cols.encoder,
                   encoder.model.path = NULL,
                   encoder.trainA = NULL, encoder.trainB = NULL,
                   dim.latent = 8, dim.encode = 64, dim.decode = 64, 
                   max.length = 12, 
                   num.encode.layers = 2, num.decode.layers = 2, 
                   batch.size = 32,
                   epochs = 500, 
                   lr = 5e-4,
                   validation.split = .2, 
                   save.dir = '~/blocking_models/', 
                   reconstruct = TRUE, reconstruct.n = 5, reconstruct.display = 20, 
                   earlystop = FALSE, earlystop.patience = 10, 
                   tensorboard = FALSE, tensorboard.runid = as.character(Sys.time()), 
                   verbose = 2) {
  
  # If encoder model path has been specified, load the load 
  if (!is.null(encoder.model.path)) {
    encoder <- keras::load_model_hdf5(encoder.model.path)
    max.length <- keras::get_layer(encoder, 'input_1')$input_shape[[2]]
    cat('\nEncoder has been loaded from ', encoder.model.path) 
  }
  else {
    cat('\nNo existing model specified. Begin training new encoder.')
    
    # Must have training data 
    if (is.null(encoder.trainA) || is.null(encoder.trainB)) {
      stop('\n `encoder.trainA` and encoder.trainB` cannot be NULL if training a new model.')
    }
    
    encoder <- trainModel(encoder.trainA, encoder.trainB, 
                          dim.latent = dim.latent, dim.encode = dim.encode, dim.decode = dim.decode, 
                          max.length = max.length, 
                          num.encode.layers = num.encode.layers, num.decode.layers = num.decode.layers, 
                          batch.size = batch.size, epochs = epochs, lr = lr,
                          validation.split = validation.split, 
                          save.dir = save.dir, 
                          reconstruct = reconstruct, reconstruct.n = reconstruct.n, 
                          reconstruct.display = reconstruct.display, 
                          earlystop = earlystop, 
                          earlystop.patience = earlystop.patience, 
                          tensorboard = tensorboard, tensorboard.runid = tensorboard.runid, 
                          verbose = verbose)
  }
  # Preallocate lists to hold encoded matrices for each encoding column
  encoded.cols.A <- vector(mode = 'list', length = length(cols.encoder[['A']]))
  encoded.cols.B <- vector(mode = 'list', length = length(cols.encoder[['A']]))
  
  for (i in length(cols.encoder[['A']])) {
    namesA <- dfA[,cols.encoder[['A']][[i]]]
    namesB <- dfB[,cols.encoder[['B']][[i]]]
    
    # Embed names 
    embedded.namesA <- recordlinkR::embedLetters(namesA, max.length = max.length)
    embedded.namesB <- recordlinkR::embedLetters(namesB, max.length = max.length)
    
    # Encode names to be matched
    encoded.A <- encoder %>% stats::predict(embedded.namesA)
    encoded.B <- encoder %>% stats::predict(embedded.namesB)

    encoded.cols.A[[i]] <- encoded.A
    encoded.cols.B[[i]] <- encoded.B
    
    # Add embedded vectors to dataframes 
    encoded.dfA <- data.frame(encoded.A)
    colnames(encoded.dfA) <- paste(cols.encoder[['A']][[i]], 'enc', 1:dim(encoded.A)[2], sep = '_')
    dfA <- cbind(dfA, encoded.dfA)
    rm(encoded.dfA)
    
    encoded.dfB <- data.frame(encoded.B)
    colnames(encoded.dfB) <- paste(cols.encoder[['B']][[i]], 'enc', 1:dim(encoded.B)[2], sep = '_')
    dfB <- cbind(dfB, encoded.dfB)
    rm(encoded.dfB)
  }
  return(list('encoder' = encoder, 
              'encoded.A' = encoded.cols.A,
              'encoded.B' = encoded.cols.B, 
              'dfA' = dfA, 
              'dfB' = dfB))
}


#' blockCluster
#' 
#' @param encoded.A List of matrices from encoded columns in dfA
#' @param encoded.B List of matrices from encoded columns in dfB
#' @param n.cores Cores 
#' @param n.centers Number of clusters to use
#' @param iter.max Maximum number of iterations
#' 
#' @export
blockClusterEncoder <- function(encoded.A, encoded.B, 
                                n.cores, n.centers = 20, iter.max=1000) {
  start.time <- Sys.time()
  cluster.blocks <- vector(mode = 'list', length = length(encoded.A))
  
  for (i in 1:length(encoded.A)) {
    combined <- rbind(encoded.A[[i]], encoded.B[[i]])
    cluster.vec <- stats::kmeans(combined, centers = n.centers, iter.max = iter.max)$cl

    vec.A <- cluster.vec[1:dim(encoded.A[[i]])[1]]
    vec.B <- cluster.vec[(dim(encoded.A[[i]])[1])+1:length(cluster.vec)]
    
    clusters <- unique(vec.A)
    
    clusterIndex <- function(cluster) {
      data.table::CJ(c(which(vec.A %in% cluster)), c(which(vec.B %in% cluster)))
    }
      
    cluster.blocks[[i]] <- data.table::rbindlist(parallel::mclapply(clusters, clusterIndex, mc.cores = n.cores))
  }
  cluster.blocks <- Reduce(data.table::fintersect, cluster.blocks)
  cat('\nCluster Encoder blocks created in', difftime(Sys.time(), start.time, units = 'secs'), 'seconds.') 
  return(cluster.blocks)
}


#' blockBinaryEncoder
#' 
#' 
#' @param encoded.A Matrix of encoded values
#' @param encoded.B Matrix of encoded values 
#' @param n.cores Number of cores to parallelize
#' 
#' @return List 
#' @export
blockBinaryEncoder <- function(encoded.A, encoded.B, n.cores) {
  start.time <- Sys.time()
  
  encoder.blocks <- vector(mode = 'list', length = length(encoded.A))
  
  for (i in 1:length(encoded.A)) {
    med <- apply(encoded.A[[i]], 2, stats::median)
    cat('  Median mu set with dimension', length(med))
    
    # Function to binarize using median 
    binarize <- function(x, med) {
      apply(t(apply(x, 1, function(m) {m >= med})), 1:2, sum)
    }
    
    binarized <- purrr::map(list(encoded.A[[i]], encoded.B[[i]]), binarize, med = med)
    
    stringified.A <- apply(format(binarized[[1]]), 1, paste, collapse = '') 
    stringified.B <- apply(format(binarized[[2]]), 1, paste, collapse = '')
    
    unique.stringified.A <- unique(stringified.A)

    blocks = vector(mode = 'list', length = length(unique.stringified.A))
    
    encIndex <- function(enc) {
      data.table::CJ(c(which(stringified.A %in% enc)), c(which(stringified.B %in% enc)))
    }
    encoder.blocks[[i]] <- data.table::rbindlist(parallel::mclapply(unique.stringified.A, encIndex, mc.cores = n.cores))
    
  }
  encoder.blocks <- Reduce(intersect, encoder.blocks)
  cat('\nBinary Encoder blocks created in', difftime(Sys.time(), start.time, units = 'secs'), 'seconds.') 
  return(encoder.blocks)
}
