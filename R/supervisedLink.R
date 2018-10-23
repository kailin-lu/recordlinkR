#' link
#' 
#' Supervised record linkage 
#' 
#' @param dfA dataframe 
#' @param dfB dataframe
#' 
#' @return list  
#' @export
supervisedLink <- function(dfA, dfB, train.indices, 
                           validation.split = 0.2, method = 'log', 
                           blocks = NULL, 
                           block.exact = NULL, 
                           block.numeric = NULL, block.numeric.range = NULL, 
                           block.encoder = NULL, block.encoder.model.path = NULL, 
                           block.encoder.method = 'cluster', 
                           comparisons = NULL, 
                           compare.string.encoder = NULL, compare.encoder.model.path = NULL, 
                           compare.string.sim = NULL, string.sim.method = 'jw', 
                           compare.numeric = NULL, 
                           compare.exact = NULL, 
                           n.cores = parallel::detectCores() - 1, 
                           seed = 0) {
  set.seed(seed) 
  
  # If blocks not provided, make blocks 
  if (is.null(blocks)) {
    block.data <- block(dfA, dfB, 
                        cols.exact = NULL, 
                        cols.numeric = NULL, numeric.range = NULL, 
                        cols.encoder = NULL, encoder.model.path = NULL, 
                        encoder.trainA = NULL, encoder.trainB = NULL,  
                        encoder.block.method = 'binary', 
                        encoder.nclusters = 5, encoder.maxiter = 1000,
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
                        n.cores = n.cores)
    blocks <- block.data[[]]
  }
  
  # If comparisons not provided, calculate comparisons 
  if (is.null(comparisons)) {
    comparisons <- compare(dfA, dfB, blocks,
                        compare.string.encoder = NULL,
                        encoder.model.path = NULL, 
                        compare.string.sim = NULL, 
                        string.sim.method = 'jw', 
                        compare.numeric = NULL, 
                        compare.exact = NULL, 
                        n.cores = n.cores) 
  }
  
  # Append match column to comparisons
  
  # Split into train and test 
  train.idx <- sample(1:nrow(comparisons), nrow(comparisons) * .8)
  comparisons.train <- comparisons[train.idx,]
  comparisons.test <- comparisons[-train.idx,]
  
  
  if (method == 'all') {
    model.log <- glm(match ~ ., data = comparisons.train, family = binomial())
    model.svm  <- svm(match ~ ., data = comparisons.train)
    
  }
  else if (method == 'log') {
    model <- glm(match ~ . , data = comparisons.train, family = binomial())
    summary(model)
  }
  else if (method == 'svm') {
    model <- svm(match ~ ., data = comparisons.train)
  }
  else {
    cat('\nNo model type selected.')
  }
  
  # Predict remaining comparisons 
  
  # Return list of metrics 
  link <- list() 
  return(link)
}