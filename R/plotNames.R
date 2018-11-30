library(Rtsne)
library(keras) 
library(AUC)

#' plotNames 
#'
#' Plots a sample of names in two dimensions using T-SNE to reduce dimensions from encoded 
#' representations of names 
#' 
#' @param names Vector of names
#' @param n.samples Number of samples to do dimentionality reduction
#' @param encoder.model.path Model path 
#' @param title Plot title 
#' @param text.size size of name labels 
#' @param seed random seed 
#'
#' @export 
plotNames <- function(names, n.samples = 1000, encoder.model.path = NULL, 
                      title = 'Sample of Names in Latent Space Plotted with t-SNE',
                      text.size = 3, seed = 0) {
  if (is.null(encoder.model.path)) {
    stop('Must provide model as path to model or Keras model object.')
  }
  set.seed(seed)
  if (typeof(encoder.model.path) == 'chr') {
    encoder <- keras::load_model_hdf5(encoder.model.path)
  }
  else if (typeof(encoder.model.path) == 'closure') {
    encoder <- encoder.model.path
  }
  else {
    stop('`encoder.model.path` must be path to encoder or Keras Model.')
  }
  max.length <- keras::get_layer(encoder, 'input_1')$input_shape[[2]]
  
  # Get sample of names 
  sample.idx <- sample(1:length(names), n.samples)
  
  # Embed sampled names 
  embedded.names <- recordlinkR::embedLetters(names[sample.idx], max.length = max.length)
  
  # Encode sample names
  encoded <- encoder %>% stats::predict(embedded.names)
  
  # Use T-SNE for dimensionality reduction
  model <- Rtsne::Rtsne(as.matrix(encoded), 
                        dims=2, check_duplicates=FALSE)
  labels <- names[sample.idx]
  result <- as.data.frame(model$Y)
  result <- cbind(result, labels)
  cols <- colnames(result)
  
  # Plot 
  ggplot2::ggplot(result, ggplot2::aes(x=result[,1], y=result[,2])) + 
    ggplot2::geom_point(size=.1, color='red') + 
    ggplot2::geom_text(ggplot2::aes(label=labels), size=text.size, check_overlap=TRUE) + 
    ggplot2::ggtitle(title) +
    ggplot2::xlab('') + ggplot2::ylab('') 
}
