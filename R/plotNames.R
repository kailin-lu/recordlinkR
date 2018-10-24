library(Rtsne)
library(keras) 

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
    cat('Must provide path to encoder model')
    return() 
  }
  set.seed(seed)
  encoder <- keras::load_model_hdf5(encoder.model.path)
  max.length <- keras::get_layer(encoder, 'input_1')$input_shape[[2]]
  
  sample.idx <- sample(1:length(names), n.samples)
  embedded.names <- recordlinkR::embedLetters(names[sample.idx], max.length = max.length)
  encoded <- encoder %>% stats::predict(embedded.names)
  model <- Rtsne::Rtsne(as.matrix(encoded), 
                        dims=2, check_duplicates=FALSE)
  labels <- names[sample.idx]
  result <- as.data.frame(model$Y)
  result <- cbind(result, labels)
  cols <- colnames(result)
  ggplot2::ggplot(result, ggplot2::aes(x=result[,1], y=result[,2])) + 
    ggplot2::geom_point(size=.1, color='red') + 
    ggplot2::geom_text(ggplot2::aes(label=labels), size=text.size, check_overlap=TRUE) + 
    ggplot2::ggtitle(title) +
    ggplot2::xlab('') + ggplot2::ylab('') 
}
