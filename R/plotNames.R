library(Rtsne)
library(keras) 

#' plotNames 
#'
#' Plots a sample of names in two dimensions using T-SNE to reduce dimensions from encoded 
#' representations of names 
#' 
#' @param names 
#' @param n.samples 
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
  encoded <- encoder %>% predict(embedded.names)
  model <- Rtsne::Rtsne(as.matrix(encoded.A), 
                        dims=2, check_duplicates=FALSE)
  labels <- names[sample.idx]
  result <- as.data.frame(model$Y)
  result <- cbind(result, labels)
  ggplot(result, aes(x=V1, y=V2)) + 
    geom_point(size=.1, color='red') + 
    geom_text(aes(label=labels), size=text.size, check_overlap=TRUE) + 
    ggtitle(title) +
    xlab('') + ylab('') 
}
