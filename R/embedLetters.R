require(purrr)
require(tensorflow)

#' embedLetters
#' 
#' Embed a character vector into one-hot encoded array
#' 
#' One-hot encoding will have dimension 28 for 27 lower case letters as well as a space 
#' 
#' @usage embedLetters(names, max.length)
#'  
#' @param names Character vector 
#' @param max.length Numeric representing number of letters to keep for each name. 
#' Names shorter than \code{max.length} will be padded with 0s from the right
#' 
#' @return Binary array with dimension: length(names) x max.length x 28 
#' 
#' @examples 
#' \dontrun{
#' # Sample names 
#' names <- c('amy', 'john', 'cathy')
#' 
#' # Embed 
#' embedded <- embedLetters(names, max.length = 12)
#' 
#' dim(embedded)
#' > [1] 3  12  28
#' }
#' 
#' @importFrom utils head 
#' @export
embedLetters <- function(names, max.length = 12) {
  names <- sapply(names, as.character)
  vec.names <- array(0, dim=c(length(names), max.length))
  for (i in 1:(length(names))) {
    name <- head(strsplit(names[i], split='')[[1]], max.length)
    for (j in 1:length(name)) {
      k <- recordlinkR:::letters_index[name[j]][[1]]
      # Add in index if character j exists in the letters index
      if (length(k) > 0) {
        vec.names[i, j] <- k
      }
    }
  }
  return (keras::to_categorical(vec.names, length(recordlinkR:::letters_index)+1) )
}


#' disembedLetters
#' 
#' Revert embedded array back into a character vector 
#' 
#' @usage disembedLetters(embeddedArray)
#' 
#' @param embeddedArray 3-dim array of one-hot encoded characters 
#'
#' @return Character vector with length dim(embeddedArray)[1]
#' @export
disembedLetters <- function(embeddedArray) {
  sess <- tensorflow::tf$Session() 
  nameMatrix <- sess$run(keras::k_argmax(embeddedArray, -1))
  return(apply(nameMatrix, 1, disembedName))
}


#' disembedName 
#' 
#' Disembed a single name
#' 
#' @param x integer vector 
#' 
#' @return character vector with one name  
#' @export
disembedName <- function(x) {
  name <- rep('', length(x))
  for (i in 1:length(x)) {
    if (x[i] <= 27  & x[i] > 0) {
      name[i] <- names(recordlinkR:::letters_index[x[i]])
    }
  }
  name <- paste(name, collapse='')
  return(name)
}
 