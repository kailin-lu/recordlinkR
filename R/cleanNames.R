require(tidyr)
require(stringr)

#' cleanNames 
#' 
#' cleanNames is used to preprocess character columns for blocking and linking functions.
#' Names can be standardized by converting all characters to lower case, splitting full name 
#' columns into individual parts such as 'first' and 'last', and punctuation can be removed. 
#' For blocking and linking on autoencoded name fields, it is recommended to split each part of 
#' the name such as first, middle, and last, into separate columns.  
#' 
#'
#' @param df Dataframe
#' @param cols Vector of variables to apply changes to. If 'all' then cols = '.'
#' @param split.pattern Named list of character to split variables. Variables are list names 
#' and values are the character to be split on 
#' @param split.col.names Named list of new variable names. Variables are list names and 
#' values are vectors of new variable names. Maximum number of splits on each column is the length of values. 
#' @param lower  if TRUE then all elements in df[cols] will be lower cased 
#' @param strip.punctuation  if TRUE then all elements in df[cols] will be stripped of punctuation
#' 
#' @return Dataframe with processed character columns
#' 
#' @importFrom magrittr "%>%"
#' @export
cleanNames <- function(df, cols = '.', 
                       split.pattern = NULL, 
                       split.col.names = NULL,
                       lower = TRUE, 
                       strip.punctuation = TRUE) {
  if (cols == '.') {
    cols <- colnames(df)
  }
  
  if (lower) {
    if (dim(df)[2] == 1) {
      colname <- colnames(df)
      df[1] <- as.data.frame(apply(df, 1, tolower), stringsAsFactors = FALSE)
      colnames(df) <- colname
    }
    else {
      df[cols] <- as.data.frame(apply(df[,cols], 2, tolower), stringsAsFactors = FALSE)
    }
  }
  
  if (!is.null(split.pattern)) {
    
    # Check that split.pattern variables match split.col.names variables
    if (all(names(split.pattern) != names(split.col.names))){
      stop('Names of `split.pattern` and `split.col.names` must match.')
    }
  
    for (i in 1:length(names(split.pattern))) {
      col <- names(split.col.names)[i]
      sep <- ifelse(split.pattern[[col]] == ' ', '\\s+', split.pattern[[col]])
      
      df <- df %>% tidyr::separate(col,
                                   sep = sep,
                                   into = split.col.names[[col]],
                                   extra = 'merge', 
                                   fill = 'right')
      df[,split.col.names[[col]]][is.na(df[,split.col.names[[col]]])] <- ''
    }
  }
  
  cols <- union(unlist(split.col.names, use.names = FALSE), intersect(cols, colnames(df)))
  if (strip.punctuation) {
    if (dim(df)[2] == 1) {
      colname <- colnames(df)
      df[1] <- as.data.frame(apply(df, 1, stringr::str_replace_all, pattern='[[:punct:]]', ''), 
                             stringsAsFactors = FALSE)
      colnames(df) <- colname
    }
    else {
      df[cols] <- as.data.frame(apply(df[cols], 1:2, stringr::str_replace_all, pattern='[[:punct:]]', ''), 
                                stringsAsFactors = FALSE)
    }
  }
  return(df)
}

