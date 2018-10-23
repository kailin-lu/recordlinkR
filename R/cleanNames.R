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
#' @usage cleanNames(df, cols, split, new.col.names, split.pattern, 
#' lower, strip.punctuation)
#'
#' @param df Dataframe
#' @param cols Vector of column names to clean
#' @param split TRUE if at least one of the columns in cols needs to be split
#' @param split.pattern Vector of pattern to split on in order of columns to be split 
#' @param new.col.names Named list. Names are names of columns to be split, values are 
#' vectors of new column names post split.  
#' @param lower if TRUE cols are lower-cased, if FALSE cols are returned with case as is 
#' @param strip.punctuation if TRUE punctuation is stripped from cols, if FALSE all 
#' puntuation is left as is 
#' 
#' @examples 
#' # Create list of new column names for columns which will be split
#' new.col.names <- list('names' = c('last', 'first'), 
#'                       'mothers.names' = c('mothers.first', 'mothers.last'))
#' 
#' # Return `df` with full.names split on comma into 'last' and 'first' 
#' # Split `full.mothers.names` into 'first.m' and 'last.m' on space character
#' # Lower case and strip punctuation from cols                        
#' df.cleaned <- cleanNames(df, 
#'                            cols = c('full.names', 'full.mothers.names', 'nicknames'), 
#'                            split = TRUE, 
#'                            split.pattern = c(',', ' '), 
#'                            new.col.names = new.col.names,
#'                            lower = TRUE, 
#'                            strip.punctuation = TRUE)
#'                            
#' @return Dataframe with processed character columns
#' 
#' @importFrom magrittr "%>%"
#' @export
cleanNames <- function(df, cols, 
                       split = FALSE,
                       new.col.names = NULL,
                       split.pattern = ',', 
                       lower = TRUE, 
                       strip.punctuation = TRUE) {
  if (lower) {
    df[cols] <- as.data.frame(apply(df[cols], 2, tolower))
  }
  if (split) {
    for (i in 1:length(new.col.names)) {
      col <- names(new.col.names)[i]
      df <- df%>% 
        tidyr::separate(col, 
                        sep = split.pattern[i], 
                        into = new.col.names[[col]], 
                        extra = 'merge')
    }
  }
  
  new.cols <- unlist(new.col.names, use.names = FALSE)
  if (strip.punctuation) {
    df[new.cols] <- as.data.frame(apply(df[new.cols], 1:2, 
                                          stringr::str_replace_all, pattern='[:punct:]', ''))
  }
  # Convert all factors to characters
  i <- sapply(df, is.factor)
  df[i] <- lapply(df[i], as.character)
    
  return(df)
}