#' loadSampleEncoders
#' 
#' Loads sample encoder models trained on Iowa dataset first names 
#' 
#' @return Example encoder models for Iowa dataset 
#' @importFrom utils data
#' @export
loadSampleEncoders <- function() {
  data("encoder_iowa_first_4")
  data("encoder_iowa_first_256")
  assign('encoder_iowa_first_4', 
         keras::unserialize_model(encoder_iowa_first_4), 
         envir = globalenv())
  assign('encoder_iowa_first_256', 
         keras::unserialize_model(encoder_iowa_first_256), 
         envir = globalenv())
}