% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cleanNames.R
\name{cleanNames}
\alias{cleanNames}
\title{cleanNames}
\usage{
cleanNames(df, cols = ".", split.pattern = NULL,
  split.col.names = NULL, lower = TRUE, strip.punctuation = TRUE)
}
\arguments{
\item{df}{Dataframe}

\item{cols}{Vector of variables to apply changes to. If 'all' then cols = '.'}

\item{split.pattern}{Named list of character to split variables. Variables are list names 
and values are the character to be split on}

\item{split.col.names}{Named list of new variable names. Variables are list names and 
values are vectors of new variable names. Maximum number of splits on each column is the length of values.}

\item{lower}{if TRUE then all elements in df[cols] will be lower cased}

\item{strip.punctuation}{if TRUE then all elements in df[cols] will be stripped of punctuation}
}
\value{
Dataframe with processed character columns
}
\description{
cleanNames is used to preprocess character columns for blocking and linking functions.
Names can be standardized by converting all characters to lower case, splitting full name 
columns into individual parts such as 'first' and 'last', and punctuation can be removed. 
For blocking and linking on autoencoded name fields, it is recommended to split each part of 
the name such as first, middle, and last, into separate columns.
}
