% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/block.R
\name{encode}
\alias{encode}
\title{encode}
\usage{
encode(dfA, dfB, cols.encoder, encoder.model.path = NULL,
  encoder.trainA = NULL, encoder.trainB = NULL, dim.latent = 8,
  dim.encode = 64, dim.decode = 64, max.length = 12,
  num.encode.layers = 2, num.decode.layers = 2, batch.size = 32,
  epochs = 500, lr = 5e-04, validation.split = 0.2,
  save.dir = "~/blocking_models/", reconstruct = TRUE,
  reconstruct.n = 5, reconstruct.display = 20, earlystop = FALSE,
  earlystop.patience = 10, tensorboard = FALSE,
  tensorboard.runid = as.character(Sys.time()), verbose = 2)
}
\arguments{
\item{dfA}{DataframeA}

\item{dfB}{DataframeB}

\item{cols.encoder}{Encoder cols}

\item{encoder.model.path}{Model path if no train}

\item{encoder.trainA}{trainA}

\item{encoder.trainB}{trainB}

\item{dim.latent}{Dim latent}

\item{dim.encode}{Dim encode}

\item{dim.decode}{Dim decode}

\item{max.length}{Max length}

\item{num.encode.layers}{encode layers}

\item{num.decode.layers}{decode layers}

\item{batch.size}{batch size}

\item{epochs}{Epochs}

\item{lr}{Learning rate}

\item{validation.split}{validation split}

\item{save.dir}{Save directory}

\item{reconstruct}{reconstruct}

\item{reconstruct.n}{How many reconstructions to show}

\item{reconstruct.display}{After many epochs to show reconstructions}

\item{earlystop}{TRUE if stopping early when validation loss is no longer decreasing,
if FALSE then train for all epochs}

\item{earlystop.patience}{Number of epochs to wait while validation loss does not
decrease before stopping training early}

\item{tensorboard}{TRUE if tensorboard metrics are to be recorded. 
Logs are recorded in the /tmp/ directory}

\item{tensorboard.runid}{Unique identifier for the run to separate tensorboard logs}

\item{verbose}{Verbosity level for training output, 0 = silence, 1 = minimal, 2 = verboses}
}
\description{
Adds encoded columns to dataframes to be linked
}
