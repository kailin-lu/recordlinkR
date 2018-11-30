require(R6)
require(keras)
require(tensorflow)

#' trainModel
#' 
#' Trains and saves a LSTM Variational Autoencoder 
#' 
#' 
#' @param namesA Character vector of names 
#' @param namesB Character vector of names known to be matches of namesA. 
#' Length of namesA and namesB must be equal. 
#' @param dim.latent Number of dimensions to use in the latent vector
#' @param dim.encode Number of hidden units to use in each of the encoding layers 
#' @param dim.decode Number of hidden units to use in each of the decoding layers 
#' @param max.length Maximum length of each name
#' @param num.encode.layers Number of encoding layers to use 
#' @param num.decode.layers Number of decoding layers to use 
#' @param batch.size Batch size of model training
#' @param epochs Number of epochs to train for 
#' @param lr Learning rate of model. This model is configured to use to the Adam optimizer. 
#' @param validation.split Percentage of data to save for validation 
#' @param save.dir Directory to save the trained encoder
#' @param reconstruct Whether or not show reconstructions 
#' @param reconstruct.n How many reconstructions to show
#' @param reconstruct.display After many epochs to show reconstructions
#' @param earlystop TRUE if stopping early when validation loss is no longer decreasing,
#'  if FALSE then train for all epochs 
#' @param earlystop.patience Number of epochs to wait while validation loss does not
#'  decrease before stopping training early 
#' @param tensorboard TRUE if tensorboard metrics are to be recorded. 
#' Logs are recorded in the /tmp/ directory
#' @param tensorboard.runid Unique identifier for the run to separate tensorboard logs
#' @param verbose Verbosity level for training output, 0 = silence, 1 = minimal, 2 = verbose
#' 
#' @return Trained encoder model 
#' 
#' @export 
trainModel <- function(namesA, 
                       namesB, 
                       dim.latent = 8, 
                       dim.encode = 64, 
                       dim.decode = 64, 
                       max.length = 12, 
                       num.encode.layers = 2, 
                       num.decode.layers = 2, 
                       batch.size = 32,
                       epochs = 100, 
                       lr = 1e-4,
                       validation.split = .2, 
                       save.dir = '~/blocking_models/', 
                       reconstruct = TRUE, 
                       reconstruct.n = 5, 
                       reconstruct.display = 20, 
                       earlystop = TRUE, 
                       earlystop.patience = 10, 
                       tensorboard = TRUE, 
                       tensorboard.runid = as.character(Sys.time()), 
                       verbose = 2) {
  # Start with cleared Keras session
  keras::k_clear_session()
  
  # Check that namesA and namesB are equal length
  if (length(namesA) != length(namesB)) {
    print('ERROR: `namesA` and `namesB` must be equal length')
    return()
  }
  
  # Remove NA
  count.na <- 0 
  count.na <- count.na + sum(is.na(namesA))
  count.na <- count.na + sum(is.na(namesB))
  if (count.na > 0) {
    namesA <- namesA[!is.na(namesA) & !is.na(namesB)]
    namesB <- namesB[!is.na(namesA) & !is.na(namesB)]
    cat('INFO: Removed ',count.na, 'NA rows.')
  }
  
  # Convert factors to characters
  if (is.factor(namesA) | is.factor(namesB)) {
    namesA <- as.character(namesA)
    namesB <- as.character(namesB)
  }
  
  # Check if save directory exists, create if it does not
  if (!dir.exists(save.dir) & !is.null(save.dir)) {
    dir.create(file.path(save.dir))
    print('INFO: Created Directory At: ', save.dir)
  }
  
  # Embed names 
  namesA <- recordlinkR::embedLetters(namesA, max.length)
  namesB <- recordlinkR::embedLetters(namesB, max.length)
  
  # Inputs 
  inputs <- keras::layer_input(shape=c(max.length, length(letters_index)+1))
  
  # Encoder 
  encoder <- inputs %>% keras::layer_lstm(units = dim.encode, 
                                   return_sequences = TRUE, 
                                   name = 'enc0') 
  
  if (num.encode.layers > 1) {
    for (i in 1:(num.encode.layers - 1)) {
      encoder <- encoder %>% keras::layer_lstm(units = dim.encode, 
                                               return_sequences = TRUE, 
                                               name = paste('enc', i, sep = ''))   
    }
  }
  
  flatten <- encoder %>% keras::layer_flatten()
  
  mu <- flatten %>% keras::layer_dense(units = dim.latent, 
                                       kernel_regularizer = keras::regularizer_l2(l = .005), 
                                       name = 'mu')
  mu <- mu %>% keras::layer_activation_leaky_relu(alpha = .02)
  
  log.sigma <- flatten %>% keras::layer_dense(units = dim.latent, 
                                              kernel_regularizer = keras::regularizer_l2(l = .005), 
                                              name = 'log_sigma')
  log.sigma <- log.sigma %>% keras::layer_activation_leaky_relu(alpha = .02)

  # Sampling layer 
  sampling <- function(args) {
    mean <- args[, 1:(dim.latent)]
    log.sigma <- args[, (dim.latent + 1):(2 * dim.latent)]
    
    epsilon <- keras::k_random_normal(
      shape = c(keras::k_shape(mean)[[1]]), 
      mean=0.,
      stddev=1.
    )
    
    return(mean + keras::k_exp(log.sigma/2)*epsilon) 
  }
  
  z <- keras::layer_concatenate(list(mu, log.sigma)) %>% keras::layer_lambda(sampling, name = 'z')
  
  # Decoder 
  repeated <- keras::layer_repeat_vector(z, max.length)
  decoder <- repeated %>% keras::layer_lstm(units = dim.decode, 
                                     return_sequences = TRUE, 
                                     name = 'dec0')
  if (num.decode.layers > 1) {
    for (i in 1:(num.decode.layers - 1)) {
      decoder <- decoder %>% keras::layer_lstm(units = dim.decode, 
                                               return_sequences = TRUE, 
                                               name = paste('dec', i, sep = ''))
    }
  }
  reconstruction <- decoder %>% keras::layer_dense(units = length(letters_index)+1,
                                                   activation = 'softmax', 
                                                   name = 'reconstruction')
  
  model <- keras::keras_model(inputs = inputs, outputs = reconstruction)
  encoder_model <- keras::keras_model(inputs = inputs, outputs = mu)
  
  vae_loss <- function(ytrue, ypred) {
    xent_loss <- keras::k_sum(keras::loss_categorical_crossentropy(ytrue, ypred), axis=-1L)
    kl_loss <- -0.5 * keras::k_mean(1 + log.sigma - keras::k_square(mu) - keras::k_exp(log.sigma), axis = -1L)
    return(xent_loss + kl_loss)
  }
  
  adam <- keras::optimizer_adam(lr = lr)
  model %>% keras::compile(optimizer = adam, loss = vae_loss, metrics='accuracy')
  
  print(summary(model))
  
  callbacks = list() 
  i = 1 
  if (earlystop) {
    early.stopping <- keras::callback_early_stopping(monitor = 'val_loss', 
                                                     patience = earlystop.patience,
                                                     min_delta = .0001, 
                                                     verbose = T)
    callbacks[[i]] <- early.stopping
    paste(c('Earlystopping is activated with patience ',earlystop.patience,'.'), sep='')
    i <- i + 1
  }
  if (tensorboard) {
    tensorboard <- keras::callback_tensorboard(log_dir = paste('/tmp/',tensorboard.runid, sep=''),
                                               histogram_freq = 10)
    callbacks[[i]] <- tensorboard
    print('Tensorboard is activated. To see logs type `tensorboard --logdir="/tmp/"` in the terminal and 
          open `localhost:6006` in the browser.')
    i <- i + 1 
  }
  if (reconstruct) {
    reconstruct <- CheckReconstruction$new(n = reconstruct.n, 
                                           display = reconstruct.display, 
                                           train.data = namesA)
    callbacks[[i]] <- reconstruct
  }

  start_time <- Sys.time()
  
  history <- model %>% keras::fit(
    namesA, namesB, 
    shuffle = T, 
    epochs = epochs, 
    batch_size = batch.size, 
    callbacks = callbacks,
    validation_split = validation.split, 
    verbose = verbose)
  end_time <- Sys.time() 
  print(paste('Model finished training in: ', 
              round(difftime(end_time, start_time,units = 's'),2), 'sec'))
  
  # Save encoder model 
  if (!is.null(save.dir)) {
    encoder_model %>% keras::save_model_hdf5(paste(save.dir, 'encoder.h5', sep='')) 
    print(paste('Encoder Saved At: ', save.dir, 'encoder.h5', sep=''))
  }
  return(encoder_model)
}


#' CheckReconstruction
#' 
#' Custom callback to check reconstruction during training
CheckReconstruction <- R6::R6Class("CheckReconstruction",
                                    inherit = KerasCallback,
                                    
                                    public = list(
                                     seen = 0,
                                     n = NULL, 
                                     display = NULL, 
                                     train.data = NULL, 
                                     initialize = function(n = 5, display = 20, train.data = NULL) {
                                       self$n <- n 
                                       self$display <- display 
                                       self$train.data <- train.data
                                     }, 
                                     
                                     on_epoch_end = function(epoch, logs = list()) {
                                       if (self$seen %% self$display == 0) {
                                         idx <- sample(1:dim(self$train.data)[1], size = self$n)
                                         orig <- self$train.data[idx,,]
                                         
                                         pred <- self$model %>% predict(orig)
                                         originals <- recordlinkR::disembedLetters(orig)
                                         
                                         reconstructions <- recordlinkR::disembedLetters(pred)
                                         
                                         print(c('Original', 'Prediction'))
                                         
                                         for (i in 1:self$n) {
                                           print(c(originals[i], reconstructions[i]))
                                         }
                                       }
                                       self$seen <- self$seen + 1 
                                     }))
