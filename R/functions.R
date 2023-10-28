##########################################
##########################################

# Custom functions used for data preparation and model optimization
# to keep the main script clean and understandable





##########################################
# General data preparation
##########################################

# creates various derived variables for modeling, indices for calibration / validation and
# shortens them to remove the rows containing NA (take care your data initially contains no NA)


# @ data:       data frame or matrix containing date, discharge, precipitation, air temperature
# @ dateformat: string containing the format of the col date (ex.: "%Y-%m-%d")
# @ colnames:   vector containing all 4 exact strings: "date", "discharge", "precip", "temp"; arrange them matching your input data
# @ start:      start year of the index vector generated for calibration validation
# @ end:        end year of the index vector generated for calibration validation

dataPrep <- function(data,
                     dateformat = "%Y-%m-%d", 
                     colname = c("date", "discharge", "precip", "temp"),
                     start = 1981,
                     end = 2020) {
  
  colnames(data) <- c("date", "discharge", "precip", "temp")
 
  data$lag1precip <- shift(x=data$precip,n=1, type= "lag")
  data$lag2precip <- shift(x=data$precip,n=2, type= "lag")
  data$lag3precip <- shift(x=data$precip,n=3, type= "lag")
  data$lag4precip <- shift(x=data$precip,n=4, type= "lag")
  data$lag5precip <- shift(x=data$precip,n=5, type= "lag")
  data$lag6precip <- shift(x=data$precip,n=6, type= "lag")
  data$lag7precip <- shift(x=data$precip,n=7, type= "lag")
  
  data$lag1temp <- shift(x=data$temp,n=1, type= "lag")
  data$lag2temp <- shift(x=data$temp,n=2, type= "lag")
  data$lag3temp <- shift(x=data$temp,n=3, type= "lag")
  data$lag4temp <- shift(x=data$temp,n=4, type= "lag")
  data$lag5temp <- shift(x=data$temp,n=5, type= "lag")
  data$lag6temp <- shift(x=data$temp,n=6, type= "lag")
  data$lag7temp <- shift(x=data$temp,n=7, type= "lag")
  
  data$sum2precip <- frollsum(x=data$precip, n= 2)
  data$sum3precip <- frollsum(x=data$precip, n= 3)
  data$sum4precip <- frollsum(x=data$precip, n= 4)
  data$sum5precip <- frollsum(x=data$precip, n= 5)
  data$sum6precip <- frollsum(x=data$precip, n= 6)
  data$sum7precip <- frollsum(x=data$precip, n= 7)
  data$sum15precip <- frollsum(x=data$precip, n= 15)
  data$sum30precip <- frollsum(x=data$precip, n= 30)
  
  data$mean3temp <- frollmean(x=data$temp, n=3, align = "right")
  data$mean7temp <- frollmean(x=data$temp, n=7, align = "right")
  data$mean30temp <- frollmean(x=data$temp, n=30, align = "right")
  data$mean60temp <- frollmean(x=data$temp, n=60, align = "right")
  data$mean30templag30 <- shift(x=data$mean30temp ,n=30, type= "lag")
  data$sum30precilag30 <- shift(x=data$sum30preci ,n=30, type= "lag")
  
  lh.filter <- as.data.frame(data$discharge)
  lh.filter$lowpass15 <- frollmean(x=data$discharge, n=15, align = "center")
  lh.filter$lowpass31 <- frollmean(x=data$discharge, n=31, align = "center")
  lh.filter$lowpass45 <- frollmean(x=data$discharge, n=45, align = "center")
  lh.filter$lowpass61 <- frollmean(x=data$discharge, n=61, align = "center")
  lh.filter$lowpass121 <- frollmean(x=data$discharge, n=121, align = "center")
  lh.filter$highpass15 <- data$discharge-lh.filter$lowpass15
  lh.filter$highpass31 <- data$discharge-lh.filter$lowpass31
  lh.filter$highpass45 <- data$discharge-lh.filter$lowpass45
  lh.filter$highpass61 <- data$discharge-lh.filter$lowpass61
  lh.filter$highpass121 <- data$discharge-lh.filter$lowpass121
  
  data <- data[!is.na(lh.filter$lowpass121),]
  lh.filter <- lh.filter[!is.na(lh.filter$lowpass121),]
  
  dat <- strptime(data$date, format = dateformat)
  dat.y <- year(dat)
 
  skp <- 1
  count <- 0
  skipunreg <- NA
  for (i in start:end) {
    count <- count + 1
    if (skp!=3) {skipunreg[count]<-i; skp <- skp + 1}
    else {skp <- 1}
  }
  
  skipunreg <- skipunreg[!is.na(skipunreg)]
  calib2 <- dat.y%in%skipunreg
  valid2 <- !calib2
  
  output <- list(data = data[,-1], 
                 date = dat, 
                 calib = calib2, 
                 valid = valid2, 
                 lh.filter = lh.filter)
  
  return(output)
}



##########################################
# Data preparation for LSTM models
##########################################

# creates the 3D array for predictor variables and 2D matrix for discharge data used for LSTM models.
# Used inside the bayes_opt_lstm function. Needed as stand alone for preparation of validation input data.


# @ x:          data frame or matrix with predictor variables used in the LSTM. Data should be standard normalized.
# @ y:          vector with the target variable used in the LSTM. Data should be standard normalized.
# @ timesteps:  number of time steps used in the LSTM
# @ weights:    vector with the same length as y containing weights used in the LSTM
# @ duplicate:  vector giving info about duplication of min or max values for training: c(mode, proportion, times) 
#               ex.: c(max,0.9,1); c(min,0.1,1) adds the highest/lowest 10% of the values 1 time

dataPrepLSTM <- function(x, 
                         y, 
                         weights = FALSE, 
                         timesteps=7,
                         duplicate = FALSE){
  
  yPrep <- matrix(nrow = as.vector(length(y)), ncol = timesteps, data = NA)
  for(i in 1:timesteps){
    
    yPrep[,i] <- shift(x=y, n=i-1, type = "lag")
  }
  yPrep <- yPrep[-(1:i-1),]
  
  dimen <- c(dim(x)[1]+1-timesteps, timesteps, dim(x)[2])
  xPrep <- array(dim=dimen,data = NA)
  for (i in 1:timesteps) {
    xPrep[,i,] <- as.matrix(x[(timesteps+1-i):(dim(x)[1]+1-i),])
  }
  if (weights[1]!=FALSE) {
    weightsPrep <- matrix(nrow = as.vector(length(y)), ncol = timesteps, data = NA)
    for(i in 1:timesteps){
      
      weightsPrep[,i] <- shift(x=weights, n=i-1, type = "lag")
    }
    weightsPrep <- weightsPrep[-(1:i-1),]
    return(list(x=xPrep, y=yPrep, weights = weightsPrep))
  }
  else if (duplicate[1]!=FALSE) {
    # create logical vector depending on proportion of max / min values
    if(duplicate[1]=="max"){add <- yPrep[,1]>quantile(x = yPrep, probs = as.numeric(duplicate[2]))}
    if(duplicate[1]=="min"){add <- yPrep[,1]<quantile(x = yPrep, probs = as.numeric(duplicate[2]))}
    
    # select the y an d x data that should be duplicated
    addy <- yPrep[add,]
    addx <- xPrep[add,,]
    
    #append the previously selected data n times to the original data.
    for (t in 1:duplicate[3]) {
      yPrep <- rbind(yPrep,addy)
      xPrep <- abind::abind(xPrep,addx, along = 1)
    }
    return(list(x=xPrep, y=yPrep))
    
  }
  else {return(list(x=xPrep, y=yPrep))}
}

########################################
# RMSE function
########################################

rmse <- function(x,y){
  rmse <- sqrt(mean((x-y)^2))
  return(rmse)
}

#######################################
# Grid search optimization for XGBoost
#######################################

# basic grid search optimization for XGBoost. No inbuilt cross validation. 
# Slower than the Bayesian optimization but maybe easyer to understand.


# @ data:       matrix with training data of the predictor variables (x)
# @ label:      matrix with training data of the target variable (x)
# @ vdata:      matrix with validation data of the predictor variables (x)
# @ vlabel:     matrix with validation data of the target variable (x)
# @ max_depth:  vector giving the max depth of the trees for the grid search optimization
# @ eta:        vector giving the the learning rates used in the grid search optimization
# @ nrounds:    vector giving the max depth of the trees for the initial grid search optimization
# @ bt:         deprecated, legacy arg. Keep on FLASE

optimize_xgb <- function(data, label, vdata = NA, vlabel = NA, max.depth = 3:8, eta = seq(0.025,0.2,0.025), 
                         nrounds = c(20,40,70,100,130,160,200), nthread = detectCores()-1, 
                         objective = "reg:squarederror", bt = FALSE){
  time1 <- as.numeric(Sys.time())
  if (bt[1] == 0) {
    
    k <- 0
    numNA <- length(max.depth)*length(nrounds)*length(eta)
    s_rmse <- rep(NA, numNA)
    s_maxdepth <- rep(NA, numNA)
    s_nrounds <- rep(NA, numNA)
    s_eta <- rep(NA, numNA)
    
    
    for (i in max.depth) {
      for (j in nrounds) {
        for (e in 1:length(eta)) {
          
          k <- k+1
          
          xgb_mod <- xgboost(data = data, label = label, max.depth = i, eta = eta[e], 
                             nrounds = j, nthread = nthread,objective = objective,
                             early_stopping_rounds = 5)
          pre_xgb <- predict(object = xgb_mod, newdata = vdata)
          
          
          s_rmse[k] <- sqrt(mean((pre_xgb-vlabel)^2))
          s_maxdepth[k] <- i
          s_nrounds[k] <- j
          s_eta[k] <- eta[e]
          
        }
      }
    }
    
    best <- which(min(s_rmse, na.rm = TRUE)==s_rmse)
    
    rmse_opt <- s_rmse[best]
    maxdepth_opt <- s_maxdepth[best]
    nrounds_opt <- s_nrounds[best]
    eta_opt <- s_eta[best]
    
    if (nrounds_opt == tail(nrounds, n=1)) {
      
      xgb_opt <- xgboost(data = data, label = label, max.depth = maxdepth_opt, eta = eta_opt, 
                         nrounds = nrounds_opt, nthread = nthread, objective = objective,
                         early_stopping_rounds = 5)
      
      opt_result <- list(xgb_opt,rmse_opt,maxdepth_opt,nrounds_opt,eta_opt)
      
    }
    
    else{
      
      if (nrounds_opt == nrounds[1]) {
        
        nrounds.2 <- 1:nrounds[2]
        
      }
      
      else{
        
        nrounds.2 <- seq(from=nrounds[match(nrounds_opt, nrounds)-1],
                         to=nrounds[match(nrounds_opt, nrounds)+1],
                         by=2)
        
      }
      
      k <- 0
      s_rmse <- rep(NA,length(nrounds.2))
      
      for (i in nrounds.2) {
        k <- k+1
        xgb_mod <- xgboost(data = data, label = label, max.depth = maxdepth_opt, eta = eta_opt, 
                           nrounds = i, nthread = nthread,objective = objective,
                           early_stopping_rounds = 5)
        pre_xgb <- predict(object = xgb_mod, newdata = vdata)
        
        
        s_rmse[k] <- sqrt(mean((pre_xgb-vlabel)^2))
        
      }
      
    rmse_opt <-  s_rmse[which(min(s_rmse, na.rm = TRUE)==s_rmse)]
    xgb_opt <- xgboost(data = data, label = label, max.depth = maxdepth_opt, eta = eta_opt, 
                       nrounds = nrounds_opt, nthread = nthread, objective = objective)
    
    opt_result <- list(xgb_opt,rmse_opt,maxdepth_opt,nrounds_opt,eta_opt)
    
    }
    
  }
  
  if(bt[1]>0){
    print("Currently not implemented")
  }
  print(paste("optimization completed in: ", as.numeric(Sys.time()-time1)%/%60, " minutes ",
              round(as.numeric(Sys.time()-time1)%%60, digits = 1), " seconds"))
  return(opt_result)
}


################################################
# normalize input data for NN,LSTM, and GRU model inputs
################################################

# to normalize the input data for NN,LSTM, and GRU


normalize <- function(x) {
  return (apply(X = as.matrix(x), MARGIN = 2,FUN = function(x) (x - min(x)) / (max(x) - min(x))))
}



trans_back <- function(x, min, max) {x * (max - min) + min}


################################################
# Bayesian optimization for XGBoost
################################################

# Implementation of Bayesian hyper parameter optimization with cross validation for XGBoost.
# Fast and efficient: on my machine a run with default parameters takes 7 to 8 minutes (i7 12700k).
# Returns a list with the optimized hyper parameter, the optimized model, and a summary of the Bayesian optimization.


# @ data:       matrix of predictor variables (x) used for training and cross validation
# @ label:      vector with target variable (y) used for training and cross validation
# @ max_depth:  vector giving the lower and upper bounds (integers) of optimization of max depth of the trees
# @ eta:        vector giving the lower and upper bounds of optimization of the learning rate
# @ alpha:      vector giving the lower and upper bounds of optimization of 
# @ lambda:     vector giving the lower and upper bounds of optimization of 
# @ nrounds:    maximum number of trees built
# @ nfold:      number indicating how many fold the cross validation is done
# @ epochs_opt: number of optimization epochs during Bayesian optimization
# @ initPoints: number of initial points calculated for Bayesian optimization


bayes_opt_xgb <- function(data, 
                         label, 
                         max_depth = c(2L,12L), 
                         eta = c(0.001,0.25),
                         alpha = c(1,12),
                         lambda = c(1,12),
                         min_child_weight = c(1,50),
                         nrounds = 250,
                         nfold = 5,
                         epochs_opt = 15,
                         initPoints= 18){
  
  time1 <- as.numeric(Sys.time())
  
  obj_func <- function(eta, max_depth, min_child_weight, lambda, alpha) { 
    
    param <- list(
      
      # Hyter parameters 
      eta = eta,
      max_depth = max_depth,
      min_child_weight = min_child_weight,
      lambda = lambda,
      alpha = alpha,
      
      # Tree model 
      booster = "gbtree",
      
      # Regression problem 
      objective = "reg:squarederror",
      
      # Use the Mean Absolute Percentage Error
      eval_metric = "rmse")
    
    xgbcv <- xgb.cv(params = param,
                    data = data,
                    label = label,
                    nrounds = nrounds,
                    nfold = nfold,
                    prediction = TRUE,
                    early_stopping_rounds = 5,
                    verbose = 0,
                    maximize = F,
                    nthread = detectCores())
    
    lst <- list(
      
      # First argument must be named as "Score"
      # Function finds maxima so inverting the output
      Score = -min(xgbcv$evaluation_log$test_rmse_mean),
      
      # Get number of trees for the best performing model
      nrounds = xgbcv$best_iteration
    )
    
    return(lst)
  }
  
  
  bounds <- list(eta = eta,
                 max_depth = max_depth
                 ,min_child_weight = min_child_weight
                 ,lambda = lambda
                 ,alpha = alpha
  )
  
  
  set.seed(1234)
  bayes_out <- bayesOpt(FUN = obj_func, bounds = bounds, initPoints = initPoints, iters.n = epochs_opt)
  
  
  # Combine best params with base params
  opt_params <- append(list(booster = "gbtree", 
                            objective = "reg:squarederror", 
                            eval_metric = "rmse"), 
                       getBestPars(bayes_out))
  
  # Run cross validation 
  xgbcv <- xgb.cv(params = opt_params,
                  data = data,
                  label = label,
                  nrounds = nrounds,
                  nfold = nfold,
                  prediction = TRUE,
                  early_stopping_rounds = 5,
                  verbose = 0,
                  maximize = F)
  
  # Get optimal number of rounds
  nrounds = xgbcv$best_iteration
  
  # Fit a xgb model
  opt_mdl <- xgboost(data = data, label = label, 
                 params = opt_params, 
                 maximize = F, 
                 early_stopping_rounds = 5, 
                 nrounds = nrounds, 
                 verbose = 0)
  
  output_list <- list(optimized_param = data.frame(getBestPars(bayes_out),nrounds),
                      optimized_mod = opt_mdl,
                      bayes_summary = bayes_out$scoreSummary)

  print(paste("optimization completed in: ", as.numeric(Sys.time()-time1)%/%60, " minutes ",
              round(as.numeric(Sys.time()-time1)%%60, digits = 1), " seconds"))
  return(output_list)
}
  


#############################################
# Bayesian optimization for lightGBM
#############################################

# Implementation of Bayesian hyper parameter optimization with cross validation for LightGBM.
# Fast and efficient: on my machine a run with default parameters takes 3 to 4 minutes.
# Returns a list with the optimized hyper parameter, the optimized model, and a summary of the Bayesian optimization.


# @ data:       matrix of predictor variables (x)
# @ label:      vector with target variable (y)
# @ max_depth:  vector giving the lower and upper bounds (integers) of optimization of max depth of the trees
# @ eta:        vector giving the lower and upper bounds of optimization of the learning rate
# @ num_leaves: vector giving the lower and upper bounds (integers) of optimization of max number of leaves per tree
# @ nrounds:    maximum number of trees built
# @ nfold:      number indicating how many fold the cross validation is done
# @ epochs_opt: number of optimization epochs during Bayesian optimization
# @ initPoints: number of initial points calculated for Bayesian optimization

bayes_opt_lgb <- function(data, 
                         label, 
                         max_depth = c(2L,12L), 
                         eta = c(0.001,0.25),
                         num_leaves = c(2L,100L),
                         nrounds = 200,
                         nfold = 5,
                         epochs_opt = 10,
                         initPoints = 12){
  
  time1 <- as.numeric(Sys.time())
  
  obj_func <- function(eta, max_depth, num_leaves) { 
    
    param <- list(
      
      # Hyper parameters 
      eta = eta,
      max_depth = max_depth,
      num_leaves = num_leaves,
      
      # Regression problem 
      objective = "regression",
      nthread = detectCores())
    
    lgbcv <- lgb.cv(params = param,
                    data = data,
                    label = label,
                    nfold = nfold,
                    early_stopping_rounds = 5,
                    nrounds = nrounds,
                    verbose = 0)
    
    lst <- list(
      
      # First argument must be named as "Score"
      # Function finds maxima so inverting the output
      Score = -lgbcv$best_score,
      
      # Get number of trees for the best performing model
      nrounds = lgbcv$best_iter
    )
    
    return(lst)
  }
  
  
  bounds <- list(eta = eta,
                 max_depth = max_depth
                 ,num_leaves = num_leaves
  )
  
  
  set.seed(1234)
  bayes_out <- bayesOpt(FUN = obj_func, bounds = bounds, initPoints = initPoints, iters.n = epochs_opt)
  
  
  # Combine best params with base params 
  opt_params <- append(list(objective = "regression"), 
                       getBestPars(bayes_out))
  
  # Run cross validation 
  lgbcv <- lgb.cv(params = opt_params,
                  data = data,
                  label = label,
                  nrounds = nrounds,
                  nfold = nfold,
                  # early_stopping_rounds = 5,
                  verbose = 0,)
  
  # Get optimal number of rounds
  nrounds = lgbcv$best_iter
  
  # Fit a lgb model
  opt_mdl <- lightgbm(data = data, label = label, 
                     params = opt_params, 
                     # early_stopping_rounds = 5, 
                     nrounds = nrounds, 
                     verbose = 0)
  
  output_list <- list(optimized_param = data.frame(getBestPars(bayes_out),nrounds),
                      optimized_mod = opt_mdl,
                      bayes_summary = bayes_out$scoreSummary)
  
  print(paste("optimization completed in: ", as.numeric(Sys.time()-time1)%/%60, " minutes ",
              round(as.numeric(Sys.time()-time1)%%60, digits = 1), " seconds"))
  return(output_list)
}  



########################################
# Function to create custom LSTM models
########################################

# This function is mainly used in the Bayesian optimization for LSTM models
#  but can also be used to create LSTM models outside of this.


# @ layers:     number of layers of the LSTM
# @ units:      number of units per layer
# @ dropout:    dropout rate
# @ timesteps:  time step size of the input data
# @ n_features: number of features used (x variables)

create_LSTM <- function(layers, units, dropout,
                         timesteps,
                         n_features){
  
  
  input <- layer_input(shape = c(timesteps, n_features))
  for(lay in 1:layers){
    # return sequences on for all except for last layer
    if(lay < layers) {
      return_sequences_flag <- TRUE
    } else {
      return_sequences_flag <- FALSE
    }
    # add a lstm layer
    if(lay == 1) {
      output <- input %>% layer_lstm(units = units,
                                    return_sequences = return_sequences_flag,
                                    dropout = dropout)
    } else {
      output <- output %>% layer_lstm(units = units,
                                     return_sequences = return_sequences_flag,
                                     dropout = dropout)
    }
  }
  output <- output %>% layer_dense(1)
  model <- keras_model(input, output) %>%
    keras::compile(loss = "mse",
                   optimizer = "adam")  
  return(model)
}



#############################################
# Bayesian optimization for LSTM models
#############################################

# Implementation of Bayesian hyper parameter optimization for LSTM.
# Take care: optimization can take quite long. On my machine (GTX 3070) a run with default parameters takes
# between 1 and 3 hours (depends on how complex the optimal model is).
# Returns a list with the optimized hyper parameter, the optimized model, and a summary of the Bayesian optimization.


# @ x:      predictor variables in a 2D format. Input data should be standard normalized. Creates the needed 3D format inside.
# @ y:      target variable in a 1D format. Input data should be standard normalized. Creates the needed 2D format inside.
# @
# @
# @
# @
# @
# @
# @
# @
# @
# @
# @ duplicate:  vector giving info about duplication of min or max values for training: c(mode, proportion, times) 
#               ex.: c(max,0.9,1); c(min,0.1,1) adds the highest/lowest 10% of the values 1 time

bayes_opt_LSTM <- function(x, 
                         y, 
                         layers = c(1L,5L), 
                         units = c(5L,150L),
                         dropout = c(0,0.4),
                         batchsize = c(5L,100L),
                         timesteps = c(5L,100L),
                         epochs_opt = 15,
                         epochs_lstm = 100,
                         earlystop = 8,
                         validation_split = 0.25,
                         initPoints = 20,
                         duplicate = FALSE){
  
  time1 <- as.numeric(Sys.time())
  
  obj_func <- function(layers, units, dropout, batchsize, timesteps) { 
    
   train.data <- dataPrepLSTM(x = x, y = y, timesteps = timesteps, duplicate = duplicate)
      
    
    mod.lstm <- create_LSTM(layers = layers, 
                            units = units,
                            dropout = dropout,
                            timesteps = timesteps,
                            n_features = dim(train.data$x)[3])
    
    history_lstm <- fit(object = mod.lstm, 
                        x=train.data$x, 
                        y=train.data$y,
                        epochs = epochs_lstm, 
                        verbose = 0, 
                        shuffle = TRUE, 
                        batch_size = batchsize,
                        validation_split = validation_split, 
                        callbacks = list(
                          callback_early_stopping(patience = earlystop, restore_best_weights = TRUE, 
                                                  mode = "min", monitor = "val_loss"))
                        )
    
    lst <- list(
      
      # First argument must be named as "Score"
      # Function finds maxima so inverting the output
      Score = -min(history_lstm$metrics$val_loss))
    
    return(lst)
  }
  
  
    bounds <- list(layers = layers, 
                 units = units, 
                 dropout = dropout, 
                 batchsize = batchsize, 
                 timesteps = timesteps)
  
  
  set.seed(1234)
  bayes_out <- bayesOpt(FUN = obj_func, bounds = bounds, initPoints = initPoints, iters.n = epochs_opt)
  
  # Get optimized parameters
  opt_params <- getBestPars(bayes_out)
  
  
  # create optimized data set
  
  opt_data <- dataPrepLSTM(x = x,
                           y = y,
                           timesteps = opt_params$timesteps)
  
  
  # create a optimized lstm model
  
  opt_mdl <- create_LSTM(layers = opt_params$layers, 
                         units = opt_params$units,
                         dropout = opt_params$dropout,
                         timesteps = opt_params$timesteps,
                         n_features = dim(opt_data$x)[3])
  
  
  # Fit a lstm model
  opt_history_lstm <- fit(object = opt_mdl, 
                          x=opt_data$x, 
                          y=opt_data$y,
                          epochs = epochs_lstm, 
                          verbose = 0, 
                          shuffle = FALSE, 
                          batch_size = opt_params$batchsize,
                          validation_split = validation_split, 
                          callbacks = list(
                            callback_early_stopping(patience = earlystop, restore_best_weights = TRUE, 
                                                    mode = "min", monitor = "val_loss")))
  
  output_list <- list(optimized_param = data.frame(getBestPars(bayes_out)),
                      optimized_history = opt_history_lstm,
                      optimized_mod = opt_mdl,
                      bayes_summary = bayes_out$scoreSummary,
                      optimized_input_dim = dim(opt_data$x))
  
  print(paste("optimization completed in: ", as.numeric(Sys.time()-time1)%/%60, " minutes ",
              round(as.numeric(Sys.time()-time1)%%60, digits = 1), " seconds"))
  return(output_list)
}



########################################
# Function to create custom GRU models
########################################

# This function is mainly used in the Bayesian optimization for GRU models
#  but can also be used to create GRU models outside of this.


# @ layers:     number of layers of the GRU
# @ units:      number of units per layer
# @ dropout:    dropout rate
# @ timesteps:  time step size of the input data
# @ n_features: number of features used (x variables)

create_GRU <- function(layers, units, dropout,
                        timesteps,
                        n_features){
  
  
  input <- layer_input(shape = c(timesteps, n_features))
  for(lay in 1:layers){
    # return sequences on for all except for last layer
    if(lay < layers) {
      return_sequences_flag <- TRUE
    } else {
      return_sequences_flag <- FALSE
    }
    # add a gru layer
    if(lay == 1) {
      output <- input %>% layer_gru(units = units,
                                     return_sequences = return_sequences_flag,
                                     dropout = dropout)
    } else {
      output <- output %>% layer_gru(units = units,
                                      return_sequences = return_sequences_flag,
                                      dropout = dropout)
    }
  }
  output <- output %>% layer_dense(1)
  model <- keras_model(input, output) %>%
    keras::compile(loss = "mse",
                   optimizer = "adam")  
  return(model)
}



#############################################
# Bayesian optimization for GRU Models
#############################################

# Implementation of Bayesian hyper parameter optimization for GRU.
# Take care: optimization can take quite long. On my machine (GTX 3070) a run with default parameters takes
# between 1 and 3 hours (depends on how complex the optimal model is).
# Returns a list with the optimized hyper parameter, the optimized model, and a summary of the Bayesian optimization.


# @ x:      predictor variables in a 2D format. Input data should be standard normalized. Creates the needed 3D format inside.
# @ y:      target variable in a 1D format. Input data should be standard normalized. Creates the needed 2D format inside.
# @
# @
# @
# @
# @
# @
# @
# @
# @
# @
# @ duplicate:  vector giving info about duplication of min or max values for training: c(mode, proportion, times) 
#               ex.: c(max,0.9,1); c(min,0.1,1) adds the highest/lowest 10% of the values 1 time

bayes_opt_GRU <- function(x, 
                           y, 
                           layers = c(1L,5L), 
                           units = c(5L,150L),
                           dropout = c(0,0.4),
                           batchsize = c(5L,100L),
                           timesteps = c(5L,100L),
                           epochs_opt = 15,
                           epochs_gru = 100,
                           earlystop = 8,
                           validation_split = 0.25,
                           initPoints = 20,
                           duplicate = FALSE){
  
  time1 <- as.numeric(Sys.time())
  
  obj_func <- function(layers, units, dropout, batchsize, timesteps) { 
    
    train.data <- dataPrepLSTM(x = x, y = y, timesteps = timesteps, duplicate = duplicate)
    
    
    mod.lstm <- create_GRU(layers = layers, 
                            units = units,
                            dropout = dropout,
                            timesteps = timesteps,
                            n_features = dim(train.data$x)[3])
    
    history_gru <- fit(object = mod.lstm, 
                        x=train.data$x, 
                        y=train.data$y,
                        epochs = epochs_gru, 
                        verbose = 0, 
                        shuffle = TRUE, 
                        batch_size = batchsize,
                        validation_split = validation_split, 
                        callbacks = list(
                          callback_early_stopping(patience = earlystop, restore_best_weights = TRUE, 
                                                  mode = "min", monitor = "val_loss"))
    )
    
    lst <- list(
      
      # First argument must be named as "Score"
      # Function finds maxima so inverting the output
      Score = -min(history_gru$metrics$val_loss))
    
    return(lst)
  }
  
  
  bounds <- list(layers = layers, 
                 units = units, 
                 dropout = dropout, 
                 batchsize = batchsize, 
                 timesteps = timesteps)
  
  
  set.seed(1234)
  bayes_out <- bayesOpt(FUN = obj_func, bounds = bounds, initPoints = initPoints, iters.n = epochs_opt)
  
  # Get optimized parameters
  opt_params <- getBestPars(bayes_out)
  
  
  # create optimized data set
  
  opt_data <- dataPrepLSTM(x = x,
                           y = y,
                           timesteps = opt_params$timesteps)
  
  
  # create a optimized gru model
  
  opt_mdl <- create_GRU(layers = opt_params$layers, 
                         units = opt_params$units,
                         dropout = opt_params$dropout,
                         timesteps = opt_params$timesteps,
                         n_features = dim(opt_data$x)[3])
  
  
  # Fit a gru model
  opt_history_gru <- fit(object = opt_mdl, 
                          x=opt_data$x, 
                          y=opt_data$y,
                          epochs = epochs_gru, 
                          verbose = 0, 
                          shuffle = FALSE, 
                          batch_size = opt_params$batchsize,
                          validation_split = validation_split, 
                          callbacks = list(
                            callback_early_stopping(patience = earlystop, restore_best_weights = TRUE, 
                                                    mode = "min", monitor = "val_loss")))
  
  output_list <- list(optimized_param = data.frame(getBestPars(bayes_out)),
                      optimized_history = opt_history_gru,
                      optimized_mod = opt_mdl,
                      bayes_summary = bayes_out$scoreSummary,
                      optimized_input_dim = dim(opt_data$x))
  
  print(paste("optimization completed in: ", as.numeric(Sys.time()-time1)%/%60, " minutes ",
              round(as.numeric(Sys.time()-time1)%%60, digits = 1), " seconds"))
  return(output_list)
}


########################################
# Function to create custom NN models
########################################

# This function is mainly used in the Bayesian optimization for GRU models
#  but can also be used to create GRU models outside of this.


# @ layers:     number of layers of the GRU
# @ units:      number of units per layer
# @ dropout:    dropout rate
# @ timesteps:  time step size of the input data
# @ n_features: number of features used (x variables)

create_NN <- function(layers, 
                      units, 
                      dropout,
                      n_features,
                      activation = "softmax"){
  
  
  input <- layer_input(shape = n_features)
  for(lay in 1:layers){
    # return sequences on for all except for last layer
    
    # add a NN layer
    if(lay == 1) {
      output <- input %>% layer_dense(units = units,
                                    activation = activation)
    } else {
      output <- output %>% layer_dropout(rate = dropout)
      output <- output %>% layer_dense(units = units,
                                     activation = activation)
    }
  }
  output <- output %>% layer_dense(units = 1)
  model <- keras_model(input, output) %>%
    keras::compile(loss = "mse",
                   optimizer = "adam")  
  return(model)
}



#############################################
# Bayesian optimization for neural network Models
#############################################

# Implementation of Bayesian hyper parameter optimization for NN.
# Take care: optimization can take quite long. On my machine (GTX 3070) a run with default parameters takes
# between 1 and 3 hours (depends on how complex the optimal model is).
# Returns a list with the optimized hyper parameter, the optimized model, and a summary of the Bayesian optimization.


# @ x:      predictor variables in a 2D format. Input data should be standard normalized. Creates the needed 3D format inside.
# @ y:      target variable in a 1D format. Input data should be standard normalized. Creates the needed 2D format inside.
# @
# @
# @
# @
# @
# @
# @
# @
# @
# @
# @ duplicate:  vector giving info about duplication of min or max values for training: c(mode, proportion, times) 
#               ex.: c(max,0.9,1); c(min,0.1,1) adds the highest/lowest 10% of the values 1 time

bayes_opt_NN <- function(x, 
                         y, 
                         layers = c(1L,5L), 
                         units = c(5L,150L),
                         dropout = c(0,0.4),
                         batchsize = c(5L,100L),
                         epochs_opt = 15,
                         epochs_nn = 100,
                         earlystop = 8,
                         validation_split = 0.25,
                         initPoints = 20,
                         duplicate = FALSE,
                         activation = "softmax"){
  
  time1 <- as.numeric(Sys.time())
  
  obj_func <- function(layers, units, dropout, batchsize) { 
    
    
    mod.nn <- create_NN(layers = layers, 
                        units = units,
                        dropout = dropout,
                        n_features = dim(x)[2],
                        activation = activation)
    
    history_nn <- fit(object = mod.nn, 
                        x=x, 
                        y=y,
                        epochs = epochs_nn, 
                        verbose = 0, 
                        shuffle = TRUE, 
                        batch_size = batchsize,
                        validation_split = validation_split, 
                        callbacks = list(
                          callback_early_stopping(patience = earlystop, restore_best_weights = TRUE, 
                                                  mode = "min", monitor = "val_loss"))
    )
    
    lst <- list(
      
      # First argument must be named as "Score"
      # Function finds maxima so inverting the output
      Score = -min(history_nn$metrics$val_loss))
    
    return(lst)
  }
  
  
  bounds <- list(layers = layers, 
                 units = units, 
                 dropout = dropout, 
                 batchsize = batchsize)
  
  
  set.seed(1234)
  bayes_out <- bayesOpt(FUN = obj_func, bounds = bounds, initPoints = initPoints, iters.n = epochs_opt)
  
  # Get optimized parameters
  opt_params <- getBestPars(bayes_out)
  
  
  # create optimized data set
  
  
  
  # create a optimized nn model
  
  opt_mdl <- create_NN(layers = opt_params$layers, 
                        units = opt_params$units,
                        dropout = opt_params$dropout,
                        n_features = dim(x)[2])
  
  
  # Fit a nn model
  opt_history_nn <- fit(object = opt_mdl, 
                         x=x, 
                         y=y,
                         epochs = epochs_nn, 
                         verbose = 0, 
                         shuffle = FALSE, 
                         batch_size = opt_params$batchsize,
                         validation_split = validation_split, 
                         callbacks = list(
                           callback_early_stopping(patience = earlystop, restore_best_weights = TRUE, 
                                                   mode = "min", monitor = "val_loss")))
  
  output_list <- list(optimized_param = data.frame(getBestPars(bayes_out)),
                      optimized_history = opt_history_nn,
                      optimized_mod = opt_mdl,
                      bayes_summary = bayes_out$scoreSummary)
  
  print(paste("optimization completed in: ", as.numeric(Sys.time()-time1)%/%60, " minutes ",
              round(as.numeric(Sys.time()-time1)%%60, digits = 1), " seconds"))
  return(output_list)
}



#############################################
# calculate monthly means of measured and modeled data
#############################################

# 
# 


# @ measured:   predictor variables in a 2D format. Input data should be standard normalized. Creates the needed 3D format inside.
# @ modeled:    target variable in a 1D format. Input data should be standard normalized. Creates the needed 2D format inside.
# @ date:       ???

monthly_mean <- function(measured, 
                         modeled, 
                         date){
  
  skip <- length(modeled)-length(measured)
  
  # check if modeled and measured data have the same length and shorten the modeled data accordingly
  if(skip<0){
    measured <- tail(measured, n=skip)
    date <- tail(date, n = skip)
  }
  
  # extract the year and month of the date
  year <- year(date)
  month <- month(date)
  
  # create data frame with year and months as factors to aggregate them later
  pre.out <- data.frame(year = as.factor(year), 
                        month = as.factor(month), 
                        measured= measured, 
                        modeled = modeled)
  
  # aggregate monthly mean values of measured and modeled data
  output.l <- aggregate(x = cbind(measured, modeled)~year+month, 
                      data = pre.out,
                      FUN = "mean")
  
  # transform long data frame with both monthly mean values into two seperate wide data frames 
  output.w <- list(measured = spread(data = output.l[,1:3], key = "month", value = "measured")[,-1],
                   modeled = spread(data = output.l[,c(1,2,4)], key = "month", value = "modeled")[,-1])
  colnames(output.w$measured) <- month.abb
  colnames(output.w$modeled) <- month.abb
  return(output.w)
  
}


#############################################
# Plot monthly mean values based on data generated by monthly_mean
#############################################

# This Function returns a  plot based on the monthly mean data generated by the 
# function monthly_mean. 


# @ data:   a list generated by the function monthly_mean
# @ main:   string to name the plot


monthly_plot <- function(data, main="Titel"){
  plot(colMeans(data$measured, na.rm = TRUE), 
       type = "l", xlab = "Month", ylab = "Mean Discharge", 
       xaxt = "n", col = "blue", main = main)
  lines(colMeans(data$modeled, na.rm = TRUE), col = "chartreuse4")
  axis(side = 1, at = 1:12, labels = month.abb)
  legend("topright", legend = c("model", "data"), bty = "n", 
         lty = 1, col = c("chartreuse4", "blue"))
}


#############################################
# Analyzes the model based on validation data and saves the results
#############################################

# 
# 


# @ measured:     vector with measured data.
# @ modeled:      vector with modeled data.
# @ catchment:    string with catchment name. Used to generate the plot titels and save names.
# @ unscale:      vector giving the standard deviation and mean used to transform the LSTM output back: c(std,mean)
# @ mod_type:     String with model name. Use one of the exact strings: "xgb", "lstm", "lgbm", "gru", "nn"
# @ model:        model if model type is xgb or lgbm

analyze_model <- function(measured, 
                          modeled,
                          catchment,
                          unscale=NULL,
                          mod_type,
                          model=NULL){
  if(mod_type=="xgb"){
    
    rmse <- round(sqrt(mean((modeled-measured)^2)), digits = 2)
    mod_mean <- round(mean(modeled), digits = 2)
    measured_mean <- round(mean(measured), digits = 2)
    mod_top10 <- round(quantile(x = modeled, probs = 0.9), digits = 2)
    measured_top10 <- round(quantile(x = measured, probs = 0.9), digits = 2)
    mod_low10 <- round(quantile(x = modeled, probs = 0.1), digits = 2)
    measured_low10 <- round(quantile(x = measured, probs = 0.1), digits = 2)
    mod_nse <- round(NSE(sim = as.matrix(modeled), obs = as.matrix(measured)), digits = 3)
    mod_kge <- round(KGE(sim = as.matrix(modeled), obs = as.matrix(measured)),digits = 3)
    
    
    maxy <- max(measured,modeled)*1.1
    miny <- min(modeled-measured)*1.1
    path1 <- paste("../Results/Plots/", catchment, "_XGBoost_ts.pdf", sep = "")
    path2 <- paste("../Results/Plots/", catchment, "_XGBoost_importance.pdf", sep = "")
    
    pdf(file = path1, width = 14, height = 7)
    plot(measured, type = "l", col="blue", ylim = c(miny,maxy), 
         main = paste(catchment, "XGBoost"), ylab = "Discharge")
    lines(modeled, col="chartreuse4")
    lines(modeled-measured, col="red")
    abline(h=0)
    abline(h = measured_mean, col = "blue")
    abline(h = mod_mean, col = "chartreuse4")
    legend("topright", legend = c("model", "data", "model - data"), bty = "n", 
           lty = 1, col = c("chartreuse4", "blue", "red"))
    dev.off()
    
    pdf(file = path2)
    xgb.plot.importance(xgb.importance(model=model), top_n = 15)
    dev.off()
  }
  
  if(mod_type=="lgbm") {
    
    rmse <- round(sqrt(mean((modeled-measured)^2)), digits = 2)
    mod_mean <- round(mean(modeled), digits = 2)
    measured_mean <- round(mean(measured), digits = 2)
    mod_top10 <- round(quantile(x = modeled, probs = 0.9), digits = 2)
    measured_top10 <- round(quantile(x = measured, probs = 0.9), digits = 2)
    mod_low10 <- round(quantile(x = modeled, probs = 0.1), digits = 2)
    measured_low10 <- round(quantile(x = measured, probs = 0.1), digits = 2)
    mod_nse <- round(NSE(sim = as.matrix(modeled), obs = as.matrix(measured)), digits = 3)
    mod_kge <- round(KGE(sim = as.matrix(modeled), obs = as.matrix(measured)),digits = 3)
    
    
    maxy <- max(measured,modeled)*1.1
    miny <- min(modeled-measured)*1.1
    path1 <- paste("../Results/Plots/", catchment, "_LightGBM_ts.pdf", sep = "")
    path2 <- paste("../Results/Plots/", catchment, "_LightGBM_importance.pdf", sep = "")
    
    pdf(file = path1, width = 14, height = 7)
    plot(measured, type = "l", col="blue", ylim = c(miny,maxy), 
         main = paste(catchment, "LightGBM"), ylab = "Discharge")
    lines(modeled, col="chartreuse4")
    lines(modeled-measured, col="red")
    abline(h=0)
    abline(h = measured_mean, col = "blue")
    abline(h = mod_mean, col = "chartreuse4")
    legend("topright", legend = c("model", "data", "model - data"), bty = "n", 
           lty = 1, col = c("chartreuse4", "blue", "red"))
    dev.off()
    
    pdf(file = path2)
    lgb.plot.importance(lgb.importance(model=model),top_n = 15)
    dev.off()
    
  }
  
  if(mod_type=="lstm") {
    
    # shorten the measured data to match the modeled
    skip <- length(modeled)-length(measured)
    measured <- tail(measured, n = skip)
    
    # transform the modeled data back
    modeled <- modeled*unscale[1]+unscale[2]
    
    # calculate statistics
    rmse <- round(sqrt(mean((modeled-measured)^2)), digits = 2)
    mod_mean <- round(mean(modeled), digits = 2)
    measured_mean <- round(mean(measured), digits = 2)
    mod_top10 <- round(quantile(x = modeled, probs = 0.9), digits = 2)
    measured_top10 <- round(quantile(x = measured, probs = 0.9), digits = 2)
    mod_low10 <- round(quantile(x = modeled, probs = 0.1), digits = 2)
    measured_low10 <- round(quantile(x = measured, probs = 0.1), digits = 2)
    mod_nse <- round(NSE(sim = as.matrix(modeled), obs = as.matrix(measured)), digits = 3)
    mod_kge <- round(KGE(sim = as.matrix(modeled), obs = as.matrix(measured)),digits = 3)
    
    
    maxy <- max(measured,modeled)*1.1
    miny <- min(modeled-measured)*1.1
    
    path1 <- paste("../Results/Plots/", catchment, "_LSTM_ts.pdf", sep = "")
  
    pdf(file = path1, width = 14, height = 7)
    plot(measured, type = "l", col="blue", ylim = c(miny,maxy), 
         main = paste(catchment, "LSTM"), ylab = "Discharge")
    lines(modeled, col="chartreuse4")
    lines(modeled-measured, col="red")
    abline(h=0)
    abline(h = measured_mean, col = "blue")
    abline(h = mod_mean, col = "chartreuse4")
    legend("topright", legend = c("model", "data", "model - data"), bty = "n", 
           lty = 1, col = c("chartreuse4", "blue", "red"))
    dev.off()
    return(modeled)
  }
  
  if(mod_type=="gru") {
    
    # shorten the measured data to match the modeled
    skip <- length(modeled)-length(measured)
    measured <- tail(measured, n = skip)
    
    # transform the modeled data back
    modeled <- modeled*unscale[1]+unscale[2]
    
    # calculate statistics
    rmse <- round(sqrt(mean((modeled-measured)^2)), digits = 2)
    mod_mean <- round(mean(modeled), digits = 2)
    measured_mean <- round(mean(measured), digits = 2)
    mod_top10 <- round(quantile(x = modeled, probs = 0.9), digits = 2)
    measured_top10 <- round(quantile(x = measured, probs = 0.9), digits = 2)
    mod_low10 <- round(quantile(x = modeled, probs = 0.1), digits = 2)
    measured_low10 <- round(quantile(x = measured, probs = 0.1), digits = 2)
    mod_nse <- round(NSE(sim = as.matrix(modeled), obs = as.matrix(measured)), digits = 3)
    mod_kge <- round(KGE(sim = as.matrix(modeled), obs = as.matrix(measured)),digits = 3)
    
    
    maxy <- max(measured,modeled)*1.1
    miny <- min(modeled-measured)*1.1
    
    path1 <- paste("../Results/Plots/", catchment, "_GRU_ts.pdf", sep = "")
    
    pdf(file = path1, width = 14, height = 7)
    plot(measured, type = "l", col="blue", ylim = c(miny,maxy), 
         main = paste(catchment, "GRU"), ylab = "Discharge")
    lines(modeled, col="chartreuse4")
    lines(modeled-measured, col="red")
    abline(h=0)
    abline(h = measured_mean, col = "blue")
    abline(h = mod_mean, col = "chartreuse4")
    legend("topright", legend = c("model", "data", "model - data"), bty = "n", 
           lty = 1, col = c("chartreuse4", "blue", "red"))
    dev.off()
    return(modeled)
  }
  
  if(mod_type=="nn") {
    
    # transform the modeled data back
    modeled <- modeled*unscale[1]+unscale[2]
    
    # calculate statistics
    rmse <- round(sqrt(mean((modeled-measured)^2)), digits = 2)
    mod_mean <- round(mean(modeled), digits = 2)
    measured_mean <- round(mean(measured), digits = 2)
    mod_top10 <- round(quantile(x = modeled, probs = 0.9), digits = 2)
    measured_top10 <- round(quantile(x = measured, probs = 0.9), digits = 2)
    mod_low10 <- round(quantile(x = modeled, probs = 0.1), digits = 2)
    measured_low10 <- round(quantile(x = measured, probs = 0.1), digits = 2)
    mod_nse <- round(NSE(sim = as.matrix(modeled), obs = as.matrix(measured)), digits = 3)
    mod_kge <- round(KGE(sim = as.matrix(modeled), obs = as.matrix(measured)),digits = 3)
    
    
    maxy <- max(measured,modeled)*1.1
    miny <- min(modeled-measured)*1.1
    
    path1 <- paste("../Results/Plots/", catchment, "_NN_ts.pdf", sep = "")
    
    pdf(file = path1, width = 14, height = 7)
    plot(measured, type = "l", col="blue", ylim = c(miny,maxy), 
         main = paste(catchment, "NN"), ylab = "Discharge")
    lines(modeled, col="chartreuse4")
    lines(modeled-measured, col="red")
    abline(h=0)
    abline(h = measured_mean, col = "blue")
    abline(h = mod_mean, col = "chartreuse4")
    legend("topright", legend = c("model", "data", "model - data"), bty = "n", 
           lty = 1, col = c("chartreuse4", "blue", "red"))
    dev.off()
    return(modeled)
  }
  
  cat("\n",catchment, ";",
      mod_type, ";",
      rmse, ";",
      mod_nse, ";",
      mod_kge, ";",
      mod_mean, ";",
      measured_mean, ";",
      mod_top10, ";",
      measured_top10, ";",
      mod_low10, ";",
      measured_low10, 
      file = "../Results/Models/model_stat.txt",
      append = TRUE,
      sep = "")
  
}



