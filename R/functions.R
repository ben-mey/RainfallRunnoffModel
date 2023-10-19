
##########################################
# Data preparation for LSTM models
##########################################

dataPrepLSTM <- function(x, y, timesteps=7){
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
  return(list(x=xPrep, y=yPrep))
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
# Bayesian optimization for XGBoost
################################################

bayesOpt_xgb <- function(data, 
                         label, 
                         max_depth = c(2L,12L), 
                         eta = c(0.001,0.25),
                         alpha = c(1,12),
                         lambda = c(1,12),
                         min_child_weight = c(1,50),
                         nrounds = 200,
                         nfold = 5,
                         iters.n = 10){
  
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
  bayes_out <- bayesOpt(FUN = obj_func, bounds = bounds, initPoints = 8, iters.n = iters.n)
  
  
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
                      optimized_mod = opt_mdl)

  print(paste("optimization completed in: ", as.numeric(Sys.time()-time1)%/%60, " minutes ",
              round(as.numeric(Sys.time()-time1)%%60, digits = 1), " seconds"))
  return(output_list)
}
  









#############################################
# Bayesian optimization for lightGBM
#############################################


bayesOpt_lgb <- function(data, 
                         label, 
                         max_depth = c(2L,12L), 
                         eta = c(0.001,0.25),
                         num_leaves = c(2L,100L),
                         nrounds = 200,
                         nfold = 5,
                         iters.n = 10){
  
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
      Score = -min(tes2$record_evals$valid$l2$eval),
      
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
  bayes_out <- bayesOpt(FUN = obj_func, bounds = bounds, initPoints = 6, iters.n = iters.n)
  
  
  # Combine best params with base params 
  opt_params <- append(list(objective = "regression"), 
                       getBestPars(bayes_out))
  
  # Run cross validation 
  xgbcv <- lgb.cv(params = opt_params,
                  data = data,
                  label = label,
                  nrounds = nrounds,
                  nfold = nfold,
                  nrounds = nrounds,
                  early_stopping_rounds = 5,
                  verbose = 0,)
  
  # Get optimal number of rounds
  nrounds = lgbcv$best_iter
  
  # Fit a xgb model
  opt_mdl <- lightgbm(data = data, label = label, 
                     params = opt_params, 
                     early_stopping_rounds = 5, 
                     nrounds = nrounds, 
                     verbose = 0)
  
  output_list <- list(optimized_param = data.frame(getBestPars(bayes_out),nrounds),
                      optimized_mod = opt_mdl)
  
  print(paste("optimization completed in: ", as.numeric(Sys.time()-time1)%/%60, " minutes ",
              round(as.numeric(Sys.time()-time1)%%60, digits = 1), " seconds"))
  return(output_list)
}  



########################################
# Function to create custom LSTM models
########################################

create_LSTM <- function(layers, units, dropout,
                         timesteps = NULL,
                         n_features = NULL){
  
  
  input <- layer_input(shape = c(timesteps, n_features))
  for(lay in 1:layers){
    # return sequances on for all except for last layer
    if(lay < layers) {
      return_sequences_flag <- TRUE
    } else {
      return_sequences_flag <- FALSE
    }
    # add lstm layer
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
# Bayesian optimization for LSTM
#############################################


bayesOpt_LSTM <- function(x, 
                         y, 
                         layers = c(1L,5L), 
                         units = c(5L,300L),
                         dropout = c(0,0.4),
                         batchsize = c(5L,100L),
                         timesteps = c(5L,200L),
                         epochs_opt = 10,
                         epochs_lstm = 100,
                         earlystop = 10,
                         validation_split = 0.25){
  
  time1 <- as.numeric(Sys.time())
  
  obj_func <- function(layers, units, dropout, batchsize, timesteps) { 
    
   train.data <- dataPrepLSTM(x = x, y = y, timesteps = timesteps)
      
    
    mod.lstm <- create_LSTM(layers = layers, 
                            units = units,
                            dropout = dropout,
                            timesteps = timesteps,
                            n_features = dim(train.data$x)[3])
    
    history_lstm <- fit(object = lstm_mod, 
                        x=train.data$x, 
                        y=train.data$y,
                        epochs = epochs_lstm, 
                        verbose = 0, 
                        shuffle = FALSE, 
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
  bayes_out <- bayesOpt(FUN = obj_func, bounds = bounds, initPoints = 12, iters.n = epochs_opt)
  
  
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
                      optimized_mod = opt_mdl)
  
  print(paste("optimization completed in: ", as.numeric(Sys.time()-time1)%/%60, " minutes ",
              round(as.numeric(Sys.time()-time1)%%60, digits = 1), " seconds"))
  return(output_list)
}



