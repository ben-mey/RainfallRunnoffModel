


dataPrepLSTM <- function(data, lag=7){
  dimen <- c(dim(data)[1]+1-lag, lag, dim(data)[2])
  outputPrep <- array(dim=dimen,data = NA)
  for (i in 1:lag) {
    outputPrep[,i,] <- as.matrix(data[(lag+1-i):(dim(data)[1]+1-i),])
  }
  return(outputPrep)
}


rmse <- function(x,y){
  rmse <- sqrt(mean((x-y)^2))
  return(rmse)
}


optimize_xgb <- function(data, label, vdata = NA, vlabel = NA, max.depth = 3:8, eta = seq(0.025,0.2,0.025), 
                         nrounds = c(20,40,70,100,130,160,200), nthread = detectCores()-1, 
                         objective = "reg:squarederror", bt = FALSE){
  
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
                             nrounds = j, nthread = nthread,objective = objective)
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
                         nrounds = nrounds_opt, nthread = nthread, objective = objective)
      
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
                           nrounds = i, nthread = nthread,objective = objective)
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
    if(!(bt[1]<1|bt[2]<=0|bt[2]>=1)){simpleError("bt not in format c(nrounds (x>0),proportion of used data (0<x>1))")}
    
    k <- 0
    numNA <- length(max.depth)*length(nrounds)*length(eta)*bt[1]
    b_rmse <- matrix(NA, numNA,bt[1])
    s_maxdepth <- rep(NA, numNA)
    s_nrounds <- rep(NA, numNA)
    s_eta <- rep(NA, numNA)
    
    
    for (i in max.depth) {
      for (j in nrounds) {
        for (e in 1:length(eta)) {
          
          k <- k+1
          s_maxdepth[k] <- i
          s_nrounds[k] <- j
          s_eta[k] <- eta[e]
          
          for (b in 1:bt[1]) {
            bt_sample <- as.logical(rbinom(n = nrow(data), size = 1, prob = bt[2]))
            xgb_mod <- xgboost(data = as.matrix(data[bt_sample,]), label = label[bt_sample], 
                               max.depth = i, eta = eta[e], 
                               nrounds = j, nthread = nthread, objective = objective)
            pre_xgb <- predict(object = xgb_mod, newdata = as.matrix(data[!bt_sample,]))
            
            
            b_rmse[k,b] <- sqrt(mean((pre_xgb-data[!bt_sample])^2))
          }
          
        }
      }
    }
    
    s_rmse <- apply(b_rmse, MARGIN = 1, FUN = mean)
    best <- which(min(s_rmse, na.rm = TRUE)==s_rmse)
    
    rmse_opt <- b_rmse[best]
    maxdepth_opt <- s_maxdepth[best]
    nrounds_opt <- s_nrounds[best]
    eta_opt <- s_eta[best]
    
    if (nrounds_opt == tail(nrounds, n=1)) {
      
      xgb_opt <- xgboost(data = data, label = label, max.depth = maxdepth_opt, eta = eta_opt, 
                         nrounds = nrounds_opt, nthread = nthread, objective = objective)
      
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
      b_rmse <- matrix(NA,length(nrounds.2),bt[1])
      
      for (i in nrounds.2) {
        k <- k+1
        for (b in 1:bt[1]) {
          bt_sample <- as.logical(rbinom(n = nrow(data), size = 1, prob = bt[2]))
          
          xgb_mod <- xgboost(data = as.matrix(data[bt_sample,]), label = label[bt_sample], 
                             max.depth = s_maxdepth,
                             eta = s_eta, nrounds = s_nrounds, nthread = nthread,objective = objective)
          pre_xgb <- predict(object = xgb_mod, newdata = as.matrix(data[!bt_sample]))
          
          
          b_rmse[k,b] <- sqrt(mean((pre_xgb-data[!bt_sample,])^2))
        }
      }
      
      s_rmse <- apply(b_rmse, MARGIN = 1, FUN = mean)
      rmse_opt <-  b_rmse[which(min(s_rmse, na.rm = TRUE)==s_rmse)]
      xgb_opt <- xgboost(data = data, label = label, max.depth = maxdepth_opt, eta = eta_opt, 
                         nrounds = nrounds_opt, nthread = nthread, objective = objective)
      
      opt_result <- list(xgb_opt,rmse_opt,maxdepth_opt,nrounds_opt,eta_opt)
      
    }
    
  }
  
  return(opt_result)
}


