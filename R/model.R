if(!'rstudioapi'%in%installed.packages()){
  install.packages('rstudioapi')
}

if(!'xgboost'%in%installed.packages()){
  install.packages('xgboost')
}

if(!'devtool'%in%installed.packages()){
  install.packages('devtool')
}

# if(!'wateRtemp'%in%installed.packages()){
#   devtools::install_github("MoritzFeigl/wateRtemp")
# }

if(!'data.table'%in%installed.packages()){
  install.packages('data.table')
}

if(!'igraph'%in%installed.packages()){
  install.packages('igraph')
}

if(!'DiagrammeR'%in%installed.packages()){
  install.packages('DiagrammeR')
}

if(!'hydroGOF'%in%installed.packages()){
  install.packages('hydroGOF')
}

if(!'tseries'%in%installed.packages()){
  install.packages('tseries')
}

if(!'parallel'%in%installed.packages()){
  install.packages('parallel')
}


if(!'ParBayesianOptimization'%in%installed.packages()){
  install.packages('ParBayesianOptimization')
}


if(!'lightgbm'%in%installed.packages()){
  install.packages('lightgbm')
}

rm(list=ls())
gc()


library(rstudioapi)
library(xgboost)
#library(transformer)
#library(attention)
library(tensorflow) #read documentation for installation https://tensorflow.rstudio.com/install/
library(keras) #read documentation for installation
#library(wateRtemp)
library(data.table)
library(igraph)
library(DiagrammeR)
library(hydroGOF) # archived because dependency hydroTSM was archived
library(tseries)
library(parallel)
library(ParBayesianOptimization)
library(lightgbm)

wd <- getSourceEditorContext()$path
wd <- substring(wd, first = 1 , last = tail(unlist(gregexpr('/', wd)), n=1)-1)
setwd(wd)
source("functions.R")

# Ticino Bellinzona
# data <- read.table(file = "../Data/Discharge/1 - priority/CAMELS_CH_obs_based_2020.txt",
#                      header = TRUE, sep = ";")

# Broy Payerne
# data <- read.table(file = "../Data/Discharge/1 - priority/CAMELS_CH_obs_based_2034.txt",
#                      header = TRUE, sep = ";")

# Thur Andelfingen
# data <- read.table(file = "../Data/Discharge/1 - priority/CAMELS_CH_obs_based_2044.txt",
#                    header = TRUE, sep = ";")

# Massa Blatten bei Naters
# data <- read.table(file = "../Data/Discharge/1 - priority/CAMELS_CH_obs_based_2161.txt",
#                    header = TRUE, sep = ";")

# Weisse Lütschine Zweilütschinen
data <- read.table(file = "../Data/Discharge/1 - priority/CAMELS_CH_obs_based_2200.txt",
                   header = TRUE, sep = ";")

# Dischmabach Davos
# data <- read.table(file = "../Data/Discharge/1 - priority/CAMELS_CH_obs_based_2327.txt",
#                    header = TRUE, sep = ";")


h.data <- data[,c(1,2,5,6)]
dat <- strptime(data$date, format = "%Y-%m-%d")
dat.y <- as.numeric(format(dat, "%Y"))
even <- rep(FALSE,length(dat))
even[61:14549] <- dat.y[61:14549]%%2 == 0
skp <- 1
count <- 0
skipunreg <- NA
for (i in 1981:2020) {
  count <- count + 1
  if (skp!=3) {skipunreg[count]<-i; skp <- skp + 1}
  else {skp <- 1}
}
skipunreg <- skipunreg[!is.na(skipunreg)]
skipunreg <- dat.y%in%skipunreg
valid2 <- rep(FALSE, length(dat.y))
valid2[61:14549] <- skipunreg[61:14549]
nvalid2 <- rep(FALSE, length(dat.y))
nvalid2[61:14549] <- !valid2[61:14549]

h.data$lag1preci <- shift(x=h.data$precipitation.mm.d.,n=1, type= "lag")
h.data$lag2preci <- shift(x=h.data$precipitation.mm.d.,n=2, type= "lag")
h.data$lag3preci <- shift(x=h.data$precipitation.mm.d.,n=3, type= "lag")
h.data$lag4preci <- shift(x=h.data$precipitation.mm.d.,n=4, type= "lag")
h.data$lag5preci <- shift(x=h.data$precipitation.mm.d.,n=5, type= "lag")
h.data$lag6preci <- shift(x=h.data$precipitation.mm.d.,n=6, type= "lag")
h.data$lag7preci <- shift(x=h.data$precipitation.mm.d.,n=7, type= "lag")

h.data$lag1temp <- shift(x=h.data$temperature..C.,n=1, type= "lag")
h.data$lag2temp <- shift(x=h.data$temperature..C.,n=2, type= "lag")
h.data$lag3temp <- shift(x=h.data$temperature..C.,n=3, type= "lag")
h.data$lag4temp <- shift(x=h.data$temperature..C.,n=4, type= "lag")
h.data$lag5temp <- shift(x=h.data$temperature..C.,n=5, type= "lag")
h.data$lag6temp <- shift(x=h.data$temperature..C.,n=6, type= "lag")
h.data$lag7temp <- shift(x=h.data$temperature..C.,n=7, type= "lag")

h.data$sum3preci <- frollsum(x=h.data$precipitation.mm.d., n= 3)
h.data$sum7preci <- frollsum(x=h.data$precipitation.mm.d., n= 7)
h.data$sum30preci <- frollsum(x=h.data$precipitation.mm.d., n= 30)

h.data$mean3temp <- frollmean(x=h.data$temperature..C., n=3, align = "right")
h.data$mean7temp <- frollmean(x=h.data$temperature..C., n=7, align = "right")
h.data$mean30temp <- frollmean(x=h.data$temperature..C., n=30, align = "right")
h.data$mean60temp <- frollmean(x=h.data$temperature..C., n=60, align = "right")
h.data$mean30templag30 <- shift(x=h.data$mean30temp ,n=30, type= "lag")
h.data$sum30precilag30 <- shift(x=h.data$sum30preci ,n=30, type= "lag")

filter <- as.data.frame(h.data$discharge_vol.m3.s.)
filter$lowpass15 <- frollmean(x=h.data$discharge_vol.m3.s., n=15, align = "center")
filter$lowpass31 <- frollmean(x=h.data$discharge_vol.m3.s., n=31, align = "center")
filter$lowpass45 <- frollmean(x=h.data$discharge_vol.m3.s., n=45, align = "center")
filter$lowpass61 <- frollmean(x=h.data$discharge_vol.m3.s., n=61, align = "center")
filter$lowpass121 <- frollmean(x=h.data$discharge_vol.m3.s., n=121, align = "center")
filter$highpass15 <- h.data$discharge_vol.m3.s.-filter$lowpass15
filter$highpass31 <- h.data$discharge_vol.m3.s.-filter$lowpass31
filter$highpass45 <- h.data$discharge_vol.m3.s.-filter$lowpass45
filter$highpass61 <- h.data$discharge_vol.m3.s.-filter$lowpass61
filter$highpass121 <- h.data$discharge_vol.m3.s.-filter$lowpass121
acf(x=h.data$discharge_vol.m3.s.,plot = TRUE, lag.max = 365)

h.weights.max <- rep(1,length(h.data$discharge_vol.m3.s.))
h.weights.min <- rep(1,length(h.data$discharge_vol.m3.s.))
h.weights.max[h.data$discharge_vol.m3.s.>quantile(x=h.data$discharge_vol.m3.s., probs = 0.9)] <- 13
h.weights.min[h.data$discharge_vol.m3.s.>quantile(x=h.data$discharge_vol.m3.s., probs = 0.2)] <- 13

calib.h <- 61:9132
valid.h <- 9133:14549 #14610
h.data.calib <- h.data[calib.h,]
h.data.valid <- h.data[valid.h,]

filter.max <- quantile(x=h.data$discharge_vol.m3.s., probs = 0.85)
filter.min <- quantile(x=h.data$discharge_vol.m3.s., probs = 0.25)
h.data.calib.max <- h.data.calib[h.data.calib$discharge_vol.m3.s.>filter.max,]
h.data.calib.mid <- h.data.calib[h.data.calib$discharge_vol.m3.s.<filter.max&h.data.calib$discharge_vol.m3.s.>filter.min,]
h.data.calib.min <- h.data.calib[h.data.calib$discharge_vol.m3.s.<filter.min,]
h.data.valid.max <- h.data.valid[h.data.valid$discharge_vol.m3.s.>filter.max,]
h.data.valid.mid <- h.data.valid[h.data.valid$discharge_vol.m3.s.<filter.max&h.data.valid$discharge_vol.m3.s.>filter.min,]
h.data.valid.min <- h.data.valid[h.data.valid$discharge_vol.m3.s.<filter.min,]


maxdepth <- 3:10
nrounds <- c(5,10,20,40,60,80,100,130,150)
# nrounds <- 150:180
eta <- seq(0.025,0.25,0.025)
k <- 0
numNA <- length(maxdepth)*length(nrounds)*length(eta)
rmse <- rep(NA, numNA)
s_maxdepth <- rep(NA, numNA)
s_nrounds <- rep(NA, numNA)
s_eta <- rep(NA, numNA)
s_gam <- rep(NA, numNA)


for (i in maxdepth) {
  for (j in nrounds) {
    for (e in 1:length(eta)) {
      
      k <- k+1
      
      xgb_mod <- xgboost(data = as.matrix(h.data[calib.h,3:27]), label = h.data$discharge_vol.m3.s.[calib.h], 
                         max.depth = i, eta = eta[e], nrounds = j, nthread = 20,objective = "reg:squarederror")
      pre_xgb <- predict(object = xgb_mod, newdata = as.matrix(h.data[valid.h,3:27]))
      
      
      rmse[k] <- sqrt(mean((pre_xgb-h.data[valid.h,2])^2))
      s_maxdepth[k] <- i
      s_nrounds[k] <- j
      s_eta[k] <- eta[e]
      
    }
  }
}

best <- which(min(rmse, na.rm = TRUE)==rmse)
rmse[best]
s_maxdepth[best]
s_nrounds[best]
s_eta[best]
# 2020 21.68 3 150 0.2
# 2044 18.68 6 150 0.1
## 2020 25.4 5 22 0.2
## 2034 4.064 4 177 0.075 the more rounds the better but improvement is marginal


xgb_mod1 <- xgboost(data = as.matrix(h.data[calib.h,3:27]), label = h.data$discharge_vol.m3.s.[calib.h],
                    max.depth = 6, eta = 0.075, nrounds = 150, nthread = 16, objective = "reg:squarederror")
pxgb1 <- predict(object = xgb_mod1, newdata = as.matrix(h.data[valid.h,3:27]))



maxy <- max(pxgb1,h.data$discharge_vol.m3.s.[valid.h])*1.1
miny <- min(pxgb1-h.data$discharge_vol.m3.s.[valid.h])*1.1
plot(pxgb1, type = "l", col="darkgreen", ylim = c(miny,maxy), main = "main")
lines(h.data$discharge_vol.m3.s.[valid.h], col="blue")
lines(pxgb1-h.data$discharge_vol.m3.s.[valid.h], col="red")
abline(h=0)
legend("topright", legend = c("model", "data", "model - data"), bty = "n", 
       lty = 1, col = c("darkgreen", "blue", "red"))

mean(pxgb1)
mean(h.data$discharge_vol.m3.s.[valid.h])
NSE(sim = as.matrix(pxgb1), obs = as.matrix(h.data$discharge_vol.m3.s.[valid.h]))
KGE(sim = as.matrix(pxgb1), obs = as.matrix(h.data$discharge_vol.m3.s.[valid.h]))

xgb.plot.deepness(xgb_mod1)
xgb.plot.importance(xgb.importance(model=xgb_mod1))
xgb.plot.multi.trees(xgb_mod1)
xgb.plot.shap.summary(data=as.matrix(h.data[calib.h,-c(1,2)]), model=xgb_mod1)




calib.h <- 60:9132
valid.h <- 9133:14610


h.mean <- lapply(h.data[,c(-1)], FUN = "mean",2, na.rm = TRUE)
h.sd <- lapply(h.data[,c(-1)], FUN = "sd",2)
h.data.scale <- scale(h.data[,c(-1)], center = TRUE)

h.data.lstm <- dataPrepLSTM(data = h.data.scale[calib.h,], lag = 10)
h.data.lstm_val <- dataPrepLSTM(data = h.data.scale[valid.h,], lag = 10)

lstm_mod <- keras_model_sequential()
lstm_mod <- layer_lstm(object = lstm_mod, units = 10, return_sequences = TRUE) %>% #, return_sequences = TRUE
# layer_dropout(rate = 0.1) %>%
layer_lstm(units = 20, return_sequences = TRUE) %>%
layer_lstm(units = 20) %>%
  layer_dense(units = 1)
compile(lstm_mod, optimizer = "adam", loss = "mse", metrics = "mse") # optimizer = "adam" / "rmsprop"



history_lstm <- fit(object = lstm_mod, x=h.data.lstm[,,-1], y=h.data.lstm[,1,1],
                    epochs = 40, verbose = 1, shuffle = FALSE, batch_size = 5,
                    validation_split = 0.25)
### 2044
# 5-47-1.1422
# 10-26-0.1445
# 40-16-0.1595
# 5:5-28-0.1675
# 5:10-25-0.1619
# 5:20-40-0.1289
# 5:40-30-0.1445
# 10:5-30-0.1660
# 10:10-15-0.1707
# 10:20-25-0.1365
# 10:40-19_0.1406
# 20:5-25-0.1450
# 20:10-24-0.1411
# 20:20-27-0.1341 
# 40:5-15-0.1534
# 40:10-10-0.1527
# 40:20-20-0.1305
# 40:40-14-0.1395
# 5:5:5-40-0.1374
# 10:10:10-40-0.1371
# 20:20:20-8-0.1465
# 40:40:40-12-0.125 very fast over fitting around 0.14
# 5:20:5-20-0.16
# 5:10:5-28.0.18

pre_lstm <- predict(object = lstm_mod, x = h.data.lstm_val[,,-1])

me <- mean(h.data$discharge_vol.m3.s.)
std <- sd(h.data$discharge_vol.m3.s.)
rmse(h.data$discharge_vol.m3.s.[9142:14610],pre_lstm*std+me) # 2040 *50.3518+46.78827  2020 *60.357+65.206 [9142:14610]

maxy <- max(pre_lstm*std+me,h.data$discharge_vol.m3.s.[9142:14610])
miny <- min(pre_lstm*std+me-h.data$discharge_vol.m3.s.[9142:14610])
plot(h.data$discharge_vol.m3.s.[9142:14610], type = "l", col = "blue", ylim = c(miny*1.1,maxy*1.1))
lines(pre_lstm*std+me, col = "green")
lines(pre_lstm*std+me-h.data$discharge_vol.m3.s.[9142:14610], col = "red")
abline(h=0)

mean(pre_lstm*std+me)
mean(h.data$discharge_vol.m3.s.[9142:14610])
NSE(sim = as.matrix(pre_lstm*std+me), obs = as.matrix(h.data$discharge_vol.m3.s.[9142:14610]))
KGE(sim = as.matrix(pre_lstm*std+me), obs = as.matrix(h.data$discharge_vol.m3.s.[9142:14610]))

nn_mod <- keras_model_sequential() %>%
  #  layer_flatten(input_shape = c(370,4)) %>%
  layer_dense(units = 30, activation = "relu") %>%
  layer_dense(units = 30, activation = "softmax") %>%
  layer_dense(units = 1)
compile(nn_mod, optimizer = "rmsprop", loss = "mse", metrics = "acc")

history_nn <- fit(object = nn_mod, x=h.data.scale[calib.h,-1], y=h.data.scale[calib.h,1],
                  epochs = 30, verbose = 1, shuffle = FALSE, batch_size = 5,
                  validation_split = 0.25)

pre_nn <- predict(object = nn_mod, x = h.data.scale[valid.h,-1])

rmse(h.data$discharge_vol.m3.s.[valid.h],pre_nn*std+me)
NSE(sim = as.matrix(pre_nn*std+me), obs = as.matrix(h.data$discharge_vol.m3.s.[valid.h]))
KGE(sim = as.matrix(pre_nn*std+me), obs = as.matrix(h.data$discharge_vol.m3.s.[valid.h]))

# lstm_rmse <- NA
# 
# for(i in 1:20){
#   history_lstm <- fit(object = lstm_mod, x=h.data.lstm[,,-1], y=h.data.lstm[,1,1],
#                       epochs = i*2, verbose = 1, shuffle = FALSE, batch_size = 5,
#                       validation_split = 0.25, initial_epoch = i*2-2)
#   pre_lstm <- predict(object = lstm_mod, x = h.data.lstm_val[,,-1])
#   
#   
#   lstm_rmse[i] <- rmse(h.data$discharge_vol.m3.s.[9142:14610],pre_lstm*50.3518+46.78827)
# }





#########



maxdepth <- 3:8
nrounds <- c(5,10,20,40,60,80.100,130,150)
# nrounds <- 150:170
eta <- seq(0.025,0.2,0.025)
k <- 0
numNA <- length(maxdepth)*length(nrounds)*length(eta)
rmse <- rep(NA, numNA)
s_maxdepth <- rep(NA, numNA)
s_nrounds <- rep(NA, numNA)
s_eta <- rep(NA, numNA)
s_gam <- rep(NA, numNA)


for (i in maxdepth) {
  for (j in nrounds) {
    for (e in 1:length(eta)) {
      
      k <- k+1
      
      xgb_mod <- xgboost(data = as.matrix(h.data[calib.h,3:27]), label = h.data$discharge_vol.m3.s.[calib.h],
                         max.depth = i, eta = eta[e], nrounds = j, nthread = 20, objective = "reg:squarederror", 
                         weight = h.weights.min[calib.h])
      pre_xgb <- predict(object = xgb_mod, newdata = as.matrix(h.data[valid.h,3:27]))
      
      
      rmse[k] <- sqrt(mean((pre_xgb-h.data[valid.h,2])^2))
      s_maxdepth[k] <- i
      s_nrounds[k] <- j
      s_eta[k] <- eta[e]
      
    }
  }
}

best <- which(min(rmse, na.rm = TRUE)==rmse)
rmse[best]
s_maxdepth[best]
s_nrounds[best]
s_eta[best]
# 2020 23.23/22.06 5/4 150/150 0.15/0.15
# 2044  7/5 150 0.125/0.175




xgb_modmax <- xgboost(data = as.matrix(h.data[calib.h,3:27]), label = h.data$discharge_vol.m3.s.[calib.h],
                    max.depth = 7, eta = 0.125, nrounds = 150, nthread = 16, objective = "reg:squarederror", 
                    weight = h.weights.max[calib.h])
pxgbmax <- predict(object = xgb_modmax, newdata = as.matrix(h.data[-c(1:59),3:27]))

xgb_modmin <- xgboost(data = as.matrix(h.data[calib.h,3:27]), label = h.data$discharge_vol.m3.s.[calib.h],
                      max.depth = 4, eta = 0.15, nrounds = 150, nthread = 16, objective = "reg:squarederror", 
                      weight = h.weights.min[calib.h])
pxgbmin <- predict(object = xgb_modmin, newdata = as.matrix(h.data[-(1:59),3:27]))


valid.hx <- 9074:14551

maxy <- max(pxgbmax[valid.hx],h.data$discharge_vol.m3.s.[valid.h])*1.1
miny <- min(pxgbmax[valid.hx]-h.data$discharge_vol.m3.s.[valid.h])*1.1
plot(pxgbmax[valid.hx], type = "l", col="darkgreen", ylim = c(miny,maxy), main = "pxgbmax")
lines(h.data$discharge_vol.m3.s.[valid.h], col="blue")
lines(pxgbmax[valid.hx]-h.data$discharge_vol.m3.s.[valid.h], col="red")
abline(h=0)
legend("topright", legend = c("model", "data", "model - data"), bty = "n", 
       lty = 1, col = c("darkgreen", "blue", "red"))

maxy <- max(pxgbmin[valid.hx],h.data$discharge_vol.m3.s.[valid.h])*1.1
miny <- min(pxgbmin[valid.hx]-h.data$discharge_vol.m3.s.[valid.h])*1.1
plot(pxgbmin[valid.hx], type = "l", col="darkgreen", ylim = c(miny,maxy), main = "pxgbmin")
lines(h.data$discharge_vol.m3.s.[valid.h], col="blue")
lines(pxgbmin[valid.hx]-h.data$discharge_vol.m3.s.[valid.h], col="red")
abline(h=0)
legend("topright", legend = c("model", "data", "model - data"), bty = "n", 
       lty = 1, col = c("darkgreen", "blue", "red"))

mean(pxgbmax[valid.hx])
mean(h.data$discharge_vol.m3.s.[valid.h])
rmse(h.data$discharge_vol.m3.s.[valid.h],pxgbmax[valid.hx])
NSE(sim = as.matrix(pxgbmax[valid.hx]), obs = as.matrix(h.data$discharge_vol.m3.s.[valid.h]))
KGE(sim = as.matrix(pxgbmax[valid.hx]), obs = as.matrix(h.data$discharge_vol.m3.s.[valid.h]))

mean(pxgbmin[valid.hx])
mean(h.data$discharge_vol.m3.s.[valid.h])
rmse(h.data$discharge_vol.m3.s.[valid.h],pxgbmin[valid.hx])
NSE(sim = as.matrix(pxgbmin[valid.hx]), obs = as.matrix(h.data$discharge_vol.m3.s.[valid.h]))
KGE(sim = as.matrix(pxgbmin[valid.hx]), obs = as.matrix(h.data$discharge_vol.m3.s.[valid.h]))

xgb.plot.importance(xgb.importance(model=xgb_modmax))
xgb.plot.importance(xgb.importance(model=xgb_modmin))
xgb.plot.multi.trees(xgb_modmax)
xgb.plot.multi.trees(xgb_modmin)


h.data$q_sim_max <- NA
h.data$q_sim_min <- NA
h.data$q_sim_max[-c(1:59)] <- pxgbmax
h.data$q_sim_min[-c(1:59)] <- pxgbmin

test_minmax <- rep(NA, length(pxgbmax))
test_minmax[pxgbmax>80] <- pxgbmax[pxgbmax>80]
test_minmax[is.na(test_minmax)] <- pxgbmin[is.na(test_minmax)]
test2_minmax <- (pxgbmax+pxgbmin)/2
mean(test_minmax[valid.hx])
mean(test2_minmax[valid.hx])
mean(h.data$discharge_vol.m3.s.[valid.h])
rmse(h.data$discharge_vol.m3.s.[valid.h],test_minmax[valid.hx])
rmse(h.data$discharge_vol.m3.s.[valid.h],test2_minmax[valid.hx])
NSE(sim = as.matrix(test_minmax[valid.hx]), obs = as.matrix(h.data$discharge_vol.m3.s.[valid.h]))
NSE(sim = as.matrix(test2_minmax[valid.hx]), obs = as.matrix(h.data$discharge_vol.m3.s.[valid.h]))
KGE(sim = as.matrix(test_minmax[valid.hx]), obs = as.matrix(h.data$discharge_vol.m3.s.[valid.h]))
KGE(sim = as.matrix(test2_minmax[valid.hx]), obs = as.matrix(h.data$discharge_vol.m3.s.[valid.h]))

maxy <- max(test_minmax[valid.hx],h.data$discharge_vol.m3.s.[valid.h])*1.1
miny <- min(test_minmax[valid.hx]-h.data$discharge_vol.m3.s.[valid.h])*1.1
plot(test_minmax[valid.hx], type = "l", col="darkgreen", ylim = c(miny,maxy), main = "minmax")
lines(h.data$discharge_vol.m3.s.[valid.h], col="blue")
lines(test_minmax[valid.hx]-h.data$discharge_vol.m3.s.[valid.h], col="red")
abline(h=0)
legend("topright", legend = c("model", "data", "model - data"), bty = "n", 
       lty = 1, col = c("darkgreen", "blue", "red"))

maxy <- max(test2_minmax[valid.hx],h.data$discharge_vol.m3.s.[valid.h])*1.1
miny <- min(test2_minmax[valid.hx]-h.data$discharge_vol.m3.s.[valid.h])*1.1
plot(test2_minmax[valid.hx], type = "l", col="darkgreen", ylim = c(miny,maxy), main = "minmax2")
lines(h.data$discharge_vol.m3.s.[valid.h], col="blue")
lines(test2_minmax[valid.hx]-h.data$discharge_vol.m3.s.[valid.h], col="red")
abline(h=0)
legend("topright", legend = c("model", "data", "model - data"), bty = "n", 
       lty = 1, col = c("darkgreen", "blue", "red"))


h.data.scale <- scale(h.data[,c(-1)], center = TRUE)

nn_mod <- keras_model_sequential() %>%
  #  layer_flatten(input_shape = c(370,4)) %>%
  layer_dense(units = 20, activation ="relu") %>%
  layer_dense(units = 20, activation = "softmax") %>%
  layer_dense(units = 1)
compile(nn_mod, optimizer = "rmsprop", loss = "mse", metrics = "acc")

history_nn <- fit(object = nn_mod, x=h.data.scale[calib.h,-1], y=h.data.scale[calib.h,1],
                  epochs = 30, verbose = 1, shuffle = FALSE, batch_size = 5,
                  validation_split = 0.3)

pre_nn <- predict(object = nn_mod, x = h.data.scale[valid.h,-1])

rmse(h.data$discharge_vol.m3.s.[valid.h],pre_nn*50.3518+46.78827)
NSE(sim = as.matrix(pre_nn*50.3518+46.78827), obs = as.matrix(h.data$discharge_vol.m3.s.[valid.h]))
KGE(sim = as.matrix(pre_nn*50.3518+46.78827), obs = as.matrix(h.data$discharge_vol.m3.s.[valid.h]))

h.data.lstm <- dataPrepLSTM(data = h.data.scale[calib.h,], lag = 10)
h.data.lstm_val <- dataPrepLSTM(data = h.data.scale[valid.h,], lag = 10)
h.weights.max.lstm <- dataPrepLSTM(data = data.frame(h.weights.max, h.weights.min), lag = 10)

lstm_mod <- keras_model_sequential()
lstm_mod <- layer_lstm(object = lstm_mod, units = 20, return_sequences = TRUE) %>% #, return_sequences = TRUE
  # layer_dropout(rate = 0.1) %>%
  # layer_lstm(units = 10, return_sequences = TRUE) %>%
  layer_lstm(units = 20) %>%
  layer_dense(units = 1)
compile(lstm_mod, optimizer = "rmsprop", loss = "mse", metrics = "mse", sample_weight_mode = "temporal")



history_lstm <- fit(object = lstm_mod, x=h.data.lstm[,,-1], y=h.data.lstm[,1,1],
                    epochs = 40, verbose = 1, shuffle = FALSE, batch_size = 5,
                    validation_split = 0.3, sample_weight = h.weights.max[calib.h])
### 2044
# 5-47-1.1422
# 10-26-0.1445
# 40-16-0.1595
# 5:5-28-0.1675
# 5:10-25-0.1619
# 5:20-40-0.1289
# 5:40-30-0.1445
# 10:5-30-0.1660
# 10:10-15-0.1707
# 10:20-25-0.1365
# 10:40-19_0.1406
# 20:5-25-0.1450
# 20:10-24-0.1411
# 20:20-27-0.1341 
# 40:5-15-0.1534
# 40:10-10-0.1527
# 40:20-20-0.1305
# 40:40-14-0.1395
# 5:5:5-40-0.1374
# 10:10:10-40-0.1371
# 20:20:20-8-0.1465
# 40:40:40-12-0.125 very fast over fitting around 0.14
# 5:20:5-20-0.16
# 5:10:5-28.0.18

pre_lstm <- predict(object = lstm_mod, x = h.data.lstm_val[,,-1])

rmse(h.data$discharge_vol.m3.s.[9142:14610],pre_lstm*50.3518+46.78827)
NSE(sim = as.matrix(pre_lstm*50.3518+46.78827), obs = as.matrix(h.data$discharge_vol.m3.s.[9142:14610]))
KGE(sim = as.matrix(pre_lstm*50.3518+46.78827), obs = as.matrix(h.data$discharge_vol.m3.s.[9142:14610]))


plot(h.data$discharge_vol.m3.s.[9142:14610], type = "l", col = "blue", ylim = c(-150,950))
lines(pre_lstm*50.3518+46.78827, col = "green")
lines(pre_lstm*50.3518+46.78827-h.data$discharge_vol.m3.s.[9142:14610], col = "red")
abline(h=0)
mean(pre_lstm*50.3518+46.78827)
mean(h.data$discharge_vol.m3.s.[9142:14610])







maxdepth <- 3:8
nrounds <- c(5,10,20,40,60,80,100,130,150)
# nrounds <- 150:170
eta <- seq(0.025,0.2,0.025)
k <- 0
numNA <- length(maxdepth)*length(nrounds)*length(eta)
rmse <- rep(NA, numNA)
s_maxdepth <- rep(NA, numNA)
s_nrounds <- rep(NA, numNA)
s_eta <- rep(NA, numNA)
s_gam <- rep(NA, numNA)

for (i in maxdepth) {
  for (j in nrounds) {
    for (e in 1:length(eta)) {
      
      k <- k+1
      
      xgb_mod <- xgboost(data = as.matrix(h.data.calib.min[,3:27]), label = h.data.calib.min$discharge_vol.m3.s.,
                         max.depth = i, eta = eta[e], nrounds = j, nthread = 20,objective = "reg:squarederror")
      pre_xgb <- predict(object = xgb_mod, newdata = as.matrix(h.data.valid.min[,3:27]))
      
      
      rmse[k] <- sqrt(mean((pre_xgb-h.data.valid.min[,2])^2))
      s_maxdepth[k] <- i
      s_nrounds[k] <- j
      s_eta[k] <- eta[e]
      
    }
  }
}

best <- which(min(rmse, na.rm = TRUE)==rmse)
rmse[best]
s_maxdepth[best]
s_nrounds[best]
s_eta[best]

xgb_mod1 <- xgboost(data = as.matrix(h.data.calib.min[,3:27]), label = h.data.calib.min$discharge_vol.m3.s.,
                    max.depth = 3, eta = 0.05, nrounds = 130, nthread = 16, objective = "reg:squarederror")

pxgb1 <- predict(object = xgb_mod1, newdata = as.matrix(h.data.valid.min[,3:27]))


maxy <- max(pxgb1,h.data$discharge_vol.m3.s.[valid.h])*1.1
miny <- min(pxgb1-h.data$discharge_vol.m3.s.[valid.h])*1.1
plot(pxgb1, type = "l", col="darkgreen", ylim = c(miny,maxy), main = "main")
lines(h.data$discharge_vol.m3.s.[valid.h], col="blue")
lines(pxgb1-h.data$discharge_vol.m3.s.[valid.h], col="red")
abline(h=0)
legend("topright", legend = c("model", "data", "model - data"), bty = "n", 
       lty = 1, col = c("darkgreen", "blue", "red"))

mean(pxgb1)
mean(h.data$discharge_vol.m3.s.[valid.h])
NSE(sim = as.matrix(pxgb1), obs = as.matrix(h.data$discharge_vol.m3.s.[valid.h]))
KGE(sim = as.matrix(pxgb1), obs = as.matrix(h.data$discharge_vol.m3.s.[valid.h]))

xgb.plot.deepness(xgb_mod1)
xgb.plot.importance(xgb.importance(model=xgb_mod1))
xgb.plot.multi.trees(xgb_mod1)
xgb.plot.shap.summary(data=as.matrix(h.data[calib.h,-c(1,2)]), model=xgb_mod1)



##############################################
#High / Low pass filter model
##############################################


lowdata <- filter$lowpass46
highdata <- filter$highpass46


xgb_lowp1 <- bayesOpt_xgb(data = as.matrix(h.data[calib.h,3:27]), label = lowdata[calib.h])
pxgb_lp1 <- predict(object = xgb_lowp1[[2]], newdata = as.matrix(h.data[valid.h,3:27]))

xgb_highp1 <- bayesOpt_xgb(data = as.matrix(h.data[calib.h,3:27]), label = highdata[calib.h])
pxgb_hp1 <- predict(object = xgb_highp1[[2]], newdata = as.matrix(h.data[valid.h,3:27]))


maxy <- max(pxgb_lp1,lowdata[valid.h])*1.1
miny <- min(pxgb_lp1-lowdata[valid.h])*1.1
plot(pxgb_lp1, type = "l", col="darkgreen", ylim = c(miny,maxy), main = "main")
lines(lowdata[valid.h], col="blue")
lines(pxgb_lp1-lowdata[valid.h], col="red")
abline(h=0)
legend("topright", legend = c("model", "data", "model - data"), bty = "n", 
       lty = 1, col = c("darkgreen", "blue", "red"))

mean(pxgb_lp1)
mean(lowdata[valid.h])
NSE(sim = as.matrix(pxgb_lp1), obs = as.matrix(lowdata[valid.h]))
KGE(sim = as.matrix(pxgb_lp1), obs = as.matrix(lowdata[valid.h]))

xgb.plot.deepness(xgb_lowp1)
xgb.plot.importance(xgb.importance(model=xgb_lowp1))
xgb.plot.shap.summary(data=as.matrix(h.data[calib.h,-c(1,2)]), model=xgb_lowp1)



maxy <- max(pxgb_hp1,highdata[valid.h])*1.1
miny <- min(pxgb_hp1-highdata[valid.h])*1.1
plot(pxgb_hp1, type = "l", col="darkgreen", ylim = c(miny,maxy), main = "main")
lines(highdata[valid.h], col="blue")
lines(pxgb_hp1-highdata[valid.h], col="red")
abline(h=0)
legend("topright", legend = c("model", "data", "model - data"), bty = "n", 
       lty = 1, col = c("darkgreen", "blue", "red"))

mean(pxgb_hp1)
mean(highdata[valid.h])
NSE(sim = as.matrix(pxgb_hp1), obs = as.matrix(highdata[valid.h]))
KGE(sim = as.matrix(pxgb_hp1), obs = as.matrix(highdata[valid.h]))

xgb.plot.deepness(xgb_highp1)
xgb.plot.importance(xgb.importance(model=xgb_highp1))
xgb.plot.shap.summary(data=as.matrix(h.data[calib.h,-c(1,2)]), model=xgb_highp1)



combined <- pxgb_hp1+pxgb_lp1

maxy <- max(combined,h.data$discharge_vol.m3.s.[valid.h])*1.1
miny <- min(combined-h.data$discharge_vol.m3.s.[valid.h])*1.1
plot(combined, type = "l", col="darkgreen", ylim = c(miny,maxy), main = "main")
lines(h.data$discharge_vol.m3.s.[valid.h], col="blue")
lines(combined-h.data$discharge_vol.m3.s.[valid.h], col="red")
abline(h=0)
legend("topright", legend = c("model", "data", "model - data"), bty = "n", 
       lty = 1, col = c("darkgreen", "blue", "red"))

mean(combined)
mean(h.data$discharge_vol.m3.s.[valid.h])
NSE(sim = as.matrix(combined), obs = as.matrix(h.data$discharge_vol.m3.s.[valid.h]))
KGE(sim = as.matrix(combined), obs = as.matrix(h.data$discharge_vol.m3.s.[valid.h]))


xgb_mod <- xgboost(data = as.matrix(h.data[calib.h,3:27]), label = highdata[calib.h], 
                   max.depth = i, eta = eta[e], nrounds = j, nthread = 20,objective = "reg:squarederror")
pre_xgb <- predict(object = xgb_mod, newdata = as.matrix(h.data[valid.h,3:27]))


rmse[k] <- sqrt(mean((pre_xgb-highdata[valid.h])^2)) 


source("functions.R")

calib.h <- 61:13200
valid.h <- 13201:14549 #14610

test1 <- optimize_xgb(data = as.matrix(h.data.calib[,3:27]),label = h.data.calib[,2],
                     vdata = as.matrix(h.data[valid.h,3:27]), vlabel = h.data[valid.h,2])

save(file = "../Results/test1.R", test1)

test2 <- optimize_xgb(data = as.matrix(h.data[calib.h,3:27]),label = h.data[calib.h,2],
                     bt = c(3,0.75))

pxgb1 <- predict(object = test1[[1]], newdata = as.matrix(h.data[valid.h,3:27]))

maxy <- max(pxgb1,h.data$discharge_vol.m3.s.[valid.h])*1.1
miny <- min(pxgb1-h.data$discharge_vol.m3.s.[valid.h])*1.1

pdf(file = "../Results/Model_Validation_test1.pdf", width = 14, height = 7)
plot(pxgb1, type = "l", col="darkgreen", ylim = c(miny,maxy), main = "main", ylab = "Discharge")
lines(h.data$discharge_vol.m3.s.[valid.h], col="blue")
lines(pxgb1-h.data$discharge_vol.m3.s.[valid.h], col="red")
abline(h=0)
legend("topright", legend = c("model", "data", "model - data"), bty = "n", 
       lty = 1, col = c("darkgreen", "blue", "red"))
dev.off()

mean(pxgb1)
mean(h.data$discharge_vol.m3.s.[valid.h])
NSE(sim = as.matrix(pxgb1), obs = as.matrix(h.data$discharge_vol.m3.s.[valid.h]))
KGE(sim = as.matrix(pxgb1), obs = as.matrix(h.data$discharge_vol.m3.s.[valid.h]))

pdf(file = "../Results/XGB_Model_test1.pdf", width = 7, height = 7)
xgb.plot.deepness(test1[[1]])
xgb.plot.importance(xgb.importance(model=test1[[1]]))
xgb.plot.shap.summary(data=as.matrix(h.data[calib.h,-c(1,2)]), model=test1[[1]])
dev.off()

xgb.plot.multi.trees(test1[[1]])



#############################################################
#############################################################
#############################################################
#############################################################


X <- as.matrix(h.data[valid2,3:27])
# Get the target variable
y <- h.data[valid2,2]



# Function must take the hyper-parameters as inputs
obj_func <- function(eta, max_depth, min_child_weight, lambda, alpha, nrounds) { #, min_child_weight, subsample, lambda, alpha
  
  param <- list(
    
    # Hyter parameters 
    eta = eta,
    max_depth = max_depth,
    min_child_weight = min_child_weight,
    # subsample = subsample,
    lambda = lambda,
    alpha = alpha,
    
    # Tree model 
    booster = "gbtree",
    
    # Regression problem 
    objective = "reg:squarederror",
    
    # Use the Mean Absolute Percentage Error
    eval_metric = "rmse")
  
  xgbcv <- xgb.cv(params = param,
                  data = X,
                  label = y,
                  nfold = 5,
                  # folds = folds,
                  prediction = TRUE,
                  early_stopping_rounds = 5,
                  nrounds = 200,
                  verbose = 0,
                  maximize = F,
                  nthread = 20)
  
  lst <- list(
    
    # First argument must be named as "Score"
    # Function finds maxima so inverting the output
    Score = -min(xgbcv$evaluation_log$test_rmse_mean),
    
    # Get number of trees for the best performing model
    nrounds = xgbcv$best_iteration
  )
  
  return(lst)
}





bounds <- list(eta = c(0.001, 0.25),
               max_depth = c(2L, 12L)
               ,min_child_weight = c(1, 50)
               # ,subsample = c(0.1, 1)
               ,lambda = c(1, 12)
               ,alpha = c(1, 12)
               )




set.seed(1234)
bayes_out <- bayesOpt(FUN = obj_func, bounds = bounds, initPoints = length(bounds) + 2, iters.n = 10)

bayes_out$scoreSummary[,-ncol(bayes_out$scoreSummary)] #[, c(3:8, 13)]
data.frame(getBestPars(bayes_out))



# Combine best params with base params
opt_params <- append(list(booster = "gbtree", 
                          objective = "reg:squarederror", 
                          eval_metric = "rmse"), 
                     getBestPars(bayes_out))

# Run cross validation 
xgbcv <- xgb.cv(params = opt_params,
                data = X,
                label = y,
                folds = folds,
                prediction = TRUE,
                early_stopping_rounds = 5,
                nrounds = 200,
                verbose = 0,
                maximize = F)

# Get optimal number of rounds
nrounds = xgbcv$best_iteration

# Fit a xgb model
mdl <- xgboost(data = X, label = y, 
               params = opt_params, 
               maximize = F, 
               early_stopping_rounds = 5, 
               verbose = 0)



source("functions.R")
mdl <- bayesOpt_xgb(data = as.matrix(h.data[valid2,3:27]), label = h.data[valid2,2])


pxgb1 <- predict(object = mdl[[2]], newdata = as.matrix(h.data[nvalid2,3:27]))

maxy <- max(pxgb1,h.data$discharge_vol.m3.s.[nvalid2])*1.1
miny <- min(pxgb1-h.data$discharge_vol.m3.s.[nvalid2])*1.1


plot(pxgb1, type = "l", col="darkgreen", ylim = c(miny,maxy), main = "main", ylab = "Discharge")
lines(h.data$discharge_vol.m3.s.[nvalid2], col="blue")
lines(pxgb1-h.data$discharge_vol.m3.s.[nvalid2], col="red")
abline(h=0)
legend("topright", legend = c("model", "data", "model - data"), bty = "n", 
       lty = 1, col = c("darkgreen", "blue", "red"))

mean(pxgb1)
mean(h.data$discharge_vol.m3.s.[nvalid2])
NSE(sim = as.matrix(pxgb1), obs = as.matrix(h.data$discharge_vol.m3.s.[nvalid2]))
KGE(sim = as.matrix(pxgb1), obs = as.matrix(h.data$discharge_vol.m3.s.[nvalid2]))

xgb.plot.deepness(mdl[[2]])
xgb.plot.importance(xgb.importance(model=mdl[[2]]))
xgb.plot.shap.summary(data=as.matrix(h.data[calib.h,-c(1,2)]), model=mdl)




bayesOpt_lgb(data = as.matrix(h.data[valid2,3:27]), label = h.data[valid2,2])
tt <- lgb.Dataset(data = as.matrix(h.data[valid2,3:27]), label = h.data[valid2,2])
tes <- lightgbm(data = tt, params = list(objective = "regression"), nrounds = 50)
tes2 <- lgb.cv(data = tt, params = list(objective = "regression", 
                                        max_depth = 8,
                                        eta = 0.15,
                                        num_leaves = 35), 
               nfold = 5, nrounds = 50, early_stopping_rounds = 5)

tes2$record_evals$valid$l2$eval
