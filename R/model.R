if(!'rstudioapi'%in%installed.packages()){
  install.packages('rstudioapi')
}

if(!'xgboost'%in%installed.packages()){
  install.packages('xgboost')
}

if(!'devtool'%in%installed.packages()){
  install.packages('devtool')
}

if(!'data.table'%in%installed.packages()){
  install.packages('data.table')
}

if(!'igraph'%in%installed.packages()){
  install.packages('igraph')
}

if(!'DiagrammeR'%in%installed.packages()){
  install.packages('DiagrammeR')
}

if(!'hydroGOF'%in%installed.packages()){ # archived because dependency hydroTSM was archived. Used here to calculate NSE and KGE
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

if(!'lubridate'%in%installed.packages()){
  install.packages('lubridate')
}



rm(list=ls())
gc()


library(rstudioapi)
library(xgboost)
#library(transformer)
#library(attention)
library(tensorflow) #read documentation for installation https://tensorflow.rstudio.com/install/ For gpu support cuda is needed. look for a guide
library(keras) #read documentation for installation. For gpu support cuda is needed. look for a guide
library(data.table)
library(igraph)
library(DiagrammeR)
library(hydroGOF) # archived because dependency hydroTSM was archived. Used here to calculate NSE and KGE
library(tseries)
library(parallel)
library(ParBayesianOptimization)
library(lightgbm)
library(lubridate)
library(tidyr)
library(e1071)


# get file path and set working directory. A long as the folder structure is the same as I used, all paths should work.
wd <- getSourceEditorContext()$path
wd <- substring(wd, first = 1 , last = tail(unlist(gregexpr('/', wd)), n=1)-1)
setwd(wd)
source("functions.R")

# Ticino Bellinzona
data <- read.table(file = "../Data/Discharge/1 - priority/CAMELS_CH_obs_based_2020.txt",
                     header = TRUE, sep = ";")
catchment <- "Ticino"

# Broy Payerne
# data <- read.table(file = "../Data/Discharge/1 - priority/CAMELS_CH_obs_based_2034.txt",
#                      header = TRUE, sep = ";")
# catchment <- "Broy"

# Thur Andelfingen
# data <- read.table(file = "../Data/Discharge/1 - priority/CAMELS_CH_obs_based_2044.txt",
#                    header = TRUE, sep = ";")
# catchment <- "Thur"

# Massa Blatten bei Naters
# data <- read.table(file = "../Data/Discharge/1 - priority/CAMELS_CH_obs_based_2161.txt",
#                    header = TRUE, sep = ";")
# catchment <- "Massa"

# Weisse Lütschine Zweilütschinen
# data <- read.table(file = "../Data/Discharge/1 - priority/CAMELS_CH_obs_based_2200.txt",
#                    header = TRUE, sep = ";")
# catchment <- "Weisse Lütschine"

# Dischmabach Davos
# data <- read.table(file = "../Data/Discharge/1 - priority/CAMELS_CH_obs_based_2327.txt",
#                    header = TRUE, sep = ";")
# catchment <- "Dischmabach"

# h.data <- data[,c(1,2,5,6)]
# dat <- strptime(data$date, format = "%Y-%m-%d")
# dat.y <- as.numeric(format(dat, "%Y"))
# even <- rep(FALSE,length(dat))
# even[61:14549] <- dat.y[61:14549]%%2 == 0
# skp <- 1
# count <- 0
# skipunreg <- NA
# for (i in 1981:2020) {
#   count <- count + 1
#   if (skp!=3) {skipunreg[count]<-i; skp <- skp + 1}
#   else {skp <- 1}
# }
# skipunreg <- skipunreg[!is.na(skipunreg)]
# skipunreg <- dat.y%in%skipunreg
# calib2 <- rep(FALSE, length(dat.y))
# calib2[61:14549] <- skipunreg[61:14549]
# valid2 <- rep(FALSE, length(dat.y))
# valid2[61:14549] <- !calib2[61:14549]
# 
# h.data$lag1preci <- shift(x=h.data$precipitation.mm.d.,n=1, type= "lag")
# h.data$lag2preci <- shift(x=h.data$precipitation.mm.d.,n=2, type= "lag")
# h.data$lag3preci <- shift(x=h.data$precipitation.mm.d.,n=3, type= "lag")
# h.data$lag4preci <- shift(x=h.data$precipitation.mm.d.,n=4, type= "lag")
# h.data$lag5preci <- shift(x=h.data$precipitation.mm.d.,n=5, type= "lag")
# h.data$lag6preci <- shift(x=h.data$precipitation.mm.d.,n=6, type= "lag")
# h.data$lag7preci <- shift(x=h.data$precipitation.mm.d.,n=7, type= "lag")
# 
# h.data$lag1temp <- shift(x=h.data$temperature..C.,n=1, type= "lag")
# h.data$lag2temp <- shift(x=h.data$temperature..C.,n=2, type= "lag")
# h.data$lag3temp <- shift(x=h.data$temperature..C.,n=3, type= "lag")
# h.data$lag4temp <- shift(x=h.data$temperature..C.,n=4, type= "lag")
# h.data$lag5temp <- shift(x=h.data$temperature..C.,n=5, type= "lag")
# h.data$lag6temp <- shift(x=h.data$temperature..C.,n=6, type= "lag")
# h.data$lag7temp <- shift(x=h.data$temperature..C.,n=7, type= "lag")
# 
# h.data$sum2preci <- frollsum(x=h.data$precipitation.mm.d., n= 2)
# h.data$sum3preci <- frollsum(x=h.data$precipitation.mm.d., n= 3)
# h.data$sum4preci <- frollsum(x=h.data$precipitation.mm.d., n= 4)
# h.data$sum5preci <- frollsum(x=h.data$precipitation.mm.d., n= 5)
# h.data$sum6preci <- frollsum(x=h.data$precipitation.mm.d., n= 6)
# h.data$sum7preci <- frollsum(x=h.data$precipitation.mm.d., n= 7)
# h.data$sum30preci <- frollsum(x=h.data$precipitation.mm.d., n= 30)
# 
# h.data$mean3temp <- frollmean(x=h.data$temperature..C., n=3, align = "right")
# h.data$mean7temp <- frollmean(x=h.data$temperature..C., n=7, align = "right")
# h.data$mean30temp <- frollmean(x=h.data$temperature..C., n=30, align = "right")
# h.data$mean60temp <- frollmean(x=h.data$temperature..C., n=60, align = "right")
# h.data$mean30templag30 <- shift(x=h.data$mean30temp ,n=30, type= "lag")
# h.data$sum30precilag30 <- shift(x=h.data$sum30preci ,n=30, type= "lag")
# 
# filter <- as.data.frame(h.data$discharge_vol.m3.s.)
# filter$lowpass15 <- frollmean(x=h.data$discharge_vol.m3.s., n=15, align = "center")
# filter$lowpass31 <- frollmean(x=h.data$discharge_vol.m3.s., n=31, align = "center")
# filter$lowpass45 <- frollmean(x=h.data$discharge_vol.m3.s., n=45, align = "center")
# filter$lowpass61 <- frollmean(x=h.data$discharge_vol.m3.s., n=61, align = "center")
# filter$lowpass121 <- frollmean(x=h.data$discharge_vol.m3.s., n=121, align = "center")
# filter$highpass15 <- h.data$discharge_vol.m3.s.-filter$lowpass15
# filter$highpass31 <- h.data$discharge_vol.m3.s.-filter$lowpass31
# filter$highpass45 <- h.data$discharge_vol.m3.s.-filter$lowpass45
# filter$highpass61 <- h.data$discharge_vol.m3.s.-filter$lowpass61
# filter$highpass121 <- h.data$discharge_vol.m3.s.-filter$lowpass121

p.data <- dataPrep(data[,c(1,2,5,6)])
calib <- p.data$calib
valid <- p.data$valid
acf(x=p.data$data$discharge,plot = TRUE, lag.max = 365)

# h.weights.max <- rep(1,length(h.data$discharge_vol.m3.s.))
# h.weights.min <- rep(1,length(h.data$discharge_vol.m3.s.))
# h.weights.max[h.data$discharge_vol.m3.s.>quantile(x=h.data$discharge_vol.m3.s., probs = 0.9)] <- 3
# h.weights.min[h.data$discharge_vol.m3.s.>quantile(x=h.data$discharge_vol.m3.s., probs = 0.2)] <- 3
# 
# calib.h <- 61:9132
# valid.h <- 9133:14549 #14610
# h.data.calib <- h.data[calib.h,]
# h.data.valid <- h.data[valid.h,]
# 
# filter.max <- quantile(x=h.data$discharge_vol.m3.s., probs = 0.85)
# filter.min <- quantile(x=h.data$discharge_vol.m3.s., probs = 0.25)
# h.data.calib.max <- h.data.calib[h.data.calib$discharge_vol.m3.s.>filter.max,]
# h.data.calib.mid <- h.data.calib[h.data.calib$discharge_vol.m3.s.<filter.max&h.data.calib$discharge_vol.m3.s.>filter.min,]
# h.data.calib.min <- h.data.calib[h.data.calib$discharge_vol.m3.s.<filter.min,]
# h.data.valid.max <- h.data.valid[h.data.valid$discharge_vol.m3.s.>filter.max,]
# h.data.valid.mid <- h.data.valid[h.data.valid$discharge_vol.m3.s.<filter.max&h.data.valid$discharge_vol.m3.s.>filter.min,]
# h.data.valid.min <- h.data.valid[h.data.valid$discharge_vol.m3.s.<filter.min,]


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





###########################



calib.h <- 60:9132
valid.h <- 9133:14610


h.mean <- lapply(h.data[,c(-1)], FUN = "mean",2, na.rm = TRUE)
h.sd <- lapply(h.data[,c(-1)], FUN = "sd",2)
h.data.scale <- scale(h.data[,c(-1)], center = TRUE)

h.data.lstm <- dataPrepLSTM(x = h.data.scale[calib2,-1], y = h.data.scale[calib2,1], timesteps = 10)
h.data.lstm_val <- dataPrepLSTM(x = h.data.scale[valid2,-1], y = h.data.scale[valid2,1], timesteps = 10)

lstm_mod <- keras_model_sequential()
lstm_mod <- layer_lstm(object = lstm_mod, units = 10, return_sequences = TRUE) %>% #, return_sequences = TRUE
# layer_dropout(rate = 0.1) %>%
layer_lstm(units = 20, return_sequences = TRUE) %>%
layer_lstm(units = 20) %>%
  layer_dense(units = 1)
compile(lstm_mod, optimizer = "adam", loss = "mse", metrics = "mse") # optimizer = "adam" / "rmsprop"
test <- dataPrepLSTM(data = h.data.scale[calib2,1])

lstm_mod <- create_LSTM(layers = 3, units = 10, timesteps = 10, n_features = 25, dropout = 0)

history_lstm <- fit(object = lstm_mod, x=h.data.lstm[,,-1], y=h.data.lstm[,,1],
                    epochs = 40, verbose = 1, shuffle = FALSE, batch_size = 5,
                    validation_split = 0.25,callbacks = list(
                      callback_early_stopping(patience = 10, restore_best_weights = TRUE, 
                                              mode = "min", monitor = "val_loss")
                    ))



pre_lstm <- predict(object = lstm_mod, x = h.data.lstm_val[,,-1])

me <- mean(h.data$discharge_vol.m3.s.)
std <- sd(h.data$discharge_vol.m3.s.)
rmse(h.data$discharge_vol.m3.s.[valid2],pre_lstm*std+me) # 2040 *50.3518+46.78827  2020 *60.357+65.206 [9142:14610]

maxy <- max(pre_lstm*std+me,h.data$discharge_vol.m3.s.[valid2])
miny <- min(pre_lstm*std+me-h.data$discharge_vol.m3.s.[valid2])
plot(h.data$discharge_vol.m3.s.[valid2], type = "l", col = "blue", ylim = c(miny*1.1,maxy*1.1))
lines(pre_lstm*std+me, col = "green")
lines(pre_lstm*std+me-h.data$discharge_vol.m3.s.[valid2], col = "red")
abline(h=0)

mean(pre_lstm*std+me)
mean(h.data$discharge_vol.m3.s.[9142:14610])
NSE(sim = as.matrix(pre_lstm*std+me), obs = as.matrix(h.data$discharge_vol.m3.s.[9142:14610]))
KGE(sim = as.matrix(pre_lstm*std+me), obs = as.matrix(h.data$discharge_vol.m3.s.[9142:14610]))




##################################




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




######################



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



calib <- p.data$calib
valid <- p.data$valid

X <- as.matrix(p.data$data[calib,-1])
# Get the target variable
y <- p.data$data[calib,1]


source("functions.R")
xgb_mod <- bayes_opt_xgb(data = as.matrix(p.data$data[calib,-1]), label = p.data$data[calib,1])

# xgb_thur <- xgb_mod
save(xgb_mod, file = paste("../Results/Models/", catchment, "_XBoost.RData", sep = ""))

pxgb1 <- predict(object = xgb_mod[[2]], newdata = as.matrix(p.data$data[valid,-1]))

analyze_model(measured = p.data$data$discharge[valid],
              modeled = pxgb1,
              catchment = catchment,
              mod_type = "xgb",
              model = xgb_mod[[2]])

mon_mean <- monthly_mean(measured = p.data$data$discharge[valid],
                         modeled = pxgb1,
                         date = p.data$date[valid])

pdf(file = paste("../Results/Plots/", catchment, "_XGBoost_monthly.pdf", sep = ""))
monthly_plot(data = mon_mean,
             main = paste(catchment, "XGBoost"))
dev.off()

quantile(x=pxgb1, probs = ((1:100)/100))
quantile(x=p.data$data$discharge[valid], probs = ((1:100)/100))




# h.mean <- lapply(p.data$data, FUN = "mean",2, na.rm = TRUE)
# h.sd <- lapply(p.data$data, FUN = "sd",2)
# min <- min(p.data$data$discharge)
# max <- max(p.data$data$discharge)
h.data.scale <- normalize(p.data$data, variant = "stdnorm")


lstm_mod2 <- bayes_opt_LSTM(x = h.data.scale[calib,-1], 
                            y = h.data.scale[calib,1], 
                            epochs_opt = 15, 
                            initPoints = 30,
                            duplicate = c(0.05, 0.95, 2),
                            validation_split = 0.2
                            )
lstm_mod2$bayes_summary

# lstm_thur <- lstm_mod2
save(lstm_mod2, file = paste("../Results/Models/", catchment, "_LSTM.RData", sep = ""))

h.data.lstm_val <- dataPrepLSTM(x = h.data.scale[valid,-1], 
                                y = h.data.scale[valid,1], 
                                timesteps = lstm_mod2$optimized_param$timesteps)



pre_lstm <- predict(object = lstm_mod2$optimized_mod, x = h.data.lstm_val$x)


std <- sd(p.data$data$discharge)
mean <- mean(p.data$data$discharge)
pre_lstm_unscaled <-  analyze_model(measured = p.data$data$discharge[valid],
                                    modeled = pre_lstm,
                                    catchment = catchment,
                                    mod_type = "lstm",
                                    unscale = c(std,mean,"stdnorm"))

mon_mean <- monthly_mean(measured = p.data$data$discharge[valid],
                         modeled = pre_lstm_unscaled,
                         date = p.data$date[valid])

pdf(file = paste("../Results/Plots/", catchment, "_LSTM_monthly.pdf", sep = ""))
monthly_plot(data = mon_mean,
             main = paste(catchment, "LSTM"))
dev.off()



summary(lstm_mod2$optimized_mod)

quantile(x=pre_lstm_unscaled, probs = ((1:100)/100))
quantile(x=p.data$data$discharge[valid], probs = ((1:100)/100))





lgbm_mod <- bayes_opt_lgb(data = as.matrix(p.data$data[calib,-1]), label = p.data$data[calib,1])

# lgb_thur <- lgbm_mod
save(lgbm_mod, file = paste("../Results/Models/", catchment, "_LightGBM.RData", sep = ""))
saveRDS.lgb.Booster(lgbm_mod$optimized_mod, 
                    file = paste("../Results/Models/", catchment, "_LightGBM_mod.RData", sep = ""))

plgbm <- predict(object = lgbm_mod$optimized_mod, data = as.matrix(p.data$data[valid,-1]))


analyze_model(measured = p.data$data$discharge[valid],
              modeled = plgbm,
              catchment = catchment,
              mod_type = "lgbm",
              model = lgbm_mod[[2]])

mon_mean <- monthly_mean(measured = p.data$data$discharge[valid],
                         modeled = plgbm,
                         date = p.data$date[valid])

pdf(file = paste("../Results/Plots/", catchment, "_LightGBM_monthly.pdf", sep = ""))
monthly_plot(data = mon_mean,
             main = paste(catchment, "LightGBM"))
dev.off()

quantile(x=plgbm, probs = ((1:100)/100))
quantile(x=p.data$data$discharge[valid], probs = ((1:100)/100))






# h.mean <- lapply(p.data$data, FUN = "mean",2, na.rm = TRUE)
# h.sd <- lapply(p.data$data, FUN = "sd",2)
# h.data.scale <- scale(p.data$data, center = TRUE)
h.data.scale <- normalize(p.data$data)




gru_mod2 <- bayes_opt_GRU(x = h.data.scale[calib,-1], 
                            y = h.data.scale[calib,1], 
                            epochs_opt = 15, 
                            initPoints = 20
                            , duplicate = c(0.1, 0.9,1)
                            , validation_split = 0.25
                        )
gru_mod2$bayes_summary

# gru_thur <- gru_mod2
save(gru_mod2, file = paste("../Results/Models/", catchment, "_GRU.RData", sep = ""))

h.data.gru_val <- dataPrepLSTM(x = h.data.scale[valid,-1], 
                                y = h.data.scale[valid,1], 
                                timesteps = gru_mod2$optimized_param$timesteps)



pre_gru <- predict(object = gru_mod2$optimized_mod, x = h.data.gru_val$x)


std <- sd(p.data$data$discharge)
mean <- mean(p.data$data$discharge)
pre_gru_unscaled <-  analyze_model(measured = p.data$data$discharge[valid],
                                    modeled = pre_gru,
                                    catchment = catchment,
                                    mod_type = "gru",
                                    unscale = c(std,mean,"stdnorm"))

mon_mean <- monthly_mean(measured = p.data$data$discharge[valid],
                         modeled = pre_gru_unscaled,
                         date = p.data$date[valid])

pdf(file = paste("../Results/Plots/", catchment, "_GRU_monthly.pdf", sep = ""))
monthly_plot(data = mon_mean,
             main = paste(catchment, "GRU"))
dev.off()

quantile(x=pre_gru_unscaled, probs = ((1:100)/100))
quantile(x=p.data$data$discharge[valid], probs = ((1:100)/100))


######################

# Ticino (0.05,0.9,2)

# h.mean <- lapply(p.data$data, FUN = "mean",2, na.rm = TRUE)
# h.sd <- lapply(p.data$data, FUN = "sd",2)
# h.data.scale <- scale(p.data$data, center = TRUE)
h.data.scale <- normalize(p.data$data)

h.calib <- h.data.scale[calib,]
filt.max <- h.calib[,1] > quantile(x = h.calib[,1], probs = 0.9)
filt.min <- h.calib[,1] < quantile(x = h.calib[,1], probs = 0.05)
add.max <- h.calib[filt.max,]
add.min <- h.calib[filt.min,]
for (t in 1:2) {
  h.calib <- rbind(add.max, h.calib)
  h.calib <- rbind(add.min, h.calib)
}



mlp_mod2 <- bayes_opt_MLP(x = h.calib[,-1], 
                          y = h.calib[,1], 
                          validation_split = 0.2)
mlp_mod2$bayes_summary

# mlp_thur <- mlp_mod2
save(mlp_mod2, file = paste("../Results/Models/", catchment, "_MLP.RData", sep = ""))



pre_mlp <- predict(object = mlp_mod2$optimized_mod, x = h.data.scale[valid,-1])


std <- sd(p.data$data$discharge)
mean <- mean(p.data$data$discharge)
pre_nn_unscaled <-  analyze_model(measured = p.data$data$discharge[valid],
                                    modeled = pre_mlp,
                                    catchment = catchment,
                                    mod_type = "mlp",
                                    unscale = c(std,mean,"stdnorm"))

mon_mean <- monthly_mean(measured = p.data$data$discharge[valid],
                         modeled = pre_nn_unscaled,
                         date = p.data$date[valid])

pdf(file = paste("../Results/Plots/", catchment, "_MLP_monthly.pdf", sep = ""))
monthly_plot(data = mon_mean,
             main = paste(catchment, "MLP"))
dev.off()

quantile(x=pre_nn_unscaled, probs = ((1:100)/100))
quantile(x=p.data$data$discharge[valid], probs = ((1:100)/100))

#######################

# Thur:   x = p.data$data[calib,c(4,5,11,19:30)]
# Massa:  x = p.data$data[calib,c(3,4,10:12,17,23:31)]
# Ticino: x = p.data$data[calib,c(3,4,11,19:21,23:31)]

svr_mod2 <- bayes_opt_SVR(x = p.data$data[calib,c(3,4,11,19:21,23:31)], 
                            y = p.data$data[calib,1], 
                            cross = 4)
svr_mod2$bayes_summary
# svr_thur <- svr_mod2
save(svr_mod2, file = paste("../Results/Models/", catchment, "_SVR.RData", sep = ""))
psvr <- predict(object = svr_mod2$optimized_mod, newdata = p.data$data[valid,c(3,4,11,19:21,23:31)])



analyze_model(measured = p.data$data$discharge[valid],
              modeled = psvr,
              catchment = catchment,
              mod_type = "svr",
              model = svr_mod2[[2]])

mon_mean <- monthly_mean(measured = p.data$data$discharge[valid],
                         modeled = psvr,
                         date = p.data$date[valid])

pdf(file = paste("../Results/Plots/", catchment, "_SVRegression_monthly.pdf", sep = ""))
monthly_plot(data = mon_mean,
             main = paste(catchment, "SVRegression"))
dev.off()

quantile(x=psvr, probs = ((1:100)/100))
quantile(x=p.data$data$discharge[valid], probs = ((1:100)/100))


######################
# lm for Thur
######################

test <- lm(formula = discharge ~ lag1precip + sum4precip + sum6precip + sum7precip + lag2precip + 
                                 sum15precip + sum30precip + mean30temp + sum5precip +
                                 mean60temp + mean7temp + sum3precip + mean3temp,
           data = p.data$data[calib,])

######################
# lm for Massa
######################

test <- lm(formula = discharge ~ lag1precip + sum30precilag30 +
             sum15precip + sum30precip + mean30temp + mean7temp + mean30templag30 +
             mean3temp + lag2temp,
           data = p.data$data[calib,])



test.pred <- predict(object = test, newdata = p.data$data[valid,])


maxy <- max(test.pred,p.data$data[valid,1])*1.1
miny <- min(test.pred-p.data$data[valid,1])*1.1

rmse(p.data$data[valid,1], test.pred)
plot(p.data$data[valid,1], type = "l", col="blue", ylim = c(miny,maxy), main = "main", ylab = "Discharge")
lines(test.pred, col="green")
lines(test.pred-p.data$data[valid,1], col="red")
abline(h=0)
legend("topright", legend = c("model", "data", "model - data"), bty = "n",
       lty = 1, col = c("green", "blue", "red"))

mean(test.pred)
mean(p.data$data[valid,1])
NSE(sim = as.matrix(test.pred), obs = as.matrix(p.data$data[valid,1]))
KGE(sim = as.matrix(test.pred), obs = as.matrix(p.data$data[valid,1]))

summary(test)


analyze_model(measured = p.data$data$discharge[valid],
              modeled = test.pred,
              catchment = catchment,
              mod_type = "lm",
              model = test)

mon_mean <- monthly_mean(measured = p.data$data$discharge[valid],
                         modeled = test.pred,
                         date = p.data$date[valid])

pdf(file = "../Results/Plots/Massa_LMRegression_monthly.pdf")
monthly_plot(data = mon_mean,
             main = "Massa LMRegression")
dev.off()


afx <- seq(-6,6,0.05)
sigm <- sigmoid(afx)
th <- tanh(afx)
relu <- pmax(afx,0)
softplus <- log(1+exp(afx))




plot(x=afx, y=sigm, type = "l", xlab = "X", ylab = "Y")
abline(h=0)
abline(v=0)


plot(x=afx, y=th, type = "l", xlab = "X", ylab = "Y")
abline(h=0)
abline(v=0)


plot(x=afx, y=relu, type = "l", xlab = "X", ylab = "Y")
abline(h=0)
abline(v=0)


plot(x=afx, y=softplus, type = "l", xlab = "X", ylab = "Y")
abline(h=0)
abline(v=0)
