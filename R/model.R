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

if(!'glmnet'%in%installed.packages()){
  install.packages('glmnet')
}

rm(list=ls())
gc()


library(rstudioapi)
library(xgboost)
library(tensorflow) #read documentation for installation https://tensorflow.rstudio.com/install/ For gpu support cuda is needed. look for a guide
library(keras) #read documentation for installation. For gpu support cuda is needed. look for a guide
library(data.table)
library(igraph)
library(DiagrammeR)
library(tseries)
library(parallel)
library(ParBayesianOptimization)
library(lightgbm)
library(lubridate)
library(tidyr)
library(e1071)
library(glmnet)

# get file path and set working directory. A long as the folder structure is the same as I used, all paths should work.
wd <- getSourceEditorContext()$path
wd <- substring(wd, first = 1 , last = tail(unlist(gregexpr('/', wd)), n=1)-1)
setwd(wd)
source("functions.R")

# Emme Emmenmatt
# data <- read.table(file = "../Data/Discharge/muelchi_prio/CAMELS_CH_obs_based_2070.csv",
#                    header = TRUE, sep = ",")
# p.data <- dataPrep(data[,c(1,2,5,7)],
#                    datasplit = 1,
#                    start = 1984,
#                    end = 2015)
# catchment <- "Emme Short Calibration"

# Plessur Chrur
# data <- read.table(file = "../Data/Discharge/muelchi_prio/CAMELS_CH_obs_based_2185.csv",
#                    header = TRUE, sep = ",")
# p.data <- dataPrep(data[,c(1,2,5,7)],
#                    datasplit = 1,
#                    start = 1984,
#                    end = 2015)
# catchment <- "Plessur Short Calibration"

# Rosegbach Pontresina
# data <- read.table(file = "../Data/Discharge/muelchi_prio/CAMELS_CH_obs_based_2256.csv",
#                    header = TRUE, sep = ",")
# p.data <- dataPrep(data[,c(1,2,5,7)],
#                    datasplit = 1,
#                    start = 1984,
#                    end = 2015)
# catchment <- "Rosegbach Short Calibration"

# Venoge Ecublens
# data <- read.table(file = "../Data/Discharge/muelchi_prio/CAMELS_CH_obs_based_2432.csv",
#                    header = TRUE, sep = ",")
# p.data <- dataPrep(data[,c(1,2,5,7)],
#                   datasplit = 1,
#                   start = 1984,
#                   end = 2015)
# catchment <- "Venoge Short Calibration"

# Kander Hondrich
# data <- read.table(file = "../Data/Discharge/muelchi_prio/CAMELS_CH_obs_based_2469.csv",
#                    header = TRUE, sep = ",")
# p.data <- dataPrep(data[,c(1,2,5,7)],
#                    datasplit = 1,
#                    start = 1984,
#                    end = 2015)
# catchment <- "Kander Short Calibration"

# Verzasca Lavertezzo
# data <- read.table(file = "../Data/Discharge/muelchi_prio/CAMELS_CH_obs_based_2605.csv",
#                    header = TRUE, sep = ",")
# p.data <- dataPrep(data[,c(1,2,5,7)],
#                    datasplit = 1,
#                    start = 1990,
#                    end = 2015)
# catchment <- "Verzasca Short Calibration"

# Ticino Bellinzona
# data <- read.table(file = "../Data/Discharge/1 - priority/CAMELS_CH_obs_based_2020.txt",
#                      header = TRUE, sep = ";")
# p.data <- dataPrep(data[,c(1,2,5,6)],
#                    datasplit = 1,
#                    start = 1984,
#                    end = 2015)
# catchment <- "Ticino Short Calibration"

# Broye Payerne
# data <- read.table(file = "../Data/Discharge/1 - priority/CAMELS_CH_obs_based_2034.txt",
#                      header = TRUE, sep = ";")
# p.data <- dataPrep(data[,c(1,2,5,6)],
#                    datasplit = 1,
#                    start = 1984,
#                    end = 2015)
# catchment <- "Broye Short Calibration"

# Thur Andelfingen
data <- read.table(file = "../Data/Discharge/1 - priority/CAMELS_CH_obs_based_2044.txt",
                   header = TRUE, sep = ";")
p.data <- dataPrep(data[,c(1,2,5,6)],
                   datasplit = 1,
                   start = 1984,
                   end = 2015)
catchment <- "Thur Short Calibration"

# Massa Blatten bei Naters ??
# data <- read.table(file = "../Data/Discharge/1 - priority/CAMELS_CH_obs_based_2161.txt",
#                    header = TRUE, sep = ";")
# p.data <- dataPrep(data[,c(1,2,5,6)])
# catchment <- "Massa"

# Weisse Lütschine Zweilütschinen ??
# data <- read.table(file = "../Data/Discharge/1 - priority/CAMELS_CH_obs_based_2200.txt",
#                    header = TRUE, sep = ";")
# p.data <- dataPrep(data[,c(1,2,5,6)])
# catchment <- "Weisse Lütschine"

# Dischmabach Davos ??
# data <- read.table(file = "../Data/Discharge/1 - priority/CAMELS_CH_obs_based_2327.txt",
#                    header = TRUE, sep = ";")
# p.data <- dataPrep(data[,c(1,2,5,6)])
# catchment <- "Dischmabach"


calib <- p.data$calib
valid <- p.data$valid
acf(x=p.data$data$discharge,plot = TRUE, lag.max = 365, main= paste("Auto Correlation Function -", catchment), ylab = "Auto Correlation")



#############################################################
#############################################################
#############################################################
#############################################################

# Duplicate the highest and lowest values two times

h.calib <- p.data$data[calib,]
filt.max <- h.calib[,1] > quantile(x = h.calib[,1], probs = 0.98)
filt.min <- h.calib[,1] < quantile(x = h.calib[,1], probs = 0.02)
add.max <- h.calib[filt.max,]
add.min <- h.calib[filt.min,]
for (t in 1:2) {
  h.calib <- rbind(add.max, h.calib)
  h.calib <- rbind(add.min, h.calib)
}


########################
## XGBoost
########################


xgb_mod <- bayes_opt_xgb(data = as.matrix(h.calib[,-1]), label = h.calib[,1]) # p.data$data[calib,-1]), label = p.data$data[calib,1]

save(xgb_mod, file = paste("../Results/Models/", catchment, "_XGBoost.RData", sep = ""))

# load(file = "C:/Users/benme/Documents/R/RainfallRunnoffModel/Results/Models/Thur_XBoost.RData")


pxgb1 <- predict(object = xgb_mod[[2]], newdata = as.matrix(p.data$data[valid,-1]))
pxgb1_calib <- predict(object = xgb_mod[[2]], newdata = as.matrix(p.data$data[calib,-1]))

xgb_vdata <- analyze_model(measured = p.data$data$discharge[valid],
                           modeled = pxgb1,
                           catchment = catchment,
                           mod_type = "xgb",
                           model = xgb_mod[[2]],
                           date = p.data$date[valid])

analyze_calib(measured = p.data$data$discharge[calib],
              modeled = pxgb1_calib,
              catchment = catchment,
              mod_type = "xgb",
              model = xgb_mod[[2]])

xgb_mon_mean <- monthly_mean(measured = p.data$data$discharge[valid],
                         modeled = pxgb1,
                         date = p.data$date[valid])

pdf(file = paste("../Results/Plots/", catchment, "_XGBoost_monthly.pdf", sep = ""))
monthly_plot(data = xgb_mon_mean,
             main = paste(catchment, "XGBoost"))
dev.off()


########################
## LightGBM
########################


lgbm_mod <- bayes_opt_lgb(data = as.matrix(h.calib[,-1]), label = h.calib[,1])


save(lgbm_mod, file = paste("../Results/Models/", catchment, "_LightGBM.RData", sep = ""))
lgb.save(lgbm_mod$optimized_mod, 
         file = paste("../Results/Models/", catchment, "_LightGBM_mod.txt", sep = ""))

plgbm <- predict(object = lgbm_mod$optimized_mod, data = as.matrix(p.data$data[valid,-1]))
plgbm_calib <- predict(object = lgbm_mod$optimized_mod, data = as.matrix(p.data$data[calib,-1]))

lgb_vdata <- analyze_model(measured = p.data$data$discharge[valid],
                           modeled = plgbm,
                           catchment = catchment,
                           mod_type = "lgbm",
                           model = lgbm_mod[[2]],
                           date = p.data$date[valid])

analyze_calib(measured = p.data$data$discharge[calib],
              modeled = plgbm_calib,
              catchment = catchment,
              mod_type = "lgbm",
              model = lgbm_mod[[2]])

lgb_mon_mean <- monthly_mean(measured = p.data$data$discharge[valid],
                             modeled = plgbm,
                             date = p.data$date[valid])

pdf(file = paste("../Results/Plots/", catchment, "_LightGBM_monthly.pdf", sep = ""))
monthly_plot(data = lgb_mon_mean,
             main = paste(catchment, "LightGBM"))
dev.off()


########################
## MLP
########################

h.data.scale <- normalize(p.data$data)

h.calib <- h.data.scale[calib,]
filt.max <- h.calib[,1] > quantile(x = h.calib[,1], probs = 0.97)
filt.min <- h.calib[,1] < quantile(x = h.calib[,1], probs = 0.03)
add.max <- h.calib[filt.max,]
add.min <- h.calib[filt.min,]
for (t in 1:2) {
  h.calib <- rbind(add.max, h.calib)
  h.calib <- rbind(add.min, h.calib)
}


mlp_mod2 <- bayes_opt_MLP(x = h.calib[,-1], 
                          y = h.calib[,1], 
                          validation_split = 0.17)
mlp_mod2$bayes_summary

# mlp_thur <- mlp_mod2
save(mlp_mod2, file = paste("../Results/Models/", catchment, "_MLP.RData", sep = ""))
save_model_tf(mlp_mod2$optimized_mod, file = paste("../Results/Models/", catchment, "_MLP_mod", sep = ""))



pre_mlp <- predict(object = mlp_mod2$optimized_mod, x = h.data.scale[valid,-1])
pre_calib <- predict(object = mlp_mod2$optimized_mod, x = h.data.scale[calib,-1])

std <- sd(p.data$data$discharge)
mean <- mean(p.data$data$discharge)
mlp_vdata <- analyze_model(measured = p.data$data$discharge[valid],
                           modeled = pre_mlp,
                           catchment = catchment,
                           mod_type = "mlp",
                           unscale = c(std,mean,"stdnorm"),
                           date = p.data$date[valid])

analyze_calib(measured = p.data$data$discharge[calib],
              modeled = pre_calib,
              catchment = catchment,
              mod_type = "mlp",
              unscale = c(std,mean,"stdnorm"))

mlp_mon_mean <- monthly_mean(measured = p.data$data$discharge[valid],
                             modeled = mlp_vdata$modeled,
                             date = p.data$date[valid])

pdf(file = paste("../Results/Plots/", catchment, "_MLP_monthly.pdf", sep = ""))
monthly_plot(data = mlp_mon_mean,
             main = paste(catchment, "MLP"))
dev.off()


########################
## LSTM
########################


h.data.scale <- normalize(p.data$data, variant = "stdnorm") #


lstm_mod2 <- bayes_opt_LSTM(x = h.data.scale[calib,-1], 
                            y = h.data.scale[calib,1], 
                            epochs_opt = 15, 
                            initPoints = 30,
                            duplicate = c(0.02, 0.97, 2),
                            validation_split = 0.17)
lstm_mod2$bayes_summary

# lstm_thur <- lstm_mod2
save(lstm_mod2, file = paste("../Results/Models/", catchment, "_LSTM.RData", sep = ""))
save_model_tf(lstm_mod2$optimized_mod, file = paste("../Results/Models/", catchment, "_LSTM_mod", sep = ""))

h.data.lstm_val <- dataPrepLSTM(x = h.data.scale[valid,-1], 
                                y = h.data.scale[valid,1], 
                                timesteps = lstm_mod2$optimized_param$timesteps)

h.data.lstm_cal <- dataPrepLSTM(x = h.data.scale[calib,-1], 
                                y = h.data.scale[calib,1], 
                                timesteps = lstm_mod2$optimized_param$timesteps)


pre_lstm <- predict(object = lstm_mod2$optimized_mod, x = h.data.lstm_val$x)

pre_lstm_calib <- predict(object = lstm_mod2$optimized_mod, x = h.data.lstm_cal$x)

std <- sd(p.data$data$discharge)
mean <- mean(p.data$data$discharge)
mini <- min(p.data$data$discharge)
maxi <- max(p.data$data$discharge)

lstm_vdata <- analyze_model(measured = p.data$data$discharge[valid],
                            modeled = pre_lstm,
                            catchment = catchment,
                            mod_type = "lstm",
                            unscale = c(std,mean,"stdnorm"),
                            date = p.data$date[valid])

analyze_calib(measured = p.data$data$discharge[calib],
              modeled = pre_lstm_calib,
              catchment = catchment,
              mod_type = "lstm",
              unscale = c(std,mean,"stdnorm")) #std,mean,"stdnorm"

lstm_mon_mean <- monthly_mean(measured = p.data$data$discharge[valid],
                         modeled = lstm_vdata$modeled,
                         date = p.data$date[valid])

pdf(file = paste("../Results/Plots/", catchment, "_LSTM_monthly.pdf", sep = ""))
monthly_plot(data = lstm_mon_mean,
             main = paste(catchment, "LSTM"))
dev.off()


########################
## GRU
########################

h.data.scale <- normalize(p.data$data)



gru_mod2 <- bayes_opt_GRU(x = h.data.scale[calib,-1], 
                            y = h.data.scale[calib,1], 
                            epochs_opt = 15, 
                            initPoints = 30
                            , duplicate = c(0.02, 0.97,2)
                            , validation_split = 0.17
                        )
gru_mod2$bayes_summary

save(gru_mod2, file = paste("../Results/Models/", catchment, "_GRU.RData", sep = ""))
save_model_tf(gru_mod2$optimized_mod, file = paste("../Results/Models/", catchment, "_GRU_mod", sep = ""))

h.data.gru_val <- dataPrepLSTM(x = h.data.scale[valid,-1], 
                                y = h.data.scale[valid,1], 
                                timesteps = gru_mod2$optimized_param$timesteps)

h.data.gru_cal <- dataPrepLSTM(x = h.data.scale[calib,-1], 
                               y = h.data.scale[calib,1], 
                               timesteps = gru_mod2$optimized_param$timesteps)

pre_gru <- predict(object = gru_mod2$optimized_mod, x = h.data.gru_val$x)
pre_gru_cal <- predict(object = gru_mod2$optimized_mod, x = h.data.gru_cal$x)

std <- sd(p.data$data$discharge)
mean <- mean(p.data$data$discharge)
gru_vdata <-  analyze_model(measured = p.data$data$discharge[valid],
                            modeled = pre_gru,
                            catchment = catchment,
                            mod_type = "gru",
                            unscale = c(std,mean,"stdnorm"),
                            date = p.data$date[valid])

analyze_calib(measured = p.data$data$discharge[calib],
              modeled = pre_gru_cal,
              catchment = catchment,
              mod_type = "gru",
              unscale = c(std,mean,"stdnorm"))

gru_mon_mean <- monthly_mean(measured = p.data$data$discharge[valid],
                         modeled = gru_vdata$modeled,
                         date = p.data$date[valid])

pdf(file = paste("../Results/Plots/", catchment, "_GRU_monthly.pdf", sep = ""))
monthly_plot(data = gru_mon_mean,
             main = paste(catchment, "GRU"))
dev.off()


########################
## SVR
########################

# Thur:     x = p.data$data[calib,c(4,5,11,19:26,30:32)]
# Massa:    x = p.data$data[calib,c(3,4,10:12,17,23:31)]
# Ticino:   x = p.data$data[calib,c(4,11,18:26,28:30,32,33)]
# Broye:    x = p.data$data[calib,c(2:4,11,13,18:25,27,28,30,31)]
# Emme:     x = p.data$data[calib,c(3,4,18:26,30:32)]
# Plessur:  x = p.data$data[calib,c(2:4,11,18:22,24:33)]
# Rosegbach: x = p.data$data[calib,c(2:4,11,18:20,23:30)]
# Venoge:   x = p.data$data[calib,c(4:5,11,16,19:25,27:33)]
# Kander:   x = p.data$data[calib,c(3,4,11,16,19:29,31,32)]
# Verzasca: x = p.data$data[calib,c(2:4,11,18:21,2:28,32)]

svr_mod2 <- bayes_opt_SVR(x = p.data$data[calib,c(4,5,11,19:26,30:32)], 
                          y = p.data$data[calib,1], 
                          cross = 5)
svr_mod2$bayes_summary
# svr_thur <- svr_mod2
save(svr_mod2, file = paste("../Results/Models/", catchment, "_SVR.RData", sep = ""))
psvr <- predict(object = svr_mod2$optimized_mod, newdata = p.data$data[valid,c(4,5,11,19:26,30:32)])
psvr_c <- predict(object = svr_mod2$optimized_mod, newdata = p.data$data[calib,c(4,5,11,19:26,30:32)])


svr_vdata <- analyze_model(measured = p.data$data$discharge[valid],
                           modeled = psvr,
                           catchment = catchment,
                           mod_type = "svr",
                           model = svr_mod2[[2]],
                           date = p.data$date[valid])

analyze_calib(measured = p.data$data$discharge[calib],
              modeled = psvr_c,
              catchment = catchment,
              mod_type = "svr",
              model = svr_mod2[[2]])

svr_mon_mean <- monthly_mean(measured = p.data$data$discharge[valid],
                         modeled = psvr,
                         date = p.data$date[valid])

pdf(file = paste("../Results/Plots/", catchment, "_SVRegression_monthly.pdf", sep = ""))
monthly_plot(data = svr_mon_mean,
             main = paste(catchment, "SVRegression"))
dev.off()


###############################################################################
# plot monthly mean data of all models in one plot and save all the results
###############################################################################

maxy <- max(colMeans(xgb_mon_mean$measured, na.rm = TRUE), 
            colMeans(xgb_mon_mean$modeled, na.rm = TRUE),
            colMeans(lgb_mon_mean$modeled, na.rm = TRUE),
            colMeans(mlp_mon_mean$modeled, na.rm = TRUE),
            colMeans(lstm_mon_mean$modeled, na.rm = TRUE),
            colMeans(gru_mon_mean$modeled, na.rm = TRUE),
            colMeans(svr_mon_mean$modeled, na.rm = TRUE)) * 1.1
miny <- min(colMeans(xgb_mon_mean$measured, na.rm = TRUE), 
            colMeans(xgb_mon_mean$modeled, na.rm = TRUE),
            colMeans(lgb_mon_mean$modeled, na.rm = TRUE),
            colMeans(mlp_mon_mean$modeled, na.rm = TRUE),
            colMeans(lstm_mon_mean$modeled, na.rm = TRUE),
            colMeans(gru_mon_mean$modeled, na.rm = TRUE),
            colMeans(svr_mon_mean$modeled, na.rm = TRUE)) * 0.9

pdf(file = paste("../Results/Plots/", catchment, "_combined_monthly.pdf", sep = ""))
par(mar = c(5, 4.3, 4, 2) + 0.1)
plot(colMeans(xgb_mon_mean$measured, na.rm = TRUE), 
     type = "l", xlab = "Month", ylab = expression("Discharge [" ~ m^{3}/s ~ "]"), 
     xaxt = "n", main = paste("Monthly Mean Discharge -", catchment),
     ylim = c(miny,maxy), lwd = 2)

lines(colMeans(xgb_mon_mean$modeled, na.rm = TRUE), col = "red", lwd = 1.3)
lines(colMeans(lgb_mon_mean$modeled, na.rm = TRUE), col = "mediumvioletred", lwd = 1.3)
lines(colMeans(mlp_mon_mean$modeled, na.rm = TRUE), col = "darkorange2", lwd = 1.3)
lines(colMeans(lstm_mon_mean$modeled, na.rm = TRUE), col = "darkgreen", lwd = 1.3)
lines(colMeans(gru_mon_mean$modeled, na.rm = TRUE), col = "springgreen3", lwd = 1.3)
lines(colMeans(svr_mon_mean$modeled, na.rm = TRUE), col = "saddlebrown", lwd = 1.3)

axis(side = 1, at = 1:12, labels = month.abb)
legend("topright", legend = c("Measured", "XGBoost", "LightGBM", "MLP", "LSTM", "GRU", "SVR"), bty = "n", 
       lty = 1, lwd = 2, col = c("black", "red", "mediumvioletred", "darkorange2", 
                                 "darkgreen", "springgreen3", "saddlebrown"))
dev.off()

vec_lstm <- rep(NA, length(xgb_vdata$date))
vec_gru <- rep(NA, length(xgb_vdata$date))
diff_lstm <- length(xgb_vdata$date)-length(lstm_vdata$date)
diff_gru <- length(xgb_vdata$date)-length(gru_vdata$date)
vec_lstm[(diff_lstm+1):length(xgb_vdata$date)] <- lstm_vdata$modeled
vec_gru[(diff_gru+1):length(xgb_vdata$date)] <- gru_vdata$modeled

data_save <- data.frame(xgb_vdata$date, 
                        xgb_vdata$measured, 
                        round(xgb_vdata$modeled, digits = 2),
                        round(lgb_vdata$modeled, digits = 2),
                        round(mlp_vdata$modeled, digits = 2),
                        round(vec_lstm, digits = 2),
                        round(vec_gru, digits = 2),
                        round(svr_vdata$modeled, digits = 2))

colnames(data_save) <- c("date", "measured", "xgboost", "lightgbm", "mlp", "lstm", "gru", "svr")

write.table(data_save, file = paste("../Results/Models/modeled_discharge_", catchment,".txt", sep = ""), sep = ";")




######################
# lm for Thur
######################
# 
# test <- lm(formula = discharge ~ lag1precip + sum4precip + sum6precip + sum7precip + lag2precip + 
#                                  sum15precip + sum30precip + mean30temp + sum5precip +
#                                  mean60temp + mean7temp + sum3precip + mean3temp,
#            data = p.data$data[calib,])

######################
# lm for Massa
######################

# test <- lm(formula = discharge ~ lag1precip + sum30precilag30 +
#              sum15precip + sum30precip + mean30temp + mean7temp + mean30templag30 +
#              mean3temp + lag2temp,
#            data = p.data$data[calib,])
# 
# 
# 
# test.pred <- predict(object = test, newdata = p.data$data[valid,])
# 
# 
# maxy <- max(test.pred,p.data$data[valid,1])*1.1
# miny <- min(test.pred-p.data$data[valid,1])*1.1
# 
# rmse(p.data$data[valid,1], test.pred)
# plot(p.data$data[valid,1], type = "l", col="blue", ylim = c(miny,maxy), main = "main", ylab = "Discharge")
# lines(test.pred, col="green")
# lines(test.pred-p.data$data[valid,1], col="red")
# abline(h=0)
# legend("topright", legend = c("model", "data", "model - data"), bty = "n",
#        lty = 1, col = c("green", "blue", "red"))
# 
# mean(test.pred)
# mean(p.data$data[valid,1])
# NSE(sim = as.matrix(test.pred), obs = as.matrix(p.data$data[valid,1]))
# KGE(sim = as.matrix(test.pred), obs = as.matrix(p.data$data[valid,1]))
# 
# summary(test)
# 
# 
# analyze_model(measured = p.data$data$discharge[valid],
#               modeled = test.pred,
#               catchment = catchment,
#               mod_type = "lm",
#               model = test)
# 
# mon_mean <- monthly_mean(measured = p.data$data$discharge[valid],
#                          modeled = test.pred,
#                          date = p.data$date[valid])
# 
# pdf(file = "../Results/Plots/Massa_LMRegression_monthly.pdf")
# monthly_plot(data = mon_mean,
#              main = "Massa LMRegression")
# dev.off()




##############################################
#High / Low pass filter model
##############################################


# lowdata <- p.data$lh.filter$lowpass61
# highdata <- p.data$lh.filter$highpass61
# 
# 
# xgb_lowp1 <- bayesOpt_xgb(data = as.matrix(h.data[calib.h,3:27]), label = lowdata[calib.h])
# pxgb_lp1 <- predict(object = xgb_lowp1[[2]], newdata = as.matrix(h.data[valid.h,3:27]))
# 
# xgb_highp1 <- bayesOpt_xgb(data = as.matrix(h.data[calib.h,3:27]), label = highdata[calib.h])
# pxgb_hp1 <- predict(object = xgb_highp1[[2]], newdata = as.matrix(h.data[valid.h,3:27]))
# 
# 
# maxy <- max(pxgb_lp1,lowdata[valid.h])*1.1
# miny <- min(pxgb_lp1-lowdata[valid.h])*1.1
# plot(pxgb_lp1, type = "l", col="darkgreen", ylim = c(miny,maxy), main = "main")
# lines(lowdata[valid.h], col="blue")
# lines(pxgb_lp1-lowdata[valid.h], col="red")
# abline(h=0)
# legend("topright", legend = c("model", "data", "model - data"), bty = "n", 
#        lty = 1, col = c("darkgreen", "blue", "red"))
# 
# mean(pxgb_lp1)
# mean(lowdata[valid.h])
# NSE(sim = as.matrix(pxgb_lp1), obs = as.matrix(lowdata[valid.h]))
# KGE(sim = as.matrix(pxgb_lp1), obs = as.matrix(lowdata[valid.h]))
# 
# xgb.plot.deepness(xgb_lowp1)
# xgb.plot.importance(xgb.importance(model=xgb_lowp1))
# xgb.plot.shap.summary(data=as.matrix(h.data[calib.h,-c(1,2)]), model=xgb_lowp1)
# 
# 
# 
# maxy <- max(pxgb_hp1,highdata[valid.h])*1.1
# miny <- min(pxgb_hp1-highdata[valid.h])*1.1
# plot(pxgb_hp1, type = "l", col="darkgreen", ylim = c(miny,maxy), main = "main")
# lines(highdata[valid.h], col="blue")
# lines(pxgb_hp1-highdata[valid.h], col="red")
# abline(h=0)
# legend("topright", legend = c("model", "data", "model - data"), bty = "n", 
#        lty = 1, col = c("darkgreen", "blue", "red"))
# 
# mean(pxgb_hp1)
# mean(highdata[valid.h])
# NSE(sim = as.matrix(pxgb_hp1), obs = as.matrix(highdata[valid.h]))
# KGE(sim = as.matrix(pxgb_hp1), obs = as.matrix(highdata[valid.h]))
# 
# xgb.plot.deepness(xgb_highp1)
# xgb.plot.importance(xgb.importance(model=xgb_highp1))
# xgb.plot.shap.summary(data=as.matrix(h.data[calib.h,-c(1,2)]), model=xgb_highp1)
# 
# 
# 
# combined <- pxgb_hp1+pxgb_lp1
# 
# maxy <- max(combined,h.data$discharge_vol.m3.s.[valid.h])*1.1
# miny <- min(combined-h.data$discharge_vol.m3.s.[valid.h])*1.1
# plot(combined, type = "l", col="darkgreen", ylim = c(miny,maxy), main = "main")
# lines(h.data$discharge_vol.m3.s.[valid.h], col="blue")
# lines(combined-h.data$discharge_vol.m3.s.[valid.h], col="red")
# abline(h=0)
# legend("topright", legend = c("model", "data", "model - data"), bty = "n", 
#        lty = 1, col = c("darkgreen", "blue", "red"))
# 
# mean(combined)
# mean(h.data$discharge_vol.m3.s.[valid.h])
# NSE(sim = as.matrix(combined), obs = as.matrix(h.data$discharge_vol.m3.s.[valid.h]))
# KGE(sim = as.matrix(combined), obs = as.matrix(h.data$discharge_vol.m3.s.[valid.h]))
# 
# 
# xgb_mod <- xgboost(data = as.matrix(h.data[calib.h,3:27]), label = highdata[calib.h], 
#                    max.depth = i, eta = eta[e], nrounds = j, nthread = 20,objective = "reg:squarederror")
# pre_xgb <- predict(object = xgb_mod, newdata = as.matrix(h.data[valid.h,3:27]))
# 
# 
# rmse[k] <- sqrt(mean((pre_xgb-highdata[valid.h])^2)) 


#############################################
#############################################
#############################################
#############################################
#############################################
#############################################


# lp.data <- p.data$data
# lp.data$discharge <- lowdata
# 
# hp.data <- p.data$data
# hp.data$discharge <- highdata
# 
# lp.data.scale <- normalize(lp.data, variant = "stdnorm")
# hp.data.scale <- normalize(hp.data, variant = "stdnorm")
# 
# lstm_mod_lp <- bayes_opt_LSTM(x = lp.data.scale[calib,-1], 
#                               y = lp.data.scale[calib,1], 
#                               epochs_opt = 15, 
#                               initPoints = 20,
#                               duplicate = c(0.02, 0.97, 3),
#                               validation_split = 0.15)
# 
# lstm_mod_hp <- bayes_opt_LSTM(x = hp.data.scale[calib,-1], 
#                               y = hp.data.scale[calib,1], 
#                               epochs_opt = 15, 
#                               initPoints = 20,
#                               duplicate = c(0.02, 0.97, 3),
#                               validation_split = 0.15)
# 
# 
# lstm_mod2$bayes_summary
# 
# # lstm_thur <- lstm_mod2
# save(lstm_mod_lp, file = paste("../Results/Models/", catchment, "_LSTM_LP.RData", sep = ""))
# save(lstm_mod_hp, file = paste("../Results/Models/", catchment, "_LSTM_HP.RData", sep = ""))
# 
# lp.data.lstm_val <- dataPrepLSTM(x = lp.data.scale[valid,-1], 
#                                  y = lp.data.scale[valid,1], 
#                                  timesteps = lstm_mod_lp$optimized_param$timesteps)
# hp.data.lstm_val <- dataPrepLSTM(x = hp.data.scale[valid,-1], 
#                                  y = hp.data.scale[valid,1], 
#                                  timesteps = lstm_mod_hp$optimized_param$timesteps)
# 
# lp.data.lstm_cal <- dataPrepLSTM(x = lp.data.scale[calib,-1], 
#                                  y = lp.data.scale[calib,1], 
#                                  timesteps = lstm_mod_lp$optimized_param$timesteps)
# hp.data.lstm_cal <- dataPrepLSTM(x = hp.data.scale[calib,-1], 
#                                  y = hp.data.scale[calib,1], 
#                                  timesteps = lstm_mod_hp$optimized_param$timesteps)
# 
# 
# lp.pre_lstm <- predict(object = lstm_mod_lp$optimized_mod, x = lp.data.lstm_val$x)
# hp.pre_lstm <- predict(object = lstm_mod_hp$optimized_mod, x = hp.data.lstm_val$x)
# 
# lp.pre_lstm_calib <- predict(object = lstm_mod_lp$optimized_mod, x = lp.data.lstm_cal$x)
# hp.pre_lstm_calib <- predict(object = lstm_mod_hp$optimized_mod, x = hp.data.lstm_cal$x)
# 
# std <- sd(p.data$data$discharge)
# mean <- mean(p.data$data$discharge)
# pre_lstm_unscaled <-  analyze_model(measured = p.data$data$discharge[valid],
#                                     modeled = pre_lstm,
#                                     catchment = catchment,
#                                     mod_type = "lstm",
#                                     unscale = c(std,mean,"stdnorm"))
# 
# analyze_calib(measured = p.data$data$discharge[calib],
#               modeled = pre_lstm_calib,
#               catchment = catchment,
#               mod_type = "lstm",
#               unscale = c(std,mean,"stdnorm"))
# 
# mon_mean <- monthly_mean(measured = p.data$data$discharge[valid],
#                          modeled = pre_lstm_unscaled,
#                          date = p.data$date[valid])
# 
# pdf(file = paste("../Results/Plots/", catchment, "_LSTM_monthly.pdf", sep = ""))
# monthly_plot(data = mon_mean,
#              main = paste(catchment, "LSTM"))
# dev.off()
# 
# 
# 
# 
# 
# ##############
# # activation functions
# ##############
# 
# afx <- seq(-6,6,0.05)
# sigm <- sigmoid(afx)
# th <- tanh(afx)
# relu <- pmax(afx,0)
# softplus <- log(1+exp(afx))
# 
# 
# 
# 
# plot(x=afx, y=sigm, type = "l", xlab = "X", ylab = "Y")
# abline(h=0)
# abline(v=0)
# 
# 
# plot(x=afx, y=th, type = "l", xlab = "X", ylab = "Y")
# abline(h=0)
# abline(v=0)
# 
# 
# plot(x=afx, y=relu, type = "l", xlab = "X", ylab = "Y")
# abline(h=0)
# abline(v=0)
# 
# 
# plot(x=afx, y=softplus, type = "l", xlab = "X", ylab = "Y")
# abline(h=0)
# abline(v=0)


#########################
# Elastic net regression
#########################

# h.calib <- p.data$data[calib,]
# filt.max <- h.calib[,1] > quantile(x = h.calib[,1], probs = 0.98)
# filt.min <- h.calib[,1] < quantile(x = h.calib[,1], probs = 0.02)
# add.max <- h.calib[filt.max,]
# add.min <- h.calib[filt.min,]
# for (t in 1:2) {
#   h.calib <- rbind(add.max, h.calib)
#   h.calib <- rbind(add.min, h.calib)
# }
# 
# test <- cv.glmnet(x=as.matrix(h.calib[,-1]), y=h.calib[,1], 
#                   family = "gaussian", 
#                   alpha = 0.5, 
#                   nlambda = 200, 
#                   nfolds = 5)
# test$lambda.min
# 
# test1 <- glmnet(x=as.matrix(h.calib[,-1]), y=h.calib[,1], lambda = test$lambda.min, alpha = 0.5)
# 
# plot(test)
# 
# test.p <- predict.glmnet(object = test1, newx = as.matrix(p.data$data[valid,-1]) ,s="lambda.min")
# 
# rmse(x=p.data$data[valid,1], y=test.p)
# nrmse(x=p.data$data[valid,1], y=test.p)
# mae(x=p.data$data[valid,1], y=test.p)
# nmae(x=p.data$data[valid,1], y=test.p)
# plot(density(pre_nn_unscaled-p.data$data[valid,1]))
# qqplot(test.p,p.data$data[valid,1])
# shapiro.test(pre_nn_unscaled-p.data$data[valid,1])
# ks.test(pre_nn_unscaled-p.data$data[valid,1], "pnorm")
# NSE(obs = p.data$data[valid,1], mod = test.p)



###################################
###################################
###################################



gru_load <- load_model_tf(file = "../Results/Models/Thur_GRU_mod")
lgb_load <- lgb.load(file = "../Results/Models/Thur_LightGBM_mod.txt")
lstm_load <- load_model_tf(file = "../Results/Models/Thur_LSTM_mod")
mlp_load <- load_model_tf(file = "../Results/Models/Thur_MLP_mod")
load(file = "../Results/Models/Thur_SVR.RData")
load(file = "../Results/Models/Thur_XBoost.RData")


load(file = "../Results/Models/Verzasca_GRU.RData")
load(file = "../Results/Models/Verzasca_LightGBM.RData")
load(file = "../Results/Models/Verzasca_LSTM.RData")
load(file = "../Results/Models/Verzasca_MLP.RData")
load(file = "../Results/Models/Verzasca_SVR.RData")
load(file = "../Results/Models/Verzasca_XGBoost.RData")


load(file = "../Results/Models/Broye_GRU.RData")
load(file = "../Results/Models/Broye_LightGBM.RData")
load(file = "../Results/Models/Broye_LSTM.RData")
load(file = "../Results/Models/Broye_MLP.RData")
load(file = "../Results/Models/Broye_SVR.RData")
load(file = "../Results/Models/Broye_XGBoost.RData")


svr_mon_mean <- monthly_mean(measured = p.data$data$discharge[calib],
                             modeled = p.data$data$discharge[calib],
                             date = p.data$date[calib])


monthly_plot(data = svr_mon_mean,
             main = paste(catchment, "SVRegression"))








catchment <- "Broye Short Calibration"

mod_data <- read.table(file = paste("../Results/Models/modeled_discharge_",catchment, ".txt", sep = ""), 
                       sep = ";", header = TRUE)
mod_data$date <- strptime(mod_data$date, format = "%Y-%m-%d")
mod_data$year <- year(mod_data$date)
mod_data$month <- month(mod_data$date)

monthly_mod_data <- aggregate(x=cbind(measured, xgboost, lightgbm, mlp, lstm, gru, svr)~month+year,
                              data = mod_data,
                              FUN = "mean")
monthly_mod_na <- monthly_mod_data
for (i in 1:nrow(monthly_mod_data)%/%12) {
  monthly_mod_na[((i-1)*13+1):((i*13)-1),] <- monthly_mod_data[((i-1)*12+1):(i*12),]
  monthly_mod_na[(i*13),] <- NA
}
monthly_mod_na <- head(monthly_mod_na, -1)

maxy <- max(monthly_mod_data[,3:9])*1.1
miny <- min(monthly_mod_data[,3:9])*0.9
pdf(file = paste("../Results/Plots/monthly_discharge_mod_", catchment, ".pdf", sep = ""),width = 22, height = 7)
par(mar = c(5, 4.3, 4, 2) + 0.1)
plot(monthly_mod_na$measured, type = "l", col="black", ylim = c(miny,maxy), 
     main = paste("Monthly Mean Discharge", catchment,"- Validation Period"), 
     ylab = expression(" Monthly Mean Discharge [" ~ m^{3}/s ~ "]"),
     xlab = "Validation Period",
     xaxt = "n",
     lwd=2)
axis(side = 1, 
     at=seq(from = 6.5, to = nrow(monthly_mod_na)-5, by = 13),
     labels = unique(monthly_mod_data$year))
abline(v=seq(from = 13, to = nrow(monthly_mod_na)-5, by = 13), lty = "dotted", lwd = 2)
lines(monthly_mod_na$xgboost, col = "red")
lines(monthly_mod_na$lightgbm, col = "mediumvioletred")
lines(monthly_mod_na$mlp, col = "darkorange2")
lines(monthly_mod_na$lstm, col = "darkgreen")
lines(monthly_mod_na$gru, col = "springgreen3")
lines(monthly_mod_na$svr, col = "saddlebrown")

legend("topright", legend = c("Measured", "XGBoost", "LightGBM", "MLP", "LSTM", "GRU", "SVR"), bty = "n", 
       lty = 1, lwd = 2, col = c("black", "red", "mediumvioletred", "darkorange2", 
                                 "darkgreen", "springgreen3", "saddlebrown"))
dev.off()


########################################
########################################


catchment <- "Thur"

mod_data <- read.table(file = paste("../Results/Models/modeled_discharge_",catchment, ".txt", sep = ""), 
                       sep = ";", header = TRUE)
mod_data$date <- strptime(mod_data$date, format = "%Y-%m-%d")

maxy <- max(mod_data[2281:2401,2:8])*1.1
miny <- min(mod_data[2281:2401,2:8])*0.9
pdf(file = paste("../Results/Plots/daily_discharge_mod_", catchment, ".pdf", sep = ""),width = 22, height = 7)
par(mar = c(5, 4.3, 4, 2) + 0.1)
plot(x= mod_data$date[2281:2401], y= mod_data$measured[2281:2401], type = "l",
     xlab = "Year 2001",
     ylab = expression("Discharge [" ~ m^{3}/s ~ "]"),
     ylim = c(miny,maxy),
     lwd=2)
lines(x= mod_data$date[2281:2401], y=mod_data$xgboost[2281:2401], col = "red")
lines(x= mod_data$date[2281:2401], y=mod_data$lightgbm[2281:2401], col = "mediumvioletred")
lines(x= mod_data$date[2281:2401],mod_data$mlp[2281:2401], col = "darkorange2")
lines(x= mod_data$date[2281:2401],mod_data$lstm[2281:2401], col = "darkgreen")
lines(x= mod_data$date[2281:2401],mod_data$gru[2281:2401], col = "springgreen3")
lines(x= mod_data$date[2281:2401],mod_data$svr[2281:2401], col = "saddlebrown")
legend("topright", legend = c("Measured", "XGBoost", "LightGBM", "MLP", "LSTM", "GRU", "SVR"), bty = "n", 
       lty = 1, lwd = 2, col = c("black", "red", "mediumvioletred", "darkorange2", 
                                 "darkgreen", "springgreen3", "saddlebrown"))
dev.off()

