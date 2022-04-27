# dummy_main
# Main

# SAME THINGS, DONE IN A SMALLER REGION AND A SHORTER TIME PERIOD
# ALL THE BASIC RESULTS ARE AS WANTED BUT TEST ERROR IS USUALLY SMALLER THAN TRAIN ERROR
# (NOT A BIG BIG PROBLEM AS WE THINK THAT TEST IS ON JUNE BUT TRAIN HAS MORE STORMY WEATHERS)

# book render -------------------------------------------------------------
bookdown::render_book("index.Rmd")
browseURL("docs/index.html")


# reduce data dimensions --------------------------------------------------
# using 6 months (5 train 1 test)
dum1 <- data1[1:4320, c(1:5, 7, 8)] # 2x2 yapalim (orjinali 3x3)
dum2 <- data2[1:4320, c(1:6, 8:10, 12:14)] # 3x3 yapalim (orjinali 4x4)
dum3 <- data3[1:4320, c(1:6, 8:10, 12:14)] # 3x3 yapalim (orjinali 4x4)
dum4 <- data4[1:4320, c(1:6, 8:10, 12:14)] # 3x3 yapalim (orjinali 4x4)
dum5 <- data5[1:4320, c(1:6, 8:10, 12:14)] # 3x3 yapalim (orjinali 4x4)
dum6 <- data6[1:4320, c(1:12)] # 3x3 yapalim (orjinali 3x4)


# new colnames ------------------------------------------------------------

colnames(dum1)[6:7] <- c("loc3", "loc4")
#
colnames(dum2)[7:12] <- c("loc4", "loc5", "loc6", "loc7", "loc8", "loc9")
colnames(dum3)[7:12] <- c("loc4", "loc5", "loc6", "loc7", "loc8", "loc9")
colnames(dum4)[7:12] <- c("loc4", "loc5", "loc6", "loc7", "loc8", "loc9")
colnames(dum5)[7:12] <- c("loc4", "loc5", "loc6", "loc7", "loc8", "loc9")
#
# colnames(dum6) stays same


# Derived Datasets -----------------------------------------------------------

# train/test

dt1 <- dum1[1:3600, ]
dt2 <- dum2[1:3600, ]
dt3 <- dum3[1:3600, ]
dt4 <- dum4[1:3600, ]
dt5 <- dum5[1:3600, ]
dt6 <- dum6[1:3600, ]

dte1 <- dum1[3601:4320, ]
dte2 <- dum2[3601:4320, ]
dte3 <- dum3[3601:4320, ]
dte4 <- dum4[3601:4320, ]
dte5 <- dum5[3601:4320, ]
dte6 <- dum6[3601:4320, ]
dim(dte1)
# lag
dt_lag1 <- createLagged(data = dt1, lags = c(-1:1), other.head = c(1:3), other.tail = 0)
dt_lag2 <- createLagged(data = dt2, lags = c(-1:1), other.head = c(1:3), other.tail = 0)
dt_lag3 <- createLagged(data = dt3, lags = c(-1:1), other.head = c(1:3), other.tail = 0)
dt_lag4 <- createLagged(data = dt4, lags = c(-1:1), other.head = c(1:3), other.tail = 0)
dt_lag5 <- createLagged(data = dt5, lags = c(-1:1), other.head = c(1:3), other.tail = 0)
dt_lag6 <- createLagged(data = dt6, lags = c(-1:1), other.head = c(1:3), other.tail = 0)

dte_lag1 <- createLagged(data = dte1, lags = c(-1:1), other.head = c(1:3), other.tail = 0)
dte_lag2 <- createLagged(data = dte2, lags = c(-1:1), other.head = c(1:3), other.tail = 0)
dte_lag3 <- createLagged(data = dte3, lags = c(-1:1), other.head = c(1:3), other.tail = 0)
dte_lag4 <- createLagged(data = dte4, lags = c(-1:1), other.head = c(1:3), other.tail = 0)
dte_lag5 <- createLagged(data = dte5, lags = c(-1:1), other.head = c(1:3), other.tail = 0)
dte_lag6 <- createLagged(data = dte6, lags = c(-1:1), other.head = c(1:3), other.tail = 0)

# power3
dt_pow1 <- createPowered(data = dt1, powers = 2, other.head = c(1:3), other.tail = 0)
dt_pow2 <- createPowered(data = dt2, powers = 2, other.head = c(1:3), other.tail = 0)
dt_pow3 <- createPowered(data = dt3, powers = 2, other.head = c(1:3), other.tail = 0)
dt_pow4 <- createPowered(data = dt4, powers = 2, other.head = c(1:3), other.tail = 0)
dt_pow5 <- createPowered(data = dt5, powers = 2, other.head = c(1:3), other.tail = 0)
dt_pow6 <- createPowered(data = dt6, powers = 2, other.head = c(1:3), other.tail = 0)

dte_pow1 <- createPowered(data = dte1, powers = 2, other.head = c(1:3), other.tail = 0)
dte_pow2 <- createPowered(data = dte2, powers = 2, other.head = c(1:3), other.tail = 0)
dte_pow3 <- createPowered(data = dte3, powers = 2, other.head = c(1:3), other.tail = 0)
dte_pow4 <- createPowered(data = dte4, powers = 2, other.head = c(1:3), other.tail = 0)
dte_pow5 <- createPowered(data = dte5, powers = 2, other.head = c(1:3), other.tail = 0)
dte_pow6 <- createPowered(data = dte6, powers = 2, other.head = c(1:3), other.tail = 0)
# temporal
dt_temp1 <- createTemporal(dt1, 2, c(1:3))
dt_temp2 <- createTemporal(dt2, 2, c(1:3))
dt_temp3 <- createTemporal(dt3, 2, c(1:3))
dt_temp4 <- createTemporal(dt4, 2, c(1:3))
dt_temp5 <- createTemporal(dt5, 2, c(1:3))
dt_temp6 <- createTemporal(dt6, 2, c(1:3))

dte_temp1 <- createTemporal(dte1 ,2, c(1:3))
dte_temp2 <- createTemporal(dte2 ,2, c(1:3))
dte_temp3 <- createTemporal(dte3 ,2, c(1:3))
dte_temp4 <- createTemporal(dte4 ,2, c(1:3))
dte_temp5 <- createTemporal(dte5 ,2, c(1:3))
dte_temp6 <- createTemporal(dte6 ,2, c(1:3))

# lag and power
dt_lagpow1 <- createLagged(data = dt_pow1, lags = c(-1:1), other.head = c(1:3), other.tail = 0)
dt_lagpow2 <- createLagged(data = dt_pow2, lags = c(-1:1), other.head = c(1:3), other.tail = 0)
dt_lagpow3 <- createLagged(data = dt_pow3, lags = c(-1:1), other.head = c(1:3), other.tail = 0)
dt_lagpow4 <- createLagged(data = dt_pow4, lags = c(-1:1), other.head = c(1:3), other.tail = 0)
dt_lagpow5 <- createLagged(data = dt_pow5, lags = c(-1:1), other.head = c(1:3), other.tail = 0)
dt_lagpow6 <- createLagged(data = dt_pow6, lags = c(-1:1), other.head = c(1:3), other.tail = 0)

dte_lagpow1 <- createLagged(data = dte_pow1, lags = c(-1:1), other.head = c(1:3), other.tail = 0)
dte_lagpow2 <- createLagged(data = dte_pow2, lags = c(-1:1), other.head = c(1:3), other.tail = 0)
dte_lagpow3 <- createLagged(data = dte_pow3, lags = c(-1:1), other.head = c(1:3), other.tail = 0)
dte_lagpow4 <- createLagged(data = dte_pow4, lags = c(-1:1), other.head = c(1:3), other.tail = 0)
dte_lagpow5 <- createLagged(data = dte_pow5, lags = c(-1:1), other.head = c(1:3), other.tail = 0)
dte_lagpow6 <- createLagged(data = dte_pow6, lags = c(-1:1), other.head = c(1:3), other.tail = 0)

# Results Table -----------------------------------------------------------
dresults <- data.table(station = character(),
                       model = character(),
                       train_error = numeric(),
                       test_error = numeric())

dresults[order(station)]

# Base Model --------------------------------------------------------------
dfit1 <- cv.glmnet(x = as.matrix(dt1[, -c(1:3)]), y = dt1$production, type.measure = "mae")
dfit2 <- cv.glmnet(x = as.matrix(dt2[, -c(1:3)]), y = dt2$production, type.measure = "mae")
dfit3 <- cv.glmnet(x = as.matrix(dt3[, -c(1:3)]), y = dt3$production, type.measure = "mae")
dfit4 <- cv.glmnet(x = as.matrix(dt4[, -c(1:3)]), y = dt4$production, type.measure = "mae")
dfit5 <- cv.glmnet(x = as.matrix(dt5[, -c(1:3)]), y = dt5$production, type.measure = "mae")
dfit6 <- cv.glmnet(x = as.matrix(dt6[, -c(1:3)]), y = dt6$production, type.measure = "mae")

# # look at coeffs:
# predict(dfit1,type="coef")

# buna simdilik gerek yok. ama aslinda modellerin nerelerde cok hata yaptigina bakmak icin gerekli biraz
# dpred1 = predict(dfit1, as.matrix(dt1[,4:7]),  s = "lambda.1se")
# dpred2 = predict(dfit2, as.matrix(dt2[,4:12]), s = "lambda.1se")
# dpred3 = predict(dfit3, as.matrix(dt3[,4:12]), s = "lambda.1se")
# dpred4 = predict(dfit4, as.matrix(dt4[,4:12]), s = "lambda.1se")
# dpred5 = predict(dfit5, as.matrix(dt5[,4:12]), s = "lambda.1se")
# dpred6 = predict(dfit6, as.matrix(dt6[,4:12]), s = "lambda.1se")

error.train1 <- min(dfit1$cvup)
error.train2 <- min(dfit2$cvup)
error.train3 <- min(dfit3$cvup)
error.train4 <- min(dfit4$cvup)
error.train5 <- min(dfit5$cvup)
error.train6 <- min(dfit6$cvup)

pred.test1 = predict(dfit1, as.matrix(dte1[,-c(1:3)]), s = "lambda.1se")
pred.test2 = predict(dfit2, as.matrix(dte2[,-c(1:3)]), s = "lambda.1se")
pred.test3 = predict(dfit3, as.matrix(dte3[,-c(1:3)]), s = "lambda.1se")
pred.test4 = predict(dfit4, as.matrix(dte4[,-c(1:3)]), s = "lambda.1se")
pred.test5 = predict(dfit5, as.matrix(dte5[,-c(1:3)]), s = "lambda.1se")
pred.test6 = predict(dfit6, as.matrix(dte6[,-c(1:3)]), s = "lambda.1se")

error.test1 = mean(abs(pred.test1 - dte1$production))
error.test2 = mean(abs(pred.test2 - dte2$production))
error.test3 = mean(abs(pred.test3 - dte3$production))
error.test4 = mean(abs(pred.test4 - dte4$production))
error.test5 = mean(abs(pred.test5 - dte5$production))
error.test6 = mean(abs(pred.test6 - dte6$production))

res1 = data.table(station = "aliaga", model = "base", train_error = error.train1, test_error = error.test1)
res2 = data.table(station = "bares",  model = "base", train_error = error.train2, test_error = error.test2)
res3 = data.table(station = "dinar",  model = "base", train_error = error.train3, test_error = error.test3)
res4 = data.table(station = "geycek", model = "base", train_error = error.train4, test_error = error.test4)
res5 = data.table(station = "soke",   model = "base", train_error = error.train5, test_error = error.test5)
res6 = data.table(station = "soma",   model = "base", train_error = error.train6, test_error = error.test6)

dresults = rbind(dresults, res1)
dresults = rbind(dresults, res2)
dresults = rbind(dresults, res3)
dresults = rbind(dresults, res4)
dresults = rbind(dresults, res5)
dresults = rbind(dresults, res6)

dresults[order(station)]

# Lagged ------------------------------------------------------------------
dfit.lag1 = cv.glmnet(x = as.matrix(dt_lag1[,-c(1:3)]), y = dt_lag1$production, type.measure = "mae")
dfit.lag2 = cv.glmnet(x = as.matrix(dt_lag2[,-c(1:3)]), y = dt_lag2$production, type.measure = "mae")
dfit.lag3 = cv.glmnet(x = as.matrix(dt_lag3[,-c(1:3)]), y = dt_lag3$production, type.measure = "mae")
dfit.lag4 = cv.glmnet(x = as.matrix(dt_lag4[,-c(1:3)]), y = dt_lag4$production, type.measure = "mae")
dfit.lag5 = cv.glmnet(x = as.matrix(dt_lag5[,-c(1:3)]), y = dt_lag5$production, type.measure = "mae")
dfit.lag6 = cv.glmnet(x = as.matrix(dt_lag6[,-c(1:3)]), y = dt_lag6$production, type.measure = "mae")

# pred1 = predict(fit.lag1, as.matrix(lagged1[,4:66]),  s = "lambda.1se")
# pred2 = predict(fit.lag2, as.matrix(lagged2[,4:115]), s = "lambda.1se")
# pred3 = predict(fit.lag3, as.matrix(lagged3[,4:115]), s = "lambda.1se")
# pred4 = predict(fit.lag4, as.matrix(lagged4[,4:115]), s = "lambda.1se")
# pred5 = predict(fit.lag5, as.matrix(lagged5[,4:115]), s = "lambda.1se")
# pred6 = predict(fit.lag6, as.matrix(lagged6[,4:87]),  s = "lambda.1se")

error.train1 = min(dfit.lag1$cvup)
error.train2 = min(dfit.lag2$cvup)
error.train3 = min(dfit.lag3$cvup)
error.train4 = min(dfit.lag4$cvup)
error.train5 = min(dfit.lag5$cvup)
error.train6 = min(dfit.lag6$cvup)

pred.test1 = predict(dfit.lag1, as.matrix(dte_lag1[,-c(1:3)]), s = "lambda.1se")
pred.test2 = predict(dfit.lag2, as.matrix(dte_lag2[,-c(1:3)]), s = "lambda.1se")
pred.test3 = predict(dfit.lag3, as.matrix(dte_lag3[,-c(1:3)]), s = "lambda.1se")
pred.test4 = predict(dfit.lag4, as.matrix(dte_lag4[,-c(1:3)]), s = "lambda.1se")
pred.test5 = predict(dfit.lag5, as.matrix(dte_lag5[,-c(1:3)]), s = "lambda.1se")
pred.test6 = predict(dfit.lag6, as.matrix(dte_lag6[,-c(1:3)]), s = "lambda.1se")

error.test1 = mean(abs(pred.test1 - dte_lag1$production))
error.test2 = mean(abs(pred.test2 - dte_lag2$production))
error.test3 = mean(abs(pred.test3 - dte_lag3$production))
error.test4 = mean(abs(pred.test4 - dte_lag4$production))
error.test5 = mean(abs(pred.test5 - dte_lag5$production))
error.test6 = mean(abs(pred.test6 - dte_lag6$production))

res1 = data.table(station = "aliaga", model = "lagged", train_error = error.train1, test_error = error.test1)
res2 = data.table(station = "bares",  model = "lagged", train_error = error.train2, test_error = error.test2)
res3 = data.table(station = "dinar",  model = "lagged", train_error = error.train3, test_error = error.test3)
res4 = data.table(station = "geycek", model = "lagged", train_error = error.train4, test_error = error.test4)
res5 = data.table(station = "soke",   model = "lagged", train_error = error.train5, test_error = error.test5)
res6 = data.table(station = "soma",   model = "lagged", train_error = error.train6, test_error = error.test6)

dresults = rbind(dresults, res1)
dresults = rbind(dresults, res2)
dresults = rbind(dresults, res3)
dresults = rbind(dresults, res4)
dresults = rbind(dresults, res5)
dresults = rbind(dresults, res6)

dresults[order(station)]
# Powered (Squares and Cubes) -------------------------------------------------------
fit.power1 = cv.glmnet(x = as.matrix(dt_pow1[,-c(1:3)]), y = dt_pow1$production, type.measure = "mae")
fit.power2 = cv.glmnet(x = as.matrix(dt_pow2[,-c(1:3)]), y = dt_pow2$production, type.measure = "mae")
fit.power3 = cv.glmnet(x = as.matrix(dt_pow3[,-c(1:3)]), y = dt_pow3$production, type.measure = "mae")
fit.power4 = cv.glmnet(x = as.matrix(dt_pow4[,-c(1:3)]), y = dt_pow4$production, type.measure = "mae")
fit.power5 = cv.glmnet(x = as.matrix(dt_pow5[,-c(1:3)]), y = dt_pow5$production, type.measure = "mae")
fit.power6 = cv.glmnet(x = as.matrix(dt_pow6[,-c(1:3)]), y = dt_pow6$production, type.measure = "mae")

# pred1 = predict(fit.power1, as.matrix(powered1[,4:30]), s = "lambda.1se")
# pred2 = predict(fit.power2, as.matrix(powered2[,4:51]), s = "lambda.1se")
# pred3 = predict(fit.power3, as.matrix(powered3[,4:51]), s = "lambda.1se")
# pred4 = predict(fit.power4, as.matrix(powered4[,4:51]), s = "lambda.1se")
# pred5 = predict(fit.power5, as.matrix(powered5[,4:51]), s = "lambda.1se")
# pred6 = predict(fit.power6, as.matrix(powered6[,4:39]), s = "lambda.1se")

error.train1 = min(fit.power1$cvup)
error.train2 = min(fit.power2$cvup)
error.train3 = min(fit.power3$cvup)
error.train4 = min(fit.power4$cvup)
error.train5 = min(fit.power5$cvup)
error.train6 = min(fit.power6$cvup)

pred.test1 = predict(fit.power1, as.matrix(dte_pow1[,-c(1:3)]), s = "lambda.1se")
pred.test2 = predict(fit.power2, as.matrix(dte_pow2[,-c(1:3)]), s = "lambda.1se")
pred.test3 = predict(fit.power3, as.matrix(dte_pow3[,-c(1:3)]), s = "lambda.1se")
pred.test4 = predict(fit.power4, as.matrix(dte_pow4[,-c(1:3)]), s = "lambda.1se")
pred.test5 = predict(fit.power5, as.matrix(dte_pow5[,-c(1:3)]), s = "lambda.1se")
pred.test6 = predict(fit.power6, as.matrix(dte_pow6[,-c(1:3)]), s = "lambda.1se")

error.test1 = mean(abs(pred.test1 - dte_pow1$production))
error.test2 = mean(abs(pred.test2 - dte_pow2$production))
error.test3 = mean(abs(pred.test3 - dte_pow3$production))
error.test4 = mean(abs(pred.test4 - dte_pow4$production))
error.test5 = mean(abs(pred.test5 - dte_pow5$production))
error.test6 = mean(abs(pred.test6 - dte_pow6$production))

res1 = data.table(station = "aliaga", model = "powered", train_error = error.train1, test_error = error.test1)
res2 = data.table(station = "bares",  model = "powered", train_error = error.train2, test_error = error.test2)
res3 = data.table(station = "dinar",  model = "powered", train_error = error.train3, test_error = error.test3)
res4 = data.table(station = "geycek", model = "powered", train_error = error.train4, test_error = error.test4)
res5 = data.table(station = "soke",   model = "powered", train_error = error.train5, test_error = error.test5)
res6 = data.table(station = "soma",   model = "powered", train_error = error.train6, test_error = error.test6)

dresults = rbind(dresults, res1)
dresults = rbind(dresults, res2)
dresults = rbind(dresults, res3)
dresults = rbind(dresults, res4)
dresults = rbind(dresults, res5)
dresults = rbind(dresults, res6)

dresults[order(station)]
# Lagged and Powered -------------------------------------------------------------
dim(dt_lagpow1)
dim(dt_lagpow2)
fit.lagpower1 = cv.glmnet(x = as.matrix(dt_lagpow1[,-c(1:3)]), y = dt_lagpow1$production, type.measure = "mae")
fit.lagpower2 = cv.glmnet(x = as.matrix(dt_lagpow2[,-c(1:3)]), y = dt_lagpow2$production, type.measure = "mae")
fit.lagpower3 = cv.glmnet(x = as.matrix(dt_lagpow3[,-c(1:3)]), y = dt_lagpow3$production, type.measure = "mae")
fit.lagpower4 = cv.glmnet(x = as.matrix(dt_lagpow4[,-c(1:3)]), y = dt_lagpow4$production, type.measure = "mae")
fit.lagpower5 = cv.glmnet(x = as.matrix(dt_lagpow5[,-c(1:3)]), y = dt_lagpow5$production, type.measure = "mae")
fit.lagpower6 = cv.glmnet(x = as.matrix(dt_lagpow6[,-c(1:3)]), y = dt_lagpow6$production, type.measure = "mae")

# pred1 = predict(fit.lagpower1, as.matrix(laggedpowered1[,4:192]), s = "lambda.1se")
# pred2 = predict(fit.lagpower2, as.matrix(laggedpowered2[,4:339]), s = "lambda.1se")
# pred3 = predict(fit.lagpower3, as.matrix(laggedpowered3[,4:339]), s = "lambda.1se")
# pred4 = predict(fit.lagpower4, as.matrix(laggedpowered4[,4:339]), s = "lambda.1se")
# pred5 = predict(fit.lagpower5, as.matrix(laggedpowered5[,4:339]), s = "lambda.1se")
# pred6 = predict(fit.lagpower6, as.matrix(laggedpowered6[,4:255]), s = "lambda.1se")

error.train1 = min(fit.lagpower1$cvup)
error.train2 = min(fit.lagpower2$cvup)
error.train3 = min(fit.lagpower3$cvup)
error.train4 = min(fit.lagpower4$cvup)
error.train5 = min(fit.lagpower5$cvup)
error.train6 = min(fit.lagpower6$cvup)

pred.test1 = predict(fit.lagpower1, as.matrix(dte_lagpow1[,-c(1:3)]), s = "lambda.1se")
pred.test2 = predict(fit.lagpower2, as.matrix(dte_lagpow2[,-c(1:3)]), s = "lambda.1se")
pred.test3 = predict(fit.lagpower3, as.matrix(dte_lagpow3[,-c(1:3)]), s = "lambda.1se")
pred.test4 = predict(fit.lagpower4, as.matrix(dte_lagpow4[,-c(1:3)]), s = "lambda.1se")
pred.test5 = predict(fit.lagpower5, as.matrix(dte_lagpow5[,-c(1:3)]), s = "lambda.1se")
pred.test6 = predict(fit.lagpower6, as.matrix(dte_lagpow6[,-c(1:3)]), s = "lambda.1se")

error.test1 = mean(abs(pred.test1 - dte_lagpow1$production))
error.test2 = mean(abs(pred.test2 - dte_lagpow2$production))
error.test3 = mean(abs(pred.test3 - dte_lagpow3$production))
error.test4 = mean(abs(pred.test4 - dte_lagpow4$production))
error.test5 = mean(abs(pred.test5 - dte_lagpow5$production))
error.test6 = mean(abs(pred.test6 - dte_lagpow6$production))

res1 = data.table(station = "aliaga", model = "lagged + powered", train_error = error.train1, test_error = error.test1)
res2 = data.table(station = "bares",  model = "lagged + powered", train_error = error.train2, test_error = error.test2)
res3 = data.table(station = "dinar",  model = "lagged + powered", train_error = error.train3, test_error = error.test3)
res4 = data.table(station = "geycek", model = "lagged + powered", train_error = error.train4, test_error = error.test4)
res5 = data.table(station = "soke",   model = "lagged + powered", train_error = error.train5, test_error = error.test5)
res6 = data.table(station = "soma",   model = "lagged + powered", train_error = error.train6, test_error = error.test6)

dresults = rbind(dresults, res1)
dresults = rbind(dresults, res2)
dresults = rbind(dresults, res3)
dresults = rbind(dresults, res4)
dresults = rbind(dresults, res5)
dresults = rbind(dresults, res6)


dresults[order(station)]





# ggplot icin dresults2 ---------------------------------------------------

dresults2 = dresults

dlevel_order <- factor(dresults2$model, level = unique(dresults2$model))[1:42]

ggplot(dresults2) +
    geom_point(mapping = aes(x = dlevel_order, y = train_error, color = station)) +
    labs(x = "Models", y = "Mean Absolute Error", title = "Final Results", subtitle = "Error Comparison")



