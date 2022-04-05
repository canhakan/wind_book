# dummy_polynomial

# OLD POLY (DEPRECIATIEITD) (time/production falan ekledim) ------------------------------------------------------------------
#
# dt_poly1 = polym(as.matrix(dt1[,-c(1:3)]), degree = 2, raw = TRUE)
# dt_poly2 = polym(as.matrix(dt2[,-c(1:3)]), degree = 2, raw = TRUE)
# dt_poly3 = polym(as.matrix(dt3[,-c(1:3)]), degree = 2, raw = TRUE)
# dt_poly4 = polym(as.matrix(dt4[,-c(1:3)]), degree = 2, raw = TRUE)
# dt_poly5 = polym(as.matrix(dt5[,-c(1:3)]), degree = 2, raw = TRUE)
# dt_poly6 = polym(as.matrix(dt6[,-c(1:3)]), degree = 2, raw = TRUE)
#
# dte_poly1 = polym(as.matrix(dte1[,-c(1:3)]),degree = 2, raw = TRUE)
# dte_poly2 = polym(as.matrix(dte2[,-c(1:3)]),degree = 2, raw = TRUE)
# dte_poly3 = polym(as.matrix(dte3[,-c(1:3)]),degree = 2, raw = TRUE)
# dte_poly4 = polym(as.matrix(dte4[,-c(1:3)]),degree = 2, raw = TRUE)
# dte_poly5 = polym(as.matrix(dte5[,-c(1:3)]),degree = 2, raw = TRUE)
# dte_poly6 = polym(as.matrix(dte6[,-c(1:3)]),degree = 2, raw = TRUE)
# fit.poly1 = cv.glmnet(x = dt_poly1, y = dt1$production, type.measure = 'mae')
# fit.poly2 = cv.glmnet(x = dt_poly2, y = dt2$production, type.measure = 'mae')
# fit.poly3 = cv.glmnet(x = dt_poly3, y = dt3$production, type.measure = 'mae')
# fit.poly4 = cv.glmnet(x = dt_poly4, y = dt4$production, type.measure = 'mae')
# fit.poly5 = cv.glmnet(x = dt_poly5, y = dt5$production, type.measure = 'mae')
# fit.poly6 = cv.glmnet(x = dt_poly6, y = dt6$production, type.measure = 'mae')
#
# error.train1 = min(fit.poly1$cvup)
# error.train2 = min(fit.poly2$cvup)
# error.train3 = min(fit.poly3$cvup)
# error.train4 = min(fit.poly4$cvup)
# error.train5 = min(fit.poly5$cvup)
# error.train6 = min(fit.poly6$cvup)
#
# pred.test1 = predict(fit.poly1, dte_poly1, s = "lambda.1se")
# pred.test2 = predict(fit.poly2, dte_poly2, s = "lambda.1se")
# pred.test3 = predict(fit.poly3, dte_poly3, s = "lambda.1se")
# pred.test4 = predict(fit.poly4, dte_poly4, s = "lambda.1se")
# pred.test5 = predict(fit.poly5, dte_poly5, s = "lambda.1se")
# pred.test6 = predict(fit.poly6, dte_poly6, s = "lambda.1se")
#
# error.test1 = mean(abs(pred.test1 - dte1$production))
# error.test2 = mean(abs(pred.test2 - dte2$production))
# error.test3 = mean(abs(pred.test3 - dte3$production))
# error.test4 = mean(abs(pred.test4 - dte4$production))
# error.test5 = mean(abs(pred.test5 - dte5$production))
# error.test6 = mean(abs(pred.test6 - dte6$production))
#
# res1 = data.table(station = "aliaga", model = "poly(2)", train_error = error.train1, test_error = error.test1)
# res2 = data.table(station = "bares",  model = "poly(2)", train_error = error.train2, test_error = error.test2)
# res3 = data.table(station = "dinar",  model = "poly(2)", train_error = error.train3, test_error = error.test3)
# res4 = data.table(station = "geycek", model = "poly(2)", train_error = error.train4, test_error = error.test4)
# res5 = data.table(station = "soke",   model = "poly(2)", train_error = error.train5, test_error = error.test5)
# res6 = data.table(station = "soma",   model = "poly(2)", train_error = error.train6, test_error = error.test6)
#
# dresults = rbind(dresults, res1)
# dresults = rbind(dresults, res2)
# dresults = rbind(dresults, res3)
# dresults = rbind(dresults, res4)
# dresults = rbind(dresults, res5)
# dresults = rbind(dresults, res6)
#
# dresults[order(station)]
# dresults %>% filter(model == "poly(2)")
# dresults = dresults %>% filter(model != "poly(2)")



# create poly data --------------------------------------------------------
# polynomial of order 2 (we may even try 3 but not yet)
dt_poly1 = cbind(dt1[,1:3], polym(as.matrix(dt1[,-c(1:3)]),degree = 2, raw = TRUE))
dt_poly2 = cbind(dt2[,1:3], polym(as.matrix(dt2[,-c(1:3)]),degree = 2, raw = TRUE))
dt_poly3 = cbind(dt3[,1:3], polym(as.matrix(dt3[,-c(1:3)]),degree = 2, raw = TRUE))
dt_poly4 = cbind(dt4[,1:3], polym(as.matrix(dt4[,-c(1:3)]),degree = 2, raw = TRUE))
dt_poly5 = cbind(dt5[,1:3], polym(as.matrix(dt5[,-c(1:3)]),degree = 2, raw = TRUE))
dt_poly6 = cbind(dt6[,1:3], polym(as.matrix(dt6[,-c(1:3)]),degree = 2, raw = TRUE))

dte_poly1 = cbind(dte1[,1:3], polym(as.matrix(dte1[,-c(1:3)]),degree = 2, raw = TRUE))
dte_poly2 = cbind(dte2[,1:3], polym(as.matrix(dte2[,-c(1:3)]),degree = 2, raw = TRUE))
dte_poly3 = cbind(dte3[,1:3], polym(as.matrix(dte3[,-c(1:3)]),degree = 2, raw = TRUE))
dte_poly4 = cbind(dte4[,1:3], polym(as.matrix(dte4[,-c(1:3)]),degree = 2, raw = TRUE))
dte_poly5 = cbind(dte5[,1:3], polym(as.matrix(dte5[,-c(1:3)]),degree = 2, raw = TRUE))
dte_poly6 = cbind(dte6[,1:3], polym(as.matrix(dte6[,-c(1:3)]),degree = 2, raw = TRUE))

# glmnet ------------------------------------------------------------------
fit.poly1 = cv.glmnet(x = as.matrix(dt_poly1[,-c(1:3)]), y = dt_poly1$production, type.measure = 'mae')
fit.poly2 = cv.glmnet(x = as.matrix(dt_poly2[,-c(1:3)]), y = dt_poly2$production, type.measure = 'mae')
fit.poly3 = cv.glmnet(x = as.matrix(dt_poly3[,-c(1:3)]), y = dt_poly3$production, type.measure = 'mae')
fit.poly4 = cv.glmnet(x = as.matrix(dt_poly4[,-c(1:3)]), y = dt_poly4$production, type.measure = 'mae')
fit.poly5 = cv.glmnet(x = as.matrix(dt_poly5[,-c(1:3)]), y = dt_poly5$production, type.measure = 'mae')
fit.poly6 = cv.glmnet(x = as.matrix(dt_poly6[,-c(1:3)]), y = dt_poly6$production, type.measure = 'mae')

error.train1 = min(fit.poly1$cvup)
error.train2 = min(fit.poly2$cvup)
error.train3 = min(fit.poly3$cvup)
error.train4 = min(fit.poly4$cvup)
error.train5 = min(fit.poly5$cvup)
error.train6 = min(fit.poly6$cvup)

pred.test1 = predict(fit.poly1, as.matrix(dte_poly1[,-c(1:3)]), s = "lambda.1se")
pred.test2 = predict(fit.poly2, as.matrix(dte_poly2[,-c(1:3)]), s = "lambda.1se")
pred.test3 = predict(fit.poly3, as.matrix(dte_poly3[,-c(1:3)]), s = "lambda.1se")
pred.test4 = predict(fit.poly4, as.matrix(dte_poly4[,-c(1:3)]), s = "lambda.1se")
pred.test5 = predict(fit.poly5, as.matrix(dte_poly5[,-c(1:3)]), s = "lambda.1se")
pred.test6 = predict(fit.poly6, as.matrix(dte_poly6[,-c(1:3)]), s = "lambda.1se")

error.test1 = mean(abs(pred.test1 - dte1$production))
error.test2 = mean(abs(pred.test2 - dte2$production))
error.test3 = mean(abs(pred.test3 - dte3$production))
error.test4 = mean(abs(pred.test4 - dte4$production))
error.test5 = mean(abs(pred.test5 - dte5$production))
error.test6 = mean(abs(pred.test6 - dte6$production))

res1 = data.table(station = "aliaga", model = "poly(2)", train_error = error.train1, test_error = error.test1)
res2 = data.table(station = "bares",  model = "poly(2)", train_error = error.train2, test_error = error.test2)
res3 = data.table(station = "dinar",  model = "poly(2)", train_error = error.train3, test_error = error.test3)
res4 = data.table(station = "geycek", model = "poly(2)", train_error = error.train4, test_error = error.test4)
res5 = data.table(station = "soke",   model = "poly(2)", train_error = error.train5, test_error = error.test5)
res6 = data.table(station = "soma",   model = "poly(2)", train_error = error.train6, test_error = error.test6)

dresults = rbind(dresults, res1)
dresults = rbind(dresults, res2)
dresults = rbind(dresults, res3)
dresults = rbind(dresults, res4)
dresults = rbind(dresults, res5)
dresults = rbind(dresults, res6)

dresults[order(station)]
dresults %>% filter(model == "poly(2)")
# dresults = dresults %>% filter(model != "poly(2)")



# Poly then Lag -----------------------------------------------------------

# *create Poly->Lag data --------------------------------------------------
dt_polylag1 = createLagged(data = dt_poly1, lags = c(-1:1), other.head = c(1:3), other.tail = 0)
dt_polylag2 = createLagged(data = dt_poly2, lags = c(-1:1), other.head = c(1:3), other.tail = 0)
dt_polylag3 = createLagged(data = dt_poly3, lags = c(-1:1), other.head = c(1:3), other.tail = 0)
dt_polylag4 = createLagged(data = dt_poly4, lags = c(-1:1), other.head = c(1:3), other.tail = 0)
dt_polylag5 = createLagged(data = dt_poly5, lags = c(-1:1), other.head = c(1:3), other.tail = 0)
dt_polylag6 = createLagged(data = dt_poly6, lags = c(-1:1), other.head = c(1:3), other.tail = 0)

dte_polylag1 = createLagged(data = dte_poly1, lags = c(-1:1), other.head = c(1:3), other.tail = 0)
dte_polylag2 = createLagged(data = dte_poly2, lags = c(-1:1), other.head = c(1:3), other.tail = 0)
dte_polylag3 = createLagged(data = dte_poly3, lags = c(-1:1), other.head = c(1:3), other.tail = 0)
dte_polylag4 = createLagged(data = dte_poly4, lags = c(-1:1), other.head = c(1:3), other.tail = 0)
dte_polylag5 = createLagged(data = dte_poly5, lags = c(-1:1), other.head = c(1:3), other.tail = 0)
dte_polylag6 = createLagged(data = dte_poly6, lags = c(-1:1), other.head = c(1:3), other.tail = 0)


# *glmnet -----------------------------------------------------------------
fit.polylag1 = cv.glmnet(x = as.matrix(dt_polylag1[,-c(1:3)]), y = dt_polylag1$production, type.measure = 'mae')
fit.polylag2 = cv.glmnet(x = as.matrix(dt_polylag2[,-c(1:3)]), y = dt_polylag2$production, type.measure = 'mae')
fit.polylag3 = cv.glmnet(x = as.matrix(dt_polylag3[,-c(1:3)]), y = dt_polylag3$production, type.measure = 'mae')
fit.polylag4 = cv.glmnet(x = as.matrix(dt_polylag4[,-c(1:3)]), y = dt_polylag4$production, type.measure = 'mae')
fit.polylag5 = cv.glmnet(x = as.matrix(dt_polylag5[,-c(1:3)]), y = dt_polylag5$production, type.measure = 'mae')
fit.polylag6 = cv.glmnet(x = as.matrix(dt_polylag6[,-c(1:3)]), y = dt_polylag6$production, type.measure = 'mae')

error.train1 = min(fit.polylag1$cvup)
error.train2 = min(fit.polylag2$cvup)
error.train3 = min(fit.polylag3$cvup)
error.train4 = min(fit.polylag4$cvup)
error.train5 = min(fit.polylag5$cvup)
error.train6 = min(fit.polylag6$cvup)

pred.test1 = predict(fit.polylag1, as.matrix(dte_polylag1[,-c(1:3)]), s = "lambda.1se")
pred.test2 = predict(fit.polylag2, as.matrix(dte_polylag2[,-c(1:3)]), s = "lambda.1se")
pred.test3 = predict(fit.polylag3, as.matrix(dte_polylag3[,-c(1:3)]), s = "lambda.1se")
pred.test4 = predict(fit.polylag4, as.matrix(dte_polylag4[,-c(1:3)]), s = "lambda.1se")
pred.test5 = predict(fit.polylag5, as.matrix(dte_polylag5[,-c(1:3)]), s = "lambda.1se")
pred.test6 = predict(fit.polylag6, as.matrix(dte_polylag6[,-c(1:3)]), s = "lambda.1se")

error.test1 = mean(abs(pred.test1 - dte_polylag1$production))
error.test2 = mean(abs(pred.test2 - dte_polylag2$production))
error.test3 = mean(abs(pred.test3 - dte_polylag3$production))
error.test4 = mean(abs(pred.test4 - dte_polylag4$production))
error.test5 = mean(abs(pred.test5 - dte_polylag5$production))
error.test6 = mean(abs(pred.test6 - dte_polylag6$production))

res1 = data.table(station = "aliaga", model = "poly(2) > lag", train_error = error.train1, test_error = error.test1)
res2 = data.table(station = "bares",  model = "poly(2) > lag", train_error = error.train2, test_error = error.test2)
res3 = data.table(station = "dinar",  model = "poly(2) > lag", train_error = error.train3, test_error = error.test3)
res4 = data.table(station = "geycek", model = "poly(2) > lag", train_error = error.train4, test_error = error.test4)
res5 = data.table(station = "soke",   model = "poly(2) > lag", train_error = error.train5, test_error = error.test5)
res6 = data.table(station = "soma",   model = "poly(2) > lag", train_error = error.train6, test_error = error.test6)

dresults = rbind(dresults, res1)
dresults = rbind(dresults, res2)
dresults = rbind(dresults, res3)
dresults = rbind(dresults, res4)
dresults = rbind(dresults, res5)
dresults = rbind(dresults, res6)

dresults[order(station)]



# Lag then Poly -----------------------------------------------------------

# *create Lag->Poly data --------------------------------------------------
dt_lagpoly1 = cbind(dt_lag1[,1:3], polym(as.matrix(dt_lag1[,-c(1:3)]),degree = 2, raw = TRUE))
# dt_lagpoly2 = cbind(dt_lag2[,1:3], polym(as.matrix(dt_lag2[,-c(1:3)]),degree = 2, raw = TRUE))
# dt_lagpoly3 = cbind(dt_lag3[,1:3], polym(as.matrix(dt_lag3[,-c(1:3)]),degree = 2, raw = TRUE))
# dt_lagpoly4 = cbind(dt_lag4[,1:3], polym(as.matrix(dt_lag4[,-c(1:3)]),degree = 2, raw = TRUE))
# dt_lagpoly5 = cbind(dt_lag5[,1:3], polym(as.matrix(dt_lag5[,-c(1:3)]),degree = 2, raw = TRUE))
# dt_lagpoly6 = cbind(dt_lag6[,1:3], polym(as.matrix(dt_lag6[,-c(1:3)]),degree = 2, raw = TRUE))

dte_lagpoly1 = cbind(dte_lag1[,1:3], polym(as.matrix(dte_lag1[,-c(1:3)]),degree = 2, raw = TRUE))
# dte_lagpoly2 = cbind(dte_lag2[,1:3], polym(as.matrix(dte_lag2[,-c(1:3)]),degree = 2, raw = TRUE))
# dte_lagpoly3 = cbind(dte_lag3[,1:3], polym(as.matrix(dte_lag3[,-c(1:3)]),degree = 2, raw = TRUE))
# dte_lagpoly4 = cbind(dte_lag4[,1:3], polym(as.matrix(dte_lag4[,-c(1:3)]),degree = 2, raw = TRUE))
# dte_lagpoly5 = cbind(dte_lag5[,1:3], polym(as.matrix(dte_lag5[,-c(1:3)]),degree = 2, raw = TRUE))
# dte_lagpoly6 = cbind(dte_lag6[,1:3], polym(as.matrix(dte_lag6[,-c(1:3)]),degree = 2, raw = TRUE))


fit.lagpoly1 = cv.glmnet(x = as.matrix(dt_lagpoly1[,-c(1:3)]), y = dt_lagpoly1$production, type.measure = 'mae')
error.train1 = min(fit.lagpoly1$cvup)
pred.test1 = predict(fit.lagpoly1, as.matrix(dte_lagpoly1[,-c(1:3)]), s = "lambda.1se")
error.test1 = mean(abs(pred.test1 - dte_lagpoly1$production))
res1 = data.table(station = "aliaga", model = "poly>lag(2)", train_error = error.train1, test_error = error.test1)
res1
dresults %>% filter(station == "aliaga")


# Poly of degree 3 --------------------------------------------------------

# *create poly data --------------------------------------------------------
# polynomial of order 3
dt_poly3.1 = cbind(dt1[,1:3], polym(as.matrix(dt1[,-c(1:3)]), degree = 3, raw = TRUE))
dt_poly3.2 = cbind(dt2[,1:3], polym(as.matrix(dt2[,-c(1:3)]), degree = 3, raw = TRUE))
dt_poly3.3 = cbind(dt3[,1:3], polym(as.matrix(dt3[,-c(1:3)]), degree = 3, raw = TRUE))
dt_poly3.4 = cbind(dt4[,1:3], polym(as.matrix(dt4[,-c(1:3)]), degree = 3, raw = TRUE))
dt_poly3.5 = cbind(dt5[,1:3], polym(as.matrix(dt5[,-c(1:3)]), degree = 3, raw = TRUE))
dt_poly3.6 = cbind(dt6[,1:3], polym(as.matrix(dt6[,-c(1:3)]), degree = 3, raw = TRUE))

dte_poly3.1 = cbind(dte1[,1:3], polym(as.matrix(dte1[,-c(1:3)]), degree = 3, raw = TRUE))
dte_poly3.2 = cbind(dte2[,1:3], polym(as.matrix(dte2[,-c(1:3)]), degree = 3, raw = TRUE))
dte_poly3.3 = cbind(dte3[,1:3], polym(as.matrix(dte3[,-c(1:3)]), degree = 3, raw = TRUE))
dte_poly3.4 = cbind(dte4[,1:3], polym(as.matrix(dte4[,-c(1:3)]), degree = 3, raw = TRUE))
dte_poly3.5 = cbind(dte5[,1:3], polym(as.matrix(dte5[,-c(1:3)]), degree = 3, raw = TRUE))
dte_poly3.6 = cbind(dte6[,1:3], polym(as.matrix(dte6[,-c(1:3)]), degree = 3, raw = TRUE))

# *glmnet ------------------------------------------------------------------
fit.poly3.1 = cv.glmnet(x = as.matrix(dt_poly3.1[,-c(1:3)]), y = dt_poly3.1$production, type.measure = 'mae')
fit.poly3.2 = cv.glmnet(x = as.matrix(dt_poly3.2[,-c(1:3)]), y = dt_poly3.2$production, type.measure = 'mae')
fit.poly3.3 = cv.glmnet(x = as.matrix(dt_poly3.3[,-c(1:3)]), y = dt_poly3.3$production, type.measure = 'mae')
fit.poly3.4 = cv.glmnet(x = as.matrix(dt_poly3.4[,-c(1:3)]), y = dt_poly3.4$production, type.measure = 'mae')
fit.poly3.5 = cv.glmnet(x = as.matrix(dt_poly3.5[,-c(1:3)]), y = dt_poly3.5$production, type.measure = 'mae')
fit.poly3.6 = cv.glmnet(x = as.matrix(dt_poly3.6[,-c(1:3)]), y = dt_poly3.6$production, type.measure = 'mae')

error.train1 = min(fit.poly3.1$cvup)
error.train2 = min(fit.poly3.2$cvup)
error.train3 = min(fit.poly3.3$cvup)
error.train4 = min(fit.poly3.4$cvup)
error.train5 = min(fit.poly3.5$cvup)
error.train6 = min(fit.poly3.6$cvup)

pred.test1 = predict(fit.poly3.1, as.matrix(dte_poly3.1[,-c(1:3)]), s = "lambda.1se")
pred.test2 = predict(fit.poly3.2, as.matrix(dte_poly3.2[,-c(1:3)]), s = "lambda.1se")
pred.test3 = predict(fit.poly3.3, as.matrix(dte_poly3.3[,-c(1:3)]), s = "lambda.1se")
pred.test4 = predict(fit.poly3.4, as.matrix(dte_poly3.4[,-c(1:3)]), s = "lambda.1se")
pred.test5 = predict(fit.poly3.5, as.matrix(dte_poly3.5[,-c(1:3)]), s = "lambda.1se")
pred.test6 = predict(fit.poly3.6, as.matrix(dte_poly3.6[,-c(1:3)]), s = "lambda.1se")

error.test1 = mean(abs(pred.test1 - dte1$production))
error.test2 = mean(abs(pred.test2 - dte2$production))
error.test3 = mean(abs(pred.test3 - dte3$production))
error.test4 = mean(abs(pred.test4 - dte4$production))
error.test5 = mean(abs(pred.test5 - dte5$production))
error.test6 = mean(abs(pred.test6 - dte6$production))

res1 = data.table(station = "aliaga", model = "poly(3)", train_error = error.train1, test_error = error.test1)
res2 = data.table(station = "bares",  model = "poly(3)", train_error = error.train2, test_error = error.test2)
res3 = data.table(station = "dinar",  model = "poly(3)", train_error = error.train3, test_error = error.test3)
res4 = data.table(station = "geycek", model = "poly(3)", train_error = error.train4, test_error = error.test4)
res5 = data.table(station = "soke",   model = "poly(3)", train_error = error.train5, test_error = error.test5)
res6 = data.table(station = "soma",   model = "poly(3)", train_error = error.train6, test_error = error.test6)

dresults = rbind(dresults, res1)
dresults = rbind(dresults, res2)
dresults = rbind(dresults, res3)
dresults = rbind(dresults, res4)
dresults = rbind(dresults, res5)
dresults = rbind(dresults, res6)

# dresults[order(station)]
# dresults %>% filter(model == "poly(3)")
# # dresults = dresults %>% filter(model != "poly(2)")





