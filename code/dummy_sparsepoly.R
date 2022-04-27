# dummy sparse polynomials


# Results Table -----------------------------------------------------------
dresults2 = dresults

dresults2[order(station)]




# create data -------------------------------------------------------------
dt_sparsepoly1 = poly3Make(data = data.table(dt1[,-c(1:3)]), nlat = 2, nlon = 2)
dt_sparsepoly2 = poly3Make(data = data.table(dt2[,-c(1:3)]), nlat = 3, nlon = 3)
dt_sparsepoly3 = poly3Make(data = data.table(dt3[,-c(1:3)]), nlat = 3, nlon = 3)
dt_sparsepoly4 = poly3Make(data = data.table(dt4[,-c(1:3)]), nlat = 3, nlon = 3)
dt_sparsepoly5 = poly3Make(data = data.table(dt5[,-c(1:3)]), nlat = 3, nlon = 3)
dt_sparsepoly6 = poly3Make(data = data.table(dt6[,-c(1:3)]), nlat = 3, nlon = 3)
#
dte_sparsepoly1 = poly3Make(data = data.table(dte1[,-c(1:3)]), nlat = 2, nlon = 2)
dte_sparsepoly2 = poly3Make(data = data.table(dte2[,-c(1:3)]), nlat = 3, nlon = 3)
dte_sparsepoly3 = poly3Make(data = data.table(dte3[,-c(1:3)]), nlat = 3, nlon = 3)
dte_sparsepoly4 = poly3Make(data = data.table(dte4[,-c(1:3)]), nlat = 3, nlon = 3)
dte_sparsepoly5 = poly3Make(data = data.table(dte5[,-c(1:3)]), nlat = 3, nlon = 3)
dte_sparsepoly6 = poly3Make(data = data.table(dte6[,-c(1:3)]), nlat = 3, nlon = 3)
#
# glmnet ------------------------------------------------------------------
dfit.sparsepoly1 = cv.glmnet(x = as.matrix(dt_sparsepoly1), y = dt1$production, type.measure = "mae")
dfit.sparsepoly2 = cv.glmnet(x = as.matrix(dt_sparsepoly2), y = dt2$production, type.measure = "mae")
dfit.sparsepoly3 = cv.glmnet(x = as.matrix(dt_sparsepoly3), y = dt3$production, type.measure = "mae")
dfit.sparsepoly4 = cv.glmnet(x = as.matrix(dt_sparsepoly4), y = dt4$production, type.measure = "mae")
dfit.sparsepoly5 = cv.glmnet(x = as.matrix(dt_sparsepoly5), y = dt5$production, type.measure = "mae")
dfit.sparsepoly6 = cv.glmnet(x = as.matrix(dt_sparsepoly6), y = dt6$production, type.measure = "mae")

# look at coeffs:
predict(dfit.sparsepoly1,type="coef")
which(predict(dfit.sparsepoly2,type="coef")>0) # bunlar kiyaslanabilir. ama belli bir pattern yok gibi
which(predict(dfit.sparsepoly3,type="coef")>0) # bunlar kiyaslanabilir. ama belli bir pattern yok gibi
which(predict(dfit.sparsepoly4,type="coef")>0) # bunlar kiyaslanabilir. ama belli bir pattern yok gibi
which(predict(dfit.sparsepoly5,type="coef")>0) # bunlar kiyaslanabilir. ama belli bir pattern yok gibi
which(predict(dfit.sparsepoly6,type="coef")>0) # bunlar kiyaslanabilir. ama belli bir pattern yok gibi

error.train1 = min(dfit.sparsepoly1$cvup)
error.train2 = min(dfit.sparsepoly2$cvup)
error.train3 = min(dfit.sparsepoly3$cvup)
error.train4 = min(dfit.sparsepoly4$cvup)
error.train5 = min(dfit.sparsepoly5$cvup)
error.train6 = min(dfit.sparsepoly6$cvup)

pred.test1 = predict(dfit.sparsepoly1, as.matrix(dte_sparsepoly1), s = "lambda.1se")
pred.test2 = predict(dfit.sparsepoly2, as.matrix(dte_sparsepoly2), s = "lambda.1se")
pred.test3 = predict(dfit.sparsepoly3, as.matrix(dte_sparsepoly3), s = "lambda.1se")
pred.test4 = predict(dfit.sparsepoly4, as.matrix(dte_sparsepoly4), s = "lambda.1se")
pred.test5 = predict(dfit.sparsepoly5, as.matrix(dte_sparsepoly5), s = "lambda.1se")
pred.test6 = predict(dfit.sparsepoly6, as.matrix(dte_sparsepoly6), s = "lambda.1se")

error.test1 = mean(abs(pred.test1 - dte1$production))
error.test2 = mean(abs(pred.test2 - dte2$production))
error.test3 = mean(abs(pred.test3 - dte3$production))
error.test4 = mean(abs(pred.test4 - dte4$production))
error.test5 = mean(abs(pred.test5 - dte5$production))
error.test6 = mean(abs(pred.test6 - dte6$production))

res1 = data.table(station = "aliaga", model = "sparse poly", train_error = error.train1, test_error = error.test1)
res2 = data.table(station = "bares",  model = "sparse poly", train_error = error.train2, test_error = error.test2)
res3 = data.table(station = "dinar",  model = "sparse poly", train_error = error.train3, test_error = error.test3)
res4 = data.table(station = "geycek", model = "sparse poly", train_error = error.train4, test_error = error.test4)
res5 = data.table(station = "soke",   model = "sparse poly", train_error = error.train5, test_error = error.test5)
res6 = data.table(station = "soma",   model = "sparse poly", train_error = error.train6, test_error = error.test6)

dresults2 = rbind(dresults2, res1)
dresults2 = rbind(dresults2, res2)
dresults2 = rbind(dresults2, res3)
dresults2 = rbind(dresults2, res4)
dresults2 = rbind(dresults2, res5)
dresults2 = rbind(dresults2, res6)

dresults2[order(station)] %>% filter(model == "poly(3)" | model == "sparse poly")


dresults2[order(station)]
# dimension comparison ----------------------------------------------------

# before that note that our sparse poly function is not perfect. it creates some duplicates.
# let's check them
repeatChecker = function(x) {
    a = ncol(x)
    counter = 0
    for(i in c(1:(a-1))){
        for(j in c((i+1):a)){
            if( sum(head(x[,..i],20) - head(x[,..j],20)) == 0 ) {
                print(paste(i, j, sep = " "))
                counter = counter  + 1
            }
        }
    }
    return(counter)
}
repeatChecker(dt_sparsepoly1) # 4 columns are repeated
repeatChecker(dt_sparsepoly2) # 13 columns are repeated

dim(dt_poly3.1) - 3 # 34
dim(dt_sparsepoly1) - 4 # 32
dim(dt_poly3.2) -3 # 219
dim(dt_sparsepoly2) - 13 # 99
# same as 2 for others



# sparse -> lagged --------------------------------------------------------
dt_sparsepolylag1 = createLagged(data = dt_sparsepoly1, lags = c(-1:1), other.head = 0, other.tail = 0)
dt_sparsepolylag2 = createLagged(data = dt_sparsepoly2, lags = c(-1:1), other.head = 0, other.tail = 0)
dt_sparsepolylag3 = createLagged(data = dt_sparsepoly3, lags = c(-1:1), other.head = 0, other.tail = 0)
dt_sparsepolylag4 = createLagged(data = dt_sparsepoly4, lags = c(-1:1), other.head = 0, other.tail = 0)
dt_sparsepolylag5 = createLagged(data = dt_sparsepoly5, lags = c(-1:1), other.head = 0, other.tail = 0)
dt_sparsepolylag6 = createLagged(data = dt_sparsepoly6, lags = c(-1:1), other.head = 0, other.tail = 0)
#
dte_sparsepolylag1 = createLagged(data = dte_sparsepoly1, lags = c(-1:1), other.head = 0, other.tail = 0)
dte_sparsepolylag2 = createLagged(data = dte_sparsepoly2, lags = c(-1:1), other.head = 0, other.tail = 0)
dte_sparsepolylag3 = createLagged(data = dte_sparsepoly3, lags = c(-1:1), other.head = 0, other.tail = 0)
dte_sparsepolylag4 = createLagged(data = dte_sparsepoly4, lags = c(-1:1), other.head = 0, other.tail = 0)
dte_sparsepolylag5 = createLagged(data = dte_sparsepoly5, lags = c(-1:1), other.head = 0, other.tail = 0)
dte_sparsepolylag6 = createLagged(data = dte_sparsepoly6, lags = c(-1:1), other.head = 0, other.tail = 0)
# glmnet
dfit.sparsepolylag1 = cv.glmnet(x = as.matrix(dt_sparsepolylag1), y = dt_lag1$production, type.measure = "mae")
dfit.sparsepolylag2 = cv.glmnet(x = as.matrix(dt_sparsepolylag2), y = dt_lag2$production, type.measure = "mae")
dfit.sparsepolylag3 = cv.glmnet(x = as.matrix(dt_sparsepolylag3), y = dt_lag3$production, type.measure = "mae")
dfit.sparsepolylag4 = cv.glmnet(x = as.matrix(dt_sparsepolylag4), y = dt_lag4$production, type.measure = "mae")
dfit.sparsepolylag5 = cv.glmnet(x = as.matrix(dt_sparsepolylag5), y = dt_lag5$production, type.measure = "mae")
dfit.sparsepolylag6 = cv.glmnet(x = as.matrix(dt_sparsepolylag6), y = dt_lag6$production, type.measure = "mae")

# look at coeffs:
# predict(dfit.sparsepoly1,type="coef")
# which(predict(dfit.sparsepoly2,type="coef")>0) # bunlar kiyaslanabilir. ama belli bir pattern yok gibi
# which(predict(dfit.sparsepoly3,type="coef")>0) # bunlar kiyaslanabilir. ama belli bir pattern yok gibi
# which(predict(dfit.sparsepoly4,type="coef")>0) # bunlar kiyaslanabilir. ama belli bir pattern yok gibi
# which(predict(dfit.sparsepoly5,type="coef")>0) # bunlar kiyaslanabilir. ama belli bir pattern yok gibi
# which(predict(dfit.sparsepoly6,type="coef")>0) # bunlar kiyaslanabilir. ama belli bir pattern yok gibi# which(predict(dfit.sparsepoly6,type="coef")>0) # bunlar kiyaslanabilir. ama belli bir pattern yok gibi

error.train1 = min(dfit.sparsepolylag1$cvup)
error.train2 = min(dfit.sparsepolylag2$cvup)
error.train3 = min(dfit.sparsepolylag3$cvup)
error.train4 = min(dfit.sparsepolylag4$cvup)
error.train5 = min(dfit.sparsepolylag5$cvup)
error.train6 = min(dfit.sparsepolylag6$cvup)

pred.test1 = predict(dfit.sparsepolylag1, as.matrix(dte_sparsepolylag1), s = "lambda.1se")
pred.test2 = predict(dfit.sparsepolylag2, as.matrix(dte_sparsepolylag2), s = "lambda.1se")
pred.test3 = predict(dfit.sparsepolylag3, as.matrix(dte_sparsepolylag3), s = "lambda.1se")
pred.test4 = predict(dfit.sparsepolylag4, as.matrix(dte_sparsepolylag4), s = "lambda.1se")
pred.test5 = predict(dfit.sparsepolylag5, as.matrix(dte_sparsepolylag5), s = "lambda.1se")
pred.test6 = predict(dfit.sparsepolylag6, as.matrix(dte_sparsepolylag6), s = "lambda.1se")

error.test1 = mean(abs(pred.test1 - dte_lag1$production))
error.test2 = mean(abs(pred.test2 - dte_lag2$production))
error.test3 = mean(abs(pred.test3 - dte_lag3$production))
error.test4 = mean(abs(pred.test4 - dte_lag4$production))
error.test5 = mean(abs(pred.test5 - dte_lag5$production))
error.test6 = mean(abs(pred.test6 - dte_lag6$production))

res1 = data.table(station = "aliaga", model = "sparse poly>lag", train_error = error.train1, test_error = error.test1)
res2 = data.table(station = "bares",  model = "sparse poly>lag", train_error = error.train2, test_error = error.test2)
res3 = data.table(station = "dinar",  model = "sparse poly>lag", train_error = error.train3, test_error = error.test3)
res4 = data.table(station = "geycek", model = "sparse poly>lag", train_error = error.train4, test_error = error.test4)
res5 = data.table(station = "soke",   model = "sparse poly>lag", train_error = error.train5, test_error = error.test5)
res6 = data.table(station = "soma",   model = "sparse poly>lag", train_error = error.train6, test_error = error.test6)

dresults2 = rbind(dresults2, res1)
dresults2 = rbind(dresults2, res2)
dresults2 = rbind(dresults2, res3)
dresults2 = rbind(dresults2, res4)
dresults2 = rbind(dresults2, res5)
dresults2 = rbind(dresults2, res6)

dresults2[order(station)]


plot(ts(dte_lag1$production))
lines(ts(pred.test1), col = 'red')
#
plot(ts(dte_lag2$production))
lines(ts(pred.test2), col = 'red')
#
plot(ts(dte_lag3$production))
lines(ts(pred.test3), col = 'red')
#
plot(ts(dte_lag4$production))
lines(ts(pred.test4), col = 'red')
#
plot(ts(dte_lag5$production))
lines(ts(pred.test5), col = 'red')
#
plot(ts(dte_lag6$production))
lines(ts(pred.test6), col = 'red')






# x -----------------------------------------------------------------------
dlevel_order <- factor(dresults2$model, level = unique(dresults2$model))[37:60]
plotx = ggplot(dresults2[37:60,]) +
    geom_point(mapping = aes(x = dlevel_order, y = train_error, color = station)) +
    labs(x = "Models", y = "Mean Absolute Error", title = "Final Results", subtitle = "Error Comparison")
print(plotx)
#
dlevel_order <- factor(dresults2$model, level = unique(dresults2$model))[43:54]
plotx = ggplot(dresults2[43:54,]) +
    geom_point(mapping = aes(x = dlevel_order, y = train_error, color = station)) +
    labs(x = "Models", y = "Mean Absolute Error", title = "Final Results", subtitle = "Error Comparison")
print(plotx)
#
dresults2[43:54,][order(station)]


