# dummy pca


# pca ---------------------------------------------------------------------
pcapoly1 = prcomp(x = as.matrix(dt_poly3.1[,-c(1:3)]))
pcapoly2 = prcomp(x = as.matrix(dt_poly3.2[,-c(1:3)]))
pcapoly3 = prcomp(x = as.matrix(dt_poly3.3[,-c(1:3)]))
pcapoly4 = prcomp(x = as.matrix(dt_poly3.4[,-c(1:3)]))
pcapoly5 = prcomp(x = as.matrix(dt_poly3.5[,-c(1:3)]))
pcapoly6 = prcomp(x = as.matrix(dt_poly3.6[,-c(1:3)]))
#
dt_pcapoly1 = pcapoly1$x
dt_pcapoly2 = pcapoly2$x
dt_pcapoly3 = pcapoly3$x
dt_pcapoly4 = pcapoly4$x
dt_pcapoly5 = pcapoly5$x
dt_pcapoly6 = pcapoly6$x
#
dte_pcapoly1 = predict(pcapoly1, dte_poly3.1)
dte_pcapoly2 = predict(pcapoly2, dte_poly3.2)
dte_pcapoly3 = predict(pcapoly3, dte_poly3.3)
dte_pcapoly4 = predict(pcapoly4, dte_poly3.4)
dte_pcapoly5 = predict(pcapoly5, dte_poly3.5)
dte_pcapoly6 = predict(pcapoly6, dte_poly3.6)
#
npc1 = pcPicker(eigenvalues = pcapoly1$sdev, var_wanted = 0.999)
npc2 = pcPicker(eigenvalues = pcapoly2$sdev, var_wanted = 0.999)
npc3 = pcPicker(eigenvalues = pcapoly3$sdev, var_wanted = 0.999)
npc4 = pcPicker(eigenvalues = pcapoly4$sdev, var_wanted = 0.999)
npc5 = pcPicker(eigenvalues = pcapoly5$sdev, var_wanted = 0.999)
npc6 = pcPicker(eigenvalues = pcapoly6$sdev, var_wanted = 0.999)
#
# glmnet ------------------------------------------------------------------
dfit.pcapoly1 = cv.glmnet(x = as.matrix(dt_pcapoly1[,1:npc1]), y = dt1$production, type.measure = "mae")
dfit.pcapoly2 = cv.glmnet(x = as.matrix(dt_pcapoly2[,1:npc2]), y = dt2$production, type.measure = "mae")
dfit.pcapoly3 = cv.glmnet(x = as.matrix(dt_pcapoly3[,1:npc3]), y = dt3$production, type.measure = "mae")
dfit.pcapoly4 = cv.glmnet(x = as.matrix(dt_pcapoly4[,1:npc4]), y = dt4$production, type.measure = "mae")
dfit.pcapoly5 = cv.glmnet(x = as.matrix(dt_pcapoly5[,1:npc5]), y = dt5$production, type.measure = "mae")
dfit.pcapoly6 = cv.glmnet(x = as.matrix(dt_pcapoly6[,1:npc6]), y = dt6$production, type.measure = "mae")

# look at coeffs:
predict(dfit.pcapoly1,type="coef")
which(predict(dfit.pcapoly2,type="coef")>0) # bunlar kiyaslanabilir. ama belli bir pattern yok gibi
which(predict(dfit.pcapoly3,type="coef")>0) # bunlar kiyaslanabilir. ama belli bir pattern yok gibi
which(predict(dfit.pcapoly4,type="coef")>0) # bunlar kiyaslanabilir. ama belli bir pattern yok gibi
which(predict(dfit.pcapoly5,type="coef")>0) # bunlar kiyaslanabilir. ama belli bir pattern yok gibi
which(predict(dfit.pcapoly6,type="coef")>0) # bunlar kiyaslanabilir. ama belli bir pattern yok gibi

error.train1 = min(dfit.pcapoly1$cvup)
error.train2 = min(dfit.pcapoly2$cvup)
error.train3 = min(dfit.pcapoly3$cvup)
error.train4 = min(dfit.pcapoly4$cvup)
error.train5 = min(dfit.pcapoly5$cvup)
error.train6 = min(dfit.pcapoly6$cvup)

pred.test1 = predict(dfit.pcapoly1, as.matrix(dte_pcapoly1[,1:npc1]), s = "lambda.1se")
pred.test2 = predict(dfit.pcapoly2, as.matrix(dte_pcapoly2[,1:npc2]), s = "lambda.1se")
pred.test3 = predict(dfit.pcapoly3, as.matrix(dte_pcapoly3[,1:npc3]), s = "lambda.1se")
pred.test4 = predict(dfit.pcapoly4, as.matrix(dte_pcapoly4[,1:npc4]), s = "lambda.1se")
pred.test5 = predict(dfit.pcapoly5, as.matrix(dte_pcapoly5[,1:npc5]), s = "lambda.1se")
pred.test6 = predict(dfit.pcapoly6, as.matrix(dte_pcapoly6[,1:npc6]), s = "lambda.1se")

error.test1 = mean(abs(pred.test1 - dte1$production))
error.test2 = mean(abs(pred.test2 - dte2$production))
error.test3 = mean(abs(pred.test3 - dte3$production))
error.test4 = mean(abs(pred.test4 - dte4$production))
error.test5 = mean(abs(pred.test5 - dte5$production))
error.test6 = mean(abs(pred.test6 - dte6$production))

res1 = data.table(station = "aliaga", model = "pca poly(3)", train_error = error.train1, test_error = error.test1)
res2 = data.table(station = "bares",  model = "pca poly(3)", train_error = error.train2, test_error = error.test2)
res3 = data.table(station = "dinar",  model = "pca poly(3)", train_error = error.train3, test_error = error.test3)
res4 = data.table(station = "geycek", model = "pca poly(3)", train_error = error.train4, test_error = error.test4)
res5 = data.table(station = "soke",   model = "pca poly(3)", train_error = error.train5, test_error = error.test5)
res6 = data.table(station = "soma",   model = "pca poly(3)", train_error = error.train6, test_error = error.test6)

dresults2 = rbind(dresults2, res1)
dresults2 = rbind(dresults2, res2)
dresults2 = rbind(dresults2, res3)
dresults2 = rbind(dresults2, res4)
dresults2 = rbind(dresults2, res5)
dresults2 = rbind(dresults2, res6)

dresults2[order(station)]

# dresults2 = dresults2[-c(49:54),]

dresults2[43:54,][order(station)]

# %99.9 Variance explained
npc1 # 17 vs 32
npc2 # 84 vs 99
npc3 # 91 vs 99
npc4 # 108 vs 99
npc5 # 84 vs 99
npc6 # 122 vs 99




results[order(station)]




