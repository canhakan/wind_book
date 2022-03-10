# Kronecker PCA

# Results Table -----------------------------------------------------------
results.kron = data.table(data = character(),
                          model = character(),
                          train_error = numeric(),
                          test_error = numeric())

results.kron[order(data)]

colnames(results.kron)[1] = "station"

# Lagged ------------------------------------------------------------------
spatial.cov1 = cor(x = train1[,4:12], y = train1[,4:12])
spatial.cov2 = cor(x = train2[,4:19], y = train2[,4:19])
spatial.cov3 = cor(x = train3[,4:19], y = train3[,4:19])
spatial.cov4 = cor(x = train4[,4:19], y = train4[,4:19])
spatial.cov5 = cor(x = train5[,4:19], y = train5[,4:19])
spatial.cov6 = cor(x = train6[,4:15], y = train6[,4:15])

temporal.cov1 = cor(temporal1, temporal1)
temporal.cov2 = cor(temporal2, temporal2)
temporal.cov3 = cor(temporal3, temporal3)
temporal.cov4 = cor(temporal4, temporal4)
temporal.cov5 = cor(temporal5, temporal5)
temporal.cov6 = cor(temporal6, temporal6)

spatial.eVal1 = eigen(spatial.cov1)$values
spatial.eVal2 = eigen(spatial.cov2)$values
spatial.eVal3 = eigen(spatial.cov3)$values
spatial.eVal4 = eigen(spatial.cov4)$values
spatial.eVal5 = eigen(spatial.cov5)$values
spatial.eVal6 = eigen(spatial.cov6)$values

spatial.eVec1 = eigen(spatial.cov1)$vectors
spatial.eVec2 = eigen(spatial.cov2)$vectors
spatial.eVec3 = eigen(spatial.cov3)$vectors
spatial.eVec4 = eigen(spatial.cov4)$vectors
spatial.eVec5 = eigen(spatial.cov5)$vectors
spatial.eVec6 = eigen(spatial.cov6)$vectors

temporal.eVal1 = eigen(temporal.cov1)$values
temporal.eVal2 = eigen(temporal.cov2)$values
temporal.eVal3 = eigen(temporal.cov3)$values
temporal.eVal4 = eigen(temporal.cov4)$values
temporal.eVal5 = eigen(temporal.cov5)$values
temporal.eVal6 = eigen(temporal.cov6)$values

temporal.eVec1 = eigen(temporal.cov1)$vectors
temporal.eVec2 = eigen(temporal.cov2)$vectors
temporal.eVec3 = eigen(temporal.cov3)$vectors
temporal.eVec4 = eigen(temporal.cov4)$vectors
temporal.eVec5 = eigen(temporal.cov5)$vectors
temporal.eVec6 = eigen(temporal.cov6)$vectors

kron.eVal1 = kronecker(temporal.eVal1, spatial.eVal1)
kron.eVal2 = kronecker(temporal.eVal2, spatial.eVal2)
kron.eVal3 = kronecker(temporal.eVal3, spatial.eVal3)
kron.eVal4 = kronecker(temporal.eVal4, spatial.eVal4)
kron.eVal5 = kronecker(temporal.eVal5, spatial.eVal5)
kron.eVal6 = kronecker(temporal.eVal6, spatial.eVal6)

npc1 = pcLister(eigenvalues = kron.eVal1, var_wanted = 0.99)
npc2 = pcLister(eigenvalues = kron.eVal2, var_wanted = 0.99)
npc3 = pcLister(eigenvalues = kron.eVal3, var_wanted = 0.99)
npc4 = pcLister(eigenvalues = kron.eVal4, var_wanted = 0.99)
npc5 = pcLister(eigenvalues = kron.eVal5, var_wanted = 0.99)
npc6 = pcLister(eigenvalues = kron.eVal6, var_wanted = 0.99)

kron.eVec1 = kronecker(temporal.eVec1, spatial.eVec1)[,npc1]
kron.eVec2 = kronecker(temporal.eVec2, spatial.eVec2)[,npc2]
kron.eVec3 = kronecker(temporal.eVec3, spatial.eVec3)[,npc3]
kron.eVec4 = kronecker(temporal.eVec4, spatial.eVec4)[,npc4]
kron.eVec5 = kronecker(temporal.eVec5, spatial.eVec5)[,npc5]
kron.eVec6 = kronecker(temporal.eVec6, spatial.eVec6)[,npc6]

kronlag1 = as.matrix(lagged1[ ,4:66])  %*% kron.eVec1
kronlag2 = as.matrix(lagged2[ ,4:115]) %*% kron.eVec2
kronlag3 = as.matrix(lagged3[ ,4:115]) %*% kron.eVec3
kronlag4 = as.matrix(lagged4[ ,4:115]) %*% kron.eVec4
kronlag5 = as.matrix(lagged5[ ,4:115]) %*% kron.eVec5
kronlag6 = as.matrix(lagged6[ ,4:87])  %*% kron.eVec6

kronlag.test1 = as.matrix(lagged.test1[,4:66])  %*% kron.eVec1
kronlag.test2 = as.matrix(lagged.test2[,4:115]) %*% kron.eVec2
kronlag.test3 = as.matrix(lagged.test3[,4:115]) %*% kron.eVec3
kronlag.test4 = as.matrix(lagged.test4[,4:115]) %*% kron.eVec4
kronlag.test5 = as.matrix(lagged.test5[,4:115]) %*% kron.eVec5
kronlag.test6 = as.matrix(lagged.test6[,4:87])  %*% kron.eVec6


# Fitting Lagged ------------------------------------------------
fit.lagkronpca1 = cv.glmnet(x = as.matrix(kronlag1), y = lagged1$production, type.measure = "mae")
fit.lagkronpca2 = cv.glmnet(x = as.matrix(kronlag2), y = lagged2$production, type.measure = "mae")
fit.lagkronpca3 = cv.glmnet(x = as.matrix(kronlag3), y = lagged3$production, type.measure = "mae")
fit.lagkronpca4 = cv.glmnet(x = as.matrix(kronlag4), y = lagged4$production, type.measure = "mae")
fit.lagkronpca5 = cv.glmnet(x = as.matrix(kronlag5), y = lagged5$production, type.measure = "mae")
fit.lagkronpca6 = cv.glmnet(x = as.matrix(kronlag6), y = lagged6$production, type.measure = "mae")

pred1 = predict(fit.lagkronpca1, as.matrix(kronlag1), s = "lambda.1se")
pred2 = predict(fit.lagkronpca2, as.matrix(kronlag2), s = "lambda.1se")
pred3 = predict(fit.lagkronpca3, as.matrix(kronlag3), s = "lambda.1se")
pred4 = predict(fit.lagkronpca4, as.matrix(kronlag4), s = "lambda.1se")
pred5 = predict(fit.lagkronpca5, as.matrix(kronlag5), s = "lambda.1se")
pred6 = predict(fit.lagkronpca6, as.matrix(kronlag6), s = "lambda.1se")

error.train1 = mean(abs(pred1 - lagged1$production))
error.train2 = mean(abs(pred2 - lagged2$production))
error.train3 = mean(abs(pred3 - lagged3$production))
error.train4 = mean(abs(pred4 - lagged4$production))
error.train5 = mean(abs(pred5 - lagged5$production))
error.train6 = mean(abs(pred6 - lagged6$production))

pred.test1 = predict(fit.lagkronpca1, as.matrix(kronlag.test1), s = "lambda.1se")
pred.test2 = predict(fit.lagkronpca2, as.matrix(kronlag.test2), s = "lambda.1se")
pred.test3 = predict(fit.lagkronpca3, as.matrix(kronlag.test3), s = "lambda.1se")
pred.test4 = predict(fit.lagkronpca4, as.matrix(kronlag.test4), s = "lambda.1se")
pred.test5 = predict(fit.lagkronpca5, as.matrix(kronlag.test5), s = "lambda.1se")
pred.test6 = predict(fit.lagkronpca6, as.matrix(kronlag.test6), s = "lambda.1se")

error.test1 = mean(abs(pred.test1 - lagged.test1$production))
error.test2 = mean(abs(pred.test2 - lagged.test2$production))
error.test3 = mean(abs(pred.test3 - lagged.test3$production))
error.test4 = mean(abs(pred.test4 - lagged.test4$production))
error.test5 = mean(abs(pred.test5 - lagged.test5$production))
error.test6 = mean(abs(pred.test6 - lagged.test6$production))

res1 = data.table(data = "aliaga", model = "lagged", train_error = error.train1, test_error = error.test1)
res2 = data.table(data = "bares",  model = "lagged", train_error = error.train2, test_error = error.test2)
res3 = data.table(data = "dinar",  model = "lagged", train_error = error.train3, test_error = error.test3)
res4 = data.table(data = "geycek", model = "lagged", train_error = error.train4, test_error = error.test4)
res5 = data.table(data = "soke",   model = "lagged", train_error = error.train5, test_error = error.test5)
res6 = data.table(data = "soma",   model = "lagged", train_error = error.train6, test_error = error.test6)

results.kron = rbind(results.kron, res1)
results.kron = rbind(results.kron, res2)
results.kron = rbind(results.kron, res3)
results.kron = rbind(results.kron, res4)
results.kron = rbind(results.kron, res5)
results.kron = rbind(results.kron, res6)



# Lagged and Powered -----------------------------------------------------------
power.cov1 = cor(x = powered1[,4:30], y = powered1[,4:30])
power.cov2 = cor(x = powered2[,4:51], y = powered2[,4:51])
power.cov3 = cor(x = powered3[,4:51], y = powered3[,4:51])
power.cov4 = cor(x = powered4[,4:51], y = powered4[,4:51])
power.cov5 = cor(x = powered5[,4:51], y = powered5[,4:51])
power.cov6 = cor(x = powered6[,4:39], y = powered6[,4:39])

# temporal.cov1 = cor(temporal1, temporal1)
# temporal.cov2 = cor(temporal2, temporal2)
# temporal.cov3 = cor(temporal3, temporal3)
# temporal.cov4 = cor(temporal4, temporal4)
# temporal.cov5 = cor(temporal5, temporal5)
# temporal.cov6 = cor(temporal6, temporal6)

power.eVal1 = eigen(power.cov1)$values
power.eVal2 = eigen(power.cov2)$values
power.eVal3 = eigen(power.cov3)$values
power.eVal4 = eigen(power.cov4)$values
power.eVal5 = eigen(power.cov5)$values
power.eVal6 = eigen(power.cov6)$values

power.eVec1 = eigen(power.cov1)$vectors
power.eVec2 = eigen(power.cov2)$vectors
power.eVec3 = eigen(power.cov3)$vectors
power.eVec4 = eigen(power.cov4)$vectors
power.eVec5 = eigen(power.cov5)$vectors
power.eVec6 = eigen(power.cov6)$vectors

# temporal.eVal1 = eigen(temporal.cov1)$values
# temporal.eVal2 = eigen(temporal.cov2)$values
# temporal.eVal3 = eigen(temporal.cov3)$values
# temporal.eVal4 = eigen(temporal.cov4)$values
# temporal.eVal5 = eigen(temporal.cov5)$values
# temporal.eVal6 = eigen(temporal.cov6)$values

# temporal.eVec1 = eigen(temporal.cov1)$vectors
# temporal.eVec2 = eigen(temporal.cov2)$vectors
# temporal.eVec3 = eigen(temporal.cov3)$vectors
# temporal.eVec4 = eigen(temporal.cov4)$vectors
# temporal.eVec5 = eigen(temporal.cov5)$vectors
# temporal.eVec6 = eigen(temporal.cov6)$vectors

kronpower.eVal1 = kronecker(temporal.eVal1, power.eVal1)
kronpower.eVal2 = kronecker(temporal.eVal2, power.eVal2)
kronpower.eVal3 = kronecker(temporal.eVal3, power.eVal3)
kronpower.eVal4 = kronecker(temporal.eVal4, power.eVal4)
kronpower.eVal5 = kronecker(temporal.eVal5, power.eVal5)
kronpower.eVal6 = kronecker(temporal.eVal6, power.eVal6)

npc1 = pcLister(eigenvalues = kronpower.eVal1, var_wanted = 0.99)
npc2 = pcLister(eigenvalues = kronpower.eVal2, var_wanted = 0.99)
npc3 = pcLister(eigenvalues = kronpower.eVal3, var_wanted = 0.99)
npc4 = pcLister(eigenvalues = kronpower.eVal4, var_wanted = 0.99)
npc5 = pcLister(eigenvalues = kronpower.eVal5, var_wanted = 0.99)
npc6 = pcLister(eigenvalues = kronpower.eVal6, var_wanted = 0.99)

kronpower.eVec1 = kronecker(temporal.eVec1, power.eVec1)[,npc1]
kronpower.eVec2 = kronecker(temporal.eVec2, power.eVec2)[,npc2]
kronpower.eVec3 = kronecker(temporal.eVec3, power.eVec3)[,npc3]
kronpower.eVec4 = kronecker(temporal.eVec4, power.eVec4)[,npc4]
kronpower.eVec5 = kronecker(temporal.eVec5, power.eVec5)[,npc5]
kronpower.eVec6 = kronecker(temporal.eVec6, power.eVec6)[,npc6]

kronpowerlag1 = as.matrix(laggedpowered1[ ,4:192]) %*% kronpower.eVec1
kronpowerlag2 = as.matrix(laggedpowered2[ ,4:339]) %*% kronpower.eVec2
kronpowerlag3 = as.matrix(laggedpowered3[ ,4:339]) %*% kronpower.eVec3
kronpowerlag4 = as.matrix(laggedpowered4[ ,4:339]) %*% kronpower.eVec4
kronpowerlag5 = as.matrix(laggedpowered5[ ,4:339]) %*% kronpower.eVec5
kronpowerlag6 = as.matrix(laggedpowered6[ ,4:255]) %*% kronpower.eVec6

kronpowerlag.test1 = as.matrix(laggedpowered.test1[ ,4:192]) %*% kronpower.eVec1
kronpowerlag.test2 = as.matrix(laggedpowered.test2[ ,4:339]) %*% kronpower.eVec2
kronpowerlag.test3 = as.matrix(laggedpowered.test3[ ,4:339]) %*% kronpower.eVec3
kronpowerlag.test4 = as.matrix(laggedpowered.test4[ ,4:339]) %*% kronpower.eVec4
kronpowerlag.test5 = as.matrix(laggedpowered.test5[ ,4:339]) %*% kronpower.eVec5
kronpowerlag.test6 = as.matrix(laggedpowered.test6[ ,4:255]) %*% kronpower.eVec6


# Fitting Lagged and Powered ----------------------------------------------
fit.lagpowerkron1 = cv.glmnet(x = as.matrix(kronpowerlag1), y = laggedpowered1$production, type.measure = "mae")
fit.lagpowerkron2 = cv.glmnet(x = as.matrix(kronpowerlag2), y = laggedpowered2$production, type.measure = "mae")
fit.lagpowerkron3 = cv.glmnet(x = as.matrix(kronpowerlag3), y = laggedpowered3$production, type.measure = "mae")
fit.lagpowerkron4 = cv.glmnet(x = as.matrix(kronpowerlag4), y = laggedpowered4$production, type.measure = "mae")
fit.lagpowerkron5 = cv.glmnet(x = as.matrix(kronpowerlag5), y = laggedpowered5$production, type.measure = "mae")
fit.lagpowerkron6 = cv.glmnet(x = as.matrix(kronpowerlag6), y = laggedpowered6$production, type.measure = "mae")
# Warning verdi?
# from glmnet Fortran code (error code -93); Convergence for 93th lambda value not reached after maxit=100000 iterations; solutions for larger lambdas returned
pred1 = predict(fit.lagpowerkron1, as.matrix(kronpowerlag1), s = "lambda.1se")
pred2 = predict(fit.lagpowerkron2, as.matrix(kronpowerlag2), s = "lambda.1se")
pred3 = predict(fit.lagpowerkron3, as.matrix(kronpowerlag3), s = "lambda.1se")
pred4 = predict(fit.lagpowerkron4, as.matrix(kronpowerlag4), s = "lambda.1se")
pred5 = predict(fit.lagpowerkron5, as.matrix(kronpowerlag5), s = "lambda.1se")
pred6 = predict(fit.lagpowerkron6, as.matrix(kronpowerlag6), s = "lambda.1se")

error.train1 = mean(abs(pred1 - laggedpowered1$production))
error.train2 = mean(abs(pred2 - laggedpowered2$production))
error.train3 = mean(abs(pred3 - laggedpowered3$production))
error.train4 = mean(abs(pred4 - laggedpowered4$production))
error.train5 = mean(abs(pred5 - laggedpowered5$production))
error.train6 = mean(abs(pred6 - laggedpowered6$production))

pred.test1 = predict(fit.lagpowerkron1, as.matrix(kronpowerlag.test1), s = "lambda.1se")
pred.test2 = predict(fit.lagpowerkron2, as.matrix(kronpowerlag.test2), s = "lambda.1se")
pred.test3 = predict(fit.lagpowerkron3, as.matrix(kronpowerlag.test3), s = "lambda.1se")
pred.test4 = predict(fit.lagpowerkron4, as.matrix(kronpowerlag.test4), s = "lambda.1se")
pred.test5 = predict(fit.lagpowerkron5, as.matrix(kronpowerlag.test5), s = "lambda.1se")
pred.test6 = predict(fit.lagpowerkron6, as.matrix(kronpowerlag.test6), s = "lambda.1se")

error.test1 = mean(abs(pred.test1 - laggedpowered.test1$production))
error.test2 = mean(abs(pred.test2 - laggedpowered.test2$production))
error.test3 = mean(abs(pred.test3 - laggedpowered.test3$production))
error.test4 = mean(abs(pred.test4 - laggedpowered.test4$production))
error.test5 = mean(abs(pred.test5 - laggedpowered.test5$production))
error.test6 = mean(abs(pred.test6 - laggedpowered.test6$production))

res1 = data.table(data = "aliaga", model = "lagged + powered", train_error = error.train1, test_error = error.test1)
res2 = data.table(data = "bares",  model = "lagged + powered", train_error = error.train2, test_error = error.test2)
res3 = data.table(data = "dinar",  model = "lagged + powered", train_error = error.train3, test_error = error.test3)
res4 = data.table(data = "geycek", model = "lagged + powered", train_error = error.train4, test_error = error.test4)
res5 = data.table(data = "soke",   model = "lagged + powered", train_error = error.train5, test_error = error.test5)
res6 = data.table(data = "soma",   model = "lagged + powered", train_error = error.train6, test_error = error.test6)

results.kron = rbind(results.kron, res1)
results.kron = rbind(results.kron, res2)
results.kron = rbind(results.kron, res3)
results.kron = rbind(results.kron, res4)
results.kron = rbind(results.kron, res5)
results.kron = rbind(results.kron, res6)

results.kron[order(data)]



