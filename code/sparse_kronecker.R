# Sparse Kronecker

# Results Table -----------------------------------------------------------
results.sparse = data.table(station = character(),
                            model = character(),
                            train_error = numeric(),
                            test_error = numeric())

results.sparse[order(station)]

# Lagged Model ------------------------------------------------------------
# create Sparse Spatial Covariance Matrix
spsp.cov1 = sparseMaker(spatial.cov1, 3, 3)
spsp.cov2 = sparseMaker(spatial.cov2, 4, 4)
spsp.cov3 = sparseMaker(spatial.cov3, 4, 4)
spsp.cov4 = sparseMaker(spatial.cov4, 4, 4)
spsp.cov5 = sparseMaker(spatial.cov5, 4, 4)
spsp.cov6 = sparseMaker(spatial.cov6, 3, 4)
plot(spsp.cov2)
# create Sparse Temporal Covariance Matrix
spte.cov1 = sparseTimer(temporal.cov1)
spte.cov2 = sparseTimer(temporal.cov2)
spte.cov3 = sparseTimer(temporal.cov3)
spte.cov4 = sparseTimer(temporal.cov4)
spte.cov5 = sparseTimer(temporal.cov5)
spte.cov6 = sparseTimer(temporal.cov6)
plot(spte.cov2)
# kronecker product
spkronpca1 = kronecker(spte.cov1, spsp.cov1)
spkronpca2 = kronecker(spte.cov2, spsp.cov2)
spkronpca3 = kronecker(spte.cov3, spsp.cov3)
spkronpca4 = kronecker(spte.cov4, spsp.cov4)
spkronpca5 = kronecker(spte.cov5, spsp.cov5)
spkronpca6 = kronecker(spte.cov6, spsp.cov6)
# eigenvalues -> number of PCs
npc1 = pcLister(eigenvalues = eigen(spkronpca1)$values, var_wanted = 0.99)
npc2 = pcLister(eigenvalues = eigen(spkronpca2)$values, var_wanted = 0.99)
npc3 = pcLister(eigenvalues = eigen(spkronpca3)$values, var_wanted = 0.99)
npc4 = pcLister(eigenvalues = eigen(spkronpca4)$values, var_wanted = 0.99)
npc5 = pcLister(eigenvalues = eigen(spkronpca5)$values, var_wanted = 0.99)
npc6 = pcLister(eigenvalues = eigen(spkronpca6)$values, var_wanted = 0.99)
# reduced train
spkronlag1 = as.matrix(lagged1[ ,4:66])  %*% eigen(spkronpca1)$vectors[,npc1]
spkronlag2 = as.matrix(lagged2[ ,4:115]) %*% eigen(spkronpca2)$vectors[,npc2]
spkronlag3 = as.matrix(lagged3[ ,4:115]) %*% eigen(spkronpca3)$vectors[,npc3]
spkronlag4 = as.matrix(lagged4[ ,4:115]) %*% eigen(spkronpca4)$vectors[,npc4]
spkronlag5 = as.matrix(lagged5[ ,4:115]) %*% eigen(spkronpca5)$vectors[,npc5]
spkronlag6 = as.matrix(lagged6[ ,4:87])  %*% eigen(spkronpca6)$vectors[,npc6]
# reduced test
spkronlag.test1 = as.matrix(lagged.test1[ ,4:66])  %*% eigen(spkronpca1)$vectors[,npc1]
spkronlag.test2 = as.matrix(lagged.test2[ ,4:115]) %*% eigen(spkronpca2)$vectors[,npc2]
spkronlag.test3 = as.matrix(lagged.test3[ ,4:115]) %*% eigen(spkronpca3)$vectors[,npc3]
spkronlag.test4 = as.matrix(lagged.test4[ ,4:115]) %*% eigen(spkronpca4)$vectors[,npc4]
spkronlag.test5 = as.matrix(lagged.test5[ ,4:115]) %*% eigen(spkronpca5)$vectors[,npc5]
spkronlag.test6 = as.matrix(lagged.test6[ ,4:87])  %*% eigen(spkronpca6)$vectors[,npc6]


# Fitting Lagged ----------------------------------------------------------
fit.sparsekron1 = cv.glmnet(x = as.matrix(spkronlag1), y = lagged1$production, type.measure = "mae")
fit.sparsekron2 = cv.glmnet(x = as.matrix(spkronlag2), y = lagged2$production, type.measure = "mae")
fit.sparsekron3 = cv.glmnet(x = as.matrix(spkronlag3), y = lagged3$production, type.measure = "mae")
fit.sparsekron4 = cv.glmnet(x = as.matrix(spkronlag4), y = lagged4$production, type.measure = "mae")
fit.sparsekron5 = cv.glmnet(x = as.matrix(spkronlag5), y = lagged5$production, type.measure = "mae")
fit.sparsekron6 = cv.glmnet(x = as.matrix(spkronlag6), y = lagged6$production, type.measure = "mae")

pred1 = predict(fit.sparsekron1, as.matrix(spkronlag1), s = "lambda.1se")
pred2 = predict(fit.sparsekron2, as.matrix(spkronlag2), s = "lambda.1se")
pred3 = predict(fit.sparsekron3, as.matrix(spkronlag3), s = "lambda.1se")
pred4 = predict(fit.sparsekron4, as.matrix(spkronlag4), s = "lambda.1se")
pred5 = predict(fit.sparsekron5, as.matrix(spkronlag5), s = "lambda.1se")
pred6 = predict(fit.sparsekron6, as.matrix(spkronlag6), s = "lambda.1se")

error.train1 = mean(abs(pred1 - lagged1$production))
error.train2 = mean(abs(pred2 - lagged2$production))
error.train3 = mean(abs(pred3 - lagged3$production))
error.train4 = mean(abs(pred4 - lagged4$production))
error.train5 = mean(abs(pred5 - lagged5$production))
error.train6 = mean(abs(pred6 - lagged6$production))

pred.test1 = predict(fit.sparsekron1, as.matrix(spkronlag.test1), s = "lambda.1se")
pred.test2 = predict(fit.sparsekron2, as.matrix(spkronlag.test2), s = "lambda.1se")
pred.test3 = predict(fit.sparsekron3, as.matrix(spkronlag.test3), s = "lambda.1se")
pred.test4 = predict(fit.sparsekron4, as.matrix(spkronlag.test4), s = "lambda.1se")
pred.test5 = predict(fit.sparsekron5, as.matrix(spkronlag.test5), s = "lambda.1se")
pred.test6 = predict(fit.sparsekron6, as.matrix(spkronlag.test6), s = "lambda.1se")

error.test1 = mean(abs(pred.test1 - lagged.test1$production))
error.test2 = mean(abs(pred.test2 - lagged.test2$production))
error.test3 = mean(abs(pred.test3 - lagged.test3$production))
error.test4 = mean(abs(pred.test4 - lagged.test4$production))
error.test5 = mean(abs(pred.test5 - lagged.test5$production))
error.test6 = mean(abs(pred.test6 - lagged.test6$production))

res1 = data.table(station = "aliaga", model = "lagged", train_error = error.train1, test_error = error.test1)
res2 = data.table(station = "bares",  model = "lagged", train_error = error.train2, test_error = error.test2)
res3 = data.table(station = "dinar",  model = "lagged", train_error = error.train3, test_error = error.test3)
res4 = data.table(station = "geycek", model = "lagged", train_error = error.train4, test_error = error.test4)
res5 = data.table(station = "soke",   model = "lagged", train_error = error.train5, test_error = error.test5)
res6 = data.table(station = "soma",   model = "lagged", train_error = error.train6, test_error = error.test6)

results.sparse = rbind(results.sparse, res1)
results.sparse = rbind(results.sparse, res2)
results.sparse = rbind(results.sparse, res3)
results.sparse = rbind(results.sparse, res4)
results.sparse = rbind(results.sparse, res5)
results.sparse = rbind(results.sparse, res6)
results.sparse


# Lagged and Powered ------------------------------------------------------
# create Sparse Spatial Covariance Matrix
sppower.cov1 = sparseMakerPower(power.cov1, 3, 3, 3)
sppower.cov2 = sparseMakerPower(power.cov2, 4, 4, 3)
sppower.cov3 = sparseMakerPower(power.cov3, 4, 4, 3)
sppower.cov4 = sparseMakerPower(power.cov4, 4, 4, 3)
sppower.cov5 = sparseMakerPower(power.cov5, 4, 4, 3)
sppower.cov6 = sparseMakerPower(power.cov6, 3, 4, 3)
# create Sparse Temporal Covariance Matrix
## spte.cov1 = sparseTimer(temporal.cov1)
## spte.cov2 = sparseTimer(temporal.cov2)
## spte.cov3 = sparseTimer(temporal.cov3)
## spte.cov4 = sparseTimer(temporal.cov4)
## spte.cov5 = sparseTimer(temporal.cov5)
## spte.cov6 = sparseTimer(temporal.cov6)
# kronecker product
sppowkronpca1 = kronecker(spte.cov1, sppower.cov1)
sppowkronpca2 = kronecker(spte.cov2, sppower.cov2)
sppowkronpca3 = kronecker(spte.cov3, sppower.cov3)
sppowkronpca4 = kronecker(spte.cov4, sppower.cov4)
sppowkronpca5 = kronecker(spte.cov5, sppower.cov5)
sppowkronpca6 = kronecker(spte.cov6, sppower.cov6)
# eigenvalues -> number of PCs
npc1 = pcListerComplex(eigenvalues = eigen(sppowkronpca1)$values, var_wanted = 0.99)
npc2 = pcListerComplex(eigenvalues = eigen(sppowkronpca2)$values, var_wanted = 0.99)
npc3 = pcListerComplex(eigenvalues = eigen(sppowkronpca3)$values, var_wanted = 0.99)
npc4 = pcListerComplex(eigenvalues = eigen(sppowkronpca4)$values, var_wanted = 0.99)
npc5 = pcListerComplex(eigenvalues = eigen(sppowkronpca5)$values, var_wanted = 0.99)
npc6 = pcListerComplex(eigenvalues = eigen(sppowkronpca6)$values, var_wanted = 0.99)
# reduced train
sppowkronlag1 = as.matrix(laggedpowered1[ ,4:192]) %*% eigen(sppowkronpca1)$vectors[,npc1]
sppowkronlag2 = as.matrix(laggedpowered2[ ,4:339]) %*% eigen(sppowkronpca2)$vectors[,npc2]
sppowkronlag3 = as.matrix(laggedpowered3[ ,4:339]) %*% eigen(sppowkronpca3)$vectors[,npc3]
sppowkronlag4 = as.matrix(laggedpowered4[ ,4:339]) %*% eigen(sppowkronpca4)$vectors[,npc4]
sppowkronlag5 = as.matrix(laggedpowered5[ ,4:339]) %*% eigen(sppowkronpca5)$vectors[,npc5]
sppowkronlag6 = as.matrix(laggedpowered6[ ,4:255]) %*% eigen(sppowkronpca6)$vectors[,npc6]
# reduced test
sppowkronlag.test1 = as.matrix(laggedpowered.test1[ ,4:192]) %*% eigen(sppowkronpca1)$vectors[,npc1]
sppowkronlag.test2 = as.matrix(laggedpowered.test2[ ,4:339]) %*% eigen(sppowkronpca2)$vectors[,npc2]
sppowkronlag.test3 = as.matrix(laggedpowered.test3[ ,4:339]) %*% eigen(sppowkronpca3)$vectors[,npc3]
sppowkronlag.test4 = as.matrix(laggedpowered.test4[ ,4:339]) %*% eigen(sppowkronpca4)$vectors[,npc4]
sppowkronlag.test5 = as.matrix(laggedpowered.test5[ ,4:339]) %*% eigen(sppowkronpca5)$vectors[,npc5]
sppowkronlag.test6 = as.matrix(laggedpowered.test6[ ,4:255]) %*% eigen(sppowkronpca6)$vectors[,npc6]


# Fitting Lagged and Powered ----------------------------------------------
fit.sparsepowerkron1 = cv.glmnet(x = Re(as.matrix(sppowkronlag1)), y = laggedpowered1$production, type.measure = "mae")
fit.sparsepowerkron2 = cv.glmnet(x = Re(as.matrix(sppowkronlag2)), y = laggedpowered2$production, type.measure = "mae")
fit.sparsepowerkron3 = cv.glmnet(x = Re(as.matrix(sppowkronlag3)), y = laggedpowered3$production, type.measure = "mae")
fit.sparsepowerkron4 = cv.glmnet(x = Re(as.matrix(sppowkronlag4)), y = laggedpowered4$production, type.measure = "mae")
fit.sparsepowerkron5 = cv.glmnet(x = Re(as.matrix(sppowkronlag5)), y = laggedpowered5$production, type.measure = "mae")
fit.sparsepowerkron6 = cv.glmnet(x = Re(as.matrix(sppowkronlag6)), y = laggedpowered6$production, type.measure = "mae")
# Warning verdi?
# from glmnet Fortran code (error code -93); Convergence for 93th lambda value not reached after maxit=100000 iterations; solutions for larger lambdas returned
pred1 = predict(fit.sparsepowerkron1, Re(as.matrix(sppowkronlag1)), s = "lambda.1se")
pred2 = predict(fit.sparsepowerkron2, Re(as.matrix(sppowkronlag2)), s = "lambda.1se")
pred3 = predict(fit.sparsepowerkron3, Re(as.matrix(sppowkronlag3)), s = "lambda.1se")
pred4 = predict(fit.sparsepowerkron4, Re(as.matrix(sppowkronlag4)), s = "lambda.1se")
pred5 = predict(fit.sparsepowerkron5, Re(as.matrix(sppowkronlag5)), s = "lambda.1se")
pred6 = predict(fit.sparsepowerkron6, Re(as.matrix(sppowkronlag6)), s = "lambda.1se")

error.train1 = mean(abs(pred1 - laggedpowered1$production))
error.train2 = mean(abs(pred2 - laggedpowered2$production))
error.train3 = mean(abs(pred3 - laggedpowered3$production))
error.train4 = mean(abs(pred4 - laggedpowered4$production))
error.train5 = mean(abs(pred5 - laggedpowered5$production))
error.train6 = mean(abs(pred6 - laggedpowered6$production))

pred.test1 = predict(fit.sparsepowerkron1, Re(as.matrix(sppowkronlag.test1)), s = "lambda.1se")
pred.test2 = predict(fit.sparsepowerkron2, Re(as.matrix(sppowkronlag.test2)), s = "lambda.1se")
pred.test3 = predict(fit.sparsepowerkron3, Re(as.matrix(sppowkronlag.test3)), s = "lambda.1se")
pred.test4 = predict(fit.sparsepowerkron4, Re(as.matrix(sppowkronlag.test4)), s = "lambda.1se")
pred.test5 = predict(fit.sparsepowerkron5, Re(as.matrix(sppowkronlag.test5)), s = "lambda.1se")
pred.test6 = predict(fit.sparsepowerkron6, Re(as.matrix(sppowkronlag.test6)), s = "lambda.1se")

error.test1 = mean(abs(pred.test1 - laggedpowered.test1$production))
error.test2 = mean(abs(pred.test2 - laggedpowered.test2$production))
error.test3 = mean(abs(pred.test3 - laggedpowered.test3$production))
error.test4 = mean(abs(pred.test4 - laggedpowered.test4$production))
error.test5 = mean(abs(pred.test5 - laggedpowered.test5$production))
error.test6 = mean(abs(pred.test6 - laggedpowered.test6$production))

res1 = data.table(station = "aliaga", model = "lagged + powered", train_error = error.train1, test_error = error.test1)
res2 = data.table(station = "bares",  model = "lagged + powered", train_error = error.train2, test_error = error.test2)
res3 = data.table(station = "dinar",  model = "lagged + powered", train_error = error.train3, test_error = error.test3)
res4 = data.table(station = "geycek", model = "lagged + powered", train_error = error.train4, test_error = error.test4)
res5 = data.table(station = "soke",   model = "lagged + powered", train_error = error.train5, test_error = error.test5)
res6 = data.table(station = "soma",   model = "lagged + powered", train_error = error.train6, test_error = error.test6)

results.sparse = rbind(results.sparse, res1)
results.sparse = rbind(results.sparse, res2)
results.sparse = rbind(results.sparse, res3)
results.sparse = rbind(results.sparse, res4)
results.sparse = rbind(results.sparse, res5)
results.sparse = rbind(results.sparse, res6)

results.sparse[order(station)]



