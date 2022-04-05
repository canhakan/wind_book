# Sparse Kronecker -> Nearest Positive Definite Cor. Matrix
results.sparse.pd = data.table(station = character(),
                               model = character(),
                               train_error = numeric(),
                               test_error = numeric())


# Lagged (PDize spatial and temporal) -------------------------------------
# spatial
pdsp.cov1 = nearPD(spsp.cov1,corr = TRUE)
pdsp.cov2 = nearPD(spsp.cov2,corr = TRUE)
pdsp.cov3 = nearPD(spsp.cov3,corr = TRUE)
pdsp.cov4 = nearPD(spsp.cov4,corr = TRUE)
pdsp.cov5 = nearPD(spsp.cov5,corr = TRUE)
pdsp.cov6 = nearPD(spsp.cov6,corr = TRUE)
# temporal
pste.cov1 = nearPD(spte.cov1, corr = TRUE)
pste.cov2 = nearPD(spte.cov2, corr = TRUE)
pste.cov3 = nearPD(spte.cov3, corr = TRUE)
pste.cov4 = nearPD(spte.cov4, corr = TRUE)
pste.cov5 = nearPD(spte.cov5, corr = TRUE)
pste.cov6 = nearPD(spte.cov6, corr = TRUE)
# kron product of Pos Def'ized spatial and temporal cov matrices
spkron1.pdbefore = kronecker(pste.cov1$mat, pdsp.cov1$mat)
spkron2.pdbefore = kronecker(pste.cov2$mat, pdsp.cov2$mat)
spkron3.pdbefore = kronecker(pste.cov3$mat, pdsp.cov3$mat)
spkron4.pdbefore = kronecker(pste.cov4$mat, pdsp.cov4$mat)
spkron5.pdbefore = kronecker(pste.cov5$mat, pdsp.cov5$mat)
spkron6.pdbefore = kronecker(pste.cov6$mat, pdsp.cov6$mat)
# choose the most explaining eigenvalues
npc1 = pcLister(eigenvalues = eigen(spkron1.pdbefore)$values, var_wanted = 0.99)
npc2 = pcLister(eigenvalues = eigen(spkron2.pdbefore)$values, var_wanted = 0.99)
npc3 = pcLister(eigenvalues = eigen(spkron3.pdbefore)$values, var_wanted = 0.99)
npc4 = pcLister(eigenvalues = eigen(spkron4.pdbefore)$values, var_wanted = 0.99)
npc5 = pcLister(eigenvalues = eigen(spkron5.pdbefore)$values, var_wanted = 0.99)
npc6 = pcLister(eigenvalues = eigen(spkron6.pdbefore)$values, var_wanted = 0.99)
# reduced train
spkronlag1.pdb = as.matrix(lagged1[ ,4:66])  %*% eigen(spkron1.pdbefore)$vectors[,npc1]
spkronlag2.pdb = as.matrix(lagged2[ ,4:115]) %*% eigen(spkron2.pdbefore)$vectors[,npc2]
spkronlag3.pdb = as.matrix(lagged3[ ,4:115]) %*% eigen(spkron3.pdbefore)$vectors[,npc3]
spkronlag4.pdb = as.matrix(lagged4[ ,4:115]) %*% eigen(spkron4.pdbefore)$vectors[,npc4]
spkronlag5.pdb = as.matrix(lagged5[ ,4:115]) %*% eigen(spkron5.pdbefore)$vectors[,npc5]
spkronlag6.pdb = as.matrix(lagged6[ ,4:87])  %*% eigen(spkron6.pdbefore)$vectors[,npc6]
# reduced test
spkronlag.test1.pdb = as.matrix(lagged.test1[ ,4:66])  %*% eigen(spkron1.pdbefore)$vectors[,npc1]
spkronlag.test2.pdb = as.matrix(lagged.test2[ ,4:115]) %*% eigen(spkron2.pdbefore)$vectors[,npc2]
spkronlag.test3.pdb = as.matrix(lagged.test3[ ,4:115]) %*% eigen(spkron3.pdbefore)$vectors[,npc3]
spkronlag.test4.pdb = as.matrix(lagged.test4[ ,4:115]) %*% eigen(spkron4.pdbefore)$vectors[,npc4]
spkronlag.test5.pdb = as.matrix(lagged.test5[ ,4:115]) %*% eigen(spkron5.pdbefore)$vectors[,npc5]
spkronlag.test6.pdb = as.matrix(lagged.test6[ ,4:87])  %*% eigen(spkron6.pdbefore)$vectors[,npc6]


# Lagged (PDize Kronecker) ------------------------------------------------
# Pos Def'ization of the kronecker product
spkron1.pdlater = nearPD(spkronpca1, corr = TRUE)$mat
spkron2.pdlater = nearPD(spkronpca2, corr = TRUE)$mat
spkron3.pdlater = nearPD(spkronpca3, corr = TRUE)$mat
spkron4.pdlater = nearPD(spkronpca4, corr = TRUE)$mat
spkron5.pdlater = nearPD(spkronpca5, corr = TRUE)$mat
spkron6.pdlater = nearPD(spkronpca6, corr = TRUE)$mat
# choose the most explaining eigenvalues
npc1 = pcLister(eigenvalues = eigen(spkron1.pdlater)$values, var_wanted = 0.99)
npc2 = pcLister(eigenvalues = eigen(spkron2.pdlater)$values, var_wanted = 0.99)
npc3 = pcLister(eigenvalues = eigen(spkron3.pdlater)$values, var_wanted = 0.99)
npc4 = pcLister(eigenvalues = eigen(spkron4.pdlater)$values, var_wanted = 0.99)
npc5 = pcLister(eigenvalues = eigen(spkron5.pdlater)$values, var_wanted = 0.99)
npc6 = pcLister(eigenvalues = eigen(spkron6.pdlater)$values, var_wanted = 0.99)
# reduced train
spkronlag1.pdl = as.matrix(lagged1[ ,4:66])  %*% eigen(spkron1.pdlater)$vectors[,npc1]
spkronlag2.pdl = as.matrix(lagged2[ ,4:115]) %*% eigen(spkron2.pdlater)$vectors[,npc2]
spkronlag3.pdl = as.matrix(lagged3[ ,4:115]) %*% eigen(spkron3.pdlater)$vectors[,npc3]
spkronlag4.pdl = as.matrix(lagged4[ ,4:115]) %*% eigen(spkron4.pdlater)$vectors[,npc4]
spkronlag5.pdl = as.matrix(lagged5[ ,4:115]) %*% eigen(spkron5.pdlater)$vectors[,npc5]
spkronlag6.pdl = as.matrix(lagged6[ ,4:87])  %*% eigen(spkron6.pdlater)$vectors[,npc6]
# reduced test
spkronlag.test1.pdl = as.matrix(lagged.test1[ ,4:66])  %*% eigen(spkron1.pdlater)$vectors[,npc1]
spkronlag.test2.pdl = as.matrix(lagged.test2[ ,4:115]) %*% eigen(spkron2.pdlater)$vectors[,npc2]
spkronlag.test3.pdl = as.matrix(lagged.test3[ ,4:115]) %*% eigen(spkron3.pdlater)$vectors[,npc3]
spkronlag.test4.pdl = as.matrix(lagged.test4[ ,4:115]) %*% eigen(spkron4.pdlater)$vectors[,npc4]
spkronlag.test5.pdl = as.matrix(lagged.test5[ ,4:115]) %*% eigen(spkron5.pdlater)$vectors[,npc5]
spkronlag.test6.pdl = as.matrix(lagged.test6[ ,4:87])  %*% eigen(spkron6.pdlater)$vectors[,npc6]

# Fitting Lagged 1 ----------------------------------------------------------
fit.sparse.pdb1 = cv.glmnet(x = as.matrix(spkronlag1.pdb), y = lagged1$production, type.measure = "mae")
fit.sparse.pdb2 = cv.glmnet(x = as.matrix(spkronlag2.pdb), y = lagged2$production, type.measure = "mae")
fit.sparse.pdb3 = cv.glmnet(x = as.matrix(spkronlag3.pdb), y = lagged3$production, type.measure = "mae")
fit.sparse.pdb4 = cv.glmnet(x = as.matrix(spkronlag4.pdb), y = lagged4$production, type.measure = "mae")
fit.sparse.pdb5 = cv.glmnet(x = as.matrix(spkronlag5.pdb), y = lagged5$production, type.measure = "mae")
fit.sparse.pdb6 = cv.glmnet(x = as.matrix(spkronlag6.pdb), y = lagged6$production, type.measure = "mae")

pred1 = predict(fit.sparse.pdb1, as.matrix(spkronlag1.pdb), s = "lambda.1se")
pred2 = predict(fit.sparse.pdb2, as.matrix(spkronlag2.pdb), s = "lambda.1se")
pred3 = predict(fit.sparse.pdb3, as.matrix(spkronlag3.pdb), s = "lambda.1se")
pred4 = predict(fit.sparse.pdb4, as.matrix(spkronlag4.pdb), s = "lambda.1se")
pred5 = predict(fit.sparse.pdb5, as.matrix(spkronlag5.pdb), s = "lambda.1se")
pred6 = predict(fit.sparse.pdb6, as.matrix(spkronlag6.pdb), s = "lambda.1se")

error.train1 = mean(abs(pred1 - lagged1$production))
error.train2 = mean(abs(pred2 - lagged2$production))
error.train3 = mean(abs(pred3 - lagged3$production))
error.train4 = mean(abs(pred4 - lagged4$production))
error.train5 = mean(abs(pred5 - lagged5$production))
error.train6 = mean(abs(pred6 - lagged6$production))

pred.test1 = predict(fit.sparse.pdb1, as.matrix(spkronlag.test1.pdb), s = "lambda.1se")
pred.test2 = predict(fit.sparse.pdb2, as.matrix(spkronlag.test2.pdb), s = "lambda.1se")
pred.test3 = predict(fit.sparse.pdb3, as.matrix(spkronlag.test3.pdb), s = "lambda.1se")
pred.test4 = predict(fit.sparse.pdb4, as.matrix(spkronlag.test4.pdb), s = "lambda.1se")
pred.test5 = predict(fit.sparse.pdb5, as.matrix(spkronlag.test5.pdb), s = "lambda.1se")
pred.test6 = predict(fit.sparse.pdb6, as.matrix(spkronlag.test6.pdb), s = "lambda.1se")

error.test1 = mean(abs(pred.test1 - lagged.test1$production))
error.test2 = mean(abs(pred.test2 - lagged.test2$production))
error.test3 = mean(abs(pred.test3 - lagged.test3$production))
error.test4 = mean(abs(pred.test4 - lagged.test4$production))
error.test5 = mean(abs(pred.test5 - lagged.test5$production))
error.test6 = mean(abs(pred.test6 - lagged.test6$production))

res1 = data.table(station = "aliaga", model = "lagged/pdb", train_error = error.train1, test_error = error.test1)
res2 = data.table(station = "bares",  model = "lagged/pdb", train_error = error.train2, test_error = error.test2)
res3 = data.table(station = "dinar",  model = "lagged/pdb", train_error = error.train3, test_error = error.test3)
res4 = data.table(station = "geycek", model = "lagged/pdb", train_error = error.train4, test_error = error.test4)
res5 = data.table(station = "soke",   model = "lagged/pdb", train_error = error.train5, test_error = error.test5)
res6 = data.table(station = "soma",   model = "lagged/pdb", train_error = error.train6, test_error = error.test6)


results.sparse.pd = rbind(results.sparse.pd, res1)
results.sparse.pd = rbind(results.sparse.pd, res2)
results.sparse.pd = rbind(results.sparse.pd, res3)
results.sparse.pd = rbind(results.sparse.pd, res4)
results.sparse.pd = rbind(results.sparse.pd, res5)
results.sparse.pd = rbind(results.sparse.pd, res6)

results.sparse.pd[order(station)]


# Fitting Lagged 2 --------------------------------------------------------
fit.sparse.pdl1 = cv.glmnet(x = as.matrix(spkronlag1.pdl), y = lagged1$production, type.measure = "mae")
fit.sparse.pdl2 = cv.glmnet(x = as.matrix(spkronlag2.pdl), y = lagged2$production, type.measure = "mae")
fit.sparse.pdl3 = cv.glmnet(x = as.matrix(spkronlag3.pdl), y = lagged3$production, type.measure = "mae")
fit.sparse.pdl4 = cv.glmnet(x = as.matrix(spkronlag4.pdl), y = lagged4$production, type.measure = "mae")
fit.sparse.pdl5 = cv.glmnet(x = as.matrix(spkronlag5.pdl), y = lagged5$production, type.measure = "mae")
fit.sparse.pdl6 = cv.glmnet(x = as.matrix(spkronlag6.pdl), y = lagged6$production, type.measure = "mae")

pred1 = predict(fit.sparse.pdl1, as.matrix(spkronlag1.pdl), s = "lambda.1se")
pred2 = predict(fit.sparse.pdl2, as.matrix(spkronlag2.pdl), s = "lambda.1se")
pred3 = predict(fit.sparse.pdl3, as.matrix(spkronlag3.pdl), s = "lambda.1se")
pred4 = predict(fit.sparse.pdl4, as.matrix(spkronlag4.pdl), s = "lambda.1se")
pred5 = predict(fit.sparse.pdl5, as.matrix(spkronlag5.pdl), s = "lambda.1se")
pred6 = predict(fit.sparse.pdl6, as.matrix(spkronlag6.pdl), s = "lambda.1se")

error.train1 = mean(abs(pred1 - lagged1$production))
error.train2 = mean(abs(pred2 - lagged2$production))
error.train3 = mean(abs(pred3 - lagged3$production))
error.train4 = mean(abs(pred4 - lagged4$production))
error.train5 = mean(abs(pred5 - lagged5$production))
error.train6 = mean(abs(pred6 - lagged6$production))

pred.test1 = predict(fit.sparse.pdl1, as.matrix(spkronlag.test1.pdl), s = "lambda.1se")
pred.test2 = predict(fit.sparse.pdl2, as.matrix(spkronlag.test2.pdl), s = "lambda.1se")
pred.test3 = predict(fit.sparse.pdl3, as.matrix(spkronlag.test3.pdl), s = "lambda.1se")
pred.test4 = predict(fit.sparse.pdl4, as.matrix(spkronlag.test4.pdl), s = "lambda.1se")
pred.test5 = predict(fit.sparse.pdl5, as.matrix(spkronlag.test5.pdl), s = "lambda.1se")
pred.test6 = predict(fit.sparse.pdl6, as.matrix(spkronlag.test6.pdl), s = "lambda.1se")

error.test1 = mean(abs(pred.test1 - lagged.test1$production))
error.test2 = mean(abs(pred.test2 - lagged.test2$production))
error.test3 = mean(abs(pred.test3 - lagged.test3$production))
error.test4 = mean(abs(pred.test4 - lagged.test4$production))
error.test5 = mean(abs(pred.test5 - lagged.test5$production))
error.test6 = mean(abs(pred.test6 - lagged.test6$production))

res1 = data.table(station = "aliaga", model = "lagged/pdl", train_error = error.train1, test_error = error.test1)
res2 = data.table(station = "bares",  model = "lagged/pdl", train_error = error.train2, test_error = error.test2)
res3 = data.table(station = "dinar",  model = "lagged/pdl", train_error = error.train3, test_error = error.test3)
res4 = data.table(station = "geycek", model = "lagged/pdl", train_error = error.train4, test_error = error.test4)
res5 = data.table(station = "soke",   model = "lagged/pdl", train_error = error.train5, test_error = error.test5)
res6 = data.table(station = "soma",   model = "lagged/pdl", train_error = error.train6, test_error = error.test6)


results.sparse.pd = rbind(results.sparse.pd, res1)
results.sparse.pd = rbind(results.sparse.pd, res2)
results.sparse.pd = rbind(results.sparse.pd, res3)
results.sparse.pd = rbind(results.sparse.pd, res4)
results.sparse.pd = rbind(results.sparse.pd, res5)
results.sparse.pd = rbind(results.sparse.pd, res6)

results.sparse.pd[order(station)]


# Lagged and Powered PDB --------------------------------------------------
# create Sparse Spatial Covariance Matrix
pdpow.cov1 = nearPD(sppower.cov1, corr = TRUE)
pdpow.cov2 = nearPD(sppower.cov2, corr = TRUE)
pdpow.cov3 = nearPD(sppower.cov3, corr = TRUE)
pdpow.cov4 = nearPD(sppower.cov4, corr = TRUE)
pdpow.cov5 = nearPD(sppower.cov5, corr = TRUE)
pdpow.cov6 = nearPD(sppower.cov6, corr = TRUE)
# kronecker Prod
sppowkron1.pdbefore = kronecker(pste.cov1$mat, pdpow.cov1$mat)
sppowkron2.pdbefore = kronecker(pste.cov2$mat, pdpow.cov2$mat)
sppowkron3.pdbefore = kronecker(pste.cov3$mat, pdpow.cov3$mat)
sppowkron4.pdbefore = kronecker(pste.cov4$mat, pdpow.cov4$mat)
sppowkron5.pdbefore = kronecker(pste.cov5$mat, pdpow.cov5$mat)
sppowkron6.pdbefore = kronecker(pste.cov6$mat, pdpow.cov6$mat)
# choose the most explaining eigenvalues
npc1 = pcLister(eigenvalues = eigen(sppowkron1.pdbefore)$values, var_wanted = 0.99)
npc2 = pcLister(eigenvalues = eigen(sppowkron2.pdbefore)$values, var_wanted = 0.99)
npc3 = pcLister(eigenvalues = eigen(sppowkron3.pdbefore)$values, var_wanted = 0.99)
npc4 = pcLister(eigenvalues = eigen(sppowkron4.pdbefore)$values, var_wanted = 0.99)
npc5 = pcLister(eigenvalues = eigen(sppowkron5.pdbefore)$values, var_wanted = 0.99)
npc6 = pcLister(eigenvalues = eigen(sppowkron6.pdbefore)$values, var_wanted = 0.99)
# reduced train
sppowlag1.pdb = as.matrix(laggedpowered1[ ,4:192])  %*% eigen(sppowkron1.pdbefore)$vectors[,npc1]
sppowlag2.pdb = as.matrix(laggedpowered2[ ,4:339]) %*% eigen(sppowkron2.pdbefore)$vectors[,npc2]
sppowlag3.pdb = as.matrix(laggedpowered3[ ,4:339]) %*% eigen(sppowkron3.pdbefore)$vectors[,npc3]
sppowlag4.pdb = as.matrix(laggedpowered4[ ,4:339]) %*% eigen(sppowkron4.pdbefore)$vectors[,npc4]
sppowlag5.pdb = as.matrix(laggedpowered5[ ,4:339]) %*% eigen(sppowkron5.pdbefore)$vectors[,npc5]
sppowlag6.pdb = as.matrix(laggedpowered6[ ,4:255])  %*% eigen(sppowkron6.pdbefore)$vectors[,npc6]
# reduced test
sppowlag.test1.pdb = as.matrix(laggedpowered.test1[ ,4:192])  %*% eigen(sppowkron1.pdbefore)$vectors[,npc1]
sppowlag.test2.pdb = as.matrix(laggedpowered.test2[ ,4:339]) %*% eigen(sppowkron2.pdbefore)$vectors[,npc2]
sppowlag.test3.pdb = as.matrix(laggedpowered.test3[ ,4:339]) %*% eigen(sppowkron3.pdbefore)$vectors[,npc3]
sppowlag.test4.pdb = as.matrix(laggedpowered.test4[ ,4:339]) %*% eigen(sppowkron4.pdbefore)$vectors[,npc4]
sppowlag.test5.pdb = as.matrix(laggedpowered.test5[ ,4:339]) %*% eigen(sppowkron5.pdbefore)$vectors[,npc5]
sppowlag.test6.pdb = as.matrix(laggedpowered.test6[ ,4:255])  %*% eigen(sppowkron6.pdbefore)$vectors[,npc6]


# Lagged and Powered PDL --------------------------------------------------
# Pos Def Kron Prod
sppowkron1.pdlater  = nearPD(sppowkronpca1,corr = TRUE)$mat
sppowkron2.pdlater  = nearPD(sppowkronpca2,corr = TRUE)$mat
sppowkron3.pdlater  = nearPD(sppowkronpca3,corr = TRUE)$mat
sppowkron4.pdlater  = nearPD(sppowkronpca4,corr = TRUE)$mat
sppowkron5.pdlater  = nearPD(sppowkronpca5,corr = TRUE)$mat
sppowkron6.pdlater  = nearPD(sppowkronpca6,corr = TRUE)$mat
# choose the most explaining eigenvalues
npc1 = pcLister(eigenvalues = eigen(sppowkron1.pdlater)$values, var_wanted = 0.99)
npc2 = pcLister(eigenvalues = eigen(sppowkron2.pdlater)$values, var_wanted = 0.99)
npc3 = pcLister(eigenvalues = eigen(sppowkron3.pdlater)$values, var_wanted = 0.99)
npc4 = pcLister(eigenvalues = eigen(sppowkron4.pdlater)$values, var_wanted = 0.99)
npc5 = pcLister(eigenvalues = eigen(sppowkron5.pdlater)$values, var_wanted = 0.99)
npc6 = pcLister(eigenvalues = eigen(sppowkron6.pdlater)$values, var_wanted = 0.99)
# reduced train
sppowlag1.pdl = as.matrix(laggedpowered1[ ,4:192]) %*% eigen(sppowkron1.pdlater)$vectors[,npc1]
sppowlag2.pdl = as.matrix(laggedpowered2[ ,4:339]) %*% eigen(sppowkron2.pdlater)$vectors[,npc2]
sppowlag3.pdl = as.matrix(laggedpowered3[ ,4:339]) %*% eigen(sppowkron3.pdlater)$vectors[,npc3]
sppowlag4.pdl = as.matrix(laggedpowered4[ ,4:339]) %*% eigen(sppowkron4.pdlater)$vectors[,npc4]
sppowlag5.pdl = as.matrix(laggedpowered5[ ,4:339]) %*% eigen(sppowkron5.pdlater)$vectors[,npc5]
sppowlag6.pdl = as.matrix(laggedpowered6[ ,4:255]) %*% eigen(sppowkron6.pdlater)$vectors[,npc6]
# reduced test
sppowlag.test1.pdl = as.matrix(laggedpowered.test1[ ,4:192]) %*% eigen(sppowkron1.pdlater)$vectors[,npc1]
sppowlag.test2.pdl = as.matrix(laggedpowered.test2[ ,4:339]) %*% eigen(sppowkron2.pdlater)$vectors[,npc2]
sppowlag.test3.pdl = as.matrix(laggedpowered.test3[ ,4:339]) %*% eigen(sppowkron3.pdlater)$vectors[,npc3]
sppowlag.test4.pdl = as.matrix(laggedpowered.test4[ ,4:339]) %*% eigen(sppowkron4.pdlater)$vectors[,npc4]
sppowlag.test5.pdl = as.matrix(laggedpowered.test5[ ,4:339]) %*% eigen(sppowkron5.pdlater)$vectors[,npc5]
sppowlag.test6.pdl = as.matrix(laggedpowered.test6[ ,4:255]) %*% eigen(sppowkron6.pdlater)$vectors[,npc6]


# Fitting Lagged+Powered 1 ------------------------------------------------
fit.sppow1.pdb = cv.glmnet(x = as.matrix(sppowlag1.pdb), y = laggedpowered1$production, type.measure = "mae")
fit.sppow2.pdb = cv.glmnet(x = as.matrix(sppowlag2.pdb), y = laggedpowered2$production, type.measure = "mae")
fit.sppow3.pdb = cv.glmnet(x = as.matrix(sppowlag3.pdb), y = laggedpowered3$production, type.measure = "mae")
fit.sppow4.pdb = cv.glmnet(x = as.matrix(sppowlag4.pdb), y = laggedpowered4$production, type.measure = "mae")
fit.sppow5.pdb = cv.glmnet(x = as.matrix(sppowlag5.pdb), y = laggedpowered5$production, type.measure = "mae")
fit.sppow6.pdb = cv.glmnet(x = as.matrix(sppowlag6.pdb), y = laggedpowered6$production, type.measure = "mae")
#
pred1 = predict(fit.sppow1.pdb, as.matrix(sppowlag1.pdb), s = "lambda.1se")
pred2 = predict(fit.sppow2.pdb, as.matrix(sppowlag2.pdb), s = "lambda.1se")
pred3 = predict(fit.sppow3.pdb, as.matrix(sppowlag3.pdb), s = "lambda.1se")
pred4 = predict(fit.sppow4.pdb, as.matrix(sppowlag4.pdb), s = "lambda.1se")
pred5 = predict(fit.sppow5.pdb, as.matrix(sppowlag5.pdb), s = "lambda.1se")
pred6 = predict(fit.sppow6.pdb, as.matrix(sppowlag6.pdb), s = "lambda.1se")

error.train1 = mean(abs(pred1 - laggedpowered1$production))
error.train2 = mean(abs(pred2 - laggedpowered2$production))
error.train3 = mean(abs(pred3 - laggedpowered3$production))
error.train4 = mean(abs(pred4 - laggedpowered4$production))
error.train5 = mean(abs(pred5 - laggedpowered5$production))
error.train6 = mean(abs(pred6 - laggedpowered6$production))

pred.test1 = predict(fit.sppow1.pdb, as.matrix(sppowlag.test1.pdb), s = "lambda.1se")
pred.test2 = predict(fit.sppow2.pdb, as.matrix(sppowlag.test2.pdb), s = "lambda.1se")
pred.test3 = predict(fit.sppow3.pdb, as.matrix(sppowlag.test3.pdb), s = "lambda.1se")
pred.test4 = predict(fit.sppow4.pdb, as.matrix(sppowlag.test4.pdb), s = "lambda.1se")
pred.test5 = predict(fit.sppow5.pdb, as.matrix(sppowlag.test5.pdb), s = "lambda.1se")
pred.test6 = predict(fit.sppow6.pdb, as.matrix(sppowlag.test6.pdb), s = "lambda.1se")

error.test1 = mean(abs(pred.test1 - laggedpowered.test1$production))
error.test2 = mean(abs(pred.test2 - laggedpowered.test2$production))
error.test3 = mean(abs(pred.test3 - laggedpowered.test3$production))
error.test4 = mean(abs(pred.test4 - laggedpowered.test4$production))
error.test5 = mean(abs(pred.test5 - laggedpowered.test5$production))
error.test6 = mean(abs(pred.test6 - laggedpowered.test6$production))

res1 = data.table(station = "aliaga", model = "lagged + powered/pdb", train_error = error.train1, test_error = error.test1)
res2 = data.table(station = "bares",  model = "lagged + powered/pdb", train_error = error.train2, test_error = error.test2)
res3 = data.table(station = "dinar",  model = "lagged + powered/pdb", train_error = error.train3, test_error = error.test3)
res4 = data.table(station = "geycek", model = "lagged + powered/pdb", train_error = error.train4, test_error = error.test4)
res5 = data.table(station = "soke",   model = "lagged + powered/pdb", train_error = error.train5, test_error = error.test5)
res6 = data.table(station = "soma",   model = "lagged + powered/pdb", train_error = error.train6, test_error = error.test6)

results.sparse.pd = rbind(results.sparse.pd, res1)
results.sparse.pd = rbind(results.sparse.pd, res2)
results.sparse.pd = rbind(results.sparse.pd, res3)
results.sparse.pd = rbind(results.sparse.pd, res4)
results.sparse.pd = rbind(results.sparse.pd, res5)
results.sparse.pd = rbind(results.sparse.pd, res6)

results.sparse.pd[order(station)]
# Fitting Lagged+Powered 2 ------------------------------------------------
fit.sppow1.pdl = cv.glmnet(x = as.matrix(sppowlag1.pdl), y = laggedpowered1$production, type.measure = "mae")
fit.sppow2.pdl = cv.glmnet(x = as.matrix(sppowlag2.pdl), y = laggedpowered2$production, type.measure = "mae")
fit.sppow3.pdl = cv.glmnet(x = as.matrix(sppowlag3.pdl), y = laggedpowered3$production, type.measure = "mae")
fit.sppow4.pdl = cv.glmnet(x = as.matrix(sppowlag4.pdl), y = laggedpowered4$production, type.measure = "mae")
fit.sppow5.pdl = cv.glmnet(x = as.matrix(sppowlag5.pdl), y = laggedpowered5$production, type.measure = "mae")
fit.sppow6.pdl = cv.glmnet(x = as.matrix(sppowlag6.pdl), y = laggedpowered6$production, type.measure = "mae")
#
pred1 = predict(fit.sppow1.pdl, as.matrix(sppowlag1.pdl), s = "lambda.1se")
pred2 = predict(fit.sppow2.pdl, as.matrix(sppowlag2.pdl), s = "lambda.1se")
pred3 = predict(fit.sppow3.pdl, as.matrix(sppowlag3.pdl), s = "lambda.1se")
pred4 = predict(fit.sppow4.pdl, as.matrix(sppowlag4.pdl), s = "lambda.1se")
pred5 = predict(fit.sppow5.pdl, as.matrix(sppowlag5.pdl), s = "lambda.1se")
pred6 = predict(fit.sppow6.pdl, as.matrix(sppowlag6.pdl), s = "lambda.1se")

error.train1 = mean(abs(pred1 - laggedpowered1$production))
error.train2 = mean(abs(pred2 - laggedpowered2$production))
error.train3 = mean(abs(pred3 - laggedpowered3$production))
error.train4 = mean(abs(pred4 - laggedpowered4$production))
error.train5 = mean(abs(pred5 - laggedpowered5$production))
error.train6 = mean(abs(pred6 - laggedpowered6$production))

pred.test1 = predict(fit.sppow1.pdl, as.matrix(sppowlag.test1.pdb), s = "lambda.1se")
pred.test2 = predict(fit.sppow2.pdl, as.matrix(sppowlag.test2.pdb), s = "lambda.1se")
pred.test3 = predict(fit.sppow3.pdl, as.matrix(sppowlag.test3.pdb), s = "lambda.1se")
pred.test4 = predict(fit.sppow4.pdl, as.matrix(sppowlag.test4.pdb), s = "lambda.1se")
pred.test5 = predict(fit.sppow5.pdl, as.matrix(sppowlag.test5.pdb), s = "lambda.1se")
pred.test6 = predict(fit.sppow6.pdl, as.matrix(sppowlag.test6.pdb), s = "lambda.1se")

error.test1 = mean(abs(pred.test1 - laggedpowered.test1$production))
error.test2 = mean(abs(pred.test2 - laggedpowered.test2$production))
error.test3 = mean(abs(pred.test3 - laggedpowered.test3$production))
error.test4 = mean(abs(pred.test4 - laggedpowered.test4$production))
error.test5 = mean(abs(pred.test5 - laggedpowered.test5$production))
error.test6 = mean(abs(pred.test6 - laggedpowered.test6$production))

res1 = data.table(station = "aliaga", model = "lagged + powered/pdl", train_error = error.train1, test_error = error.test1)
res2 = data.table(station = "bares",  model = "lagged + powered/pdl", train_error = error.train2, test_error = error.test2)
res3 = data.table(station = "dinar",  model = "lagged + powered/pdl", train_error = error.train3, test_error = error.test3)
res4 = data.table(station = "geycek", model = "lagged + powered/pdl", train_error = error.train4, test_error = error.test4)
res5 = data.table(station = "soke",   model = "lagged + powered/pdl", train_error = error.train5, test_error = error.test5)
res6 = data.table(station = "soma",   model = "lagged + powered/pdl", train_error = error.train6, test_error = error.test6)

results.sparse.pd = rbind(results.sparse.pd, res1)
results.sparse.pd = rbind(results.sparse.pd, res2)
results.sparse.pd = rbind(results.sparse.pd, res3)
results.sparse.pd = rbind(results.sparse.pd, res4)
results.sparse.pd = rbind(results.sparse.pd, res5)
results.sparse.pd = rbind(results.sparse.pd, res6)

results.sparse.pd[order(station)]


# compare dimension of 2 methods for all ---------------------------------------
# Applying nearPD to the product reduces the dimension more
# lag+power
paste(ncol(sppowlag1.pdl), ncol(sppowlag1.pdb), sep= " vs ")
paste(ncol(sppowlag2.pdl), ncol(sppowlag2.pdb), sep= " vs ")
paste(ncol(sppowlag3.pdl), ncol(sppowlag3.pdb), sep= " vs ")
paste(ncol(sppowlag4.pdl), ncol(sppowlag4.pdb), sep= " vs ")
paste(ncol(sppowlag5.pdl), ncol(sppowlag5.pdb), sep= " vs ")
paste(ncol(sppowlag6.pdl), ncol(sppowlag6.pdb), sep= " vs ")
# lag
paste(ncol(spkronlag1.pdl), ncol(spkronlag1.pdb), sep= " vs ")
paste(ncol(spkronlag2.pdl), ncol(spkronlag2.pdb), sep= " vs ")
paste(ncol(spkronlag3.pdl), ncol(spkronlag3.pdb), sep= " vs ")
paste(ncol(spkronlag4.pdl), ncol(spkronlag4.pdb), sep= " vs ")
paste(ncol(spkronlag5.pdl), ncol(spkronlag5.pdb), sep= " vs ")
paste(ncol(spkronlag6.pdl), ncol(spkronlag6.pdb), sep= " vs ")





