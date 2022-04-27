# PCA Normal


# Results Table -----------------------------------------------------------
results.pca = data.table(station = character(),
                         model = character(),
                         train_error = numeric(),
                         test_error = numeric())

results.pca[order(station)]

# Base Model ----------------------------------------------------------------
pca1 = prcomp(train1[,4:12])
pca2 = prcomp(train2[,4:19])
pca3 = prcomp(train3[,4:19])
pca4 = prcomp(train4[,4:19])
pca5 = prcomp(train5[,4:19])
pca6 = prcomp(train6[,4:15])

# plot(pca1$sdev / sum(pca1$sdev)*100)
# plot(pca2$sdev / sum(pca2$sdev)*100)
# plot(pca3$sdev / sum(pca3$sdev)*100)
# plot(pca4$sdev / sum(pca4$sdev)*100)
# plot(pca5$sdev / sum(pca5$sdev)*100)
# plot(pca6$sdev / sum(pca6$sdev)*100)

npc1 = pcPicker(eigenvalues = pca1$sdev, var_wanted = 0.99)
npc2 = pcPicker(eigenvalues = pca2$sdev, var_wanted = 0.99)
npc3 = pcPicker(eigenvalues = pca3$sdev, var_wanted = 0.99)
npc4 = pcPicker(eigenvalues = pca4$sdev, var_wanted = 0.99)
npc5 = pcPicker(eigenvalues = pca5$sdev, var_wanted = 0.99)
npc6 = pcPicker(eigenvalues = pca6$sdev, var_wanted = 0.99)

reduced1 = pca1$x[,1:npc1]
reduced2 = pca2$x[,1:npc2]
reduced3 = pca3$x[,1:npc3]
reduced4 = pca4$x[,1:npc4]
reduced5 = pca5$x[,1:npc5]
reduced6 = pca6$x[,1:npc6]

reduced.test1 = predict(pca1, test1[,4:12])[,1:npc1]
reduced.test2 = predict(pca2, test2[,4:19])[,1:npc2]
reduced.test3 = predict(pca3, test3[,4:19])[,1:npc3]
reduced.test4 = predict(pca4, test4[,4:19])[,1:npc4]
reduced.test5 = predict(pca5, test5[,4:19])[,1:npc5]
reduced.test6 = predict(pca6, test6[,4:15])[,1:npc6]


# Fitting Base Model ------------------------------------------------------
fit.pca1 = cv.glmnet(x = as.matrix(reduced1), y = train1$production, type.measure = "mae")
fit.pca2 = cv.glmnet(x = as.matrix(reduced2), y = train2$production, type.measure = "mae")
fit.pca3 = cv.glmnet(x = as.matrix(reduced3), y = train3$production, type.measure = "mae")
fit.pca4 = cv.glmnet(x = as.matrix(reduced4), y = train4$production, type.measure = "mae")
fit.pca5 = cv.glmnet(x = as.matrix(reduced5), y = train5$production, type.measure = "mae")
fit.pca6 = cv.glmnet(x = as.matrix(reduced6), y = train6$production, type.measure = "mae")

pred1 = predict(fit.pca1, as.matrix(reduced1), s = "lambda.1se")
pred2 = predict(fit.pca2, as.matrix(reduced2), s = "lambda.1se")
pred3 = predict(fit.pca3, as.matrix(reduced3), s = "lambda.1se")
pred4 = predict(fit.pca4, as.matrix(reduced4), s = "lambda.1se")
pred5 = predict(fit.pca5, as.matrix(reduced5), s = "lambda.1se")
pred6 = predict(fit.pca6, as.matrix(reduced6), s = "lambda.1se")

error.train1 = mean(abs(pred1 - train1$production))
error.train2 = mean(abs(pred2 - train2$production))
error.train3 = mean(abs(pred3 - train3$production))
error.train4 = mean(abs(pred4 - train4$production))
error.train5 = mean(abs(pred5 - train5$production))
error.train6 = mean(abs(pred6 - train6$production))

pred.test1 = predict(fit.pca1, as.matrix(reduced.test1), s = "lambda.1se")
pred.test2 = predict(fit.pca2, as.matrix(reduced.test2), s = "lambda.1se")
pred.test3 = predict(fit.pca3, as.matrix(reduced.test3), s = "lambda.1se")
pred.test4 = predict(fit.pca4, as.matrix(reduced.test4), s = "lambda.1se")
pred.test5 = predict(fit.pca5, as.matrix(reduced.test5), s = "lambda.1se")
pred.test6 = predict(fit.pca6, as.matrix(reduced.test6), s = "lambda.1se")

error.test1 = mean(abs(pred.test1 - test1$production))
error.test2 = mean(abs(pred.test2 - test2$production))
error.test3 = mean(abs(pred.test3 - test3$production))
error.test4 = mean(abs(pred.test4 - test4$production))
error.test5 = mean(abs(pred.test5 - test5$production))
error.test6 = mean(abs(pred.test6 - test6$production))

res1 = data.table(station = "aliaga", model = "base", train_error = error.train1, test_error = error.test1)
res2 = data.table(station = "bares",  model = "base", train_error = error.train2, test_error = error.test2)
res3 = data.table(station = "dinar",  model = "base", train_error = error.train3, test_error = error.test3)
res4 = data.table(station = "geycek", model = "base", train_error = error.train4, test_error = error.test4)
res5 = data.table(station = "soke",   model = "base", train_error = error.train5, test_error = error.test5)
res6 = data.table(station = "soma",   model = "base", train_error = error.train6, test_error = error.test6)

results.pca = rbind(results.pca, res1)
results.pca = rbind(results.pca, res2)
results.pca = rbind(results.pca, res3)
results.pca = rbind(results.pca, res4)
results.pca = rbind(results.pca, res5)
results.pca = rbind(results.pca, res6)


# Lagged ------------------------------------------------------------
lagpca1 = prcomp(lagged1[,4:66])
lagpca2 = prcomp(lagged2[,4:115])
lagpca3 = prcomp(lagged3[,4:115])
lagpca4 = prcomp(lagged4[,4:115])
lagpca5 = prcomp(lagged5[,4:115])
lagpca6 = prcomp(lagged6[,4:87])

# plot(lagpca1$sdev / sum(lagpca1$sdev)*100)
# plot(lagpca2$sdev / sum(lagpca2$sdev)*100)
# plot(lagpca3$sdev / sum(lagpca3$sdev)*100)
# plot(lagpca4$sdev / sum(lagpca4$sdev)*100)
# plot(lagpca5$sdev / sum(lagpca5$sdev)*100)
# plot(lagpca6$sdev / sum(lagpca6$sdev)*100)

npc1 = pcPicker(eigenvalues = lagpca1$sdev, var_wanted = 0.99)
npc2 = pcPicker(eigenvalues = lagpca2$sdev, var_wanted = 0.99)
npc3 = pcPicker(eigenvalues = lagpca3$sdev, var_wanted = 0.99)
npc4 = pcPicker(eigenvalues = lagpca4$sdev, var_wanted = 0.99)
npc5 = pcPicker(eigenvalues = lagpca5$sdev, var_wanted = 0.99)
npc6 = pcPicker(eigenvalues = lagpca6$sdev, var_wanted = 0.99)

lagreduced1 = lagpca1$x[,1:npc1]
lagreduced2 = lagpca2$x[,1:npc2]
lagreduced3 = lagpca3$x[,1:npc3]
lagreduced4 = lagpca4$x[,1:npc4]
lagreduced5 = lagpca5$x[,1:npc5]
lagreduced6 = lagpca6$x[,1:npc6]

lagreduced.test1 = predict(lagpca1, lagged.test1[,4:66])[,1:npc1]
lagreduced.test2 = predict(lagpca2, lagged.test2[,4:115])[,1:npc2]
lagreduced.test3 = predict(lagpca3, lagged.test3[,4:115])[,1:npc3]
lagreduced.test4 = predict(lagpca4, lagged.test4[,4:115])[,1:npc4]
lagreduced.test5 = predict(lagpca5, lagged.test5[,4:115])[,1:npc5]
lagreduced.test6 = predict(lagpca6, lagged.test6[,4:87])[,1:npc6]


# Fitting Lagged ----------------------------------------------------------
fit.lagpca1 = cv.glmnet(x = as.matrix(lagreduced1), y = lagged1$production, type.measure = "mae")
fit.lagpca2 = cv.glmnet(x = as.matrix(lagreduced2), y = lagged2$production, type.measure = "mae")
fit.lagpca3 = cv.glmnet(x = as.matrix(lagreduced3), y = lagged3$production, type.measure = "mae")
fit.lagpca4 = cv.glmnet(x = as.matrix(lagreduced4), y = lagged4$production, type.measure = "mae")
fit.lagpca5 = cv.glmnet(x = as.matrix(lagreduced5), y = lagged5$production, type.measure = "mae")
fit.lagpca6 = cv.glmnet(x = as.matrix(lagreduced6), y = lagged6$production, type.measure = "mae")

pred1 = predict(fit.lagpca1, as.matrix(lagreduced1), s = "lambda.1se")
pred2 = predict(fit.lagpca2, as.matrix(lagreduced2), s = "lambda.1se")
pred3 = predict(fit.lagpca3, as.matrix(lagreduced3), s = "lambda.1se")
pred4 = predict(fit.lagpca4, as.matrix(lagreduced4), s = "lambda.1se")
pred5 = predict(fit.lagpca5, as.matrix(lagreduced5), s = "lambda.1se")
pred6 = predict(fit.lagpca6, as.matrix(lagreduced6), s = "lambda.1se")

error.train1 = mean(abs(pred1 - lagged1$production))
error.train2 = mean(abs(pred2 - lagged2$production))
error.train3 = mean(abs(pred3 - lagged3$production))
error.train4 = mean(abs(pred4 - lagged4$production))
error.train5 = mean(abs(pred5 - lagged5$production))
error.train6 = mean(abs(pred6 - lagged6$production))

pred.test1 = predict(fit.lagpca1, as.matrix(lagreduced.test1), s = "lambda.1se")
pred.test2 = predict(fit.lagpca2, as.matrix(lagreduced.test2), s = "lambda.1se")
pred.test3 = predict(fit.lagpca3, as.matrix(lagreduced.test3), s = "lambda.1se")
pred.test4 = predict(fit.lagpca4, as.matrix(lagreduced.test4), s = "lambda.1se")
pred.test5 = predict(fit.lagpca5, as.matrix(lagreduced.test5), s = "lambda.1se")
pred.test6 = predict(fit.lagpca6, as.matrix(lagreduced.test6), s = "lambda.1se")

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

results.pca = rbind(results.pca, res1)
results.pca = rbind(results.pca, res2)
results.pca = rbind(results.pca, res3)
results.pca = rbind(results.pca, res4)
results.pca = rbind(results.pca, res5)
results.pca = rbind(results.pca, res6)


# Powered -----------------------------------------------------------------
powerpca1 = prcomp(powered1[,4:30])
powerpca2 = prcomp(powered2[,4:51])
powerpca3 = prcomp(powered3[,4:51])
powerpca4 = prcomp(powered4[,4:51])
powerpca5 = prcomp(powered5[,4:51])
powerpca6 = prcomp(powered6[,4:39])

# plot(powerpca1$sdev / sum(powerpca1$sdev)*100)
# plot(powerpca2$sdev / sum(powerpca2$sdev)*100)
# plot(powerpca3$sdev / sum(powerpca3$sdev)*100)
# plot(powerpca4$sdev / sum(powerpca4$sdev)*100)
# plot(powerpca5$sdev / sum(powerpca5$sdev)*100)
# plot(powerpca6$sdev / sum(powerpca6$sdev)*100)

npc1 = pcPicker(eigenvalues = powerpca1$sdev, var_wanted = 0.99)
npc2 = pcPicker(eigenvalues = powerpca2$sdev, var_wanted = 0.99)
npc3 = pcPicker(eigenvalues = powerpca3$sdev, var_wanted = 0.99)
npc4 = pcPicker(eigenvalues = powerpca4$sdev, var_wanted = 0.99)
npc5 = pcPicker(eigenvalues = powerpca5$sdev, var_wanted = 0.99)
npc6 = pcPicker(eigenvalues = powerpca6$sdev, var_wanted = 0.99)

powerreduced1 = powerpca1$x[,1:npc1]
powerreduced2 = powerpca2$x[,1:npc2]
powerreduced3 = powerpca3$x[,1:npc3]
powerreduced4 = powerpca4$x[,1:npc4]
powerreduced5 = powerpca5$x[,1:npc5]
powerreduced6 = powerpca6$x[,1:npc6]

powerreduced.test1 = predict(powerpca1, powered.test1[,4:30])[,1:npc1]
powerreduced.test2 = predict(powerpca2, powered.test2[,4:51])[,1:npc2]
powerreduced.test3 = predict(powerpca3, powered.test3[,4:51])[,1:npc3]
powerreduced.test4 = predict(powerpca4, powered.test4[,4:51])[,1:npc4]
powerreduced.test5 = predict(powerpca5, powered.test5[,4:51])[,1:npc5]
powerreduced.test6 = predict(powerpca6, powered.test6[,4:39])[,1:npc6]


# Fitting Powered ---------------------------------------------------------------
fit.powerpca1 = cv.glmnet(x = as.matrix(powerreduced1), y = powered1$production, type.measure = "mae")
fit.powerpca2 = cv.glmnet(x = as.matrix(powerreduced2), y = powered2$production, type.measure = "mae")
fit.powerpca3 = cv.glmnet(x = as.matrix(powerreduced3), y = powered3$production, type.measure = "mae")
fit.powerpca4 = cv.glmnet(x = as.matrix(powerreduced4), y = powered4$production, type.measure = "mae")
fit.powerpca5 = cv.glmnet(x = as.matrix(powerreduced5), y = powered5$production, type.measure = "mae")
fit.powerpca6 = cv.glmnet(x = as.matrix(powerreduced6), y = powered6$production, type.measure = "mae")

pred1 = predict(fit.powerpca1, as.matrix(powerreduced1), s = "lambda.1se")
pred2 = predict(fit.powerpca2, as.matrix(powerreduced2), s = "lambda.1se")
pred3 = predict(fit.powerpca3, as.matrix(powerreduced3), s = "lambda.1se")
pred4 = predict(fit.powerpca4, as.matrix(powerreduced4), s = "lambda.1se")
pred5 = predict(fit.powerpca5, as.matrix(powerreduced5), s = "lambda.1se")
pred6 = predict(fit.powerpca6, as.matrix(powerreduced6), s = "lambda.1se")

error.train1 = mean(abs(pred1 - powered1$production))
error.train2 = mean(abs(pred2 - powered2$production))
error.train3 = mean(abs(pred3 - powered3$production))
error.train4 = mean(abs(pred4 - powered4$production))
error.train5 = mean(abs(pred5 - powered5$production))
error.train6 = mean(abs(pred6 - powered6$production))

pred.test1 = predict(fit.powerpca1, as.matrix(powerreduced.test1), s = "lambda.1se")
pred.test2 = predict(fit.powerpca2, as.matrix(powerreduced.test2), s = "lambda.1se")
pred.test3 = predict(fit.powerpca3, as.matrix(powerreduced.test3), s = "lambda.1se")
pred.test4 = predict(fit.powerpca4, as.matrix(powerreduced.test4), s = "lambda.1se")
pred.test5 = predict(fit.powerpca5, as.matrix(powerreduced.test5), s = "lambda.1se")
pred.test6 = predict(fit.powerpca6, as.matrix(powerreduced.test6), s = "lambda.1se")

error.test1 = mean(abs(pred.test1 - powered.test1$production))
error.test2 = mean(abs(pred.test2 - powered.test2$production))
error.test3 = mean(abs(pred.test3 - powered.test3$production))
error.test4 = mean(abs(pred.test4 - powered.test4$production))
error.test5 = mean(abs(pred.test5 - powered.test5$production))
error.test6 = mean(abs(pred.test6 - powered.test6$production))

res1 = data.table(station = "aliaga", model = "powered", train_error = error.train1, test_error = error.test1)
res2 = data.table(station = "bares",  model = "powered", train_error = error.train2, test_error = error.test2)
res3 = data.table(station = "dinar",  model = "powered", train_error = error.train3, test_error = error.test3)
res4 = data.table(station = "geycek", model = "powered", train_error = error.train4, test_error = error.test4)
res5 = data.table(station = "soke",   model = "powered", train_error = error.train5, test_error = error.test5)
res6 = data.table(station = "soma",   model = "powered", train_error = error.train6, test_error = error.test6)

results.pca = rbind(results.pca, res1)
results.pca = rbind(results.pca, res2)
results.pca = rbind(results.pca, res3)
results.pca = rbind(results.pca, res4)
results.pca = rbind(results.pca, res5)
results.pca = rbind(results.pca, res6)



# Lagged and Powered -----------------------------------------------------------
lagpowerpca1 = prcomp(laggedpowered1[,4:192])
lagpowerpca2 = prcomp(laggedpowered2[,4:339])
lagpowerpca3 = prcomp(laggedpowered3[,4:339])
lagpowerpca4 = prcomp(laggedpowered4[,4:339])
lagpowerpca5 = prcomp(laggedpowered5[,4:339])
lagpowerpca6 = prcomp(laggedpowered6[,4:255])

# plot(lagpowerpca1$sdev / sum(lagpowerpca1$sdev)*100)
# plot(lagpowerpca2$sdev / sum(lagpowerpca2$sdev)*100)
# plot(lagpowerpca3$sdev / sum(lagpowerpca3$sdev)*100)
# plot(lagpowerpca4$sdev / sum(lagpowerpca4$sdev)*100)
# plot(lagpowerpca5$sdev / sum(lagpowerpca5$sdev)*100)
# plot(lagpowerpca6$sdev / sum(lagpowerpca6$sdev)*100)

npc1 = pcPicker(eigenvalues = lagpowerpca1$sdev, var_wanted = 0.99)
npc2 = pcPicker(eigenvalues = lagpowerpca2$sdev, var_wanted = 0.99)
npc3 = pcPicker(eigenvalues = lagpowerpca3$sdev, var_wanted = 0.99)
npc4 = pcPicker(eigenvalues = lagpowerpca4$sdev, var_wanted = 0.99)
npc5 = pcPicker(eigenvalues = lagpowerpca5$sdev, var_wanted = 0.99)
npc6 = pcPicker(eigenvalues = lagpowerpca6$sdev, var_wanted = 0.99)

lagpowerreduced1 = lagpowerpca1$x[,1:npc1]
lagpowerreduced2 = lagpowerpca2$x[,1:npc2]
lagpowerreduced3 = lagpowerpca3$x[,1:npc3]
lagpowerreduced4 = lagpowerpca4$x[,1:npc4]
lagpowerreduced5 = lagpowerpca5$x[,1:npc5]
lagpowerreduced6 = lagpowerpca6$x[,1:npc6]

lagpowerreduced.test1 = predict(lagpowerpca1, laggedpowered.test1[,4:192])[,1:npc1]
lagpowerreduced.test2 = predict(lagpowerpca2, laggedpowered.test2[,4:339])[,1:npc2]
lagpowerreduced.test3 = predict(lagpowerpca3, laggedpowered.test3[,4:339])[,1:npc3]
lagpowerreduced.test4 = predict(lagpowerpca4, laggedpowered.test4[,4:339])[,1:npc4]
lagpowerreduced.test5 = predict(lagpowerpca5, laggedpowered.test5[,4:339])[,1:npc5]
lagpowerreduced.test6 = predict(lagpowerpca6, laggedpowered.test6[,4:255])[,1:npc6]


# Fitting Lagged and Powered -------------------------------------------------
fit.lagpowerpca1 = cv.glmnet(x = as.matrix(lagpowerreduced1), y = laggedpowered1$production, type.measure = "mae")
fit.lagpowerpca2 = cv.glmnet(x = as.matrix(lagpowerreduced2), y = laggedpowered2$production, type.measure = "mae")
fit.lagpowerpca3 = cv.glmnet(x = as.matrix(lagpowerreduced3), y = laggedpowered3$production, type.measure = "mae")
fit.lagpowerpca4 = cv.glmnet(x = as.matrix(lagpowerreduced4), y = laggedpowered4$production, type.measure = "mae")
fit.lagpowerpca5 = cv.glmnet(x = as.matrix(lagpowerreduced5), y = laggedpowered5$production, type.measure = "mae")
fit.lagpowerpca6 = cv.glmnet(x = as.matrix(lagpowerreduced6), y = laggedpowered6$production, type.measure = "mae")

pred1 = predict(fit.lagpowerpca1, as.matrix(lagpowerreduced1), s = "lambda.1se")
pred2 = predict(fit.lagpowerpca2, as.matrix(lagpowerreduced2), s = "lambda.1se")
pred3 = predict(fit.lagpowerpca3, as.matrix(lagpowerreduced3), s = "lambda.1se")
pred4 = predict(fit.lagpowerpca4, as.matrix(lagpowerreduced4), s = "lambda.1se")
pred5 = predict(fit.lagpowerpca5, as.matrix(lagpowerreduced5), s = "lambda.1se")
pred6 = predict(fit.lagpowerpca6, as.matrix(lagpowerreduced6), s = "lambda.1se")

error.train1 = mean(abs(pred1 - laggedpowered1$production))
error.train2 = mean(abs(pred2 - laggedpowered2$production))
error.train3 = mean(abs(pred3 - laggedpowered3$production))
error.train4 = mean(abs(pred4 - laggedpowered4$production))
error.train5 = mean(abs(pred5 - laggedpowered5$production))
error.train6 = mean(abs(pred6 - laggedpowered6$production))

pred.test1 = predict(fit.lagpowerpca1, as.matrix(lagpowerreduced.test1), s = "lambda.1se")
pred.test2 = predict(fit.lagpowerpca2, as.matrix(lagpowerreduced.test2), s = "lambda.1se")
pred.test3 = predict(fit.lagpowerpca3, as.matrix(lagpowerreduced.test3), s = "lambda.1se")
pred.test4 = predict(fit.lagpowerpca4, as.matrix(lagpowerreduced.test4), s = "lambda.1se")
pred.test5 = predict(fit.lagpowerpca5, as.matrix(lagpowerreduced.test5), s = "lambda.1se")
pred.test6 = predict(fit.lagpowerpca6, as.matrix(lagpowerreduced.test6), s = "lambda.1se")

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

results.pca = rbind(results.pca, res1)
results.pca = rbind(results.pca, res2)
results.pca = rbind(results.pca, res3)
results.pca = rbind(results.pca, res4)
results.pca = rbind(results.pca, res5)
results.pca = rbind(results.pca, res6)

results.pca[order(station)]



