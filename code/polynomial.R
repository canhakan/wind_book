# Polynomial


# Results Table -----------------------------------------------------------

results.poly = data.table(station = character(),
                            model = character(),
                            train_error = numeric(),
                            test_error = numeric())

results.poly[order(station)]


# Poly Maker of degree 3 (Only Neighbors) Function ---------------------------------------------------------------
# only put features (no date, production etc.)
# nlat nlon for finding neighbors
poly3Make <- function(data, nlat, nlon){
    # her feature icin komsularini alip sadece onlari kullanarak polinom olusturuyoruz
    res = data[,1]*0
    for(i in c(1:ncol(data))){
        neis = neighborLister(nlat = nlat, nlon = nlon, i = i)
        a = data[,..neis]
        for(j1 in c(0:ncol(a))){
            for(j2 in c(max(1,j1):ncol(a))){
                if(j1 > 0){
                    res = cbind(res, data[,..i] * a[,..j1] * a[,..j2])
                }else{
                    res = cbind(res, data[,..i] * a[,..j2])
                }
            }
        }
    }
    res = res[,-1]
    return(res)
}
# Create Data ------------------------------------------------------------------

spoly1 = poly3Make(t1,3,3)
spoly2 = poly3Make(t2,4,4)
spoly3 = poly3Make(t3,4,4)
spoly4 = poly3Make(t4,4,4)
spoly5 = poly3Make(t5,4,4)
spoly6 = poly3Make(t6,3,4)
#
spoly.test1 = poly3Make(te1,3,3)
spoly.test2 = poly3Make(te2,4,4)
spoly.test3 = poly3Make(te3,4,4)
spoly.test4 = poly3Make(te4,4,4)
spoly.test5 = poly3Make(te5,4,4)
spoly.test6 = poly3Make(te6,3,4)


# glmnet ------------------------------------------------------------------
fit.spoly1 = cv.glmnet(x = as.matrix(spoly1), y = as.matrix(train1$production), type.measure = 'mae')
fit.spoly2 = cv.glmnet(x = as.matrix(spoly2), y = as.matrix(train2$production), type.measure = 'mae')
fit.spoly3 = cv.glmnet(x = as.matrix(spoly3), y = as.matrix(train3$production), type.measure = 'mae')
fit.spoly4 = cv.glmnet(x = as.matrix(spoly4), y = as.matrix(train4$production), type.measure = 'mae')
fit.spoly5 = cv.glmnet(x = as.matrix(spoly5), y = as.matrix(train5$production), type.measure = 'mae')
fit.spoly6 = cv.glmnet(x = as.matrix(spoly6), y = as.matrix(train6$production), type.measure = 'mae')

pred1 = predict(fit.spoly1, as.matrix(spoly1), s = "lambda.1se")
pred2 = predict(fit.spoly2, as.matrix(spoly1), s = "lambda.1se")
pred3 = predict(fit.spoly3, as.matrix(spoly1), s = "lambda.1se")
pred4 = predict(fit.spoly4, as.matrix(spoly1), s = "lambda.1se")
pred5 = predict(fit.spoly5, as.matrix(spoly1), s = "lambda.1se")
pred6 = predict(fit.spoly6, as.matrix(spoly1), s = "lambda.1se")

error.train1 = mean(abs(pred1 - train1$production))
error.train2 = mean(abs(pred2 - train2$production))
error.train3 = mean(abs(pred3 - train3$production))
error.train4 = mean(abs(pred4 - train4$production))
error.train5 = mean(abs(pred5 - train5$production))
error.train6 = mean(abs(pred6 - train6$production))

pred.test1 = predict(fit.spoly1, as.matrix(spoly.test1), s = "lambda.1se")
pred.test2 = predict(fit.spoly2, as.matrix(spoly.test2), s = "lambda.1se")
pred.test3 = predict(fit.spoly3, as.matrix(spoly.test3), s = "lambda.1se")
pred.test4 = predict(fit.spoly4, as.matrix(spoly.test4), s = "lambda.1se")
pred.test5 = predict(fit.spoly5, as.matrix(spoly.test5), s = "lambda.1se")
pred.test6 = predict(fit.spoly6, as.matrix(spoly.test6), s = "lambda.1se")

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

results.poly = rbind(results.poly, res1)
results.poly = rbind(results.poly, res2)
results.poly = rbind(results.poly, res3)
results.poly = rbind(results.poly, res4)
results.poly = rbind(results.poly, res5)
results.poly = rbind(results.poly, res6)
results.poly

plot(fit.spoly1)
plot(fit.spoly2)
plot(fit.spoly3)
plot(fit.spoly4)
plot(fit.spoly5)
plot(fit.spoly6)

dim(spoly1)
dim(spoly2)
dim(spoly3)
dim(spoly4)
dim(spoly5)
dim(spoly6)

# PCA on Poly3 Data -------------------------------------------------------
pca.spoly1 = prcomp(spoly1)
pca.spoly2 = prcomp(spoly2)
pca.spoly3 = prcomp(spoly3)
pca.spoly4 = prcomp(spoly4)
pca.spoly5 = prcomp(spoly5)
pca.spoly6 = prcomp(spoly6)

# plot(pca1$sdev / sum(pca1$sdev)*100)
# plot(pca2$sdev / sum(pca2$sdev)*100)
# plot(pca3$sdev / sum(pca3$sdev)*100)
# plot(pca4$sdev / sum(pca4$sdev)*100)
# plot(pca5$sdev / sum(pca5$sdev)*100)
# plot(pca6$sdev / sum(pca6$sdev)*100)

npc1 = pcPicker(eigenvalues = pca.spoly1$sdev, var_wanted = 0.99)
npc2 = pcPicker(eigenvalues = pca.spoly2$sdev, var_wanted = 0.99)
npc3 = pcPicker(eigenvalues = pca.spoly3$sdev, var_wanted = 0.99)
npc4 = pcPicker(eigenvalues = pca.spoly4$sdev, var_wanted = 0.99)
npc5 = pcPicker(eigenvalues = pca.spoly5$sdev, var_wanted = 0.99)
npc6 = pcPicker(eigenvalues = pca.spoly6$sdev, var_wanted = 0.99)

print(npc1)
print(npc2)
print(npc3)
print(npc4)
print(npc5)
print(npc6)

reduced.spoly1 = pca.spoly1$x[,1:npc1]
reduced.spoly2 = pca.spoly2$x[,1:npc2]
reduced.spoly3 = pca.spoly3$x[,1:npc3]
reduced.spoly4 = pca.spoly4$x[,1:npc4]
reduced.spoly5 = pca.spoly5$x[,1:npc5]
reduced.spoly6 = pca.spoly6$x[,1:npc6]

reduced.spoly.test1 = predict(pca.spoly1, spoly.test1)[,1:npc1]
reduced.spoly.test2 = predict(pca.spoly2, spoly.test2)[,1:npc2]
reduced.spoly.test3 = predict(pca.spoly3, spoly.test3)[,1:npc3]
reduced.spoly.test4 = predict(pca.spoly4, spoly.test4)[,1:npc4]
reduced.spoly.test5 = predict(pca.spoly5, spoly.test5)[,1:npc5]
reduced.spoly.test6 = predict(pca.spoly6, spoly.test6)[,1:npc6]


# Fit PCAd ----------------------------------------------------------------
fit.pca.spoly1 = cv.glmnet(x = as.matrix(reduced.spoly1), y = train1$production, type.measure = "mae")
fit.pca.spoly2 = cv.glmnet(x = as.matrix(reduced.spoly2), y = train2$production, type.measure = "mae")
fit.pca.spoly3 = cv.glmnet(x = as.matrix(reduced.spoly3), y = train3$production, type.measure = "mae")
fit.pca.spoly4 = cv.glmnet(x = as.matrix(reduced.spoly4), y = train4$production, type.measure = "mae")
fit.pca.spoly5 = cv.glmnet(x = as.matrix(reduced.spoly5), y = train5$production, type.measure = "mae")
fit.pca.spoly6 = cv.glmnet(x = as.matrix(reduced.spoly6), y = train6$production, type.measure = "mae")

pred1 = predict(fit.pca.spoly1, as.matrix(reduced.spoly1), s = "lambda.1se")
pred2 = predict(fit.pca.spoly2, as.matrix(reduced.spoly2), s = "lambda.1se")
pred3 = predict(fit.pca.spoly3, as.matrix(reduced.spoly3), s = "lambda.1se")
pred4 = predict(fit.pca.spoly4, as.matrix(reduced.spoly4), s = "lambda.1se")
pred5 = predict(fit.pca.spoly5, as.matrix(reduced.spoly5), s = "lambda.1se")
pred6 = predict(fit.pca.spoly6, as.matrix(reduced.spoly6), s = "lambda.1se")

error.train1 = mean(abs(pred1 - train1$production))
error.train2 = mean(abs(pred2 - train2$production))
error.train3 = mean(abs(pred3 - train3$production))
error.train4 = mean(abs(pred4 - train4$production))
error.train5 = mean(abs(pred5 - train5$production))
error.train6 = mean(abs(pred6 - train6$production))

pred.test1 = predict(fit.pca.spoly1, as.matrix(reduced.spoly.test1), s = "lambda.1se")
pred.test2 = predict(fit.pca.spoly2, as.matrix(reduced.spoly.test2), s = "lambda.1se")
pred.test3 = predict(fit.pca.spoly3, as.matrix(reduced.spoly.test3), s = "lambda.1se")
pred.test4 = predict(fit.pca.spoly4, as.matrix(reduced.spoly.test4), s = "lambda.1se")
pred.test5 = predict(fit.pca.spoly5, as.matrix(reduced.spoly.test5), s = "lambda.1se")
pred.test6 = predict(fit.pca.spoly6, as.matrix(reduced.spoly.test6), s = "lambda.1se")

error.test1 = mean(abs(pred.test1 - test1$production))
error.test2 = mean(abs(pred.test2 - test2$production))
error.test3 = mean(abs(pred.test3 - test3$production))
error.test4 = mean(abs(pred.test4 - test4$production))
error.test5 = mean(abs(pred.test5 - test5$production))
error.test6 = mean(abs(pred.test6 - test6$production))

res1 = data.table(station = "aliaga", model = "pca", train_error = error.train1, test_error = error.test1)
res2 = data.table(station = "bares",  model = "pca", train_error = error.train2, test_error = error.test2)
res3 = data.table(station = "dinar",  model = "pca", train_error = error.train3, test_error = error.test3)
res4 = data.table(station = "geycek", model = "pca", train_error = error.train4, test_error = error.test4)
res5 = data.table(station = "soke",   model = "pca", train_error = error.train5, test_error = error.test5)
res6 = data.table(station = "soma",   model = "pca", train_error = error.train6, test_error = error.test6)

results.poly = rbind(results.poly, res1)
results.poly = rbind(results.poly, res2)
results.poly = rbind(results.poly, res3)
results.poly = rbind(results.poly, res4)
results.poly = rbind(results.poly, res5)
results.poly = rbind(results.poly, res6)
results.poly[order(station)]

results[order(station)]
