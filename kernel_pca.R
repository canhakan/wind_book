# Kernel PCA (KPCA)

require(kernlab)

# Results Table -----------------------------------------------------------

results.kernel = data.table(station = character(),
                         model = character(),
                         train_error = numeric(),
                         test_error = numeric())

results.kernel[order(station)]


results



# Split to 5 pieces (5er 5er artan sequencelar) ---------------------------

i1.1 = seq(1,nrow(t1),by=5)
i2.1 = seq(1,nrow(t2),by=5)
i3.1 = seq(1,nrow(t3),by=5)
i4.1 = seq(1,nrow(t4),by=5)
i5.1 = seq(1,nrow(t5),by=5)
i6.1 = seq(1,nrow(t6),by=5)

i1.2 = seq(2,nrow(t1),by=5)
i2.2 = seq(2,nrow(t2),by=5)
i3.2 = seq(2,nrow(t3),by=5)
i4.2 = seq(2,nrow(t4),by=5)
i5.2 = seq(2,nrow(t5),by=5)
i6.2 = seq(2,nrow(t6),by=5)

i1.3 = seq(3,nrow(t1),by=5)
i2.3 = seq(3,nrow(t2),by=5)
i3.3 = seq(3,nrow(t3),by=5)
i4.3 = seq(3,nrow(t4),by=5)
i5.3 = seq(3,nrow(t5),by=5)
i6.3 = seq(3,nrow(t6),by=5)

i1.4 = seq(4,nrow(t1),by=5)
i2.4 = seq(4,nrow(t2),by=5)
i3.4 = seq(4,nrow(t3),by=5)
i4.4 = seq(4,nrow(t4),by=5)
i5.4 = seq(4,nrow(t5),by=5)
i6.4 = seq(4,nrow(t6),by=5)

i1.5 = seq(5,nrow(t1),by=5)
i2.5 = seq(5,nrow(t2),by=5)
i3.5 = seq(5,nrow(t3),by=5)
i4.5 = seq(5,nrow(t4),by=5)
i5.5 = seq(5,nrow(t5),by=5)
i6.5 = seq(5,nrow(t6),by=5)
#
t1.1 = t1[i1.1,]
t2.1 = t2[i2.1,]
t3.1 = t3[i3.1,]
t4.1 = t4[i4.1,]
t5.1 = t5[i5.1,]
t6.1 = t6[i6.1,]

t1.2 = t1[i1.2,]
t2.2 = t2[i2.2,]
t3.2 = t3[i3.2,]
t4.2 = t4[i4.2,]
t5.2 = t5[i5.2,]
t6.2 = t6[i6.2,]

t1.3 = t1[i1.3,]
t2.3 = t2[i2.3,]
t3.3 = t3[i3.3,]
t4.3 = t4[i4.3,]
t5.3 = t5[i5.3,]
t6.3 = t6[i6.3,]

t1.4 = t1[i1.4,]
t2.4 = t2[i2.4,]
t3.4 = t3[i3.4,]
t4.4 = t4[i4.4,]
t5.4 = t5[i5.4,]
t6.4 = t6[i6.4,]

t1.5 = t1[i1.5,]
t2.5 = t2[i2.5,]
t3.5 = t3[i3.5,]
t4.5 = t4[i4.5,]
t5.5 = t5[i5.5,]
t6.5 = t6[i6.5,]
#
i1.1 = seq(1,nrow(te1),by=5)
i2.1 = seq(1,nrow(te2),by=5)
i3.1 = seq(1,nrow(te3),by=5)
i4.1 = seq(1,nrow(te4),by=5)
i5.1 = seq(1,nrow(te5),by=5)
i6.1 = seq(1,nrow(te6),by=5)

i1.2 = seq(2,nrow(te1),by=5)
i2.2 = seq(2,nrow(te2),by=5)
i3.2 = seq(2,nrow(te3),by=5)
i4.2 = seq(2,nrow(te4),by=5)
i5.2 = seq(2,nrow(te5),by=5)
i6.2 = seq(2,nrow(te6),by=5)

i1.3 = seq(3,nrow(te1),by=5)
i2.3 = seq(3,nrow(te2),by=5)
i3.3 = seq(3,nrow(te3),by=5)
i4.3 = seq(3,nrow(te4),by=5)
i5.3 = seq(3,nrow(te5),by=5)
i6.3 = seq(3,nrow(te6),by=5)

i1.4 = seq(4,nrow(te1),by=5)
i2.4 = seq(4,nrow(te2),by=5)
i3.4 = seq(4,nrow(te3),by=5)
i4.4 = seq(4,nrow(te4),by=5)
i5.4 = seq(4,nrow(te5),by=5)
i6.4 = seq(4,nrow(te6),by=5)

i1.5 = seq(5,nrow(te1),by=5)
i2.5 = seq(5,nrow(te2),by=5)
i3.5 = seq(5,nrow(te3),by=5)
i4.5 = seq(5,nrow(te4),by=5)
i5.5 = seq(5,nrow(te5),by=5)
i6.5 = seq(5,nrow(te6),by=5)
#
te1.1 = te1[i1.1,]
te2.1 = te2[i2.1,]
te3.1 = te3[i3.1,]
te4.1 = te4[i4.1,]
te5.1 = te5[i5.1,]
te6.1 = te6[i6.1,]

te1.2 = te1[i1.2,]
te2.2 = te2[i2.2,]
te3.2 = te3[i3.2,]
te4.2 = te4[i4.2,]
te5.2 = te5[i5.2,]
te6.2 = te6[i6.2,]

te1.3 = te1[i1.3,]
te2.3 = te2[i2.3,]
te3.3 = te3[i3.3,]
te4.3 = te4[i4.3,]
te5.3 = te5[i5.3,]
te6.3 = te6[i6.3,]

te1.4 = te1[i1.4,]
te2.4 = te2[i2.4,]
te3.4 = te3[i3.4,]
te4.4 = te4[i4.4,]
te5.4 = te5[i5.4,]
te6.4 = te6[i6.4,]

te1.5 = te1[i1.5,]
te2.5 = te2[i2.5,]
te3.5 = te3[i3.5,]
te4.5 = te4[i4.5,]
te5.5 = te5[i5.5,]
te6.5 = te6[i6.5,]


# Kernel PCA on 1/5th of datas --------------------------------------------
kp1.1 = kpca(as.matrix(t1.1), kernel = "polydot", kpar = list(degree = 3, offset = 1))
kp2.1 = kpca(as.matrix(t2.1), kernel = "polydot", kpar = list(degree = 3, offset = 1))
kp3.1 = kpca(as.matrix(t3.1), kernel = "polydot", kpar = list(degree = 3, offset = 1))
kp4.1 = kpca(as.matrix(t4.1), kernel = "polydot", kpar = list(degree = 3, offset = 1))
kp5.1 = kpca(as.matrix(t5.1), kernel = "polydot", kpar = list(degree = 3, offset = 1))
kp6.1 = kpca(as.matrix(t6.1), kernel = "polydot", kpar = list(degree = 3, offset = 1))

kp1.2 = kpca(as.matrix(t1.2), kernel = "polydot", kpar = list(degree = 3, offset = 1))
kp2.2 = kpca(as.matrix(t2.2), kernel = "polydot", kpar = list(degree = 3, offset = 1))
kp3.2 = kpca(as.matrix(t3.2), kernel = "polydot", kpar = list(degree = 3, offset = 1))
kp4.2 = kpca(as.matrix(t4.2), kernel = "polydot", kpar = list(degree = 3, offset = 1))
kp5.2 = kpca(as.matrix(t5.2), kernel = "polydot", kpar = list(degree = 3, offset = 1))
kp6.2 = kpca(as.matrix(t6.2), kernel = "polydot", kpar = list(degree = 3, offset = 1))

kp1.3 = kpca(as.matrix(t1.3), kernel = "polydot", kpar = list(degree = 3, offset = 1))
kp2.3 = kpca(as.matrix(t2.3), kernel = "polydot", kpar = list(degree = 3, offset = 1))
kp3.3 = kpca(as.matrix(t3.3), kernel = "polydot", kpar = list(degree = 3, offset = 1))
kp4.3 = kpca(as.matrix(t4.3), kernel = "polydot", kpar = list(degree = 3, offset = 1))
kp5.3 = kpca(as.matrix(t5.3), kernel = "polydot", kpar = list(degree = 3, offset = 1))
kp6.3 = kpca(as.matrix(t6.3), kernel = "polydot", kpar = list(degree = 3, offset = 1))

kp1.4 = kpca(as.matrix(t1.4), kernel = "polydot", kpar = list(degree = 3, offset = 1))
kp2.4 = kpca(as.matrix(t2.4), kernel = "polydot", kpar = list(degree = 3, offset = 1))
kp3.4 = kpca(as.matrix(t3.4), kernel = "polydot", kpar = list(degree = 3, offset = 1))
kp4.4 = kpca(as.matrix(t4.4), kernel = "polydot", kpar = list(degree = 3, offset = 1))
kp5.4 = kpca(as.matrix(t5.4), kernel = "polydot", kpar = list(degree = 3, offset = 1))
kp6.4 = kpca(as.matrix(t6.4), kernel = "polydot", kpar = list(degree = 3, offset = 1))

kp1.5 = kpca(as.matrix(t1.5), kernel = "polydot", kpar = list(degree = 3, offset = 1))
kp2.5 = kpca(as.matrix(t2.5), kernel = "polydot", kpar = list(degree = 3, offset = 1))
kp3.5 = kpca(as.matrix(t3.5), kernel = "polydot", kpar = list(degree = 3, offset = 1))
kp4.5 = kpca(as.matrix(t4.5), kernel = "polydot", kpar = list(degree = 3, offset = 1))
kp5.5 = kpca(as.matrix(t5.5), kernel = "polydot", kpar = list(degree = 3, offset = 1))
kp6.5 = kpca(as.matrix(t6.5), kernel = "polydot", kpar = list(degree = 3, offset = 1))



# apply kpca to train and test --------------------------------------------

kp.t1.1 = predict(kp1.1, t1)
kp.t2.1 = predict(kp2.1, t2)
kp.t3.1 = predict(kp3.1, t3)
kp.t4.1 = predict(kp4.1, t4)
kp.t5.1 = predict(kp5.1, t5)
kp.t6.1 = predict(kp6.1, t6)

kp.te1.1 = predict(kp1.1, te1)
kp.te2.1 = predict(kp2.1, te2)
kp.te3.1 = predict(kp3.1, te3)
kp.te4.1 = predict(kp4.1, te4)
kp.te5.1 = predict(kp5.1, te5)
kp.te6.1 = predict(kp6.1, te6)


# should do the other partitions, too.

# glmnet ------------------------------------------------------------------
# 1 - glmnet with fold id's
# 2 - glmnet with minimum lambdas

# # 1 : fold ids
foldid1.1 = rep(c(1:10), length = nrow(kp.t1.1))
foldid2.1 = rep(c(1:10), length = nrow(kp.t2.1))
foldid3.1 = rep(c(1:10), length = nrow(kp.t3.1))
foldid4.1 = rep(c(1:10), length = nrow(kp.t4.1))
foldid5.1 = rep(c(1:10), length = nrow(kp.t5.1))
foldid6.1 = rep(c(1:10), length = nrow(kp.t6.1))
# fitting witb fold ids
a0 = Sys.time()
fit.id.kpca1.1 = cv.glmnet(x = kp.t1.1, y = train1$production, type.measure = "mae", foldid = foldid1.1)
fit.id.kpca2.1 = cv.glmnet(x = kp.t2.1, y = train2$production, type.measure = "mae", foldid = foldid2.1)
Sys.time() - a0  # 2 minutes
# fit.id.kpca3.1 = cv.glmnet(x = kp.t3.1, y = train3$production, type.measure = "mae", foldid = foldid3.1)
a0 = Sys.time()
fit.id.kpca4.1 = cv.glmnet(x = kp.t4.1, y = train4$production, type.measure = "mae", foldid = foldid4.1)
fit.id.kpca5.1 = cv.glmnet(x = kp.t5.1, y = train5$production, type.measure = "mae", foldid = foldid5.1)
fit.id.kpca6.1 = cv.glmnet(x = kp.t6.1, y = train6$production, type.measure = "mae", foldid = foldid6.1)
Sys.time() - a0 # hoping for 3 minutes
# plot lambda/cv-error
plot(fit.id.kpca1.1)
plot(fit.id.kpca2.1)
# plot(fit.id.kpca3.1)
plot(fit.id.kpca4.1)
plot(fit.id.kpca5.1)
plot(fit.id.kpca6.1)
# train error (min)
min(fit.id.kpca1.1$cvm)
min(fit.id.kpca2.1$cvm)
# min(fit.id.kpca3.1$cvm)
min(fit.id.kpca4.1$cvm)
min(fit.id.kpca5.1$cvm)
min(fit.id.kpca6.1$cvm)
# predict
pred.id.kpca1.1 = predict(fit.id.kpca1.1, kp.te1.1)
pred.id.kpca2.1 = predict(fit.id.kpca2.1, kp.te2.1)
# pred.id.kpca3.1 = predict(fit.id.kpca3.1, kp.te3.1)
pred.id.kpca4.1 = predict(fit.id.kpca4.1, kp.te4.1)
pred.id.kpca5.1 = predict(fit.id.kpca5.1, kp.te5.1)
pred.id.kpca6.1 = predict(fit.id.kpca6.1, kp.te6.1)
# test error
mean(abs(pred.id.kpca1.1 - test1$production))
mean(abs(pred.id.kpca2.1 - test2$production))
# mean(abs(pred.id.kpca3.1 - test3$production))
mean(abs(pred.id.kpca4.1 - test4$production))
mean(abs(pred.id.kpca5.1 - test5$production))
mean(abs(pred.id.kpca6.1 - test6$production))






