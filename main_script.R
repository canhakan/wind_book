# Main


# book render -------------------------------------------------------------
bookdown::render_book("index.Rmd")
browseURL("docs/index.html")


# Libraries ---------------------------------------------------------------
require(dplyr)
require(data.table)
require(ggplot2)
require(glmnet)
# require(DT)
# require(caret)
require(plot.matrix)
require(kernlab)
require(tidyr)
# require(stringr)
require(tidyverse)
#
require(knitr)


# Load Data ---------------------------------------------------------------
data1 = read.csv(file = "/Users/canhakan/tez/models_main/stations/aliaga.csv")
data2 = read.csv(file = "/Users/canhakan/tez/models_main/stations/bares.csv")
data3 = read.csv(file = "/Users/canhakan/tez/models_main/stations/dinar.csv")
data4 = read.csv(file = "/Users/canhakan/tez/models_main/stations/geycek.csv")
data5 = read.csv(file = "/Users/canhakan/tez/models_main/stations/soke.csv")
data6 = read.csv(file = "/Users/canhakan/tez/models_main/stations/soma.csv")

data1 = data.table(data1)
data2 = data.table(data2)
data3 = data.table(data3)
data4 = data.table(data4)
data5 = data.table(data5)
data6 = data.table(data6)

# Column Names ------------------------------------------------------------
# for all data change var1 and last row
var1 = ncol(data6)-3
#
newnames = c("date","hour","production")
for(i in c(1:var1)){
    n = paste("loc",i,sep="")
    newnames = c(newnames,n)
}
#
colnames(data6) = newnames


# Derived Datasets -----------------------------------------------------------

# train/test
index1 = floor(nrow(data1)*0.75)
index2 = floor(nrow(data2)*0.75)
index3 = floor(nrow(data3)*0.75)
index4 = floor(nrow(data4)*0.75)
index5 = floor(nrow(data5)*0.75)
index6 = floor(nrow(data6)*0.75)

train1 = data1[1 : index1, ]
train2 = data2[1 : index2, ]
train3 = data3[1 : index3, ]
train4 = data4[1 : index4, ]
train5 = data5[1 : index5, ]
train6 = data6[1 : index6, ]

test1  = data1[index1 : nrow(data1), ]
test2  = data2[index2 : nrow(data2), ]
test3  = data3[index3 : nrow(data3), ]
test4  = data4[index4 : nrow(data4), ]
test5  = data5[index5 : nrow(data5), ]
test6  = data6[index6 : nrow(data6), ]

# lag
lagged1 = createLagged(data = train1, lags = c(-3:3), other.head = c(1:3), other.tail = 0)
lagged2 = createLagged(data = train2, lags = c(-3:3), other.head = c(1:3), other.tail = 0)
lagged3 = createLagged(data = train3, lags = c(-3:3), other.head = c(1:3), other.tail = 0)
lagged4 = createLagged(data = train4, lags = c(-3:3), other.head = c(1:3), other.tail = 0)
lagged5 = createLagged(data = train5, lags = c(-3:3), other.head = c(1:3), other.tail = 0)
lagged6 = createLagged(data = train6, lags = c(-3:3), other.head = c(1:3), other.tail = 0)

lagged.test1 = createLagged(data = test1, lags = c(-3:3), other.head = c(1:3), other.tail = 0)
lagged.test2 = createLagged(data = test2, lags = c(-3:3), other.head = c(1:3), other.tail = 0)
lagged.test3 = createLagged(data = test3, lags = c(-3:3), other.head = c(1:3), other.tail = 0)
lagged.test4 = createLagged(data = test4, lags = c(-3:3), other.head = c(1:3), other.tail = 0)
lagged.test5 = createLagged(data = test5, lags = c(-3:3), other.head = c(1:3), other.tail = 0)
lagged.test6 = createLagged(data = test6, lags = c(-3:3), other.head = c(1:3), other.tail = 0)

# power3
powered1 = createPowered(data = train1, powers = c(2:3), other.head = c(1:3), other.tail = 0)
powered2 = createPowered(data = train2, powers = c(2:3), other.head = c(1:3), other.tail = 0)
powered3 = createPowered(data = train3, powers = c(2:3), other.head = c(1:3), other.tail = 0)
powered4 = createPowered(data = train4, powers = c(2:3), other.head = c(1:3), other.tail = 0)
powered5 = createPowered(data = train5, powers = c(2:3), other.head = c(1:3), other.tail = 0)
powered6 = createPowered(data = train6, powers = c(2:3), other.head = c(1:3), other.tail = 0)

powered.test1 = createPowered(data = test1, powers = c(2:3), other.head = c(1:3), other.tail = 0)
powered.test2 = createPowered(data = test2, powers = c(2:3), other.head = c(1:3), other.tail = 0)
powered.test3 = createPowered(data = test3, powers = c(2:3), other.head = c(1:3), other.tail = 0)
powered.test4 = createPowered(data = test4, powers = c(2:3), other.head = c(1:3), other.tail = 0)
powered.test5 = createPowered(data = test5, powers = c(2:3), other.head = c(1:3), other.tail = 0)
powered.test6 = createPowered(data = test6, powers = c(2:3), other.head = c(1:3), other.tail = 0)
# temporal
temporal1 = createTemporal(train1,6,c(1:3))
temporal2 = createTemporal(train2,6,c(1:3))
temporal3 = createTemporal(train3,6,c(1:3))
temporal4 = createTemporal(train4,6,c(1:3))
temporal5 = createTemporal(train5,6,c(1:3))
temporal6 = createTemporal(train6,6,c(1:3))

temporal.test1 = createTemporal(test1,6,c(1:3))
temporal.test2 = createTemporal(test2,6,c(1:3))
temporal.test3 = createTemporal(test3,6,c(1:3))
temporal.test4 = createTemporal(test4,6,c(1:3))
temporal.test5 = createTemporal(test5,6,c(1:3))
temporal.test6 = createTemporal(test6,6,c(1:3))

# lag and power
laggedpowered1 = createLagged(data = powered1, lags = c(-3:3), other.head = c(1:3), other.tail = 0)
laggedpowered2 = createLagged(data = powered2, lags = c(-3:3), other.head = c(1:3), other.tail = 0)
laggedpowered3 = createLagged(data = powered3, lags = c(-3:3), other.head = c(1:3), other.tail = 0)
laggedpowered4 = createLagged(data = powered4, lags = c(-3:3), other.head = c(1:3), other.tail = 0)
laggedpowered5 = createLagged(data = powered5, lags = c(-3:3), other.head = c(1:3), other.tail = 0)
laggedpowered6 = createLagged(data = powered6, lags = c(-3:3), other.head = c(1:3), other.tail = 0)

laggedpowered.test1 = createLagged(data = powered.test1, lags = c(-3:3), other.head = c(1:3), other.tail = 0)
laggedpowered.test2 = createLagged(data = powered.test2, lags = c(-3:3), other.head = c(1:3), other.tail = 0)
laggedpowered.test3 = createLagged(data = powered.test3, lags = c(-3:3), other.head = c(1:3), other.tail = 0)
laggedpowered.test4 = createLagged(data = powered.test4, lags = c(-3:3), other.head = c(1:3), other.tail = 0)
laggedpowered.test5 = createLagged(data = powered.test5, lags = c(-3:3), other.head = c(1:3), other.tail = 0)
laggedpowered.test6 = createLagged(data = powered.test6, lags = c(-3:3), other.head = c(1:3), other.tail = 0)



# Results Table -----------------------------------------------------------
results = data.table(station = character(),
                     model = character(),
                     train_error = numeric(),
                     test_error = numeric())

results[order(station)]


# Base Model --------------------------------------------------------------
fit1 = cv.glmnet(x = as.matrix(train1[,4:12]), y = train1$production, type.measure = "mae")
fit2 = cv.glmnet(x = as.matrix(train2[,4:19]), y = train2$production, type.measure = "mae")
fit3 = cv.glmnet(x = as.matrix(train3[,4:19]), y = train3$production, type.measure = "mae")
fit4 = cv.glmnet(x = as.matrix(train4[,4:19]), y = train4$production, type.measure = "mae")
fit5 = cv.glmnet(x = as.matrix(train5[,4:19]), y = train5$production, type.measure = "mae")
fit6 = cv.glmnet(x = as.matrix(train6[,4:15]), y = train6$production, type.measure = "mae")

pred1 = predict(fit1, as.matrix(train1[,4:12]), s = "lambda.1se")
pred2 = predict(fit2, as.matrix(train2[,4:19]), s = "lambda.1se")
pred3 = predict(fit3, as.matrix(train3[,4:19]), s = "lambda.1se")
pred4 = predict(fit4, as.matrix(train4[,4:19]), s = "lambda.1se")
pred5 = predict(fit5, as.matrix(train5[,4:19]), s = "lambda.1se")
pred6 = predict(fit6, as.matrix(train6[,4:15]), s = "lambda.1se")

error.train1 = mean(abs(pred1 - train1$production))
error.train2 = mean(abs(pred2 - train2$production))
error.train3 = mean(abs(pred3 - train3$production))
error.train4 = mean(abs(pred4 - train4$production))
error.train5 = mean(abs(pred5 - train5$production))
error.train6 = mean(abs(pred6 - train6$production))

pred.test1 = predict(fit1, as.matrix(test1[,4:12]), s = "lambda.1se")
pred.test2 = predict(fit2, as.matrix(test2[,4:19]), s = "lambda.1se")
pred.test3 = predict(fit3, as.matrix(test3[,4:19]), s = "lambda.1se")
pred.test4 = predict(fit4, as.matrix(test4[,4:19]), s = "lambda.1se")
pred.test5 = predict(fit5, as.matrix(test5[,4:19]), s = "lambda.1se")
pred.test6 = predict(fit6, as.matrix(test6[,4:15]), s = "lambda.1se")

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

results = rbind(results, res1)
results = rbind(results, res2)
results = rbind(results, res3)
results = rbind(results, res4)
results = rbind(results, res5)
results = rbind(results, res6)
results[order(station)]


# Lagged ------------------------------------------------------------------
fit.lag1 = cv.glmnet(x = as.matrix(lagged1[,4:66]),  y = lagged1$production, type.measure = "mae")
fit.lag2 = cv.glmnet(x = as.matrix(lagged2[,4:115]), y = lagged2$production, type.measure = "mae")
fit.lag3 = cv.glmnet(x = as.matrix(lagged3[,4:115]), y = lagged3$production, type.measure = "mae")
fit.lag4 = cv.glmnet(x = as.matrix(lagged4[,4:115]), y = lagged4$production, type.measure = "mae")
fit.lag5 = cv.glmnet(x = as.matrix(lagged5[,4:115]), y = lagged5$production, type.measure = "mae")
fit.lag6 = cv.glmnet(x = as.matrix(lagged6[,4:87]),  y = lagged6$production, type.measure = "mae")

pred1 = predict(fit.lag1, as.matrix(lagged1[,4:66]),  s = "lambda.1se")
pred2 = predict(fit.lag2, as.matrix(lagged2[,4:115]), s = "lambda.1se")
pred3 = predict(fit.lag3, as.matrix(lagged3[,4:115]), s = "lambda.1se")
pred4 = predict(fit.lag4, as.matrix(lagged4[,4:115]), s = "lambda.1se")
pred5 = predict(fit.lag5, as.matrix(lagged5[,4:115]), s = "lambda.1se")
pred6 = predict(fit.lag6, as.matrix(lagged6[,4:87]),  s = "lambda.1se")

error.train1 = mean(abs(pred1 - lagged1$production))
error.train2 = mean(abs(pred2 - lagged2$production))
error.train3 = mean(abs(pred3 - lagged3$production))
error.train4 = mean(abs(pred4 - lagged4$production))
error.train5 = mean(abs(pred5 - lagged5$production))
error.train6 = mean(abs(pred6 - lagged6$production))

pred.test1 = predict(fit.lag1, as.matrix(lagged.test1[,4:66]), s = "lambda.1se")
pred.test2 = predict(fit.lag2, as.matrix(lagged.test2[,4:115]), s = "lambda.1se")
pred.test3 = predict(fit.lag3, as.matrix(lagged.test3[,4:115]), s = "lambda.1se")
pred.test4 = predict(fit.lag4, as.matrix(lagged.test4[,4:115]), s = "lambda.1se")
pred.test5 = predict(fit.lag5, as.matrix(lagged.test5[,4:115]), s = "lambda.1se")
pred.test6 = predict(fit.lag6, as.matrix(lagged.test6[,4:87]), s = "lambda.1se")

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

results = rbind(results, res1)
results = rbind(results, res2)
results = rbind(results, res3)
results = rbind(results, res4)
results = rbind(results, res5)
results = rbind(results, res6)


# Powered (Squares and Cubes) -------------------------------------------------------
fit.power1 = cv.glmnet(x = as.matrix(powered1[,4:30]), y = powered1$production, type.measure = "mae")
fit.power2 = cv.glmnet(x = as.matrix(powered2[,4:51]), y = powered2$production, type.measure = "mae")
fit.power3 = cv.glmnet(x = as.matrix(powered3[,4:51]), y = powered3$production, type.measure = "mae")
fit.power4 = cv.glmnet(x = as.matrix(powered4[,4:51]), y = powered4$production, type.measure = "mae")
fit.power5 = cv.glmnet(x = as.matrix(powered5[,4:51]), y = powered5$production, type.measure = "mae")
fit.power6 = cv.glmnet(x = as.matrix(powered6[,4:39]), y = powered6$production, type.measure = "mae")

pred1 = predict(fit.power1, as.matrix(powered1[,4:30]), s = "lambda.1se")
pred2 = predict(fit.power2, as.matrix(powered2[,4:51]), s = "lambda.1se")
pred3 = predict(fit.power3, as.matrix(powered3[,4:51]), s = "lambda.1se")
pred4 = predict(fit.power4, as.matrix(powered4[,4:51]), s = "lambda.1se")
pred5 = predict(fit.power5, as.matrix(powered5[,4:51]), s = "lambda.1se")
pred6 = predict(fit.power6, as.matrix(powered6[,4:39]), s = "lambda.1se")

error.train1 = mean(abs(pred1 - powered1$production))
error.train2 = mean(abs(pred2 - powered2$production))
error.train3 = mean(abs(pred3 - powered3$production))
error.train4 = mean(abs(pred4 - powered4$production))
error.train5 = mean(abs(pred5 - powered5$production))
error.train6 = mean(abs(pred6 - powered6$production))

pred.test1 = predict(fit.power1, as.matrix(powered.test1[,4:30]), s = "lambda.1se")
pred.test2 = predict(fit.power2, as.matrix(powered.test2[,4:51]), s = "lambda.1se")
pred.test3 = predict(fit.power3, as.matrix(powered.test3[,4:51]), s = "lambda.1se")
pred.test4 = predict(fit.power4, as.matrix(powered.test4[,4:51]), s = "lambda.1se")
pred.test5 = predict(fit.power5, as.matrix(powered.test5[,4:51]), s = "lambda.1se")
pred.test6 = predict(fit.power6, as.matrix(powered.test6[,4:39]), s = "lambda.1se")

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

results = rbind(results, res1)
results = rbind(results, res2)
results = rbind(results, res3)
results = rbind(results, res4)
results = rbind(results, res5)
results = rbind(results, res6)

# Lagged and Powered -------------------------------------------------------------

fit.lagpower1 = cv.glmnet(x = as.matrix(laggedpowered1[,4:192]), y = laggedpowered1$production, type.measure = "mae")
fit.lagpower2 = cv.glmnet(x = as.matrix(laggedpowered2[,4:339]), y = laggedpowered2$production, type.measure = "mae")
fit.lagpower3 = cv.glmnet(x = as.matrix(laggedpowered3[,4:339]), y = laggedpowered3$production, type.measure = "mae")
fit.lagpower4 = cv.glmnet(x = as.matrix(laggedpowered4[,4:339]), y = laggedpowered4$production, type.measure = "mae")
fit.lagpower5 = cv.glmnet(x = as.matrix(laggedpowered5[,4:339]), y = laggedpowered5$production, type.measure = "mae")
fit.lagpower6 = cv.glmnet(x = as.matrix(laggedpowered6[,4:255]), y = laggedpowered6$production, type.measure = "mae")

pred1 = predict(fit.lagpower1, as.matrix(laggedpowered1[,4:192]), s = "lambda.1se")
pred2 = predict(fit.lagpower2, as.matrix(laggedpowered2[,4:339]), s = "lambda.1se")
pred3 = predict(fit.lagpower3, as.matrix(laggedpowered3[,4:339]), s = "lambda.1se")
pred4 = predict(fit.lagpower4, as.matrix(laggedpowered4[,4:339]), s = "lambda.1se")
pred5 = predict(fit.lagpower5, as.matrix(laggedpowered5[,4:339]), s = "lambda.1se")
pred6 = predict(fit.lagpower6, as.matrix(laggedpowered6[,4:255]), s = "lambda.1se")

error.train1 = mean(abs(pred1 - laggedpowered1$production))
error.train2 = mean(abs(pred2 - laggedpowered2$production))
error.train3 = mean(abs(pred3 - laggedpowered3$production))
error.train4 = mean(abs(pred4 - laggedpowered4$production))
error.train5 = mean(abs(pred5 - laggedpowered5$production))
error.train6 = mean(abs(pred6 - laggedpowered6$production))

pred.test1 = predict(fit.lagpower1, as.matrix(laggedpowered.test1[,4:192]), s = "lambda.1se")
pred.test2 = predict(fit.lagpower2, as.matrix(laggedpowered.test2[,4:339]), s = "lambda.1se")
pred.test3 = predict(fit.lagpower3, as.matrix(laggedpowered.test3[,4:339]), s = "lambda.1se")
pred.test4 = predict(fit.lagpower4, as.matrix(laggedpowered.test4[,4:339]), s = "lambda.1se")
pred.test5 = predict(fit.lagpower5, as.matrix(laggedpowered.test5[,4:339]), s = "lambda.1se")
pred.test6 = predict(fit.lagpower6, as.matrix(laggedpowered.test6[,4:255]), s = "lambda.1se")

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

results = rbind(results, res1)
results = rbind(results, res2)
results = rbind(results, res3)
results = rbind(results, res4)
results = rbind(results, res5)
results = rbind(results, res6)


results[order(station)]





