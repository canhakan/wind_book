# Lag and Power
# Lagged and Powered -------------------------------------------------------------

fit.lagpower = cv.glmnet(x = as.matrix(laggedpowered[,-c(1:3)]), y = laggedpowered$production, type.measure = "mae")
#
error.train = min(fit.lagpower$cvup)
# print(error.train)
pred.test = predict(fit.lagpower, as.matrix(laggedpowered.test[,-c(1:3)]), s = "lambda.1se")
#
error.test = mean(abs(pred.test - laggedpowered.test$production))
# print(error.test)
res = data.table(station = station_name, model = "lagged + powered", train_error = error.train, test_error = error.test)
#
results = rbind(results, res)
