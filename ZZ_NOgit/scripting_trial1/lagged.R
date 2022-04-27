# Lagged

# Lagged ------------------------------------------------------------------
fit.lag = cv.glmnet(x = as.matrix(lagged[,-c(1:3)]),  y = lagged$production, type.measure = "mae")
# train error
error.train = min(fit.lag$cvup)
# prediction
pred.test = predict(fit.lag, as.matrix(lagged.test[,-c(1:3)]), s = "lambda.1se")
# test error
error.test = mean(abs(pred.test - lagged.test$production))
# results
res = data.table(station = station_name, model = "lagged", train_error = error.train, test_error = error.test)
#
results = rbind(results, res)
