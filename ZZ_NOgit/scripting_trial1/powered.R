# Powered

# Powered -----------------------------------------------------------------
fit.power = cv.glmnet(x = as.matrix(powered[,-c(1:3)]), y = powered$production, type.measure = "mae")
# train error
error.train1 = min(fit.power$cvup)
# prediction
pred.test = predict(fit.power, as.matrix(powered.test[,-c(1:3)]), s = "lambda.1se")
# test error
error.test = mean(abs(pred.test - powered.test$production))
# results
res = data.table(station = station_name, model = "powered", train_error = error.train, test_error = error.test)
results = rbind(results, res)
