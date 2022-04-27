# 01 Base Model

# Base Model --------------------------------------------------------------
# fit
fit = cv.glmnet(x = as.matrix(train[,-c(1:3)]), y = train$production, type.measure = "mae")
# train error
error.train = min(fit$cvup)
# predict
pred.test = predict(fit, as.matrix(test[,-c(1:3)]), s = "lambda.1se")
# test error
error.test = mean(abs(pred.test - test$production))
# tabling
res = data.table(station = station_name, model = "base", train_error = error.train, test_error = error.test)
results = rbind(results, res)




