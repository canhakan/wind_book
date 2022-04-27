# Scaling

# I create another script for scaling because we need to scale every other modified data before running models.


# Base Train/Test ---------------------------------------------------------
t1 = as.matrix(scale(train1[,4:12]))
t2 = as.matrix(scale(train2[,4:19]))
t3 = as.matrix(scale(train3[,4:19]))
t4 = as.matrix(scale(train4[,4:19]))
t5 = as.matrix(scale(train5[,4:19]))
t6 = as.matrix(scale(train6[,4:15]))

mean1 = apply(train1[,-c(1:3)],2,mean)
mean2 = apply(train2[,-c(1:3)],2,mean)
mean3 = apply(train3[,-c(1:3)],2,mean)
mean4 = apply(train4[,-c(1:3)],2,mean)
mean5 = apply(train5[,-c(1:3)],2,mean)
mean6 = apply(train6[,-c(1:3)],2,mean)

sd1   = apply(train1[,-c(1:3)],2,sd)
sd2   = apply(train2[,-c(1:3)],2,sd)
sd3   = apply(train3[,-c(1:3)],2,sd)
sd4   = apply(train4[,-c(1:3)],2,sd)
sd5   = apply(train5[,-c(1:3)],2,sd)
sd6   = apply(train6[,-c(1:3)],2,sd)

t1 = (train1[,-c(1:3)] - mean1) / sd1
te1 = (test1[,-c(1:3)] - mean1) / sd1
t2 = (train2[,-c(1:3)] - mean2) / sd2
te2 = (test2[,-c(1:3)] - mean2) / sd2
t3 = (train3[,-c(1:3)] - mean3) / sd3
te3 = (test3[,-c(1:3)] - mean3) / sd3
t4 = (train4[,-c(1:3)] - mean4) / sd4
te4 = (test4[,-c(1:3)] - mean4) / sd4
t5 = (train5[,-c(1:3)] - mean5) / sd5
te5 = (test5[,-c(1:3)] - mean5) / sd5
t6 = (train6[,-c(1:3)] - mean6) / sd6
te6 = (test6[,-c(1:3)] - mean6) / sd6

