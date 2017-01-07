# 1.a
library(ISLR)
attach(OJ)

set.seed(644)

train = sample(1:nrow(OJ), 800)
OJ.train = OJ[train, ]
OJ.test = OJ[-train, ]

# 1.b
library(e1071)

svm_1 = svm(Purchase ~ ., kernel = "linear", data = OJ.train, cost = 0.01)
summary(svm_1)

# The number of support vectors created is 430 out of 800 training points
# 215 of 430 belong to CH class and the remaining 215 to MM

# 1.c
train.pred = predict(svm_1, OJ.train)
table(OJ.train$Purchase, train.pred)
(78 + 51) / (436 + 51 + 78 + 235)
# [1] 0.16125 ~ 16.1%

test.pred = predict(svm_1, OJ.test)
table(OJ.test$Purchase, test.pred)
(29 + 23) / (137 + 23 + 29 + 81)
# [1] 0.1925926 ~ 19.3%

# 1.d
tune.out = tune(svm, Purchase ~., data = OJ.train, kernel = "linear", ranges = list(cost = 10^seq(-2, 1, by = 0.25)))
summary(tune.out)

# Tuning shows that optimal cost is 0.56

# 1.e
svm_2 = svm(Purchase ~., kernel = "linear", data = OJ.train, cost = tune.out$best.parameters$cost)
train.pred = predict(svm_2, OJ.train)
table(OJ.train$Purchase, train.pred)
(71 + 53) / (434 + 53 + 71 + 242)
# [1] 0.155 ~ 15.5%

test.pred = predict(svm_2, OJ.test)
table(OJ.test$Purchase, test.pred)
(31 + 18) / (135 + 18 + 31 + 86)
# [1] 0.1814815 ~ 18.1%

# Training error rate decreased from 16.1% to 15.5%
# Test error rate increased from 19.3% to 18.1%

# 1.f

svm.radial = svm(Purchase ~ ., data = OJ.train, kernel = "radial")
summary(svm.radial)

# The number of support vectors created is 371 out of 800 training points
# 183 of 371 belong to CH class and the remaining 188 to MM

train.pred = predict(svm.radial, OJ.train)
table(OJ.train$Purchase, train.pred)

(82 + 35) / (452 + 35 + 82 + 231)
# [1] 0.14625 ~ 14.6%

test.pred = predict(svm.radial, OJ.test)
table(OJ.test$Purchase, test.pred)

(24 + 24) / (142 + 24 + 24 + 80)
# [1] 0.1777778 ~ 17.8%

set.seed(866)
tune.out = tune(svm, Purchase ~ ., data = OJ.train, kernel = "radial", ranges = list(cost = 10^seq(-2, 1, by = 0.25)))
summary(tune.out)

svm.radial = svm(Purchase ~ ., data = OJ.train, kernel = "radial", cost = tune.out$best.parameters$cost)
train.pred = predict(svm.radial, OJ.train)
table(OJ.train$Purchase, train.pred)

(37 + 80) / (450 + 37 + 80 + 233)
# [1] 0.14625 ~ 14.6%

test.pred = predict(svm.radial, OJ.test)
table(OJ.test$Purchase, test.pred)

(23 + 23) / (143 + 23 + 23 + 81)
# [1] 0.1703704 ~ 17%

# Training error rate remained the same with 14.6%
# Test error rate decreased from 17.8% to 17%

# 1.g

svm.poly = svm(Purchase ~ ., data = OJ.train, kernel = "poly", degree = 2)
summary(svm.poly)

# The number of support vectors created is 448 out of 800 training points
# 223 of 448 belong to CH class and the remaining 225 to MM

train.pred = predict(svm.poly, OJ.train)
table(OJ.train$Purchase, train.pred)

(109 + 29) / (458 + 29 + 204 + 109)
# 0.1725 ~ 17.3%

test.pred = predict(svm.poly, OJ.test)
table(OJ.test$Purchase, test.pred)

(34 + 23) / (143 + 34 + 23 + 70)
# 0.2111111 ~ 21.1%

set.seed(64)
tune.out = tune(svm, Purchase ~ ., data = OJ.train, kernel = "poly", degree = 2, ranges = list(cost = 10^seq(-2, 1, by = 0.25)))
summary(tune.out)

svm.poly = svm(Purchase ~ ., data = OJ.train, kernel = "poly", degree = 2, cost = tune.out$best.parameters$cost)
train.pred = predict(svm.poly, OJ.train)
table(OJ.train$Purchase, train.pred)

(87 + 33) / (454 + 33 + 87 + 226)
# 0.15 ~ 15%

test.pred = predict(svm.poly, OJ.test)
table(OJ.test$Purchase, test.pred)

(27 + 22) / (144 + 27 + 22 + 77)
# 0.1814815 ~ 18.1%

# Training error rate decreased from 17.3% to 15%
# Test error rate decreased from 21.1% to 18.1%

# 2.h

# After the analysis it can be clearly seen that the radial basis kernel produces the fewest errors


