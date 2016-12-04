# 2.a
library(ISLR)
attach(Carseats)
set.seed(4)

train = sample(1:nrow(Carseats), nrow(Carseats) / 2)
Carseats.test = Carseats[-train, ]
Carseats.train = Carseats[train, ]

# 2.b
library(tree)
tree.carseats = tree(Sales ~ ., data = Carseats.train)
plot(tree.carseats)
text(tree.carseats, pretty = 0)

pred.carseats = predict(tree.carseats, Carseats.test)
mean((Carseats.test$Sales - pred.carseats)^2)
# Test MSE is around 4.8
# 2.c

cv.carseats = cv.tree(tree.carseats, FUN = prune.tree)
plot(cv.carseats$size, cv.carseats$dev, type = "b")
pruned.carseats = prune.tree(tree.carseats, best = 13)

pred.pruned = predict(pruned.carseats, Carseats.test)
mean((Carseats.test$Sales - pred.pruned)^2)
# Test MSE is around 4.7
# Therefore prunning the tree decreases Test MSE

# 2.d

library(randomForest)

bag.carseats = randomForest(Sales ~ ., data = Carseats.train, mtry = 10, importance = TRUE)
bag.pred = predict(bag.carseats, Carseats.test)
mean((Carseats.test$Sales - bag.pred)^2)
# Test MSE is around 2.9

importance(bag.carseats)
# Most important predictors are Price (~498), ShelveLoc (~438) and Age (~150)

# 2.e

rf.carseats = randomForest(Sales ~ ., data = Carseats.train, mtry = 10, importance = TRUE)
rf.pred = predict(rf.carseats, Carseats.test)
mean((Carseats.test$Sales - rf.pred)^2)
# Test MSE is around 2.9
rf.carseats = randomForest(Sales ~ ., data = Carseats.train, mtry = 5, importance = TRUE)
rf.pred = predict(rf.carseats, Carseats.test)
mean((Carseats.test$Sales - rf.pred)^2)
# Test MSE is around 3.0
rf.carseats = randomForest(Sales ~ ., data = Carseats.train, mtry = 2, importance = TRUE)
rf.pred = predict(rf.carseats, Carseats.test)
mean((Carseats.test$Sales - rf.pred)^2)
# Test MSE is around 3.8

# Decreasing the value of m seems to increase Test MSE 

importance(rf.carseats)

# Most important predictors are Price (~321), ShelveLoc (~261) and Age (~174)
