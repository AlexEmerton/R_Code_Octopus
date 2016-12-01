library(MASS)
library(randomForest)

set.seed(22)

boston_var = ncol(Boston) - 1
train = sample(1:nrow(Boston), nrow(Boston)/2)
boston.test = Boston [-train ,"medv"]

# bag.boston_full = randomForest(medv~., data=Boston, subset=train, mtry=boston_var)
# bag.boston_half = randomForest(medv~., data=Boston, subset=train, mtry=boston_var / 2)
# bag.boston_sqrt = randomForest(medv~., data=Boston, subset=train, mtry=sqrt(boston_var))
# plot(1:500, bag.boston_full$test$mse, col="coral2", type="l", xlab="Ntree", ylab = "Test MSE %")
# lines(1:500, bag.boston_half$test$mse, col="blueviolet", type="l")
# lines(1:500, bag.boston_sqrt$test$mse, col="forestgreen", type="l")

test.mse=rep(0,100)
test.mse.half=rep(0, 100)
test.mse.sqrt=rep(0, 100)

for(i in 1:100){
  set.seed(100)
  bag.boston = randomForest(medv~., data=Boston, subset=train, mtry=boston_var, ntree = i, importance = TRUE)
  yhat.bag=predict(bag.boston,newdata=Boston[-train,])
  test.mse[i]=mean((yhat.bag-boston.test)^2)
  
  bag.boston_half = randomForest(medv~., data=Boston, subset=train, mtry=boston_var / 2, ntree = i, importance = TRUE)
  yhat.bag=predict(bag.boston_half,newdata=Boston[-train,])
  test.mse.half[i]=mean((yhat.bag-boston.test)^2)
  
  bag.boston_sqrt = randomForest(medv~., data=Boston, subset=train, mtry=sqrt(boston_var), ntree = i, importance = TRUE)
  yhat.bag=predict(bag.boston_sqrt,newdata=Boston[-train,])
  test.mse.sqrt[i]=mean((yhat.bag-boston.test)^2)
}

plot(test.mse,xlab="Number of Bootstrap Data Sets",ylab="Test Means Sum of Squares", type="l", col="coral2", ylim = c(10, 25))
lines(test.mse.half, col="blueviolet", type="l")
lines(test.mse.sqrt, col="forestgreen", type="l")

legend("topright", c("m = p", "m = p/2", "m = sqrt(p)"), col = c("coral2", "blueviolet", "forestgreen"), cex = 0.5, lty = 1)
