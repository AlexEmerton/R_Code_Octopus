# 2.a
set.seed(322)
x1 = runif(500) - 0.5
x2 = runif(500) - 0.5
y = 1 * (x1^2 - x2^2 > 0)

# 2.b
plot(x1[y == 0], x2[y == 0], col = "black", xlab = "X1", ylab = "X2", pch = "+")
points(x1[y == 1], x2[y == 1], col = "green", pch = 4)

# 2.c
lm.fit = glm(y ~ x1 + x2, family = binomial)
summary(lm.fit)

# 2.d
data_1 = data.frame(x1 = x1, x2 = x2, y = y)

lm_1 = predict(lm.fit, data_1, type = "response")
lm.pred = ifelse(lm_1 > 0.52, 1, 0)

data.plus = data_1[lm.pred == 1, ]
data.minus = data_1[lm.pred == 0, ]

plot(data.plus$x1, data.plus$x2, col = "green", xlab = "X1", ylab = "X2", pch = "+")
points(data.minus$x1, data.minus$x2, col = "black", pch = 4)

# 2.e
lm.fit = glm(y ~ poly(x1, 2) + poly(x2, 2) + I(x1 * x2), data = data_1, family = binomial)

# 2.f
lm_2 = predict(lm.fit, data_1, type = "response")
lm.pred = ifelse(lm_2 > 0.5, 1, 0)

data.plus = data_1[lm.pred == 1, ]
data.minus = data_1[lm.pred == 0, ]

plot(data.plus$x1, data.plus$x2, col = "green", xlab = "X1", ylab = "X2", pch = "+")
points(data.minus$x1, data.minus$x2, col = "black", pch = 4)

# 2.g
library(e1071)

svm.fit = svm(as.factor(y) ~ x1 + x2, data_1, kernel = "linear", cost = 0.1)
svm.pred = predict(svm.fit, data_1)

data.plus = data_1[svm.pred == 1, ]
data.minus = data_1[svm.pred == 0, ]

plot(data.plus$x1, data.plus$x2, col = "green", xlab = "X1", ylab = "X2", pch = "+")
points(data.minus$x1, data.minus$x2, col = "black", pch = 4)

# 2.h
svm.fit = svm(as.factor(y) ~ x1 + x2, data_1, gamma = 1)
svm.pred = predict(svm.fit, data_1)

data.plus = data_1[svm.pred == 1, ]
data.minus = data_1[svm.pred == 0, ]

plot(data.plus$x1, data.plus$x2, col = "green", xlab = "X1", ylab = "X2", pch = "+")
points(data.minus$x1, data.minus$x2, col = "black", pch = 4)

# 2.i
# As it can be deducted from this example, SVMs with non-linear kernel and  logistic regression 
# with interaction terms are both useful in finding the non-linear boundaries. 
# On the other hand SVM with linear kernel and logistic regression without any interaction term
# both fail to identify devision boundary or as shown in this particular case even a single class.
# The biggest difference in implementing either of the techniques is the ammount of tuning required.
# Radial basis only requires gamma as a parameter while the other simple non-linear requires  
# finding the right interaction terms.


