#3.a
library(ISLR)
library(tree)
attach(OJ)
set.seed(322)

train = sample(1:nrow(OJ), 800)
OJ.train = OJ[train, ]
OJ.test = OJ[-train, ]

#3.b
OJ.tree = tree(Purchase~ ., data = OJ.train)
summary(OJ.tree)
# Tree uses 4 variables: LoyalCH, PriceDiff, ListPriceDiff and PctDiscMM
# The training error rate is 0.165. It has 7 terminal nodes.

#3.c
OJ.tree
# The object of analysis is a node named "3)". The splitting variable here is LoyalCH.
# The splitting value is around 0.45. In the subtree under this node there are 512 points.
# The deviance for the points is 469.9. yval is CH showing the prediction for this node.
# The percentages show that 83% of points at this node have CH as a prediction and 17% have MM.
# There is no * sign meaning it is not a terminal node

#3.d
plot(OJ.tree)
text(OJ.tree, pretty = 0)
# The root of the tree is LoyalCH. The tree predicts MM if LoyalCH is < 0.28
# Alternatively it predicts CH if LoyalCH is > 0.7. As the secondary predictors tree uses:
# PriceDiff, ListPriceDiff and PctDiscMM

#3.e
OJ.pred = predict(OJ.tree, OJ.test, type = "class")
table(OJ.test$Purchase, OJ.pred)
(42+16)/270 # -> 0.21
# The test error rate is around 21%

#3.f
cv.OJ = cv.tree(OJ.tree, FUN = prune.tree)

#3.g
plot(cv.OJ$size, cv.OJ$dev, type = "b")

#3.h
# The plot shows that deviation is the lowest when the size of the tree is 5

#3.i
OJ.prunned = prune.tree(OJ.tree, best = "5")

#3.j
summary(OJ.prunned)
# The training error rate is 0.17, which is higher than the same of unprunned tree. 

#3.k
pred.prunned = predict(OJ.prunned, OJ.test, type = "class")
table(OJ.test$Purchase, pred.prunned)
(33+23)/(134+33+23+80) # -> 0.20
# Test error rate of the prunned tree is lower than unprunned with 0.20 against 0.21