library(ISLR)
set.seed(322)

# 3.a
hc.complete = hclust(dist(USArrests), method="complete")
plot(hc.complete)

# 3.b
cutree(hc.complete, 3)
table(cutree(hc.complete, 3))

# 3.c
data = scale(USArrests)
hc.s.complete = hclust(dist(data), method="complete")
plot(hc.s.complete)

# 3.d
cutree(hc.s.complete, 3)
table(cutree(hc.s.complete, 3))
table(cutree(hc.s.complete, 3), cutree(hc.complete, 3))

# Scaling the variables affects the max height of the dendogram. 
# More specifically it directly affects the clusters split from cutting the dendogram.
# In my opinion data should be standardised because quite often data is measured using 
# many different units.
