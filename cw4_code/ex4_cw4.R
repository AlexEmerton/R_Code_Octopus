# 4.a
set.seed(322)

a = matrix(rnorm(20*3*50, mean=0, sd=0.001), ncol=50)

a[1:20, 2] = 1
a[21:40, 1] = 2
a[21:40, 2] = 2
a[41:60, 1] = 1

# 4.b
pca.out = prcomp(a)
summary(pca.out)

plot(pca.out$x[,1:2], col=2:4, xlab="X", ylab="Y", pch=10) 

# 4.c
km.out = kmeans(a, 3, nstart=20)
table(km.out$cluster, c(rep(1,20), rep(2,20), rep(3,20)))

# The clusters are spread relatively unevenly. Clusters 1-2 being to close to each other
# And clusters 1-3 and 2-3 too farm from each other

# 4.d
km.out = kmeans(a, 2, nstart=20)
km.out$cluster

# Class "1" is inside of Class "2" now

# 4.e
km.out = kmeans(a, 4, nstart=20)
km.out$cluster

# It looks like Class 1 was split into two making it "1" and "3". Classes 2 and 4 
# remained the same.

# 4.f
km.out = kmeans(pca.out$x[,1:2], 3, nstart=20)
table(km.out$cluster, c(rep(1,20), rep(2,20), rep(3,20)))

# This looks exactly like the mirror image of the clustering done earlier. Still uneven.

# 4.g
km.out = kmeans(scale(a), 3, nstart=20)
km.out$cluster

# The results seem to be worse with scaled data. The scaling affect the distance between
# classes directly

