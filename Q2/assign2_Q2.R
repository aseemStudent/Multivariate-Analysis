# (a)
wine <- read.csv(file.choose()) # semester 4, sta30005, assign 2, Q5
sum(is.na(wine))
wine$cultivar <- as.factor(wine$cultivar)
n <- dim(wine)[2]

for(i in 2:n){
  boxplot(scale(wine[,i]))
}

boxplot(wine[,3])$out


str(wine)
summary(wine)
describe.by(wine)

# Outlier detection
wine_scale <- dist(scale(wine[, 2:14]))

wine_cluster <-hclust(wine_scale, method = "complete")
plot(wine_cluster, xlab = "Entities", 
     main = "Hierarchical Clustering\nComplete linkage (k = 4)")
rect.hclust(wine_cluster, k = 10, border = "red")
wine_cluster


# k-means
wine_kmean <- kmeans(wine_scale, 20, 50)
wine_kmean$size
wine_kmean$cluster[wine_kmean$cluster == 4]
wine_kmean$cluster[wine_kmean$cluster == 15]
# 2 in a group 60 and 116


# mahalanobis outliers
md <- mahalanobis(wine[,2:14], colMeans(wine[,2:14]), cov(wine[,2:14]))
boxplot(md)
match(boxplot(md)$out, md)


# boxplot
#for(i in 2:n){
#outliers_list[i, i] <- match(boxplot(wine[,i])$out, wine[,i])
#}
match(boxplot(wine[,2])$out, wine[,2])
match(boxplot(wine[,3])$out, wine[,3])
match(boxplot(wine[,4])$out, wine[,4])
match(boxplot(wine[,5])$out, wine[,5])
match(boxplot(wine[,6])$out, wine[,6])
match(boxplot(wine[,7])$out, wine[,7])
match(boxplot(wine[,8])$out, wine[,8])
match(boxplot(wine[,9])$out, wine[,9])
match(boxplot(wine[,10])$out, wine[,10])
match(boxplot(wine[,11])$out, wine[,11])
match(boxplot(wine[,12])$out, wine[,12])
match(boxplot(wine[,13])$out, wine[,13])
match(boxplot(wine[,14])$out, wine[,14])



wine <- wine[-c(60, 74, 96, 122), ]
dim(wine)

#------------------------------------------------------------
# (b)
cor(wine[, 2:14])
write.csv(round(cor(wine[, 2:14]), 2), "cor_martix.csv")
# data is not measured on same scale so correlation matrix
#-------------------------------------------------------------
# (c)
sd(wine$chem13)
sapply(wine[,2:14], mean)
sapply(wine[, 2:14], sd)
describe.by(wine)$mean
#----------------------------------------------------------
#(d)
?pca
pca_wine <- pca(cor(wine[, 2:14]), nfactors = 13, rotate = "none")
pca_wine
plot(pca_wine$values, type = "l", main = "Variance Chart")
abline(h=1, lty =2)
#-------------------------------------------------------------
# (e)
?hornpa
dim(wine[,2:14])
hornpa(k = 13, size = 174, reps = 500, seed = 100) # k = variables, size = observations
# if hornpa value is < pcs$values --- then retain that pca

#----------------------------------------------------------------
# (f)

wine[,2:14]%*%pca_wine$loadings

a1 <- as.matrix(c(0.336, -0.531, -0.044, -0.624, 0.279, 0.854, 0.936, -0.654, 0.681, -0.181, 0.635, 0.814, 0.628))
a2 <- as.matrix(c(.772, .355, .495, -0.071, .537, 0.089, -0.03, 0.031, 0.026, .835, -.439, -.287, .582))

a_matrix <- cbind(as.matrix(a), as.matrix(a2))

b <- as.matrix(scale(wine[,2:14]))
dim(b)

round(cor(b%*%a_matrix), 2)

b[1,]%*%a1
b[1,]%*%a2
