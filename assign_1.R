utilities <- read.csv(file.choose())
attach(utilities)

#euclidean distance
utilities.scale <- scale(utilities[, 2:9])
which(abs(utilities.scale[,1]) >2)
utilities.dist <- dist(utilities.scale, method = "euclidean") 
utilities.dist
boxplot(utilities.scale)

#method 2
#install.packages("fields")
#library(fields)
#d <- rdist(utilities[,2:9])
#class(d)

# 2 clusters
kmeans_2 <- kmeans(utilities.dist, 2, 25)
par(mfrow = c(2, 2))
# plot k = 2
plot(X2~X6, col = colo[kmeans_2$cluster], xlab = "Sales", ylab = "Return on Capital", 
     main = "Cluster Scatter Plot\nK=2", pch = 19) +
  with(text(X2~X6, pos = 4, cex = .5))
# plot k = 3
plot(X2~X6, col = colo[kmeans_3$cluster], xlab = "Sales", ylab = "Return on Capital", 
     main = "Cluster Scatter Plot\nK=3", pch = 19) +
  with(text(X2~X6, pos = 4, cex = .5))
# plot k = 4
plot(X2~X6, col = colo[kmeans_4$cluster], xlab = "Sales", ylab = "Return on Capital", 
     main = "Cluster Scatter Plot\nK=4", pch = 19) +
  with(text(X2~X6, pos = 4, cex = .5))
# plot k = 5
plot(X2~X6, col = colo[kmeans_5$cluster], xlab = "Sales", ylab = "Return on Capital", 
     main = "Cluster Scatter Plot\nK=5", pch = 19) +
  with(text(X2~X6, pos = 4, cex = .5))


colo <- c("red","green","blue","black","purple")
plotcluster(utilities[,c(7,3)], kmeans_5$cluster, xlab = "X6: Sales", ylab = "X2: ", col = colo[kmeans_5$cluster])
plot(discrproj(utilities[ , 2:9], kmeans_2$cluster)$proj, col = kmeans_2$cluster)


plot(utilities[, c(7,3)], col = kmeans_3$cluster)
plot(utilities[,c(7,3)], col = kmeans_4$cluster) + with(text(utilities[,c(6,7)], lables = kmeans_4$cluster, pos = 4))
# plotting all at once
for (i in 2:9){
plot(utilities[,c(7,i)], col = (kmeans_2$cluster +1), main="K-Means Clustering with K=2",
     xlab = "", ylab= "", pch =20, cex =2, lables = utilities$Name) 
#text(utilities$X3, utilities$X2, row.names(utilities), cex=0.1, pos=1, col=(kmeans_2$cluster +1))
}
# 3 clusters
kmeans_3 <- kmeans(utilities.dist, 3, 25)
kmeans_3

# 4 clusters
kmeans_4 <- kmeans(utilities.dist, 4, 25)
kmeans_4

# 5 clusters
kmeans_5 <- kmeans(utilities.dist, 5, 25)
kmeans_5


# ELBOW METHOD for SoS
total_within <- NULL 
for(i in 1:10){
  total_within <- c(total_within, kmeans(utilities.dist, i, 25)$tot.withinss) 
}
plot(total_within, type = "b", xlab = "Number of Clusters (k)", ylab = "Total within Sum of Squares",
     main = "Elbow Method", pch = 19, cex = 1.5, col = "red")



# how to choose best K
k_utilities <- list()

for (i in 1:10){
  k_utilities[[i]] <- kmeans(utilities.dist, i)
}
k_utilities

# propotion chart of between SS / total ss for k 1 to 10
bet.SS_tot.SS <- list()
for (i in 1:10){
  bet.SS_tot.SS[[i]] <- k_utilities[[i]]$betweenss/k_utilities[[i]]$totss
}
plot(1:10, bet.SS_tot.SS, type = "b", ylab = "Between SS / Total SS", xlab = "Number of Clusters (k)",
     main = "Proportin of between SS/Total SS\nfor each k", pch = 19, cex = 1.5, col = "red")

# produces 5 graphs with cluster k 1 to 5
for (i in 1:5){
  plot(utilities[, 2:9], col = k_utilities[[i]]$cluster)
}

# internal validation measures
internal_v <- clValid(utilities[, 2:9], nClust = 2:5, clMethods = "kmeans",
                      validation = "internal")
internal_v

optimalScores(internal_v)

# Stability Measures
stability_m <- clValid(utilities[, 2:9], nClust = 2:5, clMethods = "kmeans",
                       validation = "stability")
stability_m

optimalScores(stability_m)

# Both internal and stability

int_stab <- clValid(utilities[, 2:9], nClust = 2:5, clMethods = "kmeans",
                    validation = c("internal", "stability"))
int_stab                    
optimalScores(int_stab)

#------------------------------------------------------------------------------
#gap_check <- clusGap(d, FUN = kmeans, nstart = 25,
#                    K.max = 10, B = 50)
#plot(gap_check)
d_clust <- Mclust(utilities.dist, G=1:10, 
                  modelNames = mclust.options("emModelNames"))
d_clust$BIC
plot(d_clust)

nb <- NbClust(scale(d), diss=NULL, distance = "euclidean", 
              min.nc=2, max.nc=5, method = "kmeans", 
              index = "all", alphaBeale = 0.1)
hist(nb$Best.nc[1,], breaks = max(na.omit(nb$Best.nc[1,])))

d
#---------------------------------------------------------------------------------
#(c)
complete_hclust <-hclust(utilities.dist, method = "complete")
plot(complete_hclust, hang = -1, xlab = "Entities", 
     main = "Hierarchical Clustering\nComplete linkage (k = 4)")
rect.hclust(complete_hclust, k = 4, border = "red")

# Both internal and stability "hierarchical"

int_stab_2 <- clValid(utilities[, 2:9], nClust = 2:5, clMethods = c("kmeans", "hierarchial"),
                    validation = c("internal", "stability"))
int_stab_2

optimalScores(int_stab_2)

rect.hclust(complete_hclust, k = 4, border = "red")
cluster_map <- cutree(complete_hclust, 4)
cluster_map

plot(utilities[,c(7,3)], col = cluster_map)

# mclust

ut_mclust <- Mclust(scale(utilities[2:9]))
ut_mclust
plot(ut_mclust)


# k-means VS hierarchical

par(mfrow = c(1, 2))

plot(X2~X6, col = colo[kmeans_4$cluster], xlab = "Sales", ylab = "Return on Capital", 
     main = "k-means ClusterPlot\nK=4", pch = 19) +
  with(text(X2~X6, pos = 4, cex = .5))

cluster_map <- cutree(complete_hclust, 4)
cluster_map
plot(utilities[,c(7,3)], col = colo[cluster_map], xlab = "Sales", ylab = "Return on Capital", 
     main = "Hierarchical Cluster Scatter Plot\nK=4", pch = 19) +
  with(text(X2~X6, pos = 4, cex = .5))
#plot(utilities[,c(7,3)], colo[cluster_map], xlab = "Sales", ylab = "Return on Capital", 
 #    main = "Hierarchical Cluster Scatter Plot\nK=4", pch = 19) +
  #with(text(X2~X6, pos = 4, cex = .5))

#--------------------------------------------------------------------------------
#(d)
# calculating correlation matrix of after transpose
cor_ut <- cor(t(utilities[,2:9]))
#ecludean distance
dist_cor_ut <- as.dist(1-abs(cor_ut))

# clustering, ploting & border
cor_complete_hclust <-hclust(dist_cor_ut, method = "complete")
plot(cor_complete_hclust, xlab = "Entities", main = "Hierarchial Clustering with Pearson's Correlation\nComplete linkage")
rect.hclust(cor_complete_hclust, k = 4, border = "red")

# cluster map for ploting color on scatter plot
cluster_map_2 <- cutree(cor_complete_hclust, 4)
cluster_map_2

# ploting Hierarchical Cluster With Pearson's Correlation VS without
par(mfrow = c(2, 2))

plot(utilities[,c(7,3)], col = colo[cluster_map_2], xlab = "Sales", ylab = "Return on Capital", 
     main = "Hierarchical Cluster Plot\nWith Pearson's Correlation (K=4)", pch = 19) +
  with(text(X2~X6, pos = 4, cex = .5))

plot(utilities[,c(7,3)], col = colo[cluster_map], xlab = "Sales", ylab = "Return on Capital", 
     main = "Hierarchical Cluster Scatter Plot\nK=4", pch = 19) +
  with(text(X2~X6, pos = 4, cex = .5))
#----------------------------------------------
set.seed(123)
fviz_nbclust(utilities.scale, kmeans, nstart = 25,
             method = "gap_stat", nboot = 50) +
  labs(subtitle = "GAP statistic Method")
#-----------------------------------------------------------------------

den <- dendlist(as.dendrogram(complete_hclust), as.dendrogram(cor_complete_hclust))
tanglegram(den, sort = TRUE, common_subtrees_color_lines = T, highlight_distinct_edges  = T, highlight_branches_lwd = T,
                    main_left = "Euclidean distance", main_right = "Pearson’s correlation", main = "Hierarchical Clusters")
