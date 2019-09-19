# reading file
chernoff <- read.csv(file.choose())
attach(chernoff)

c(6, 14, 16, 19, 24, 30, 34, 55, 63, 64, 80, 82)
faces(chernoff[ c(28:43),], main = "Chernoff Faces")
chernoff[34,]
str(chernoff)               
summary(chernoff)
cor(chernoff[, c(6,7)])
plot(chernoff[, c(6,7)])
#euclidean distance
chernoff.scale <- scale(chernoff[, 2:7])
which(abs(chernoff.scale[,1]) >2)
chernoff.dist <- dist(chernoff.scale, method = "euclidean") 
chernoff.dist
boxplot(chernoff.dist)


k_2 <- kmeans(chernoff.dist, 5, 25)
plot(chernoff[, c(7,6)], col = k_2$cluster) +
with(text(chernoff[, c(5,6)], pos = 4, cex = .5))
k_2$cluster

H_clus<-hclust(chernoff.dist, method = "complete")
plot(H_clus)


a <-chull(chernoff[, c(5,6)])
a <- c(a, a[1])
plot(chernoff$Z5 ~ chernoff$Z6, data = chernoff, pch = 19, cexx = 1.25)
lines(chernoff$Z6[a], chernoff$Z5[a], type = "1", col = 2, lwd =2)

# mahalanobis
m <- colMeans(chernoff[, 2:7])
c <- cov(chernoff[, 2:7])
mahalanobis(chernoff[, 2:7], m, c)



cor(Z5, Z3)
#Select Z1, Z3 & Z6
#look for outliers
par(mfrow = c(1, 3))
bagplot(cbind(Z1, Z3), xlab = "Z1: inner diameter of embryonic chamber", 
             ylab = "Z3: number of chambers in first whorl")
bagplot(cbind(Z1, Z5), xlab = "Z1: inner diameter of embryonic chamber", 
        ylab = "Z5:  maximum height of chambers in first whorl")
bagplot(cbind(Z3, Z5), xlab = "Z3: number of chambers in first whorl", 
        ylab = "Z5: maximum height of chambers in first whorl")
a$pxy.outlier

chernoff_2 <- chernoff[-34, c(2, 4, 6)]
chernoff_2
chernoff_2.scale <- scale(chernoff_2)
chernoff_2_dist <- dist(chernoff_2.scale, method = "euclidean")
chernoff_k <- kmeans(chernoff_2_dist, 3, 25)
plot(chernoff_2, col = chernoff_k$cluster) +
  with(text(chernoff_2, pos = 4, cex = .5))



par(mfrow = c(1, 3))



chernoff_k$cluster
#faces(chernoff_2[c(3:8, 32:38, 74:87, 41:57, 59:73, 9:22, 25:29), ], main = "k-means")
faces(chernoff_2[c(1, 3, 4, 5, 7, 24, 32, 38, 62, 76, 84, 86, 
                   41, 44, 45, 47, 50, 54, 58, 60, 61, 65, 69, 72, 
                   2, 9, 10, 11, 12, 15, 16, 17, 20, 25, 28, 29), ], main = "k-means", cex = 1)


faces(chernoff_2[c(1, 3, 4, 5, 7, 24, 32, 38, 62, 76, 84, 86), ], main = "k-means 1", cex = 1)
faces(chernoff_2[c(41, 44, 45, 47, 50, 54, 58, 60, 61, 65, 69, 72), ], main = "k-means 2", cex = 1)
faces(chernoff_2[c(2, 9, 10, 11, 12, 15, 16, 17, 20, 25, 28, 29), ], main = "k-means 3", cex = 1)


chernoff_hc <- hclust(chernoff_2_dist, method = "complete")
plot(chernoff_hc)
rect.hclust(chernoff_hc, k = 3, border = "red")
chernoff_hc_cuttree <- cutree(chernoff_hc, 3)
chernoff_hc_cuttree
faces(chernoff_2[c(1, 3, 5, 7, 24, 32, 36, 58, 63, 77, 81, 86, 
                   2, 8, 19, 22, 42, 45, 54, 56, 61, 66, 69, 73, 
                   10, 11, 12, 14, 16, 18, 21, 25, 26, 27, 28, 29), ], main = "Hierarchial Clustering - Complete")

faces(chernoff_2[c(1, 3, 5, 7, 24, 32, 36, 58, 63, 77, 81, 86), ], main = "Hierarchial Clustering - Complete 1")
faces(chernoff_2[c(2, 8, 19, 22, 42, 45, 54, 56, 61, 66, 69, 73), ], main = "Hierarchial Clustering - Complete 2")
faces(chernoff_2[c(10, 11, 12, 14, 16, 18, 21, 25, 26, 27, 28, 29), ], main = "Hierarchial Clustering - Complete 3")



chernoff_hs <- hclust(chernoff_2_dist, method = "single")
chernoff_hs_cuttree <- cutree(chernoff_hs, 3)
chernoff_hs_cuttree
faces(chernoff_2[c(1, 3, 5, 10, 17, 21, 14, 31, 38, 46, 53, 63,
                   75:86,
                   19, 22), ], main = "Hierarchial Clustering - Single")

faces(chernoff_2[c(1, 3, 5, 10, 17, 21, 14, 31, 38, 46, 53, 63), ], main = "Hierarchial Clustering - Single 1")
faces(chernoff_2[c(19, 22), ], main = "Hierarchial Clustering - Single 2")
faces(chernoff_2[c(75:86), ], main = "Hierarchial Clustering - Single 3")

chernoff_hw <- hclust(chernoff_2_dist, method = "ward.D2")
chernoff_hw_cuttree <- cutree(chernoff_hw, 3)
chernoff_hw_cuttree
faces(chernoff_2[c(1, 3, 5, 10, 14, 17, 19, 21, 25, 29, 31, 57,
                   2, 40, 43, 48, 51, 54, 55, 58, 63, 67, 70, 73,
                   75:86), ], main = "Hierarchial Clustering - Ward D2")
faces(chernoff_2[c(1, 3, 5, 10, 14, 17, 19, 21, 25, 29, 31, 57), ], main = "Hierarchial Clustering - Ward 1")
faces(chernoff_2[c(2, 40, 43, 48, 51, 54, 55, 58, 63, 67, 70, 73), ], main = "Hierarchial Clustering - Ward 2")
faces(chernoff_2[c(75:86), ], main = "Hierarchial Clustering - Ward 3")


# correlation

chernoff_cor <- cor(t(chernoff_2))
chernoff_cor_dist <- as.dist(1-abs(chernoff_cor))
chernoff_cor_dist_hc <- hclust(chernoff_cor_dist, method = "complete")
plot(chernoff_cor_dist_hc)
rect.hclust(chernoff_cor_dist_hc, k = 3, border = "red")
chernoff_cor_cuttree <- cutree(chernoff_cor_dist_hc, 3)
chernoff_cor_cuttree
faces(chernoff_2[c(1, 3, 10, 13, 15, 19, 24, 40, 50, 55, 64, 70,
                   8, 16, 17, 21, 23, 26, 30, 32, 35, 39, 73, 86,
                   33, 37, 76:85), ], main = "Hierarchial Clustering - Correlation")

                 