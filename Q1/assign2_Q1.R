bears_cov <- matrix(c(3266.46, 1343.97, 731.54, 1175.50, 162.68, 238.37,
                  1343.97, 721.91, 324.25, 537.35, 80.17, 117.73,
                  731.54, 324.25, 179.28, 281.17, 39.15, 56.80,
                  1175.50, 537.35, 281.17, 474.98, 63.73, 94.85,
                  162.68, 80.17, 39.15, 63.73, 9.95, 13.88,
                  238.37, 117.73, 56.80, 94.85, 13.88, 21.26), ncol = 6, byrow = T)
bears_cov
isSymmetric(bears_cov) # TRUE
#--------------------------------------------------------------------------------------

# (a) PCA using covariance matrix
pca_bears_cov <- principal(bears_cov, nfactors = 6, rotate = "none")
plot(pca_bears_cov$values, type = "l", main = "Variance explained by each PCA")
abline(h=1, lty =2)

hornpa(k = 6, size = 61, reps = 500, seed = 100)
#-----------------------------------------------------------------

# (b) PCA using correlation matrix

bears_cor <- cov2cor(bears_cov)

pca_bears_cor <- principal(bears_cor, nfactors = 6, rotate = "none")
plot(pca_bears_cor$values, type = "l", main = "Variance explained by each PCA")
abline(h=1, lty =2)

hornpa(k = 6, size = 61, reps = 500, seed = 100)
#------------------------------------------------------------


