# (a)
household <- read.csv(file.choose())
head(household)
dim(household)

sum(is.na(household))
household <- household[,3:32]
dim(household)
head(household)
sum(is.na(household))
summary(household)

match(boxplot(household[,3])$out, household[,3])

md <- mahalanobis(household, colMeans(household), cov(household))
a <- match(boxplot(md, title = "s")$out, md)
a
#ha <- household[-c(91, 93, 175, 186, 319, 334, 373, 386, 388, 404, 468, 489, 619, 
#                    724, 774, 785, 823, 878, 893, 909, 932, 945, 962, 981, 992, 994, 999),]
#md1 <- mahalanobis(ha, colMeans(ha), cov(ha))
#amd1 <- match(boxplot(md1, title = "s")$out, md1)
#---------------------------------------------------------------
#(b)
household_cor <- round(cor(household), 2)
write.csv(household_cor, "household_cor.csv")
household_2 <- household[, -c(3,4,6,8,14,15,29)]
household_2_cor <- round(cor(household_2), 2)
write.csv(household_2_cor, "household_2_cor.csv")
household <- household_2
dim(household)
#-----------------------------------------------------------
# (c)

KMO(household) #sample adequacy
bartlett.test(household) # p < .05
round(bartlett.test(household)$p.value, 10)
#-----------------------------------------------------------
# (d)
dim(household)
household_fa23 <-fa(household, nfactors = 23,fm = "minres", rotate = "none") 
household_fa23$loadings

fa.parallel(household, fa = "fa")

#-----------------------------------------------------------
# (e)
dim(household)
household_fa6 <-fa(household, nfactors = 6,fm = "minres", rotate = "none") 
temp <- household_fa6$loadings
write.csv(round(temp, 2), "q3_d.csv")

#-----------------------------------------------------------
# (g)
household_fa6_v <-fa(household, nfactors = 6,fm = "minres", rotate = "varimax")
temp <- household_fa6_v$loadings
write.csv(round(temp, 2), "q3_f.csv")


household_fa6_o <-fa(household, nfactors = 6,fm = "minres", rotate = "oblimin")
temp <- household_fa6_o$loadings
write.csv(round(temp, 2), "q3_f2.csv")

#-----------------------------------------------------------
# (h)

temp<-round(household_fa6_o$residual,2)
write.csv(temp, "q3_h_redidual.csv")
#-----------------------------------------------------------
# (i)

household_fa5_o <-fa(household, nfactors = 5,fm = "minres", rotate = "oblimin")
household_fa5_o$loadings
temp <- household_fa5_o$loadings
write.csv(round(temp, 2), "q3_i1.csv")
# removing question 9
household_2 <- household[,-5]
head(household_2)
# Factor analysis with 5 factors, minres method and oblimin rotation
household_2_fa5_o <- fa(household_2, nfactors = 5,fm = "minres", rotate = "oblimin")
temp <- household_2_fa5_o$loadings
write.csv(round(temp, 2), "q3_i2.csv")

# removing question 11 and 16

household_3 <- household_2[, -c(6,9)]
head(household_3)
household_3_fa5 <- fa(household_3, nfactors = 5,fm = "minres", rotate = "oblimin")
temp <- household_3_fa5$loadings
write.csv(round(temp, 2), "q3_i3.csv")
household_3_fa5

temp<-round(household_3_fa5$residual,2)
write.csv(temp, "q3_i_residual.csv")

dim(household_3)


