gp1 <- matrix(rnorm(1000, mean = 50), nrow = 20, ncol = 50)
#just checking the number of columns and rows in gp1
ncol(gp1)
nrow(gp1)
gp2 <- matrix(rnorm(1000, mean = 250), nrow = 20, ncol = 50)
ncol(gp2)
nrow(gp2)
gp3 <- matrix(rnorm(1000, mean = 500, sd = 100), nrow = 20, ncol = 50)
head(gp3)
names(gp1)
# Combining the classes
data <- rbind(gp1, gp2, gp3)
#just analyzing the data
dim(data)
head(data)

?prcomp
pca_f <- prcomp(data, scale. = TRUE, center = TRUE)

#performing some analysis on the pca_class
names(pca_f)
head(pca_f)
pca_f$rotation
pca_f$scale
pca_f$x

#extracting the first 2 PCs as required

pc12 <- as.data.frame(pca_f$x[,1:2])
colors <- c(rep("red", 20), rep("green", 20), rep("blue", 20))
plot(pc12$PC1, pc12$PC2, xlab="PC1", ylab="PC2", col = colors, pch =16, cex=1.25)

kcluster <- kmeans(data, centers = 3, iter.max = 20000, nstart =4)
names(kcluster)
?kmeans
kcluster$size
kcluster$iter
kcluster$cluster

#comparing the observations in each class to the clusters formed by kmeans by rep()
table(kcluster$cluster, rep(1:3, each = 20))

kcluster_D <- kmeans(data, centers = 2, iter.max = 20000, nstart =5)
kcluster_D$size
kcluster_D$iter
kcluster_D$cluster

kcluster_E <- kmeans(data, centers = 4, iter.max = 20000, nstart =10)
kcluster_E$size
kcluster_E$iter
kcluster_E$cluster

pc12
names(pc12)
kcluster_F <- kmeans(pc12, centers=3, iter.max = 20000)
kcluster_F$size
kcluster_F$iter
kcluster_F$cluster

# scale the data to have standard deviation 1

scaled_data <- scale(data, center = TRUE, scale = TRUE)
#we have even centered the data so as to remove any bias
names(scaled_data)
head(scaled_data)
dim(scaled_data)


kcluster_scaled_G <- kmeans(scaled_data, centers = 3, nstart = 20)
names(kcluster_scaled_G)
kcluster_scaled_G$size
kcluster_scaled_G$iter
kcluster_scaled_G$cluster
