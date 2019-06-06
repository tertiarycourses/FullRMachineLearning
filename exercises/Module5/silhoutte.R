### data preprocessing for silhoute clustering. 
### Only 10% of the data have been used  for clustering. More data is not a problem for
### clustering algorithm but the visualization becomes extremely dense.
index <- sample(c(TRUE, FALSE), nrow(iris), p = c(0.1, 0.8), replace = TRUE)
myIris <- iris[index,3:4]
myIris

library(cluster)

# Generate a k-means model using the pam() function with a k = 2
pam_k2 <- pam(myIris, k = 2)

# Plot the silhouette visual for the pam_k2 model
plot(silhouette(pam_k2))


pam_k2$silinfo$widths
pam_k2$silinfo$avg.width


#Silhouette analysis allows you to calculate how similar each observations is with the cluster 
#it is assigned relative to other clusters. This metric (silhouette width) 
#ranges from -1 to 1 for each observation in your data and can be interpreted as follows:
  
#Values close to 1 suggest that the observation is well matched to the assigned cluster
#Values close to 0 suggest that the observation is borderline matched between two clusters
#Values close to -1 suggest that the observations may be assigned to the wrong cluster



