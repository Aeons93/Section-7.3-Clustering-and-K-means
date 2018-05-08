# This mini-project is based on the K-Means exercise from 'R in Action'
# Go here for the original blog post and solutions
# http://www.r-bloggers.com/k-means-clustering-from-r-in-action/

# Exercise 0: Install these packages if you don't have them already

# install.packages(c("cluster", "rattle.data","NbClust"))

# Now load the data and look at the first few rows
data(wine, package="rattle.data")
head(wine)

# Exercise 1: Remove the first column from the data and scale
# it using the scale() function 

wine_2 <- wine
wine_2[1] <- NULL # remove type column
df<-scale(wine_2) #Set scaled data to variable df

# Now we'd like to cluster the data using K-Means. 
# How do we decide how many clusters to use if you don't know that already?
# We'll try two methods.

# Method 1: A plot of the total within-groups sums of squares against the 
# number of clusters in a K-means solution can be helpful. A bend in the 
# graph can suggest the appropriate number of clusters. 

wssplot <- function(data, nc=15, seed=1234){
	              wss <- (nrow(data)-1)*sum(apply(data,2,var))
               	      for (i in 2:nc){
		        set.seed(seed)
	                wss[i] <- sum(kmeans(data, centers=i)$withinss)}
	                
		      plot(1:nc, wss, type="b", xlab="Number of Clusters",
	                        ylab="Within groups sum of squares")
	   }

wssplot(df) #scaled
wssplot(wine) #unscaled would indicate about the same anyway. 

# Exercise 2:
#   * How many clusters does this method suggest?

#       First method suggests 3 clusters. 
#       The major bend in the plotted graph wssplot(wine) appears from points 1,2,3,4 which would suggest 3 clusters is appropriate.

#   * Why does this method work? What's the intuition behind it?

#       This method works by comparing the variance between adding clusters.
#       The bend in the plot represents the greatest change in variance when modeling the data, which is indicated between clusters 1-3.
#       At cluster 4 and beyond the change is very minor between each cluster which implies that adding further clusters would not change how well the data is modeled.

#   * Look at the code for wssplot() and figure out how it works

wssplot <- function(data, nc=15, seed=1234){  # data is our dataset, nc is the number of clusters we want to check, and seed is a random number. 
  wss <- (nrow(data)-1)*sum(apply(data,2,var)) #WSS calculates the the homogeneity throughout the data.  
  for (i in 2:nc){                              # The for loop iterates through each cluster and calculates "WSS", "Within sums of squares"
    set.seed(seed)
    wss[i] <- sum(kmeans(data, centers=i)$withinss)} # Each WSS is set to a number of clusters 
  
  plot(1:nc, wss, type="b", xlab="Number of Clusters", # Plot created for WSS as Y, and number of clusters as X, which can be used to determine the elbow point.
       ylab="Within groups sum of squares")
}

# Method 2: Use the NbClust library, which runs many experiments
# and gives a distribution of potential number of clusters.

library(NbClust)
set.seed(1234)
nc <- NbClust(df, min.nc=2, max.nc=15, method="kmeans")
barplot(table(nc$Best.n[1,]),
	          xlab="Numer of Clusters", ylab="Number of Criteria",
		            main="Number of Clusters Chosen by 26 Criteria")


# Exercise 3: How many clusters does this method suggest?

#     This method suggests 3 clusters, similar to the first method. 

# Exercise 4: Once you've picked the number of clusters, run k-means 
# using this number of clusters. Output the result of calling kmeans()
# into a variable fit.km

fit.km <- kmeans( df, centers = 3) # kmeans with 3 clusters.
fit.km

# Now we want to evaluate how well this clustering does.

# Exercise 5: using the table() function, show how the clusters in fit.km$clusters
# compares to the actual wine types in wine$Type. Would you consider this a good
# clustering?

c<-table(wine$Type,fit.km$cluster)
c
install.packages("flexclust")
library(flexclust)
#The adjusted Rand index provides a measure of the agreement between two partitions, adjusted for chance. 
#It ranges from -1 (no agreement) to 1 (perfect agreement).
randIndex(c) # the randIndex returned 0.897495. which is pretty close to 1, so it seems to be good clustering.


# Exercise 6:
# * Visualize these clusters using  function clusplot() from the cluster library
# * Would you consider this a good clustering?
library("cluster")
clusplot(wine, fit.km$cluster ) # visual plot of clusters based on fit.km$cluster
clusplot(wine, wine$Type ) # visual plot of clusters based on wine$Type
#In comparing the two plots, the plot utilizing fit.km$cluster seems to have better and tighter clustering.
#So we can conclude that this is good custering since each cluster, visually, have distinct borders and are separate. 
