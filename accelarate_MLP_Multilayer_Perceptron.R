
# Sample data
library(monmlp)
n <- 1000
k <- 7
x <- matrix( rnorm(k*n), nr=n )
w <- rnorm(k)
y <- ifelse( logistic( x %*% w ) + rnorm(n, sd = 0.2) > 1, 0, 1 )

# Fit the model and compute the predictions
r <- monmlp.fit(x, y, hidden1=3, n.ensemble=15, monotone=1, bag=TRUE)
z <- monmlp.predict(x = x, weights = r)

# Compute the AUC
library(ROCR)
plot( performance( prediction( z, y ), "tpr","fpr" ) )
performance( prediction( z, y ), "auc" )@y.values[[1]]