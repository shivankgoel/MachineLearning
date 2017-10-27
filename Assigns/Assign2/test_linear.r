#!/usr/bin/env Rscript
# your code must have this first line.

# Test code for linear regression part goes here

checkaccuracy <- function(labelfile,groundfile){
	inpframe3 <- read.csv(labelfile,header=F)
	X1 <- data.matrix(inpframe3)
	inpframe4 <- read.csv(groundfile,header=F)
	X2 <- data.matrix(inpframe4)
	error <- (sqrt(sum((X1 - X2)^2)) / nrow(X1))
	return(error)
}


featurescale <- function(X){
	nr <- ncol(X)
	for(i in 1:nr-1){
		X[,i] <- (X[,i] - mean(X[,i]))/(sd(X[,i])) 
	}
	return(X)
}

extendfeatures <- function(X){
	temp <- X
	for(i in 1:5){
		X <- cbind(X,(temp^i))
	}
	#X <- cbind(X,exp(temp))
	nr <- ncol(temp) 
	for(i in 1:nr){
			for(j in 1:nr){
				if(i != j){
					X <- cbind(X,(temp[,i]*temp[,j]))
				}
			}
	}
	nr <- ncol(X)
	for(i in 1:nr){
		X[,i] <- (X[,i] - mean(X[,i]))/(sd(X[,i])) 
	}
	return(X)
}

main <- function(testfile,modelfile,labelfile){
    
	inpframe1 <- read.csv(testfile,header=F)
	X <- data.matrix(inpframe1)
	t <- nrow(X)
	onesmatrix <- matrix(1,nrow=t,ncol=1)
	X <- featurescale(X)
	X <- cbind(X,onesmatrix)
	
	inpframe2 <- read.csv(modelfile,header=F)
	weight <- data.matrix(inpframe2)
	
	results <- X %*% weight
	write.table(results,labelfile,row.names = FALSE,col.names=FALSE,sep=",")
	
	print(checkaccuracy(labelfile,"public_solution1.txt"))

}