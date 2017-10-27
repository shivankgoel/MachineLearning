#!/usr/bin/env Rscript
# your code must have this first line.

# Test code for logistic regression part goes here

sigmoid <- function(v){
	
	ans <- 1 / (1 + exp((-1 * v)))

}

main <- function(testfile,modelfile,labelfile){
    
	inpframe1 <- read.csv(testfile,header=F)
	X <- data.matrix(inpframe1)
	t <- nrow(X)
	onesmatrix <- matrix("numeric",nrow=t,ncol=1)
	X <- cbind(X,onesmatrix)
	
	inpframe2 <- read.csv(modelfile,header=F)
	weight <- data.matrix(inpframe2)
	
	resultTemp1 <- X *%* weight
	resultTemp2 <- sigmoid(resultTemp)
	results <- vector("numeric" , length = t)
	results[resultTemp2 >= 0.5] <- 1
	results[resultTemp2 < 0.5] <- 0
	
	write.table(results,labelfile,row.names = FALSE,col.names=FALSE,sep=",")
	
}