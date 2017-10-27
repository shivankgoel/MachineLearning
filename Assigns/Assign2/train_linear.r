#!/usr/bin/env Rscript
# your code must have this first line.

# Train code for linear regression part goes here

library("MASS")

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
	for(i in 1:nr-1){
		X[,i] <- (X[,i] - mean(X[,i]))/(sd(X[,i])) 
	}
	return(X)
}



main <- function(trainfile,modelfile){

	inpframe <- read.csv(trainfile,header=F)
	inpmatrix <- data.matrix(inpframe)
	m <- nrow(inpmatrix)
	n <- ncol(inpmatrix)
	X <- inpmatrix[,1:(n-1)]
	X <- featurescale(X)
	write.table(X,"X.txt",row.names = FALSE,col.names=FALSE,sep="	")
	onesmatrix <- matrix(1,nrow=m,ncol=1)
	X <- cbind(X,onesmatrix)
	y <- inpmatrix[,n]
	#X <- selectfeatures(Xtemp,y,m,n)
	weights <- ((ginv(t(X) %*% X)) %*% (t(X) %*% y))
	write.table(weights,modelfile,row.names = FALSE,col.names=FALSE,sep=",")

}