#!/usr/bin/env Rscript
# your code must have this first line.

# Train code for logistic regression part goes here

selectfeatures <- function(Xtemp,y,m,n){
	X <- matrix(nrow = m, ncol = 1)
	for(i in 1:n){
		x  <- Xtemp[,i]
		avg <- mean(x)
		greatery1 <- (1:n)[ (x>avg) & (y==1) ]
		lessery1 <- (1:n)[ (x<=avg)& (y==1) ]
		greater <- (1:n)[x>avg]
		lesser <- (1:n)[x<=avg]
		probgreater <- (length(greatery1)/length(greater)) 
		problesser <- (length(lessery1)/length(lesser))
		if(abs(probgreater - problesser) > 0.2 ){
			X <- cbind(X,x)
		}
	}
	X <- X(2:ncol(X))
	return(X)
}

main <- function(trainfile,modelfile){

	inpframe <- read.csv(trainfile,header=F)
	inpmatrix <- data.matrix(inpframe)
	m <- nrow(inpmatrix)
	n <- ncol(inpmatrix)
	X <- inpmatrix[,1:(n-1)]
	y <- inpmatrix[,n]
	#X <- selectfeatures(Xtemp,y,m,n)
	onesmatrix <- matrix(1,nrow=m,ncol=1)
	X <- cbind(X,onesmatrix)
	weight <- matrix(data = rexp(n, rate = 10),nrow=n,ncol=1)
	alpha = 0.1
	for(iter in 1:10000){
		prediction <- X %*% weight
		dif <- (y - prediction)	
		for(i in 1:m){
			weight <- weight - (as.numeric(dif[i,1]) * (X[i,]))
		}
	}
	write.table(weight,modelfile,row.names = FALSE,col.names=FALSE,sep=",")

}