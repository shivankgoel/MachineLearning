args<-commandArgs(TRUE)
myfile <- args[1]


torref <- function(A,m,n)   #Ref : Wikipedia Algorithm RREF
{	
	lead <- 1
	tol <-  1e-5

	for( i in 1:m ){
		
		if(lead > n){
			return(A);
		}
				
		if( abs(A[i,lead]) > tol){
			
			A[i,] <- (A[i,] / A[i,lead]);
		
			for(j in 1:m){
				if( j != i){
					A[j,] <- (A[j,] - (A[j,lead] * A[i,]))
				}	
			}
		
			lead <- lead + 1
		}else{
			k <- i
			
			while(abs(A[k,lead]) <= tol ){
				k <- k + 1
				if( k > m ){
					lead <- lead + 1 
					if( lead > n ){
						return(A)
					}
					k <- i
				}
			}
		
			if(i != k){
				temp <- A[i,]
				A[i,] <- A[k,]
				A[k,] <- temp
			}
			i <- i -1
		}
	
	}
	
	A[ abs(A) <= tol ] <-  0
	return(A)
}

getinput <- function(filename){
	inp <- read.csv(filename,header=F)
}

getpivots <- function(A,m,n,r){
    tol <- 1e-5;
	pivots <- vector("numeric" , length = n)
	for(i in 1:m){
	    if(sum(A[i,]^2)==0){
			break
		} 
		for(j in 1:n){
			if(abs(A[i,j]) > tol){
				pivots[j] <- 1
				break
			}
		}
	}
	return(pivots)
}


findrank <- function(A,m,n){
	r <- 0
	for(i in 1:m){
		if((sum((A[i,])^2)) != 0){ 
			r <- r + 1
		}
		else{
			break
		}
	}
	return(r)
}

findnullspace <- function(A , m ,n , pivots){
    
    rownull <- n
	pivotssize <- length((1:n)[pivots==1])
	colnull <- n - pivotssize
	if(colnull == 0) return(0);
	ans <- matrix(nrow = rownull, ncol = colnull )
	t <- 1
	c <- 1
	for(i in 1:n){
	
	    if(pivots[i] == 0){
			next
		}
		
		temp <- vector("numeric" , length = n)
		temp[i] <- -1
		nonpivotcolumn <- A[,i]
		k <- 1
		for(j in seq_along(temp)){
			if(pivots[j] == 1){
				temp[j] <- nonpivotcolumn[k]
				k <- k + 1
			}
		}
		ans[,c] <- temp
		c <- c + 1
	}
	return(ans)
} 


findnullspacenew <- function(A,rank,n,pivots){
	nonpivots <- (1:n)[pivots == 0]
    F <- A[(1:rank),nonpivots]
	F <- -1 * F
	temp <- diag(n-rank,n-rank)
    nullspace <- rbind(F,temp) 
}


findnullspacenew2 <- function(A,rank,m,n,pivots){
	ans <- matrix(nrow = n, ncol = (n-rank) )  
	nonpivots <- (1:n)[pivots == 0]
	pivots <- (1:n)[pivots == 1]
	
	#print(pivots)
	
	for(i in seq_along(nonpivots)){
		temp <- vector("numeric", length= n)
		temp[nonpivots[i]] <- 1
		ithnonpivotcol <- A[,nonpivots[i]]
		temp[pivots] <- (-1 * ( ithnonpivotcol[1:rank] ) ) #number of pivots = rank
		ans[,i] <- temp
		#print(temp)
    } 
    return(ans)
}

checksolnexist <- function(y,rank,m){
	tol <- 1e-5
	c <- 0
	start <- rank+1
	while(start<=m ){
		if( abs(y[start]) > tol){
			c <- 1
			break
		}
		start <- start + 1
	}
	if(c==0){
		return(TRUE)
	}
	else{
		return(FALSE) 
	} 
}

main <- function(myfilename){
    #myfilename <- "inputsmall3"
	inpframe <- getinput(myfilename)
	inpmatrix <- data.matrix(inpframe)
	#storage.mode(inpmatrix) <- "double"
	#inpmatrix <- matrix(c(2,2,-1,0,1,0,-1,-1,2,-3,1,0,1,1,-2,0,-1,0,0,0,1,1,1,0),nrow=4 , ncol=6 , byrow=TRUE)
	m <- nrow(inpmatrix)
	n <- ncol(inpmatrix)
	#print(m)
	#print(n)
	rre <- torref(inpmatrix,m,n)
	A <- rre[,1:(n-1)]
	y <- rre[,n]
	n <- (n-1)
	#rank <- findrank(A,m,n)
	
	pivots <- getpivots(A,m,n,rank)
	rank <- length((1:n)[pivots == 1])
	#print(rank)
	
	#print(pivots)
	columnspace <- inpmatrix[,(1:n)[pivots == 1]]
	write.table(columnspace,"out_1.txt" , row.names = FALSE ,col.names = FALSE,sep=",")
	
	if(rank == n ){
		#nullspace <- vector("numeric" , length=n)
		nullspace <- 0
	}else{
	nullspace <- findnullspacenew2(A,rank,m,n,pivots)
	}
	write.table(nullspace,"out_2.txt" , row.names = FALSE , col.names=FALSE,sep=",")
	#print(nullspace)
	

	if(!checksolnexist(y,rank,m)){
		solnspace <- 0
	}else{
		
		onesoln <- matrix(0, nrow=n, ncol = 1 )
		onesoln[(1:n)[pivots == 1],1] <- y[1:rank]
		
		if(rank == n){
			solnspace <- onesoln
		}else{
			temp <- matrix(0, nrow=n, ncol = (n-rank) )
			for(iter in 1:(n-rank)){
				temp[,iter] <- onesoln + nullspace[,iter]
			}
		    solnspace <- cbind(onesoln,temp)
		}	
		
	}
	
	write.table(solnspace,"out_3.txt",row.names = FALSE,col.names=FALSE,sep=",")
	#print(solnspace)
}

main(myfile)
