args <- commandArgs(trailingOnly = TRUE)
filename<-args[1]
myinp=read.csv(filename, sep=",", header=FALSE)
inp=as.matrix(myinp)
inpCopy=inp
nRows=nrow(inp)
nCols=ncol(inp)

i=1
j=1
while(i<=nRows && j<=(nCols-1))	
{
	if(abs(inp[i, j])>0.00001)
	{
		inp[i, ]<-inp[i, ]/inp[i,j]

		for(k in 1:nRows)
		{
			if(k!=i)
			{
				inp[k, ]<-inp[k, ]-inp[k,j]*inp[i, ]
			}
		}
		i=i+1
		j=j+1

	}
	else
	{
		k=i
		while(k<=nRows)
		{
			if(abs(inp[k,j])>0.00001)
			{
				break
			}
			k=k+1
		}
		if(k==nRows+1)
		{
			j=j+1
		}
		else
		{
			temp=inp[i, ]
			inp[i, ]=inp[k, ]
			inp[k, ]=temp
		}
	}
	
}

#Removing precision errors
for(i in 1:nRows)
{
	for(j in 1:nCols)
	{
		if(abs(inp[i,j])<0.00001)
		{
			inp[i,j]=0
		}
	}
}

A=inp[1:nRows, 1:(nCols-1)]
b=inp[1:nRows, nCols:nCols]

rc=0
rn=0
U_cols=matrix(nrow=nRows, ncol=0)
N_cols=matrix(nrow=(nCols-1), ncol=0)
k=1
rowNumbers=c(-1)
pivotRowNumbers=c(-1)
pivotColNumbers=c(-1)
rank=0
j=1
for(i in 1:nRows)
{
	while(j<=(nCols-1))
	{
		if(A[i,j]==1)
		{
			tempRow=c(i)
			tempCol=c(j)
			pivotRowNumbers=c(pivotRowNumbers, tempRow)
			pivotColNumbers=c(pivotColNumbers, tempCol)
			rank=rank+1
			j=j+1
			break
		}
		else
		{
			j=j+1
		}

	}
}

j=2
for(i in 1:(nCols-1))
{
	if(j<=rank+1 && pivotColNumbers[j]==i)
	{
		rc=rc+1
		U_cols=cbind(U_cols, inpCopy[ ,i])
		j=j+1
	}
	else
	{
		tempCol=matrix(0, nrow=(nCols-1), ncol=1)
		tempCol[1:rank, 1]=tempCol[1:rank, 1]-A[1:rank, i]
		tempCol[i,1]=1
		N_cols=cbind(N_cols, tempCol)
		rn=rn+1
	}
}


if(nrow(U_cols)>0 && ncol(U_cols)>0)
{
	write.table(U_cols, file="out_1.txt", append=FALSE, sep=",", row.names=FALSE, col.names=FALSE)
} else {
	write(0, file="out_1.txt", append=FALSE)
}

if(nrow(N_cols)>0 && ncol(N_cols)>0)
{
	write.table(N_cols, file="out_2.txt", append=FALSE, sep=",", row.names=FALSE, col.names=FALSE)
} else {
	write(0, file="out_2.txt", append=FALSE)
}

#Checking if NO solution exist
solutionExists=TRUE
if(rc<nRows)
{
	i=rc+1
	while(i<=nRows)
	{
		if(b[i]!=0)
			solutionExists=FALSE
		i=i+1
	}
}


#Find particular solution
i=2
partSol=matrix(0, nrow=nCols-1, ncol=1)
while(i<(length(pivotRowNumbers)+1))
{
	partSol[pivotColNumbers[i], 1]=b[pivotRowNumbers[i]]
	i=i+1
}

finalSol=partSol
if(rn>0)
{
	for(j in 1:rn)
	{
		N_cols[ ,j]=N_cols[ ,j]+partSol
		j=j+1
	}
	finalSol=cbind(partSol, N_cols)
}

if(nrow(finalSol)>0 && ncol(finalSol)>0 && solutionExists)
{
	write.table(finalSol, file="out_3.txt", append=FALSE, sep=",", row.names=FALSE, col.names=FALSE)
} else {
	write(0, file="out_3.txt", append=FALSE)
}