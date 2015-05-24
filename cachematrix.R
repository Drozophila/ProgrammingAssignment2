## This pair of matrices calculates the inverse of a matrix and stores
## the value in cache for later use to avoid recalculating

## Function makeCacheMatrix creates a list containing 4 elements:
## 	1. set the value of a matrix
## 	2. get the value of a matrix
## 	3. set the value of an inverse of the matrix
## 	4. get the value of an inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
	i <- NULL
	set <- function(y){		## If we ever call the set function we
		x <<- y			## can replace the matrix makeCacheMatrix
		i <<- NULL			## is using with another one
	}
	get <- function(){x}
	setinv <- function(inv){i <<- inv}
	getinv <- function(){i}
	list(set=set,get=get,setinv=setinv,getinv=getinv)
}


## Function cacheSolve checks if the inverse is stored in cache,
## calculates and stores it if it isn't, else uses the stored value 

cacheSolve <- function(x,...){
	i <- x$getinv()
	if(is.null(i)){
		data <- x$get()
		i <- solve(data,...)
		x$setinv(i)
		}
	else{
		message("getting cached data")
	}
	i
}
