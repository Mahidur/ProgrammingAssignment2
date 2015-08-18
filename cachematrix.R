## There are two functions here namely "makeCacheMatrix" and "cacheSolve". The first function makes an invertible matix, calculates and caches its inverse. The second function returns the invrse of the same matrix by retreiving the cached value, if the matrix has not changed. Otherwise, the function calculates and returns the inverse of the same matrix. This is an important function which helps to save time by avoiding the repeated calculation of a given parameter of a matrix when the matrix have not changed. For example, if we have to calculate the mean temperature of the day everyday starting say from January 1, 2000, we can calculate the mean of the entire dataset or all the days since January 1, 2000 upto yesterday say August 18, 2015 for once and then "cache" the result. Next time, if we have to calculated the mean temperature per day on August 25, 2015, we can retrieve the cached mean and calculate the mean including the temperature of the later days. Thus everytime we have to calculate the mean with some new data, we do not have to compute afresh the entire dataset. This will save huge amount of time compared to the situation where we have to calculate the mean for entire dataset for every new day added for calculation. 

## makeCacheMatrix is a function which makes an invertible matrix, calculates the inverse of the matrix and caches the inverse value. 

makeCacheMatrix <- function(x = matrix()) {
				ivs <- NULL 				## default inverse is null
				setmatrix <- function(y) {  ## matrix is the function of y where
					x <<- y					## x assigned to be y in an enviornment different from the current enviornment 
					ivs <<- NULL
				}
				getmatrix <- function()x	## gets the matrix x
				setinverse <- function(solve) ivs <<- solve ## calculates the inverse of the matrix x 
				getinverse <- function() ivs  ## returns the inverse of the matrix x with the following list of functions which caches the value and is later to be used by the function cacheSolve()
				list(setmatrix = setmatrix, getmatrix = getmatrix,
				setinverse = setinverse,
				getinverse = getinverse)
}

 
## cacheSolve has two functions. If the matrix has not changed, cacheSolve retrieves the cached inverse of the matrix and returns the value. However, if the matrix has changed or there is no cached inverse of the matrix, then cacheSolve calculates and returns the inverse of the matrix. 

cacheSolve <- function(x, ...) {
			ivs <- x$getinverse()  ## creates an empty inverse matrix
			if (!is.null(ivs)) {	
					message("getting cached data")
					return(ivs)
			} 		 ## returns the inverse of the matrix x from the cache if available, otherwise 
			data <-x$getmatrix()  ## creates an empty matrix
			ivs <- solve(data, ...)  ## it calculates and 
			x$setmatrix(ivs)
			ivs
        ## Returns a matrix that is the inverse of 'x'
}
