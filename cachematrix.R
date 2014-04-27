## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
## creates a function with argument "x"
	solveMatrix <- NULL
##creates empty object "solveMatrix", to add returned values
	set <- function(y) {
##creates function that accepts argument "y", allows to reset the data used
		x <<- y
		solveMatrix <<- NULL
## assigns "x" value of "y" and "solveMatrix" value of NULL, in the first PARENT environment level 
where "x" and "m" exist... effectively, this resets "m" to be null when new "x" is passed
	}
	get <- function () x
##creates a function that accepts no arguments but returns x 
	setInverse <- function(Inverse) solveMatrix <<- Inverse
## creates a function that takes an argument and reassigns the value of solveMatrix in the parent environment
	getInverse <- function () solveMatrix
## creates a function that accepts no arguments but returns solveMatrix
	list (set = set, get = get, 
	setInverse = setInverse, 
	getInverse = getInverse)
## creates a list of the above four functions, which is used as the argument for cacheSolve
}

## Write a short comment describing this function

cacheSolve <- function(x) {
## creates a function with argument which is a list returned by makeCacheMatrix
        ## Return a matrix that is the inverse of 'x'
	solveMatrix <- x$getInverse()
## assigns solveMatrix to the value of solveMatrix specified in the parent environment to the environment in which x$getInverse was defined
	if(!is.null(solveMatrix)) {
		message("Getting cached data")
		return(solveMatrix)
## if "solveMatrix is not null" is TRUE, return this message and cached value of solveMatrix
	}
	data <- x$get()
## otherwise, solve - assign data to the argument given to makeCacheMatrix
	solveMatrix <- solve(data)
## call solve on that data and assign the value to solveMatrix
	x$setInverse(solveMatrix)
## gives solveMatrix as argument and sets this value of solveMatrix at the parent environment to the environment in which x$setInverse was defined
	solveMatrix
## returns value of solveMatrix
}

