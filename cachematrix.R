## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
##
## makeCacheMatrix creates a special "matrix" in the form of a list.
## The output of the function is a list that contains the results of the internal computations of the function.
## First makeCacheMatrix sets the value of this matrix via the set function.
## Then gets the value of this matrix via the get function.
## The setinverse function sets and computes the inverse of the matrix.
## The getinverse functon gets the inverse matrix computed by setinverse.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y){
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) m <<- solve
        getinverse <- function() m
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## Write a short comment describing this function
##
## cacheSolve computes the inverse of the "special" matrix created as output of makeCacheMatrix
## cacheSolve first checks if the inverse of the matrix has been previously computed by makeCacheMatrix
## If the inverse has been alredy computed it gets the inverse via the getinverse from makeCacheMatrix 
## returns it and doesn't perform any computation.
## If the inverse hasn't been computed it computes the inverse of the given matrix and sets the result in cache
## via the setinverse function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x' 
        m <- x$getinverse()
        if (!is.null(m)){
                message("getting cached matrix")
                return(m)
        }
        matrix <- x$get()
        m <- solve(matrix, ...)
        x$setinverse(m)
        m
}
