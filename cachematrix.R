## This first function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) i <<- inverse
        getInverse <- function() i
        list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## cacheSolve is a function which computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then the second function should retrieve the 
## inverse from the cache

cacheSolve <- function(x, ...) {
## Return a matrix that is the inverse of 'x'
        i <- x$getInverse()
        if(!is.null(i)) {
                message("getting cached data")
                return(inv)
        }
        res <- x$get()
        i <- solve(res, ...)
        x$setInverse(i)
        i
}
Checking:

x <- matrix(rnorm(11),5,5)
x1 <- makeCacheMatrix(x)
cacheSolve(x1)

[,1]      [,2]      [,3]       [,4]        [,5]
[1,] -1.240635  2.319679 -1.678644  0.6219847  0.38235459
[2,]  2.336214 -2.468674  1.109675 -0.1411495 -0.02836016
[3,] -3.870524  2.161947  1.962985 -2.5802404  1.12994664
[4,] -1.338204  2.599266 -1.916088  0.6228841 -0.59677956
[5,]  5.037333 -6.378886  3.700099  0.9311596  0.48545208
