## Assignment: Caching the Inverse of a Matrix

# makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()){ 
    m <- NULL
    set <- function(y) { # define vector value
        x <<- y
        m <<- NULL
    }
    get <- function() x # obtain the vector value
    setinverse <- function(inverse) m <<- inverse #define inverse value
    getinverse <- function() m #obtain inverse value
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

#cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
# If the inverse has already been calculated (and the matrix has not changed), 
#then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...){ #Fuction return x inverse matrix
    m <- x$getinverse() 
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m
}
