## Creates a special matrix which is a list that sets the value of the matrix, 
## gets the value of the matrix, sets the inverse of the matrix, 
## and then gets the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y = matrix()) {
        x <<- y *
            m <<- NULL
    }
    
    get <- function()
        x
    setinverse <- function(solve)
        m <<- solve
    getinverse <- function()
        m
    list (
        set = set,
        get = get,
        setinverse = setinverse,
        getinverse = getinverse
    )
}

## Creates the inverse of a matrix created from the function above
## The function checks if the inversed matrix has already been created
## If it has, then it pulls the inverse matrix from the cache and skips the inverting
## Otherwise, the matrix is inversed and sets the matrix to be cached

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m = x$getinverse()
    
    if (!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    
    mat.data <- x$get()
    m <- solve(mat.data, ...)
    x$setinverse(m)
    return(m)
}