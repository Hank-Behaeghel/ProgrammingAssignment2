## Put comments here that give an overall description of what your
## functions do

## This function is supposed to cache the inverse of the matrix supplied

makeCacheMatrix <- function(x = matrix()) {
    #this sets the value of the matrix
    m <- NULL
    set <- function(y) {
        x <<- matrix(y)
        m <<- NULL
    }
    #Now that the value of the matrix is set we can solve and cache the inverse
    
    get <- function() x 
        setinverse <- function(solve) x <<- solve
        getinverse <- function() x
        list(set = set, get = get, setinverse = setinverse, 
             getinverse = getinverse)
}


## cacheSolve is supposed to return the inverse of a matrix 
## who's inverse has been cached

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    m <- x$getinverse()
    ##if the inverse exists in cahce we pull it by testing not(is.null)
    if(!is.null(m)) {
        message('getting cahced inverse')
        return(m)
    }
    ##if the inverse is not in the cahce this part solves the inverse then saves
    ## it in the cahce
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
}

