makeCacheMatrix <- function(x = matrix()) { # create a function which starts with a null matrix argument
        m <- NULL  # initialize the value of the matrix inverse to NULL
        set <- function(y) { # take the input matrix
                x <<- c(y) # save the input matrix
                m <<- NULL # reset the value of the inverse matrix
        }
        get <- function() { x } # return the input matrix
        setinv <- function(solve) { m <<- solve } # called by cacheSolve and will store the initial inverse matrix
        getinv <- function() { m } # gets the inverse matrix 
        list(set = set, get = get, setinv = setinv, getinv = getinv) # passes the value of the function makeCacheMatrix  
}
cacheSolve <- function(x, ...) { # the input x is an object created by makeCacheMatrix
        m <- x$getinv() # acesses the object 'x' and gets the inverse matrix
        if(!is.null(m)) {
                message("getting cached data")
                return(m) # if mean was already cached return it with the message
        }
        data <- x$get()
        m <- solve(data, ...) # if m was NULL then we have to invert the input matrix
        m # Return a matrix that is the inverse of 'x'
}
