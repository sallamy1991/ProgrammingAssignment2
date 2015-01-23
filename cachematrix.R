## makeCacheMatrix function transfers the matrix into a special type matrix.
## This special type is a list, with setters and getters to the matrix and the matrix inverse.
## In this new matrix type, the Inverse of the matrix can be calculated and cached.

## cacheSolve is a function that takes a special type matrix and returns the matrix inverse.


## The following function creates a special type of matrix.
## The input is a normal matrix, the output is a list -The special type matrix-
## The function has setters and getters to set and get the matrix value and the matrix inverse.

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    # setting matrix value
    set<- function(y){
        x <<- y
        inv <<- NULL
    }
    # getting matrix value
    get<- function() x
    # setting matrix invere (caching it)
    setInv <- function(inverse) inv<<-inverse
    # getting function inverse
    getInv <- function() inv
    # returns a special type matrix defined a as a list
    list(set = set, get=get,setInv = setInv, getInv = getInv)
}


## The following function takes a special type matrix returned from the function above
## a an input, and returns its inverse.
## If the inverse is cached it is returned, else, it is calculated, then cached

cacheSolve <- function(x, ...) {
    # Getting matrix aved inverse.
    inv <- x$getInv()
    # Check if the invere is cached
    if(!is.null(inv)){
        message("getting cached data")
        # Return cached result
        return(inv)
    }
    # Getting matrix data.
    data <- x$get()
    # Calculating the matrix inverse
    inv <- solve(data, ...)
    # Caching the calculatedinverse
    x$setInv(inv)
    # Returning the matrix inverse
    inv
}