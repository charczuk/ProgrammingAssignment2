##sets 4 vectors to combine in a list named set, get, setInverse, getInverse
##x and inv are cached to be referenced in second function
makeCacheMatrix <- function(x = matrix()) {
    
    ##assigns NULL value to inv
    inv <- NULL
    
      ##assigns matrix to set; backs up original data
      set <- function(y) {
        
          ##caches y as x; caches NULL as inv
          x <<- y
          inv <<- NULL
      }
    
    ##return x and assign to get
    get <- function() x
    
    ##assign inverse to inv
    setInverse <- function(inverse) inv <<- inverse
    
    ##return inv and assign to getInverse
    getInverse <- function() inv
    
    ##returns vector for all of these in list
    list(set = set,
         get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


##Step 1:  pulls inv in from cache, if it's not NULL, it outputs inv

##Step 2:  otherwise, it pulls get column from list in first function, 
##assigns to inv, runs inverse of new inv (get) and 
##assigns to inv cache; returns inv
cacheSolve <- function(x, ...) {
    
    inv <- x$getInverse()
    
    ##inv isn't NULL, output vector; otherwise, go get it, solve for it, 
    ##and output inv
    if (!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    
    mat <- x$get()
    inv <- solve(mat, ...)
    x$setInverse(inv)
    inv
}
