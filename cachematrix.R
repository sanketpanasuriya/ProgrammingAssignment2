#makeCacheMatrix is a function which creates a special 
#"matrix" object that can cache its inverse for the input 
#(which is an invertible square matrix)

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  
  get <- function() x
  
  setInv <- function(inverse) inv <<- inverse
  
  getInv <- function() inv
  
  list(set = set, get = get, getInv = getInv, setInv = setInv)
} 


#cacheSolve is a function which computes the inverse of the special "matrix"
#returned by makeCacheMatrix above. If the inverse has already been calculated
#(and the matrix has not changed), then the cachesolve should retrieve the
#inverse from the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getInv()
  
  if(!is.null(inv)){
    message("getting cached result")
    return(inv)
  }
  
  data <- x$get()
  inv <- solve(data, ...)
  x$setInv(inv)
  inv
}

# 
# my_matrix <- makeCacheMatrix(matrix(1:4, 2, 2))
# 
# my_matrix$get()
# my_matrix$getInv()
# 
# cacheSolve(my_matrix)
# cacheSolve(my_matrix)
# my_matrix$getInv()
