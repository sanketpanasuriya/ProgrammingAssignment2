#makeCacheMatrix is a function which creates a special 
#"matrix" object that can cache its inverse for the input 
#(which is an invertible square matrix)

makeCacheMatrix <- function(x = matrix()) {
        ## The process initialize the inverse property
        inv <- NULL
  
        ## Process to set the matrix
        set <- function(y){
            x <<- y
            inv <<- NULL
        }
        
        ## Process the get the matrix
        get <- function() x
        
        ## Process to set the inverse of the matrix
        setInv <- function(inverse) inv <<- inverse

        ## Process to get the inverse of the matrix
        getInv <- function() inv

        ## Return a list of the processes facts
        list(set = set, get = get, getInv = getInv, setInv = setInv)
} 


#cacheSolve is a function which computes the inverse of the special "matrix"
#returned by makeCacheMatrix above. If the inverse has already been calculated
#(and the matrix has not changed), then the cachesolve should retrieve the
#inverse from the cache

cacheSolve <- function(x, ...) {
        
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getInv()
         
        ## Return the inverse if its already set
        if(!is.null(inv)){
              message("getting cached result")
              return(inv)
        }
        
        ## Get the matrix from the data 
        data <- x$get()
  
        ## Use matrix multiplication to get the inverse 
        inv <- solve(data, ...)
  
        ## Set the inverse
        x$setInv(inv)
  
        ## return inverse matrix 
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
