## Put comments here that give an overall description of what your
## functions do

##set- sets the matrix, get- retrieves the matrix, setinv- sets the inverse of matrix, getinv- retrieves the inverse of matrix

makeCacheMatrix <- function(x = matrix()) 
                {
                    inv <- NULL
                    set <- function(y) 
                          { x <<- y
                            inv <<- NULL
                          }
  
                    get <- function()
                    x
  
                   setinv <- function(inverse)
                   inv <<- inverse
  
                   getinv <- function()
                   inv
  
                   list(set = set, get = get, setinv = setinv, getinv = getinv)
  
                }


## Checks if the inverse is stored in cache and returns it, if not the new inverse is calculated

cacheSolve <- function(x, ...) 
        {
        
            inv <- x$getinv()
            if(!is.null(inv)) 
              {
                 message("getting cached data")
                 return(inv)
               }
            data <- x$get()
            inv <- solve(data, ...)
            x$setinv(inv)
            inv
        }
