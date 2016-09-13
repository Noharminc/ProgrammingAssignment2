#########################################################################################
## FUNCTIONS MakeCacheMatrix / cacheSolve
##    Version 0.1
##    Noharminc / 20160913
##    Coursera | Datascience | R Programming | Week 3 | Programming Assigment 2
#########################################################################################


#########################################################################################
## makeCacheMatrix
#     args              matrix
#     output            list with four functions (set / get / setInverse / getInverse)
#     functionality     create a list with four matrix functions
#########################################################################################

makeCacheMatrix <- function(x = matrix()) {
      # set n to NULL
      n <- NULL
      # set function
      set <- function(y) {
            # assign input value to object x (parent)
            x <<- y
            # assign NULL value to object n (parent)
            n <<- NULL
      }
      # get function
      get <- function() x
      # calculate inverse matrix, store in n (parent)
      setInverse <- function(value) n <<- solve(x)
      # get inverse matrix, stored in n  
      getInverse <- function() n
      # return named list with all functions created
      list(set = set, get = get, getInverse = getInverse, setInverse = setInverse)
      }


#########################################################################################
## cacheSolve
#     args              x     cache
#                       ...
#     output            x-1   inversed matrix
#     functionality     return a matrix that is the inverse of 'x'
#########################################################################################
cacheSolve <- function(input, ...) {
      # determine whether inversed matrix exists
      value <- input$getInverse()
      # if exists, return cached data
      if(!is.null(value)) {
            message("getting cached data")
            # return cached data and stop function
            return(value)
      }
      # else calculate inversed matrix
      data <- input$get()
      value <- solve(data, ...)
      input$setInverse(value)
      value
}


## End of lines