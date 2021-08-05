## These functions are part of the second Programming Assignment from the
## R Programming Course in the Data Science Specialization on Coursera.

## Here we make use if the "<<-" operator to save some objects in the cache
## for further use. This is based on lexical soping which R uses internally 
## (e.g. to find symbols/objects internally).

## This function creates a special "matrix" object that can cache its inverse.
## This is helpful since calculating the inverse is "expensive" / time consuming
## for R. Calculating it once and saving the output temporarily within the
## user's cache can be time saving and hence very useful.
## The key in this function is that is builds a set of functions and returns 
## them within a list to the parent environment.

makeCacheMatrix <- function(x = matrix()) { #initializes x as empty matrix
  im <- NULL #initializes im as an object within the makeCacheMatrix() 
             # environment to be filled with the resulting inversed matrix later
  set <- function(y) { #defining the set function with temporary argument y 
    x <<- y            #(not x again to not confuse)
    im <<- NULL        #assigning to the parent environment trough "<<-"
  }
  get <- function() x  #y is retrieved from the parent environment
  setinverse <- function(inverse) im <<- inverse #defines the setter
  getinverse <- function() im #defines the getter for the inversed matrix
  list(set = set, #assigns each of these functions as an element within a list()
       get = get, #for the parent enironment
       setinverse = setinverse, #naming them to retrieve them later trough the
       getinverse = getinverse) #"$"-operator
}


## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated (and the 
## matrix has not changed), then cacheSolve should retrieve the inverse from 
## the cache.

cacheSolve <- function(x, ...) { #... allows for further arguments
  im <- x$getinverse() #retrieves already inversed matrix for cache checking
  if(!is.null(im)) { #checks if the inversed matrix in cache isn't empty
    message("getting data from the cache")
    return(im) #returns the inversed matrix from cache and ends the function
  }
  data <- x$get() #otherwise it calculates the inversed matrix
  im <- solve(data, ...) #taking the inverse since no chached can be found
  x$setinverse(im) #sets the output in the input object
  im #returns the (newly created) matrix that is the inverse of 'x' 
}

## Testing the function
# A simple matrix m1 with a simple matrix inverse n1
# Creating a matrix with randomly choosen entries
m1 <- matrix(c(4, 8, -4, -16), nrow = 2, ncol = 2)
m1

# Create Identity matrix I2
I2 <- matrix(c(1,0,0,1), nrow = 2, ncol = 2)
I2

# When n1 is the inverse of m1, then
# n1 %*% m1 is equal to I2
n1 <- matrix(c(.5,.25,-.125,-.125), nrow = 2, ncol = 2)
n1
# Checking:
m1 %*% n1
n1 %*% m1
# True both ways
solve(m1)
solve(n1)
# Also with the (used) solve() function

# Now the test
myMatrix_object <- makeCacheMatrix(m1)
cacheSolve(myMatrix_object)
# should return exactly the matrix n1
# calling cacheSolve again should retrieve (not recalculate)
cacheSolve(myMatrix_object)
