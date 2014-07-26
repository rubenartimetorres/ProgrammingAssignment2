## Coursera 
## R programming - Programming Assignmet 2

## This functions demostrates how to cache a previously done calculation
## The calculus is the inverse of a matrix
## To do so, we build a special object "cachematrix" that initially contains
## the matrix without inverse, but the first time the inverse is calculated
## using the solve() function, the inverse is stored in the object so the 
## second time we need it we don't calculate again, we recover the inverse stores 


## The function makeCacheMatrix builds an object with the parameter x
## The input parameter x is a matrix
## The output parameter is a list containing the original matrix x
## and a second parameter m wich initially is NULL. The list contains
## the following functions
## set.-
## get.- to access the original matrix x
## setinverse.- to set in the parameter m the inverse of x once calculated
## getinverse.- to accesee the parameter m

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## cacheSolve takes a matrix x as input and calculates and returns the inverse of x
## input parameter x.- a cached matrix
## output.- the inverse of the matrix x
## How it works.- this function looks for the inverse of the x object using the getinverse function
## if it returns NULL then it calculates the inverse using the solve method but previouly
## stores it for later access. If the getinverse method returns a NON-NULL object then it returns it

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  ## First see if the inverse is already calculated and stored
  m <- x$getinverse()
  ## if it is not null the return it
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  ## If the inverse is not cached then calculate
  ## Get the matrix stored in x
  data <- x$get()
  ## calculate the inverse
  m <- solve(data, ...)
  ## store the inverse for later access
  x$setinverse(m)
  ## return the inverse matrix
  m  
}

## This test function demostrates how to use the previous function

testCacheSolve <- function() {
  c1<-c(1,0,0)
  c2<-c(0,2,0)
  c3<-c(0,0,1)
  m1<-cbind(c1,c2,c3)
  message("let's calculate the inverse of...")
  print(m1)
  mc1<-makeCacheMatrix(m1)
  message("first calculus...")
  ic1<-cacheSolve(mc1)
  print(ic1)
  message("second calculus...")
  ic2<-cacheSolve(mc1)
  print(ic2)
}

