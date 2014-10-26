debugging = TRUE


# The first function, makeCacheMatrix creates a special "matrix", which is really a list containing a function to
# 	set the value of the matrix
# 	get the value of the matrix
# 	set the value of the inverse
# 	get the value of the inverse
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


# The following function calculates the inverse of the special "matrix" created with the above function. 
# However, it first checks to see if the inverse has already been calculated. 
# If so, it gets the inverse from the cache and skips the computation. 
# Otherwise, it calculates the inverse of the data and sets the value of the inverse in the cache via the setinverse function.
cacheSolve <- function(x, ...) {
    m <- x$getinverse()
    if(!is.null(m)) {
            message("getting cached data")
            return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m
}

########
# Matrix inversion is usually a costly computation and 
# their may be some benefit to caching the inverse of a matrix 
# rather than compute it repeatedly (there are also alternatives to matrix inversion that we will not discuss here). 
########

# makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
# makeCacheMatrix <- function(x = matrix()){
# 	specialMatrix <-  data.frame("matrix" = x, "inverse" = matrix()) 

# 	specialMatrix
# }


# # cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
# #  If the inverse has already been calculated (and the matrix has not changed), 
# #  then the cacheSolve should retrieve the inverse from the cache.
# cacheSolve <- function(x, ...){
# 	# Computing the inverse of a square matrix can be done with the solve function in R. 
# 	# For example, if X is a square invertible matrix, then solve(X) returns its inverse.
# 	if(is.null(x$inverse)){
# 		if(debugging){
# 			print("Calculating the inverse matrix")
# 		}
# 		x$inverse <- solve(x)
# 	}else{
# 		if(debugging){
# 			print("The inverse matrix was already calculated")
# 		}
# 	}
	
# 	## Return a matrix that is the inverse of 'x'
# 	x$inverse 
# }


test <-function(){
	B <- matrix( 
	   c(2, 4, 3, 1, 5, 7), 
	  nrow=2, 
	  ncol=2) ;
	print(B)

	Bcachable <- makeCacheMatrix(B)
	# print(Bcachable)

	Binverse <- cacheSolve(Bcachable)
	print(Binverse)	

	Binverse
}

if(debugging){
	x<-test()
}

