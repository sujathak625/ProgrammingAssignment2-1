## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeVector <- function(x = numeric()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setmean <- function(mean) m <<- mean
        getmean <- function() m
        list(set = set, get = get,
             setmean = setmean,
             getmean = getmean)
}

## Write a short comment describing this function

cachemean <- function(x, ...) {
        m <- x$getmean()
        if(!is.null(m)) {
                message("Mean cached already")
                return(m)
        }
        data <- x$get()
        m <- mean(data, ...)
        x$setmean(m)
        m
}



# Creating a matrix using matrix() function and passing it to makeCacheMatrix

makeCacheMatrix <- function(x = matrix()) {
        inverse1 <- NULL
        set <- function(y) {
                x <<- y
                inverse1 <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) inverse1 <<- inverse
        getInverse <- function() inverse1 
        list(set = set,
             get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


# Call the cacheSolve function first time for the matrix
# Call again the cacheSolve without changing the matrix. It would return it 
# from the cache
# > m<-makeCacheMatrix(matrix(c(2, 0, 0, 1), c(2, 2)))
# > cacheSolve(m)
#     [,1] [,2]
#[1,]  0.5    0
#[2,]  0.0    1
#> cacheSolve(m1)
# fetching cached matrix
#     [,1] [,2]
#[1,]  0.5    0
#[2,]  0.0    1
# > m<-makeCacheMatrix(matrix(c(2, 0, 0, 2), c(2, 2)))
#> m<-makeCacheMatrix(matrix(c(2, 0, 0, 2), c(2, 2)))
#> cacheSolve(m)
#     [,1] [,2]
#[1,]  0.5  0.0
#[2,]  0.0  0.5
#> m1<-makeCacheMatrix(matrix(c(2, 0, 0, 2), c(2, 2)))
#> cacheSolve(m1)
#     [,1] [,2]
#[1,]  0.5  0.0
#[2,]  0.0  0.5
#> cacheSolve(m1)
#fetching cached matrix
#     [,1] [,2]
#[1,]  0.5  0.0
#[2,]  0.0  0.5

cacheSolve <- function(x) {
        ## Return a matrix that is the inverse of 'x'
        inverse1 <- x$getInverse()

        #Checks if the matrix inverse already exists in cache
        if (!is.null(inverse1 )) {
                message("fetching cached matrix")
                return(inverse1 )
        }

        # If inverse is not cached, it gets the matrix
        mat <- x$get()

        # computes the inverse
        inverse1 <- solve(mat)

        # Sets the inverse in the cache
        x$setInverse(inverse1 )

        # prints the inversed matrix
        inverse1 
}