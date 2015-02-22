      ## Computing the inverse of a matrix can be resource intensive. 
      ## The following functions cache the inverse of a matrix so that it does not have to be computed repeatedly.
      
      ## makeCacheMatrix sets the value of the matrix and then gets the value of the matrix.
      ## The function then sets the value of the inverse of the matrix and then gets that same value.
      
      makeCacheMatrix <- function(x = matrix()) {
            z<-NULL
            set<-function(y){
                  x<<-y
                  z<<-NULL
            }
            get<-function() x
            setmatrix<-function(solve) z<<- solve
            getmatrix<-function() z
            list(set=set, get=get,
                 setmatrix=setmatrix,
                 getmatrix=getmatrix)
      }
      
      
      ## cacheSolve first checks to see if the inverse of the matrix is already known. 
      ## If the inverse is known, it returns that result. 
      ## If not, it computes the inverse and sets it in the cache.
      
      cacheSolve <- function(x, ...) {      
              ## Return a matrix that is the inverse of 'x'
            z<-x$getmatrix()
            if(!is.null(z)){
                  message("returning cached data")
                  return(z)
            }
            matrix<-x$get()
            z<-solve(matrix, ...)
            x$setmatrix(z)
            z
      }
      
      
      ## Example Test:
      x <- rbind (c(1,2), c(2,1))
      z <- makeCacheMatrix(x)
      cacheSolve(z)

