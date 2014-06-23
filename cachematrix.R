
## The function returns the inverted matrix, but since the process is computationally 
## intensive, it saves the inverted matrix in cache to facilitate its further retrieval. There are
## two parts to the function - the first creates a matrix object via super assignement
## operator that works across environments. The second part computes the inverse, but 
## first checks whether it was computed already and, if yes, simply returns it from cache.

makeCacheMatrix <- function(x = matrix()) {
      m<-NULL ##assign the inverse matrix to NULL

      set<-function(y){
            x<<-y
            m<<-NULL
      } ## super-assign the initial matrix to variable
      get<-function() 
            x
      setmatrix<-function(mat) 
            m<<-mat
      getmatrix<-function() 
            m
      list(set=set, get=get,
           setmatrix=setmatrix,
           getmatrix=getmatrix)
}


## cacheSolve computes the inverse of the matrix created in the above function, taking it
## as an arugment. It first checks whether it was computed and, if yes, retruns it from cache.
## If not, it computes the inverse and stores it in cache.

cacheSolve <- function(x, ...) {
      m<-x$getmatrix() ## assign the value returned from the above function
      if(!is.null(m)){
            message("getting cashed matrix")
            return(m)
      } ## check whether the cached matrix is not NULL
      data<-x$get() ## get data from the above
      m<-solve(data,...) ## inverse it
      x$setmatrix(m) 
      m
      ## Return a matrix that is the inverse of 'x'
}