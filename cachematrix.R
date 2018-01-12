## creates a 'special' Matrix, ie cachematrix where it can have its own inverse associated with it.
## if the inverse is asked of it, it searches for the inverse in the cache first thing; if found
## returns it, else computes it, stores it in the cache and returns it.

## Creates a special matrix with 4 functions named get, set, getInvMat, setInvMat associated with it.

makeCacheMatrix <- function(Mat = matrix())    {
    InvMat <<- NULL

    setMat <- function(M)   {
        Mat <<- M
        InvMat <<- NULL
    }
    getMat <- function()    {
        Mat
    }

    setInvMat <- function(InvMatrix)    {
        InvMat <<- InvMatrix
    }
    getInvMat <- function() {
        InvMat
    }

    list(set = setMat, get = getMat,
         setInvMat = setInvMat, getInvMat = getInvMat )
}


## if the inverse is already in the cache, the function retreives it and returns
## otherwise computes the function computes the inverse, pushes to cache and returns it

cacheSolve <- function(M, ...)  {
    ## Return a matrix that is the inverse of 'x'
    Mat <- M$get()
    if(is.matrix(M$getInvMat())) {
        invMat <- M$getInvMat()
    }
    else    {
        invMat <- solve(Mat)
        M$setInvMat(invMat)
    }

    invMat
}