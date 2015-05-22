test.makeCacheMatrix <- function() {
  matrix <- matrix(1:4, 2, 2)
  cachableMatrix <- makeCacheMatrix(matrix)
  
  checkEquals(matrix, cachableMatrix$getMatrix(), "Input matrix did not set properly")
  checkEquals(NULL, cachableMatrix$getInverse(), "Inverse should be NULL")
  
  matrix2 <- matrix(5:8, 2, 2)
  cachableMatrix$setMatrix(matrix2)
  
  checkEquals(matrix2, cachableMatrix$getMatrix(), "Setter did not set matrix properly")
}

test.cacheSolve <- function() {
  m <- matrix(7:10, 2, 2)
  inverseMatrix <- solve(m)
  
  checkEquals(diag(2), m %*% inverseMatrix)
  
  cachableMatrix <- makeCacheMatrix(m)
  i <- cacheSolve(cachableMatrix)
  
  checkEquals(inverseMatrix, i)
  checkEquals(inverseMatrix, cachableMatrix$getInverse())
  
  cachableMatrix$setMatrix(matrix(10:13, 2, 2))
  checkEquals(NULL, cachableMatrix$getInverse())
}

test.invalidSetSequnce <- function() {
  mat<-matrix(1:4, 2, 2)
  i<-solve(mat)
  n<-makeCacheMatrix()
  
  checkEquals(matrix(), n$getMatrix())
  checkEquals(NULL, n$getInverse())
  
  n$setMatrix(mat)
  
  checkEquals(mat, n$getMatrix())
  
  c<-cacheSolve(n)
  
  checkEquals(i, c)
  checkEquals(i, n$getInverse())
}

# test.deactivation <- function() {
#   DEACTIVATED('Deactivating this test function')
# }