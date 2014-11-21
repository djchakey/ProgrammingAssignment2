makeCacheMatrix <- function(x = matrix(),y = list(), z = list()) {
## Last Change 20141120 @13:00
## This function creates a special "matrix" object that can cache its inverse
## x=Input Matrix, y=Matrix Vector Cache, z=Inverse Matrix Vector Cache
## Get Input matrix x, See if previously cached by comparing input matrix x with those previously cached in y
## If y list is empty, add the matrix to the cache list, get the inverse, add inverse to z list, return inverse
## Does x = any matrix in the y list (answer<- x==y[n]; truth<-!answer; sum(truth);if the sum is zero we have a match)
## Loop through every entry in the list to make that determination
## If no match,add matrix to vector cache, obtain inverse matrix, add inverse matrix to z list cache and return matrix
## If we get a match, pull the cached inverse matrix from the z list and return the inverse matrix 
## If y is the vector cache them y[1]<-list(x) would insert the matrix into position 1 of the vector list  
nmb_rows<-0
nmb_cols<-0
nmb_matrices_in_ylist<-length(y)
nmb_matrices_in_zlist<-length(z)
## check for a valid x matrix
  if (is.matrix(x) < 1)
  {
    print("x is not a matrix class")
    break
  }
  if (nmb_matrices_in_ylist != nmb_matrices_in_zlist)
  {
    print("Indexing mis-match between cached lists -- Original vs Inverted")
    break
  }
## No need to check cache on the 1st attempt, get the inverse, cache it and return inverse to caller

  if (length(y)<1)
  {
    Imatrix<-cacheSolve(x,nmb_matrices_in_ylist,y,z)
    print("1st Attempt Check Cached y Matrix")
    print(y)
    thelist<-list(Imatrix,y,z)
    return(thelist)
  } 

## The returned list contains the following results:
## Inverted Matrix =  thelist[[1]][[1]]
## Cached Input Matrix List = thelist [[1]] [[2]]
## Cached Inverted Matrix list = thelist [[1]] [[3]]




## If it's not the 1st attempt, Loop through all matrices in the cache looking 
## for a match, -- the sum of "truth" will be zero when a match is detected
## See if the lapply function can be substituted for the "for loop" if time permits
## Determine if the input matrix x matches any matrix in the y list by the following method:
## (answer<- x==y[n]; truth<-!answer; sum(truth);if the sum is zero we have a match)
## If no match,add matrix to vector cache, obtain inverse matrix, add inverse matrix to z list cache and return matrix
## If we get a match, pull the cached inverse matrix from the z list and return the previously computed inverse matrix 
## If y is the vector cache them y[1]<-list(x) would insert the input matrix x into position 1 of the vector list 
  
  for (i in 1:nmb_matrices_in_ylist)
  {
    answer<- x==y[[1]][[i]]
    truth<-!answer
    gotamatch<-sum(truth)
  }
    
## When every cell matches completely, the !answer will contain all zeroes and 
## the sum(truth) will equal zero
  print(gotamatch)  
  if (gotamatch < 1)
    {
    print("Got a Match!")
    thelist<-list(Imatrix,y,z)       
    return(thelist)
    }  
            
## The returned list contains the following results:
## Inverted Matrix =  thelist[[1]] [[1]]
## Cached Input Matrix List = thelist [[1]] [[2]]
## Cached Inverted Matrix list = thelist [[1]] [[3]]  


## Since there was No match, add matrix to vector cache, obtain inverse matrix, 
## add inverse matrix to z list cache and return matrix
  
  Imatrix<-cacheSolve(x,nmb_matrices_in_ylist,y,z)
  thelist<-list(Imatrix,y,z)
  return(thelist)
  
}

## Function cacheSolve calculates the inverse of the provided matrix
## and caches both the original and inverted matrices by appending it to the respective y and z lists
## The argument cachepos points to the last cached entry in the y and z lists

cacheSolve <- function(x = matrix, cachepos=integer, y=list(), z=list()) {
## Returns a matrix that is the inverse of the submitted matrix 'x' along with the cached lists y and z inorder
## to preserve the storage locations of the callers cache memory
  print("cacheSolve x Matrix")
  print(x)
  Inverse_Matrix<-solve(x)
## cache the inverse of x on the z list at the z length + 1 position and cache the x matrix as well
  position<-cachepos + 1
  z[position]<-list(Inverse_Matrix)
  y[position]<-list(x)
  print("Inverse Matrix")
  print(Inverse_Matrix)
  print("Position Index")
  print(position)
  print("Cached Position Y Matrix")
  print(y[position])
  print("Cached Position Z Matrix")
  print(z[position])
  thelist<-list(Inverse_Matrix,y,z)
  return(thelist)
}

