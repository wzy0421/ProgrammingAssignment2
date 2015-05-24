## Functions in this file allows you to cache previously computed matrix inverse and
## return without extra calculation. First call makeCacheMatrix to get an obj for
## caching and use the functions in the returned obj to get/set matrix and get its inverse

## given a matrix, return the functions for setting/getting value and getting/setting inverse of the matrix;

makeCacheMatrix <- function(x = matrix()) {
  mat <- NULL;
  set <- function(y) {
    x <<- y;
    mat <<- NULL;
  }
  get <- function() x;
  set_inverse <- function(inv_mat) mat <<- inv_mat;
  get_inverse <- function() mat;
  list(
    set = set, 
    get = get, 
    get_inverse = get_inverse,
    set_inverse = set_inverse
  );
}


## Give a cacheMatrix returned by makeCacheMatrix, 
## this functions first try to get the cached inv result, 
## if there's not, calculate the inverse and cache & return it

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  cached_inv <- x$get_inverse();
  if (!is.null(cached_inv)) {
    message('getting cached data');
    return(cached_inv);
  }
  mat <- x$get();
  inv_mat <- solve(mat,...);
  x$set_inverse(inv_mat);
  return(inv_mat);
}
