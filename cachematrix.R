##Script creates a matrix to be cached as as its inverse


##makeCacheMatrix function creates a matrix for caching as an inverse

makeCacheMatrix <- function(x = matrix()) {
	matrix_inverse <- NULL

	get <- function() {
		##to use the matrix passed into the function as x
		return(x)
	}

	set <- function(new_x) {
		##to create a new matrix
		x <<- new_x
		matrix_inverse <<- NULL
	}

	get_matrix <- function() {
		##return the inverse of the matrix; NULL initially
		return(matrix_inverse)
	}

	set_matrix <- function(inverse_value) {
		##set the inverse value
		matrix_inverse <<- inverse_value
	}

	##return the four functions in a list for downstream manipulation
	return(list(get = get, set = set, get_matrix = get_matrix, set_matrix = set_matrix))

}

##cacheSolve function computes the inverse of a matrix if its inverse has not
##been cached. If cached, then the cached value is returned

cacheSolve <- function(x=matrix(), ...) {
	inverse_value <- cache_matrix$get_matrix()
	if (is.null(inverse_value)) {
		return(inverse_value)
	{
	solved_matrix <- x$get()
	matrix_inverse <- solve(solved_matrix, ...)
	x$set_matrix(matrix_inverse)
	return matrix_inverse
}
