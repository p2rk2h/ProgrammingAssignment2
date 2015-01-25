makeCacheMatrix <- function( x = matrix( ) ) {
	mInv <- NULL	# set mInv to NULL (default if cacheSolve has not been called)
	set <- function( y ) {	# set matrix
		x <<- y	# cache input matrix so cacheSolve can check if input changed
		mInv <<- NULL	# set inverse matrix in cacheSolve to NULL
	}
	get <- function( ) { x }
	setInverse <- function( solvedInv ) { mInv <<- solvedInv }
	getInverse <- function ( ) { mInv }
	list( set = set , get = get , setInverse = setInverse , getInverse = getInverse )
}
 
cacheSolve <- function( x = matrix( ) , ... ) {	# compare matrix
	m <- x$getInverse( )	# get cached inverse matrix
	if( ! is.null( m ) ) {	# check if already cached
	#	if ( x$set( ) == x$get( ) ) {	# ck same input matrix???
		message( "getting cached data" )
		return( m )	# return cached inverse
	#	}
	}
	newMatrix <- x$get( )	# get new input matrix
	x$set( newMatrix )	# NB cache new input matrix
	m <- solve( newMatrix , ... )	# compute inverse matrix m
	x$setInverse( m )	# cache inverse matrix
	m	# return computed inverse
 }
