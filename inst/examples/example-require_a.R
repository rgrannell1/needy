
safeMap <- function (f, x) {
	# map, with verification by require_a

	pcall <- "safeMap(f, x)"
	require_a('unary function', f, pcall)
	require_a('listy', x)

	Map(f, x)
}

safeSum <- function (a, b) {

	require_a( "finite numeric", a, sys.call(sys.parent()) )
	require_a( "finite numeric", b, sys.call(sys.parent()) )

	a + b
}
