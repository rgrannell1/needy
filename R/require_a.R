
#' @export

require_a <- function (properties, value) {
	# test that an object with the appropriate values
	# exists in the parent frame

	this_call <- deparse_to_string( sys.call() )
	pframe <- parent.frame()

	value_name <- deparse_to_string(
		as.list( match.call()[-1] )$value)

	if ( missing(value)) ) {
		stopf ("%s: the parameter value was missing but required", 
			this_call)
	}
	if ( missing(properties)) ) {
		stopf ("%s: the parameter properties was missing but required", 
			this_call)
	}
	if (!is.character(properties)) {
		stopf ("%s: properties must be a character vector", 
			this_call)
	}
	if (length(properties) == 0) {
		return (TRUE)
	}

	# split each composite property string 
	# into a character vector

	keys <- lapply(properties, function (group) {
		
	})

	# reorder the keys so that shorter composite 
	# properties are checked first

	group_sizes <- vapply(keys, length, 'a')
	keys <- keys[ sort(order(group_sizes)[keys]) ]



}

property_tests <- new.env()











