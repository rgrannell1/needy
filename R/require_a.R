
#' @export

require_a <- function (properties, value) {
	# test that a value has the specified properties

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
	if (length(properties) == 0) return (TRUE)
	
	keys <- parse_properties(properties)


}

parse_properties <- function (properties) {
	# process the raw seach terms, and output
	# a list of terms corresponding to processes

	keys <- lapply(properties, function (group) {
		strsplit(group, '[ \t\n]+')[[1]]
	})

	# reorder the keys so that shorter composite 
	# properties are checked first

	group_sizes <- vapply(keys, length, 1)
	keys
}

property_tests <- ( function () { 
	# create a hash table containing 
	# property-testing functions

	lookup <- new.env(parent = emptyenv())
	lookup$any = 
		function (value) TRUE
	lookup$array =
		is.array
	lookup$atomic = 
		is.atomic
	lookup$call = 
		is.call






	lookup
} )()











