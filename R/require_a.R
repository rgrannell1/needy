
#' @export

require_a <- function (properties, value) {
	# test that a value has the specified properties

	this_call <- deparse_to_string( sys.call() )
	pframe <- parent.frame()

	value_name <- deparse_to_string(
		as.list( match.call()[-1] )$value)

	if (missing(value)) {
		stopf ("%s: the parameter 'value' was missing but is required", 
			this_call)
	}
	if (missing(properties)) {
		stopf ("%s: the parameter 'properties' was missing but is required", 
			this_call)
	}
	if (!is.character(properties)) {
		stopf ("%s: properties must be a character vector", 
			this_call)
	}
	if (length(properties) == 0) return (TRUE)
	
	keys <- parse_properties(properties)
	check_properties(keys, value)
}

parse_properties <- function (properties) {
	# process the raw seach terms, and output
	# a list of terms corresponding to processes

	this_call <- "require_a(properties, value)"

	keys <- lapply(properties, function (group) {

		members <- strsplit(group, '[ \t\n]+')[[1]]
		invalid <- setdiff(members, 
			property_tests$listed_properties)

		if (length(invalid) > 0) {

			what <- if (length(invalid) == 1) {
				'property'
			} else 'properties'  

			stopf("%s: unrecognised %s (%s)", 
				this_call, what,
				paste0(invalid, collapse = ', '))
		}

		members
	})

	# reorder the keys so that shorter composite 
	# properties are checked first

	group_sizes <- vapply(keys, length, 1)
	keys
}

check_properties <- function (properties, value) {
	# does the value have at least one 
	# group of properties?
	# if yes, return true.
	# otherwise, throw a descriptive error
	
	this_call <- "require_a(properties, value)"

	for (propgroup in properties) {

		group_matched <- TRUE
		
		for (propname in propgroup) {

			predicate <- property_tests[propname]
			
			member_matched <- tryCatch({
					# testing the value is risky
					result <- predicate(value)

					if (!is.logical(result) || result) {
						stopf(
							"%s: the test %s returned a non true/false value: actual value was %s",
							this_call, propname, deparse_to_string(result))

					} else result

				},
				,
				warning = function (warning) {

				},
				error = function (error) {

				}
			)

			if (!member_matched) break else {
				group_matched <- group_matched && member_matched
			}
		}

		if (group_matched) return (TRUE)
	}



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
	lookup$character =
		is.character

	# object related predicates
	lookup$object = 
		function (value) {
			!is.null(attr(value, 'class'))
		}
	lookup$s4 = 
		isS4


	lookup$listed_properties <- ls(lookup)
	lookup
} )()











