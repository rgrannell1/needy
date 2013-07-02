
#' @export

require_a <- function (properties, value) {
	# test that a value has the specified properties

	this_call <- deparse_to_string( sys.call() )
	pframe <- parent.frame()

	value_name <- deparse_to_string(
		as.list( match.call()[-1] )$value)

	if (missing(value)) {
		stopf (
			"%s: the parameter 'value' was missing but is required", 
			this_call)
	}
	if (missing(properties)) {
		stopf (
			"%s: the parameter 'properties' was missing but is required", 
			this_call)
	}
	if (!is.character(properties)) {
		stopf (
			"%s: properties must be a character vector", 
			this_call)
	}
	if (length(properties) == 0) {
		return (TRUE)
	}
	
	keys <- parse_properties(properties)
	check_properties(keys, value)
}

parse_properties <- function (properties) {
	# process the raw seach terms, and output
	# a list of terms corresponding to processes

	this_call <- "require_a(properties, value)"
	split_regexp <- '[ \t\n]+'

	keys <- lapply(properties, function (group) {

		members <- strsplit(group, split_regexp)[[1]]
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
	msg <- list (
		non_boolean = 
			'%s: the test for the property "%s" returned a non true/false value: actual value was %s',
		error_encounted = '%s: an error was encountered while testing for the the property "%s": \n %s'
	) 

	for (propgroup in properties) {

		group_matched <- TRUE
		
		for (propname in propgroup) {
			
			member_matched <- tryCatch({
					# testing the value is risky, so do it
					# in a trycatch

					predicate <- property_tests[[propname]]
					result <- predicate(value)

					if (!is.logical(result) || is.na(result)) {
						stopf(msg$non_boolean,
							this_call, propname, deparse_to_string(result))

					} else result

				},
				error = function (error) {
					stopf(msg$error_encounted,
						this_call, propname, error$message
					)
				}
			)

			# short-circuit if the member didn't match
			if (!member_matched) {
				group_matched <- FALSE
				break
			} else {
				group_matched <- group_matched && member_matched
			}
		}

		if (group_matched) {
			return (TRUE)
		}
	}

	human_readable_properties <- 
		paste0( sapply(properties, function (group) {
			paste0(unlist(group), collapse = ' and ')
		}), collapse = ', or ' )

	stopf(
		"%s: the value %s didn't satisfy any of the following groups of properties:\n %s",
		this_call, deparse_to_string(value), human_readable_properties

	)

}








