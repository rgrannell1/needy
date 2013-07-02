
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
	# if yes, return true. otherwise, throw a descriptive error
	
	this_call <- "require_a(properties, value)"
	
	report <- list (
		non_boolean = 
			function (val, property) {
				# report that the value wasn't a boolean value,
				# along with the property being tested

				msg <- '%s:
					the test for the property "%s" returned a non true/false value:
					actual value was %s'

				stopf(msg,
					this_call, property, deparse_to_string(result))
			},
		no_match =
			function (value) {
				# report that the value didn't
				# match any the required properties
				
				msg <- "%s: the value %s didn't match any of the following:
				%s"

				and_collapse <- function (x) {
					paste0(x, collapse = ' and ')
				}
				or_collapse <- function (x) {
					paste0(unlist(x), collapse = ', or ')
				}

				readable_properties <- 
					or_collapse(sapply(properties, and_collapse))

				stopf(
					msg, this_call,
					deparse_to_string(value),
					readable_properties)
			}
		error_encountered = 
			function (error) {
				# report the error along with what
				# was being tested at the time

				msg <- '%s:
				an error was encountered while testing for the the property "%s":
				%s'

				stopf(msg,
					this_call, property, error$message)
			}
	) 

	for (propgroup in properties) {

		group_matched <- TRUE
		
		for (property in propgroup) {
			
			member_matched <- 
				tryCatch({
					# testing the value is risky, so do it
					# in a trycatch

					has_property <- property_tests[[property]]
					result <- has_property(value)

					if (!is.logical(result) || is.na(result)) {
						report$non_boolean(result, property)
					}
					result},
					error = report$error_encountered
				)
			
			if (!member_matched) {
				# short-circuit group if the
				# member didn't match

				group_matched <- FALSE
				break
			} else {
				group_matched <- 
					group_matched && member_matched
			}
		}

		if (group_matched) {
			return (TRUE)
		}
	}

	report$no_match(value)
}








