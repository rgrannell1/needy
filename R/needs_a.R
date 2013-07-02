
#' @export

needs_a <- function (traits, value) {
	# test that a value has the specified traits

	this_call <- deparse_to_string( sys.call() )
	pframe <- parent.frame()

	value_name <- deparse_to_string(
		as.list( match.call()[-1] )$value)

	if (missing(value)) {
		stopf (
			"%s: the parameter 'value' was missing but is required", 
			this_call)
	}
	if (missing(traits)) {
		stopf (
			"%s: the parameter 'traits' was missing but is required", 
			this_call)
	}
	if (!is.character(traits)) {
		stopf (
			"%s: traits must be a character vector", 
			this_call)
	}
	if (length(traits) == 0) {
		return (TRUE)
	}

	check_traits(
		traits = parse_traits(traits),
		value)
}

parse_traits <- function (traits) {
	# takes the raw traits string, and 
	# transforms it into a list of
	# trait groups to test
	
	this_call <- "require_a(traits, value)"

	report <- list(
		invalid_traits = 
			function (invalid) {
				# some traits were unmatched.
				# report them.

				stopf(
					"%s: unrecognised trait(s): (%s)", 
					this_call, 
					paste0(invalid, collapse = ', '))
			}
	)

	delimiter <- '[\t\n]+'

	traits <- lapply(
		traits,
		function (group) {

			members <- strsplit(group, split = delimiter)[[1]]
			invalid <- setdiff(
				members, 
				trait_tests$listed_traits)

			if (length(invalid) > 0) {
				report$invalid_traits(invalid)
			}
			members
		}
	)
	traits
}

check_traits <- function (traits, value) {
	# does the value have at least one 
	# group of traits?
	# if yes, return true. otherwise, throw a descriptive error
	
	this_call <- "require_a(traits, value)"
	
	report <- list (
		non_boolean = 
			function (val, trait) {
				# report that the value wasn't a boolean value,
				# along with the trait being tested

				msg <- '%s:
					the test for the trait "%s" returned a non true/false value:
					actual value was %s'

				stopf(msg,
					this_call, trait, deparse_to_string(result))
			},
		no_match =
			function (value) {
				# report that the value didn't
				# match any the required traits
				
				msg <- "%s: the value %s didn't match any of the following:
				%s"

				and_collapse <- function (x) {
					paste0(x, collapse = ' and ')
				}
				or_collapse <- function (x) {
					paste0(unlist(x), collapse = ', or ')
				}

				readable_traits <- 
					or_collapse(sapply(traits, and_collapse))

				stopf(
					msg, this_call,
					deparse_to_string(value),
					readable_traits)
			},
		error_encountered = 
			function (error) {
				# report the error along with what
				# was being tested at the time

				msg <- '%s:
				an error was encountered while testing for the the trait "%s":
				%s'

				stopf(msg,
					this_call, trait, error$message)
			}
	) 

	# iterate over the traits, trying to find 
	# some group of traits that value matched

	for (propgroup in traits) {

		group_matched <- TRUE
		
		for (trait in propgroup) {
			
			member_matched <- 
				tryCatch({
					# testing the value is risky, 
					# so do it in a trycatch

					has_trait <- trait_tests[[trait]]
					result <- has_trait(value)

					if (!is.logical(result) || is.na(result)) {
						report$non_boolean(result, trait)
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








