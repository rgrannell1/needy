
#' input validation

#' @export

report <- list(
	missing_traits = function (pcall) {
		stopf (
			"%s: the parameter 'value' was missing but is required\n", 
			pcall)
	},
	missing_value = function (pcall) {
		stopf (
			"%s: the parameter 'traits' was missing but is required\n", 
			pcall)
	},
	traits_not_character = function (pcall, traits) {
		stopf (
			"%s: the parameter 'traits' must be a character vector\n
			actual class was %s
			", 
			pcall, paste0(class(traits), collapse = ", "))
	},
	invalid_traits = function (pcall, invalid) {
		stopf(
			"%s: unrecognised trait(s): (%s)\n", 
			pcall, 
			paste0(invalid, collapse = ', '))		
	},
	non_boolean = 
		function (pcall, value, subtrait, result) {
			# report that the value wasn't a boolean value,
			# along with the subtrait being tested, the parent call and 
			# the actual non boolean result

			msg <- '%s:
				the value %s returned a non true/false value when tested for the trait %s:\n
				actual value was %s\n'

			stopf(msg,
				pcall, deparse_to_string(value),
				subtrait, deparse_to_string(result))
		},
	no_match =
		function (pcall, value, traits) {
			# report that the value didn't
			# match any the required traits
			
			msg <- "%s: the value %s didn't match any of the following:
			%s\n"

			and_collapse <- function (x) {
				paste0(x, collapse = ' and ')
			}
			or_collapse <- function (x) {
				paste0(unlist(x), collapse = ', or ')
			}

			readable_traits <- 
				or_collapse(sapply(traits, and_collapse))

			stopf(msg,
				pcall,
				deparse_to_string(value),
				readable_traits)
		},
	error_encountered = 
		function (pcall, error, value, subtrait) {
			# report the error along with what
			# was being tested at the time

			msg <- '%s:\n
			an error was encountered while testing the value %s for the the trait "%s":\n
			%s\n'

			stopf(msg,
				pcall, deparse_to_string(value),
				subtrait, error$message)
		},
	warning_encountered =
		function (pcall, warning, value, subtrait) {
			# report the warning along with what
			# was being tested at the time

			msg <- '%s:\n
			a warning was encountered while testing the value %s for the the trait "%s":\n
			%s\n'

			warningf(msg,
				pcall, deparse_to_string(value),
				subtrait, warning$message)
		}
)

needs_a <- function (traits, value, pcall = NULL) {
	# test if the value has the required traits,
	# if it doesn't throw a helpful error. decorate with 
	# pcall so the error will look like it came from the user's 
	# function of choice.

	valid_pcall <- !is.null(pcall) ||
		is.character(pcall) ||
		is.call(pcall)

	pcall <- if (valid_pcall) {
		deparse_to_string(pcall)
	} else {
		deparse_to_string( sys.call() )
	}

	value_name <- deparse_to_string(
		as.list( match.call()[-1] )$value)

	if (missing(value)) {
		report$missing_value(pcall)
	}
	if (missing(traits)) {
		report$missing_traits(pcall)
	}
	if (!is.character(traits)) {
		report$traits_not_character(pcall, traits)
	}
	if (length(traits) == 0) {
		TRUE
	} else {
		check_traits(
			parse_traits(
				traits,
				pcall
			),
			value, pcall)		
	}
}

parse_traits <- function (trait_string, pcall) {
	# takes the raw traits string, and 
	# transforms it into a list of
	# trait groups to test
	
	delimiter <- '[ \t\n]+'

	lapply(
		trait_string,
		function (supertrait) {
			# each element defines a supertrait.
			# split into subtraits and check them.

			subtraits <- strsplit(supertrait, split = delimiter)[[1]]
			invalid <- setdiff(
				subtraits, 
				trait_tests$valid_traits)

			if (length(invalid) > 0) {
				report$invalid_traits(pcall, invalid)
			} else {
				subtraits
			}
		}
	)
}

check_traits <- function (traits, value, pcall) {
	# does the value have at least one 
	# group of traits?
	# if yes, return true. otherwise, throw a descriptive error.

	# iterate over the traits, trying to find 
	# some group of traits that value matched

	for (supertrait in traits) {

		supertrait_matched <- TRUE
		
		for (subtrait in supertrait) {
			
			member_matched <- 
				tryCatch({
					# testing the value is risky, 
					# so do it in a trycatch

					has_trait <- trait_tests[[subtrait]]
					result <- has_trait(value)

					if (!is.logical(result) || is.na(result)) {
						report$non_boolean(
							pcall, value, subtrait, result)
					}
					result},
					error = function (error) {
						report$error_encountered(
							pcall, error, value, subtrait)
					},
					warning = function (warning) {
						report$warning_encountered(
							pcall, warning, value, subtrait)
					}
				)
			
			if (!member_matched) {
				# short-circuit group if the
				# member didn't match

				supertrait_matched <- FALSE
				break
			}
		}

		if (supertrait_matched) {
			return (TRUE)
		}
	}

	report$no_match(value)
}
