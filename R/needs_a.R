
#' ensure a value has a desired set of traits. 
#'
#' @section Traits:
#'
#' The \code{traits} parameter is a character vector of whitespace-seperated traits. For example, the following
#' are syntactically valid 
#'
#' \code{"integer"}
#'
#' \code{"positive integer"}
#'
#' \code{c("positive    integer", "na")}
#'
#' \code{c("na", "null", "length_one   pairlist")}
#' 
#' while the following are not
#' 
#' \code{"positive && integer" # just use whitespace to 'and' traits}
#'
#' \code{"positive || integer" # use two elements to 'or' traits}
#'
#' The latter two examples, correctly implemented, would be:
#'
#' \code{"positive integer"}
#'
#' \code{c("positive", "integer")}
#'
#' As suggested above, whitespace between traits is interpreted as "trait a AND trait b", while
#' seperate elements are intepreted as \code{ c("trait one", OR "trait two") }
#' the order of traits in a compound trait is not significant; a \code{"positive integer"} is 
#' equivelant to \code{"integer positive"}. 
#'
#' If a test corresponding to an atomic trait is not found, an error is thrown:
#'
#' \code{needs_a("white-whale", 1)}
#'
#' \code{Error: needs_a("white-whale", 1): unrecognised trait(s): (white-whale)}
#'
#' similarily, if a value doesn't have any other desired compound traits then an error is thrown: 
#'
#' \code{needs_a(c("length_one list", "null"), 1)}
#'
#' \code{Error: needs_a(c("length_one list", "null"), 1): the value 1 didn't match any of the following compound traits:
#'			length_one and list, or null'}
#'
#' @param traits a character vector, with each element being a space-seperated
#' string of properties to test the value for. See "traits". required.
#' @value an arbitrary R object to test for certain properties. required.
#' @param pcall an call or string that provides the call to be 
#' displayed when an error is thrown by needs_a. See details. optional.
#' @export

needs_a <- function (traits, value, pcall = NULL) {
	# test if the value has the required traits,
	# if it doesn't throw a helpful error. decorate with 
	# pcall so the error will look like it came from the user's 
	# function of choice.

	valid_pcall <- 
		!is.null(pcall) && (
		is.character(pcall) ||
		is.call(pcall))

	pcall <- if (valid_pcall) {
		deparse_to_string(pcall)
	} else {
		deparse_to_string( sys.call() )
	}

	if (missing(value)) {
		report$missing_value(pcall)
	}
	if (missing(traits)) {
		report$missing_traits(pcall)
	}
	if (!is.character(traits)) {
		report$traits_not_character(pcall, traits)
	}

	# no traits are specified, or 
	# the value has at least one group of traits

	(length(traits) == 0) ||
		check_traits(
			parse_traits(
				traits,
				pcall
			),
			value, pcall)
}

report <- list(
	# an object (well, list...) containing 
	# functions that report various errors and warnings
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
		function (pcall, inputs, actual) {

			text <- '%s:
				the value %s returned a non true/false value when tested for the trait %s:\n
				actual value was %s\n'

			readable <- list(
				value = deparse_to_string(inputs$value),
				actual = deparse_to_string(actual)
			)

			stopf(text,
				pcall, 
				readable$value, inputs$subtrait,
				readable$actual)
		},
	no_match =
		function (pcall, value, traits) {
			
			text <- "%s: 
				the value %s didn't match any of the following compound traits:
				%s\n"

			and_collapse <- function (x) {
				paste0(x, collapse = ' and ')
			}
			or_collapse <- function (x) {
				paste0(unlist(x), collapse = ', or ')
			}

			readable <- list(
				value = deparse_to_string(value),
				expected = or_collapse(sapply(traits, and_collapse))
			)

			stopf(text,
				pcall,
				readable$value,
				readable$expected)
		},
	error_encountered = 
		function (pcall, error, inputs) {

			text <- '%s:\n
			an error was encountered while testing the value %s for the the trait "%s":\n
			%s\n'

			readable <- list(
				value = deparse_to_string(inputs$value)
			)

			stopf(text,
				pcall, readable$value,
				inputs$subtrait, error$message)
		},
	warning_encountered =
		function (pcall, warning, inputs) {

			text <- '%s:\n
			a warning was encountered while testing the value %s for the the trait "%s":\n
			%s\n'

			readable <- list(
				value = deparse_to_string(inputs$value)
			)

			stopf(text,
				pcall, readable$value,
				inputs$subtrait, warning$message)
		}
)

parse_traits <- function (trait_string, pcall) {
	# takes the raw traits string, and 
	# transforms it into a list of
	# trait groups to test
	
	delimiter <- '[ \t\n\r]+'

	lapply(
		trait_string,
		function (supertrait) {
			# each element defines a supertrait.
			# split into subtraits and check them.

			subtraits <- strsplit(supertrait, split = delimiter)[[1]]
			invalid <- setdiff(
				subtraits, 
				trait_tests$valid_traits)

			if (length(invalid) == 0) {
				subtraits
			} else {
				report$invalid_traits(pcall, invalid)
			}
	})
}

is_boolean <- function (x) {
	is.logical(x) && !is.na(x)
}

check_traits <- function (trait_vector, value, pcall) {
	# does the value have at least one 
	# group of traits?
	# if yes, return true. otherwise, throw a descriptive error.

	error_handler <- function (error) {

		report$error_encountered(
			pcall, error, 
			inputs = list(
				value = value,
				value = subtrait))
	}
	warning_handler <- function (warning) {

		report$warning_encountered(
			pcall, warning, 
			inputs = list(
				value = value,
				subtrait = subtrait))
	}

	for (supertrait in trait_vector) {

		supertrait_matched <- TRUE
		
		for (subtrait in supertrait) {
			# return true if every value matched every 
			# member in this group of traits 
			
			subtrait_matched <- tryCatch({
				# testing the value is risky, 
				# so do it in a trycatch

				has_subtrait <- trait_tests[[subtrait]]
				subtrait_matched <- has_subtrait(value)

				if (!is_boolean(subtrait_matched)) {
					
					report$non_boolean(
						pcall,
						inputs = list(
							value = value,
							subtrait = subtrait),
						actual = subtrait_matched)
				}
				
				subtrait_matched
				},
				error = error_handler,
				warning = warning_handler
			)
				
			if (!subtrait_matched) {
				# short-circuit group if the member didn't match

				supertrait_matched <- FALSE
				break
			}
		}

		if (supertrait_matched) {
			break
		}
	}

	# throw an error if no supertraits matched
	supertrait_matched ||report$no_match(
		pcall, value, trait_vector)	
}
