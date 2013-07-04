
#' assert that value has specific properties

#' needs_a provides a terse way of asserting that a value has certain traits, 
#' such as being a certain length, being named or being a numeric value.

#' @sections Traits
#'
#' functions generally only operate on values that have certain traits; a set of 
#' properties that a value must have for a sensible value to be computable. 
#'
#' for example, \code{Reduce} requires its function argument \code{f} is a binary function,
#' and that the value \code{x} it reduces over is a list, or vector, or pairlist.  
#'
#' the \code{traits} parameter that needs_a requires encapsulates this way of descriping an object.
#' traits is a character vector, of any length. Each string element of this character vector
#' is a compound trait; a whitespace-seperated collection of traits that an object should have
#' to be considered a valid value for your program.
#' 
#' if the character vector \code{traits} is longer that length-one, then every string element of this 
#' vector is interpreted as an individual compound-trait, and an object should have every trait
#' in at least one compound trait to be consider valid. For example,
#' 
#' \code{ needs_a("positive numeric integer", +1L) }
#' 
#' passes because +1 is indeed positive AND a number AND an integer.
#'
#' \code{ needs_a(c("length_one list", "length_one pairlist", "null"), NULL) } 

#' @param traits a character vector, with each element being a space-seperated
#' string of properties to test the value for. required. See details.

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

# an object (well, list...) containing 
# functions that report various errors and warnings

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
		function (pcall, inputs, actual) {
			# report that the value wasn't a boolean value,
			# along with the subtrait being tested, the parent call and 
			# the actual non boolean result

			msg <- '%s:
				the value %s returned a non true/false value when tested for the trait %s:\n
				actual value was %s\n'

			stopf(msg,
				pcall, 
				deparse_to_string(inputs$value), inputs$subtrait,
				deparse_to_string(actual))
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
		function (pcall, error, inputs) {
			# report the error along with what
			# was being tested at the time

			msg <- '%s:\n
			an error was encountered while testing the value %s for the the trait "%s":\n
			%s\n'

			stopf(msg,
				pcall, deparse_to_string(inputs$value),
				inputs$subtrait, error$message)
		},
	warning_encountered =
		function (pcall, warning, inputs) {
			# report the warning along with what
			# was being tested at the time

			msg <- '%s:\n
			a warning was encountered while testing the value %s for the the trait "%s":\n
			%s\n'

			warningf(msg,
				pcall, deparse_to_string(inputs$value),
				inputs$subtrait, warning$message)
		}
)

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

is_boolean <- function (x) {
	is.logical(x) && !is.na(x)
}

check_traits <- function (traits, value, pcall) {
	# does the value have at least one 
	# group of traits?
	# if yes, return true. otherwise, throw a descriptive error.

	# display errors/warnings and the data
	# that triggered them
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

	for (supertrait in traits) {

		supertrait_matched <- TRUE
		
		for (subtrait in supertrait) {
			# return true if every value matched every 
			# member in this group of traits 

			subtrait_matched <- 
				tryCatch({
					# testing the value is risky, 
					# so do it in a trycatch

					has_trait <- trait_tests[[subtrait]]
					subtrait_matched <- has_trait(value)

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
				# short-circuit group if the
				# member didn't match, otherwise
				# check the rest of the traits in the group too

				supertrait_matched <- FALSE
				break
			}
		}

		if (supertrait_matched) {
			# no need to check any more supertraits,
			# value does match some group of traits
			break
		}
	}

	# throw an error if no supertraits matched
	supertrait_matched ||report$no_match(value)	
}
