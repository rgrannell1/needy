
#' Ensure a value has a desired set of traits. 
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
#' \code{c("positive, integer", "na")}
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
#' \code{require_a("white-whale", 1)}
#'
#' \code{Error: require_a("white-whale", 1): unrecognised trait(s): (white-whale)}
#'
#' Similarily, if a value doesn't have any other desired compound traits then an error is thrown: 
#'
#' \code{require_a(c("length_one list", "null"), 1)}
#'
#' \code{Error: require_a(c("length_one list", "null"), 1): the value 1 didn't match any of the following compound traits:
#'			length_one and list, or null'}
#'
#' As of version 0.3 trait modifiers are allowed:
#'
#' \code{require_a("!null", NULL)}
#'
#' \code{Error: require_a("!null", NULL): the value NULL didn't match any of the following compound traits:
#'			!null'}
#'
#' \code{require_a("list_of_string", list('a', 'b'))}
#'
#' @details the option \code{pcall} is included so that it is possible to customise where the errors seem to originate from.
#' for example,
#'
#'\code{myfunc <- function (x) require_a("integer", x, sys.call( sys.parent(1) )) }
#'
#' will display the following if called with a string "a":
#'
#' \code{Error: myfunc("a"): 
#'				the value "a" didn\'t match any of the following compound traits:
#'				integer}
#' 
#' In this example, the user-facing function \code{myfun} is shown to throw the error rather than an obscure inner function,
#' making debugging easier. For cases in which
#' working with the call stack directly (\code{sys.call()}) is too difficult
#' a string can be passed to \code{pcall}, and this string is printed
#' in front of the error message
#'
#' @param traits a character vector, with each element being a space-seperated
#'     string of properties to test the value for. See "traits". required.
#' @param value an arbitrary R object to test for certain properties. required.
#' @param pcall an call or string that provides the call to be 
#'     displayed when an error is thrown by require_a. See details. optional, defaults to displaying the call to require_a().
#' @param name a string giving the name of the test to add. required.
#' @param trait_test a unary function that returns a true or false value. 
#'     This function should tests for a particular trait.required.
#' @param trait_modifier a unary function that takes a trait test, and returns a new trait test.
#'     Examples include '!', which takes any trait test \code{f} and returns a trait test 
#'     that is true when the f returns false.
#' @export
#' @rdname require_a
#' @example inst/examples/example-require_a.R

require_a <- function (traits, value, pcall = Null) {
	# character -> a -> call|string -> boolean
	# test if the value has the required traits,
	# if it doesn't throw a helpful error. decorate with 
	# pcall so the error will look like it came from the user's 
	# function of choice."

	valid_pcall <- 
		!is.null(pcall) && (
		is.character(pcall) ||
		is.call(pcall))

	pcall <- if (valid_pcall) {
		deparse_to_string(pcall)
	} else {
		deparse_to_string( sys.call() )
	}

	tryCatch(
	    { force(traits) },
	    error = function (error) {
	    	say$missing_traits(pcall)
	    })
	tryCatch(
	    { force(value) },
	    error = function (error) {
	    	say$missing_value(pcall)
	    })

	if (missing(value)) {
		say$missing_value(pcall)
	}
	if (missing(traits)) {
		say$missing_traits(pcall)
	}
	if (!is.character(traits)) {
		say$traits_not_character(pcall, traits)
	}

	# no traits are specified, or 
	# the value has at least one group of traits

	(length(traits) == 0) ||
		check_value(
			parse_traits(
				traits,
				pcall
			),
			value, pcall)
}

check_value <- function (compound_trait_list, value, pcall) {
	# does the value have at least one 
	# group of traits, with modifiers applied when needed?
	# if yes, return true.
	# otherwise, throw a descriptive error.

	for (compound_trait in compound_trait_list) {
		# assume True until shown otherwise.
		compound_matched <- True
		
		for (trait in compound_trait) {
			# return True if every value matched every 
			# member in this group of traits 

			trait_matched <- tryCatch({

				modifier <- trait_modifiers[[trait$modifier]]
				trait_test <- trait_tests[[trait$trait]]
				
				trait_matched <- modifier(trait_test)(value)

				if (is_boolean(trait_matched)) {
					trait_matched
				} else {
					# say non-boolean encountered.

					say$non_boolean(
						pcall,
						inputs = list(
							value = value,
							trait = trait$input_string),
						actual = trait_matched)
				}
				},
				error = function (error) {
					# say error encountered.

					say$error_encountered(
						pcall, error, 
						inputs = list(
							value = value,
							trait = trait$input_string))
				},
				warning = function (warning) {
					# say warning encountered.

					say$warning_encountered(
						pcall, warning, 
						inputs = list(
							value = value,
							trait = trait$input_string))
				}
			)
				
			# short-circuit group if the member didn't match
			if (!trait_matched) {
				compound_matched <- False
				break
			}
		}

		if (compound_matched) {
			break
		}
	}

	# throw an error if no supertraits matched
	compound_matched ||say$no_match(
		pcall, value, compound_trait_list)	
}

#' @export
#' @rdname require_a

implemented_traits <- function () {
	# print all traits available in the current version"

	cat('currently implemented traits:\n',
		paste0(trait_tests$valid_traits, collapse = ", ")
	)
}
#' @export
#' @rdname require_a

implemented_modifiers <- function () {
	"print all modifiers available in the current version"

	cat('currently implemented modifiers:\n',
		paste0(trait_modifiers$valid_modifiers, collapse = ", ")
	)
}

#' @export
#' @rdname require_a

add_trait <- function (name, trait_test) {
	# string -> (a -> boolean) -> null
	# add a new trait to the trait tests.

	pcall <- sys.call()
	require_a("string", name, pcall)
	require_a("unary function", trait_test)

	if (name %in% trait_tests$valid_traits) {
		say$trait_overwrote(pcall, name)
	}

	trait_tests[[name]] <- trait_test
	trait_tests$valid_traits <- ls(trait_tests)
}

#' @export
#' @rdname require_a

add_modifier <- function (name, trait_modifier) {
	# string -> (a -> boolean) -> null
	# add a new modifier to the trait modifier.

	pcall <- sys.call()
	require_a("string", name, pcall)
	require_a("unary function", trait_modifier)

	if (name %in% trait_tests$valid_modifiers) {
		say$modifier_overwrote(pcall, name)
	}

	trait_modifiers[[name]] <- trait_modifier
	trait_modifiers$valid_modifiers <- ls(trait_modifiers)
}
