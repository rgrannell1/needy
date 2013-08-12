
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
#' As of version 0.2 trait negation is also supported:
#'
#' \code{require_a("!null", NULL)}
#'
#' \code{Error: require_a("!null", NULL): the value NULL didn't match any of the following compound traits:
#'			!null'}
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
#' @export
#' @rdname require_a
#' @example inst/examples/example-require_a.R

require_a <- function (traits, value, pcall = NULL) {
	" character -> a -> call|string -> boolean
	  test if the value has the required traits,
	  if it doesn't throw a helpful error. decorate with 
	  pcall so the error will look like it came from the user's 
	  function of choice."

	valid_pcall <- 
		!is.null(pcall) && (
		is.character(pcall) ||
		is.call(pcall))

	pcall <- if (valid_pcall) {
		deparse_to_string(pcall)
	} else {
		deparse_to_string( sys.call() )
	}

	force_error_handler <- function (error) {
        stop(paste0(pcall, ": ", error$message), call. = FALSE)
	}

	tryCatch(
	    { force(traits) },
	    error = force_error_handler)
	tryCatch(
	    { force(value) },
	    error = force_error_handler)
	tryCatch(
	    { force(pcall) },
	    error = force_error_handler)

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


parse_traits <- function (trait_string, pcall) {
	# character -> call -> [modifier: string, trait: string]
	# takes the raw traits string, and 
	# transforms it into a list of
	# trait groups to test. Throws errors if the inputs are invalid.
	
	get_modifier <- function (trait) {
		# check if a trait is prefixed with any modifiers.
		# as of version 0.3, only one modifier is allowed.

		unlist(lapply(
			trait_modifiers$valid_modifiers,
			function (modifier) {

				trait_has_modifier <- grepl(paste0("^", modifier), trait)

				if (trait_has_modifier) {
					modifier
				} else {
					NULL
				}
			}
		))
	}

	whitespace <- '[ \t\n\r]+'

	lapply(
		trait_string,
		function (compound_trait) {
			# each element defines a compound_trait.
			# split into traits and check them.

			traits <- strsplit(compound_trait, split = whitespace)[[1]]

			parsed_traits <- lapply(
				traits, 
				function (trait) {

					modifier <- get_modifier(trait)

					list(
						modifier = 
							if (length(modifier) == 0) {
								"id_" 
							} else {
								modifier
							},
						trait =
							if (length(modifier) == 0) {
								trait
							} else {
								substring(trait, nchar(modifier) + 1)								
							}
					)
				}				
			)

			invalid <- Reduce(
				function (acc, parsed_trait) {
					# test if modifier-less trait is implemented.

					any_invalid <- parsed_trait$trait %in% 
						trait_tests$valid_traits
					c(acc, 
						if (any_invalid) {
							parsed_traits$trait
						} else {
							character(0)
						}
					)
				},
				parsed_traits,
				character(0)
			)

			if (length(invalid) == 0) {
				parsed_traits
			} else {
				report$invalid_traits(pcall, invalid)
			}
	})
}

check_traits <- function (trait_options, value, pcall) {
	# does the value have at least one 
	# group of traits, with modifiers applied when needed?
	# if yes, return true.
	# otherwise, throw a descriptive error."

	for (compound_trait in trait_options) {

		compound_trait_matched <- TRUE
		
		for (trait in compound_trait) {
			# return true if every value matched every 
			# member in this group of traits 

			trait_matched <- tryCatch({
				# testing the value is risky, so do it in a trycatch

				# say trait one more time...
				# apply a modifier function to a trait test,
				# returning a composite trait.
				has_trait <- trait_modifiers[[trait$modifier]](
					trait_tests[[trait$trait]]
				)
				
				trait_matched <- has_trait(value)

				if (is_boolean(trait_matched)) {
					trait_matched
				} else {
					# report non-boolean encountered.

					report$non_boolean(
						pcall,
						inputs = list(
							value = value,
							trait = trait$trait),
						actual = trait_matched)
				}
				},
				error = function (error) {

					report$error_encountered(
						pcall, error, 
						inputs = list(
							value = value,
							value = trait$trait))
				},
				warning = function (warning) {

					report$warning_encountered(
						pcall, warning, 
						inputs = list(
							value = value,
							trait = trait$trait))
				}
			)
				
			# short-circuit group if the member didn't match
			if (!trait_matched) {
				compound_trait_matched <- FALSE
				break
			}
		}

		if (compound_trait_matched) {
			break
		}
	}

	# throw an error if no supertraits matched
	compound_trait_matched ||report$no_match(
		pcall, value, trait_options)	
}

#' @export
#' @rdname require_a

implemented_traits <- function () {
	"print all traits available in the current version"

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
	"string -> (a -> boolean) -> null
	add a new trait to the trait tests"

	pcall <- sys.call()
	require_a("string", name, pcall)
	require_a("unary function", trait_test)

	if (name %in% trait_tests$valid_traits) {
		report$trait_overwrote(pcall, name)
	}

	trait_tests[[name]] <- trait_test
	trait_tests$valid_traits <- ls(trait_tests)
}
