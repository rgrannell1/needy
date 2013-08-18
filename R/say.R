
say <- list(
	# an object (well, list...) containing 
	# functions that say various errors and warnings
	missing_traits = function (pcall) {

		text <- paste0(
			"%s: the parameter ",
			dQuote("traits"),
			" was missing but is required\n")

		stopf(
			text, 
			pcall)
	},
	missing_value = function (pcall) {

		text <- paste0(
			"%s: the parameter ",
			dQuote("value"),
			" was missing but is required\n")
		stopf(
			text, 
			pcall)
	},
	traits_not_character = function (pcall, traits) {

		text <- paste0(
			"%s: the parameter ", dQuote('traits'), 
			" must be a character vector\n",
			"actual class was %s")

		stopf(
			text, 
			pcall, paste0(class(traits), collapse = ", "))
	},
	inputs_not_list = function (pcall, inputs) {

		text <- paste0(
			"%s: the parameter ", dQuote(traits), " must be a list\n",
			"actual class was %s")

		stopf(
			text, 
			pcall, paste0(class(inputs), collapse = ", "))
	},
	invalid_traits = function (pcall, invalid) {

		text <- "%s: unrecognised trait or trait modifier: (%s)\n"

		stopf(
			text, 
			pcall, 
			paste0(invalid, collapse = ', '))		
	},
	non_boolean = 
		function (pcall, inputs, actual) {

			text <- paste0(
				"%s:",
				"the value %s returned a non true/false value when tested for the trait ",
				dQuote("%s"), " :\n",
				"actual value was %s\n")

			readable <- list(
				value = deparse_to_string(inputs$value),
				actual = deparse_to_string(actual)
			)

			stopf(
				text,
				pcall, 
				readable$value, inputs$trait,
				readable$actual)
		},
	no_match =
		function (pcall, value, compound_trait_list) {

			text <- paste0(
				"%s:\n", 
				"the value %s didn't match any of the following compound traits:\n",
				"%s\n")

			and_collapse <- function (trait) {
				paste0(trait, collapse = ' and ')
			}
			or_collapse <- function (trait) {
				paste0(unlist(trait), collapse = ', or ')
			}

			readable <- list(
				value = deparse_to_string(value),
				traits = Map(
					function (compound_trait) {
						# convert each trait from a $modifier, $trait pair to the 
						# original input trait.

						vapply(
							compound_trait, 
							function (trait) {
								trait$input_string
							},
							"string")
					},
					compound_trait_list
				)
			)
			readable$expected <- or_collapse(sapply(readable$traits, and_collapse))

			stopf(text,
				pcall,
				readable$value,
				readable$expected)
		},
	error_encountered = 
		function (pcall, error, inputs) {

			text <- paste0(
				"%s:\n",
				"an error was encountered while testing the value %s for the the trait ",
				dQuote("%s"), " :\n",
				"%s\n")

			readable <- list(
				value = deparse_to_string(inputs$value)
			)

			stopf(text,
				pcall, readable$value,
				inputs$trait, error$message)
		},
	warning_encountered =
		function (pcall, warning, inputs) {

			text <- paste0(
				"%s:\n",
				"a warning was encountered while testing the value %s for the the trait ",
				dQuote("%s"), " :\n",
				"%s\n")

			readable <- list(
				value = deparse_to_string(inputs$value)
			)

			stopf(text,
				pcall, readable$value,
				inputs$trait, warning$message)
		},
	trait_overwrote = 
		function (pcall, name) {

			text <- paste0(
				"%s:\n",
				"the trait ", dQuote("%s"),
				" already exists: overwriting.\n")

			warningf(text, pcall, name)
		}
)
