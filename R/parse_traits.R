
get_modifier <- function (trait) {
	# string -> string
	# returns a single matching modifier,
	# or a length-zero collection.

	unlist(lapply(
		trait_modifiers$valid_modifiers,
		function (modifier) {
			# string -> null | string
			# does the trait contain this modifier?

			has_modifier <- grepl(paste0("^", modifier), trait)
			if (has_modifier) {
				modifier
			} else {
				NULL
			}
		}
	))
}

parse_one_trait <- function (trait) {
	# string -> [modifier: string, trait: string, input_string: string]
	# split a trait into a modifier ('id_' if none is given), an underlying trait
	# and the original string.

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
			},
		input_string = trait
	)
}

get_invalid_traits <- function (parsed_traits) {
	# [[modifier: string, trait: string, input_string: string]] -> 
	# [[modifier: string, trait: string, input_string: string]]
	# return a list of traits whose modifiers or underlying
	# trait is not currently implemented.

	valid <- list(
		traits = 
			trait_tests$valid_traits,
		modifiers = 
			trait_tests$valid_modifiers
	)

	Reduce(
		function (acc, new) {

			is_valid <- 
				is.element(new$traits, valid$traits) &&
				is.element(new$modifier, valid$modifiers)

			if (!is_valid) c(acc, list(new)) else acc
		},
		parsed_traits,
		list()
	)
}

parse_traits <- function (trait_string, pcall) {
	# character -> call -> [modifier: string, trait: string]
	# takes the raw traits string, and 
	# transforms it into a list of
	# trait groups to test. Throws errors if the inputs are invalid.

	whitespace <- "[ \t\n\r]+"

	lapply(
		trait_string,
		function (compound_trait) {

			traits <- strsplit(compound_trait, split = whitespace)[[1]]
			parsed_traits <- lapply(traits, parse_one_trait)
			invalid_traits <- get_invalid_traits(parsed_traits)

			if (length(invalid_traits) == 0) {
				parsed_traits
			} else {
				# report the invalid input strings.

				invalid_input_strings <- vapply(
					invalid_traits,
					function (trait) {
						trait$input_string
					}, "string"
				)
				say$invalid_traits(pcall, invalid_input_strings)
			}
	})
}
