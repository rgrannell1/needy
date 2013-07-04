
context("error functions on report$ can be invoked directly")

forall(
	"error functions don't throw errors themselved (call is always displayed)",
	list(
		pcall  = word_gen, traits = words_gen, invalid = words_gen,
		inputs = list(
			list(
				trait = "a trait",
				value = +9001)
		),
		actual = word_gen,
		value = word_gen,
		error = list(
			list(message = 'error thrown')
		)
	),
	function (pcall, traits, invalid, inputs, actual, value, error) {
		# check that each of the expected fields are being 
		# displayed in the output (won't happen in function is broken).

		expect_error(
			report$missing_traits(pcall),
			pcall)

		expect_error(
			report$missing_value(pcall),
			pcall)

		expect_error(
			report$traits_not_character(pcall, traits),
			pcall, traits)

		expect_error(
			report$invalid_traits(pcall, invalid),
			all_patterns(c(pcall, invalid)) )

		expect_error(
			report$non_boolean(pcall, inputs, actual),
			all_patterns(c(pcall, inputs$value, inputs$trait, actual)) )

		print( "asdasd")

		expect_error(
			report$no_match(pcall, value, traits),
			all_patterns(c(pcall, value, traits)) )

		expect_error(
			report$error_encountered(pcall, error, inputs),
			all_patterns(c(
				pcall, error,
				inputs$value, inputs$trait)) )

		expect_error(
			report$warning_encountered(pcall, error, inputs),
			all_patterns(c(
				pcall, error,
				inputs$value, inputs$trait)) )

		TRUE
	}
)
