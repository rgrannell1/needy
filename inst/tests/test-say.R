
context(
	"error functions on say have to be shown to be fully functional
	when given reasonable inputs."
)

test_that(
	"check that each of the expected fields are being 
	displayed in the output (won't happen if function is broken", {

		pcall <- 'this is a call'
		traits <- c("trait one", "trait two")
		invalid <- "white-elephant"
		inputs <- list(value = 10000, trait = "string")
		actual <- "actual-value"
		value  <- "this is a value"
		name <- "cat"
		error <- list(message = "I am an error message")
		trait_list <- 
			list(
				list(
					list(
						modifier = "id_",
						trait = "positive", 
				    	input_string = "positive")) )


		expect_error(say$missing_traits(pcall), pcall)

		expect_error(say$missing_value(pcall), pcall)

		expect_error(
			say$traits_not_character(pcall, traits),
			pcall)

		expect_error(
			say$invalid_traits(pcall, invalid),
			pcall)

		expect_error(
			say$non_boolean(pcall, inputs, actual),
			pcall)

		expect_error(
			say$no_match(pcall, value, trait_list),
			pcall)

		expect_error(
			say$error_encountered(pcall, error, inputs),
			pcall)

		expect_error(
			say$warning_encountered(pcall, error, inputs),
			pcall)
		expect_warning(
			say$trait_overwrote(pcall, name),
			pcall)
		expect_warning(
			say$modifier_overwrote(pcall, name),
			pcall)
	}
)
