
context("verify that invoking tests on report$ directly work")

test_that("if all tests work correctly, then the values for pcall and test
	should be seen in the ouput (rather than intermediate errors)", {

	pcall <- 'I am le call'
	test <- 'dummy'

	pattern <- 'I am le call.+dummy'

	expect_error(report$missing_traits(pcall), pcall)
	expect_error(report$missing_value(pcall), pcall)
	
	expect_error(
		report$traits_not_character(pcall, test),
		pcall)
	
	expect_error(
		report$invalid_traits(pcall, test),
		pattern)

	expect_error(
		report$non_boolean(pcall, list(test, test), 
			pattern)
	)
	expect_error(
		report$no_match(pcall, test, test),
		pattern
	)

})
