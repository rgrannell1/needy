
context ("needs_a reports errors")

test_that("valid errors are triggered where expected (+ group)", {

	pcall <- "this shows that the expected error was thrown"

	expect_error(
		needs_a(value = 1, pcall = pcall), pcall
	)
	expect_error(
		needs_a(traits = "integer", pcall), pcall
	)
	expect_error(
		needs_a(traits = list(), 1, pcall), pcall
	)
	expect_error(
		needs_a("white-elephant", 1, pcall), pcall
	)
	expect_error(
		needs_a("positive integer", 'string', pcall), pcall
	)
	
})

test_that("errors aren't thrown for valid inputs (-)", {

	needs_a("positive integer", +1L)
	needs_a("whole numeric", 100)
	needs_a(c("null", "na", "pairlist"), NULL)
	needs_a("matrix", matrix(1:4, 2, 2))

	needs_a("integer", 2L, "myfunc(x)")
	needs_a("integer", 2L, call('mean', 1,2))

})

