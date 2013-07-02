
context("errors: positive controls")

	test_that("errors are thrown if the value doesn't exist", {

		f <- function (val) {
			needs_a('any', val)
		}
		expect_error(f(), 'missing')

		g <- function (prop) {
			needs_a(prop, 'hi')
		}
		expect_error(g(), 'missing')

	})

	test_that("errors are thrown if 'traits' is the wrong class", {
		
		expect_error(
			needs_a( 1, 1 ),
			'character vector')
	})

	test_that("errors are thrown for invalid traits", {

		expect_error(
			needs_a('ten-foot-elephant', 1),
			'ten-foot-elephant'
		)
	})

	test_that("errors are thrown when the object doesn't match its traits", {

		expect_error(
			needs_a('integer call', 0.1),
			'integer and call')
		expect_error(
			needs_a('null na', NULL),
			'null and na'
		)
		expect_error(
			needs_a(c('positive integer', 'complex'), -0.1)
		)

	})

