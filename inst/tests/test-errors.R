
context("errors: positive controls")

	test_that("errors are thrown if the value doesn't exist", {

		f <- function (val) {
			require_a('any', val)
		}
		expect_error(f(), 'missing')

		g <- function (prop) {
			require_a(prop, 'hi')
		}
		expect_error(g(), 'missing')

	})

	test_that("errors are thrown if properties is the wrong class", {
		
		expect_error(
			require_a( 1, 1 ),
			'character vector')

	})

	test_that("errors are thrown for invalid properties", {

		expect_error(
			require_a('ten-foot-elephant', 1),
			'ten-foot-elephant'
		)

	})

	test_that("errors are thrown when the object doesn't match its properties", {

		expect_error(
			require_a('integer call', 0.1),
			'integer and call')

	})

