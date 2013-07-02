
context("brute force needs_a with many inputs, check for errors")

listed_traits <- trait_tests$listed_traits

pick_one <- function (x) {
	sample(x, size = 1)
}
pick_len <- function (n) {
	seq_len( pick_one(seq_len(n)) )
}

as_vector <- function (gen) {
	# convert a single generator to a vector

	Generator(
		function () {
			sapply(
				pick_len(20),
				function (...) gen[[1]]$f()
			)
		})
}

number_gen <- Generator(
	function () pick_one(-100000:100000)
)
complex_gen <- Generator(
	function () pick_one(-100000:10000) + 1i
)
infinite_gen <- Generator(
	function () pick_one(c(-Inf, +Inf))
)
null_gen <- Generator(function () NULL)
na_gen <- Generator(function () NA)

functions <- Map(
	function (name) match.fun(name),
	Filter(
	    function (f) {
	    	# get all functions from the 
	    	# base library

	        exists(f, mode = 'function')
	    },
	    ls('package:base')
	)
)

list_gen <- Generator( function () as.list(pick_len(100)) )
vector_gen <- Generator(
	function () {
		vect <- pick_len(100)
		if (pick_one(0:1) == 1) {
			paste0(vect)
		} else vect
	}
)

tmp <- list(
	number_gen, complex_gen, null_gen, na_gen,
	infinite_gen, functions, list_gen, vector_gen
)
value_gen <- tmp[[ pick_one(length(tmp)) ]]

forall("try to minimise the errors thrown by needs_a",
	list(
		traits = as_vector(
			Generator(
				function () {
					# generate a single random group 
					# of traits

					traits <- sample(
						listed_traits,
						size = pick_one(1:10),
						replace = TRUE)

					spaces <- sample(
						c(' ', '\t', '\n'),
						size = pick_one(5),
						replace = TRUE)

					paste0(traits, collapse = ' ')	
				}
			)),
		value = value_gen[[1]]$f() ),
	function (traits, value) {

		traits <- c(traits, 'any')
		needs_a(traits, value)		
	}
)

