
context("brute force needs_a with many inputs, check for errors")

listed_traits <- trait_tests$listed_traits

pick_one <- function (x) sample(x, size = 1)
pick_len <- function (n) seq_len(pick_one(n))

trait_gen <- Generator(
	function () {

		unlist(pick_one(
			list( 
				paste0(
					sample(listed_traits, size = pick_len(10)),
					collapse = ' ')) ))
	} 
)

number_gen <- Generator(
	function () pick_one(-100000:100000)
)
complex_gen <- Generator(
	function () pick_one(-100000:10000) + 1i
)
null_gen <- Generator(
	function () NULL
)
na_gen <- Generator(
	function () NA
)
infinite_gen <- Generator(
	function () pick_one(c(-Inf, +Inf))
)
functions <- Map(
	function (name) {
		match.fun(name)
	},
	Filter(
	    function (f) {
	        exists(f, mode='function')
	    },
	    ls('package:base')
	)
)
list_gen <- Generator(
	function () {
		as.list( pick_len(100) )
	}
)
vector_gen <- Generator(
	function () {
		vect <- pick_len(100)
		if (pick_one(0:1) == 1) {
			paste0(vect)
		} else vect
	}
)

gens <- list(
	number_gen, complex_gen,
	null_gen, na_gen,
	infinite_gen, functions,
	list_gen, vector_gen
)

value_gen <- gens[[ pick_one(seq_along(gens)) ]]

as_vector <- function (gen) {
	Generator(
		function () {

			sapply(
				pick_len(20),
				function (...) {
					gen[[1]]$f()
				})	

		}
	)
}


fuzzy = list(
	traits = as.vector( trait_gen ),
	value = value_gen
)

forall(
	fuzzy,
	function (traits, value) {

		traits <- c(traits, 'any')
		needs_a( traits, value )

	}
)