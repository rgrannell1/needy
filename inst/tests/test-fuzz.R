

listed_traits <- trait_tests$valid_traits

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
function_gen <- Generator(
	function () pick_one(functions)
)

list_gen <- Generator(
	function () {
		as.list(pick_len(100))
	} )
factor_gen <- Generator(
	function () as.factor(pick_len(100))
)
vector_gen <- Generator(
	function () {
		vect <- pick_len(100)
		if (pick_one(0:1) == 1) {
			paste0(vect)
		} else vect
	}
)
expression_gen <- Generator(
	function () {
		as.expression(pick_len(100))
	}
)
pairlist_gen <- Generator(
	function () {
		pairlist(pick_len(100))
	}
)
environment_gen <- Generator(
	function () {
		new.env()
	}
)
nan_gen <- Generator(
	function () {
		NaN
	}	
)
object_gen <- Generator(
	function () {

		structure(
			pick_len(100),
			class = 'asdsaasdasd'
		)
	}
)
length_0_gen <- Generator(
	function () {
		pick_one(
			list(character(0), integer(0), numeric(0),
				raw(0), complex(0), list())
		)
	}
)

value_gen <- Generator(
	function () {

		tmp <- list(
			number_gen, complex_gen, null_gen, na_gen,
			infinite_gen, list_gen, vector_gen,
			factor_gen, expression_gen, function_gen,
			pairlist_gen, environment_gen, nan_gen, length_0_gen,
			object_gen
		)
		to_return <- tmp[[ pick_one(length(tmp)) ]]
		to_return[[1]]$f()
	}
)

needs_cases <- list(
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
	value = value_gen
)

context("fuzz test needs_a to prevent random errors")

forall("try to minimise the errors thrown by needs_a",
	needs_cases,
	function (traits, value) {

		traits <- c(traits, 'any')
		res <- needs_a(traits, value)
	},
	opts = list(time = 20)
)

