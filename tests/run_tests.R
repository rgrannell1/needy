
library(testthat)
library(combinat)
library(itertools)

# # random input generators # # #

pick_one <- function (x) {
	sample(x, size = 1)
}
pick_len <- function (n) {
	seq_len( pick_one(seq_len(n)) )
}

vector_gen <- function (gen) {
	# convert a single generator to a vector

	Generator(
		function () {
			sapply(
				pick_len(20),
				function (...) gen[[1]]$f()
			)
		})
}

word_gen <- Generator(
	function () {

		paste0(
			sample(letters, size = pick_one(1:26)),
			collapse = '' )
	}
)
words_gen <- vector_gen(word_gen)

all_patterns <- function (x) {
	# get every permutation of elements in x,
	# collapse them with |.

	permutations <- sapply(
		combinat::permn(x),
		function (perm) {
			paste0(perm, collapse = ".+")
		})

	paste0(permutations, collapse = "|")
}

# # # # # # # # # # # # # # # # # #

test_package('needy')


