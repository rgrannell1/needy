
library(testthat)
library(itertools)

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

test_package('needy')

