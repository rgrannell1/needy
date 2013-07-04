
context(
	"error functions on report$ can be invoked directly"
)

pick_one <- function (x) 
	sample(x, size = 1)

pick_len <- function (n)
	seq_len(pick_one( seq_len(n) ))

word_gen <- Generator(
	function () {

		paste0(
			sample(letters, size = pick_one(1:26)),
			collapse = '' )
	}
)

forall(
	"error functions don't throw errors themselved (call is always displayed)",
	list(
		pcall  = word_gen,
		traits = ,
		invalid = ,
		inputs = ,
		actual = ,
		value = ,
		error = ,
		inputs = 
	),
	function (pcall, traits, invalid, inputs, actual, value
		error, inputs) {
		# check that each of the expected fields are being 
		# displayed in the output (won't happen in function is broken).

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

		expect_error(
			report$missing_traits(pcall),
			pcall)

		expect_error(
			report$missing_value(pcall),
			pcall)

		expect_error(
			report$traits_not_character(pcall, traits),
			all_patterns(pcall, traits))

		expect_error(
			report$invalid_traits(pcall, invalid),
			all_patterns(pcall, invalid))

		expect_error(
			report$non_boolean(pcall, inputs, actual),
			all_patterns(pcall, inputs$value, inputs$trait, actual))

		expect_error(
			report$no_match(pcall, value, traits),
			all_patterns(pcall, value, traits))

		expect_error(
			report$error_encountered(pcall, error, inputs),
			all_patterns(
				pcall, error,
				inputs$value, inputs$trait))

		expect_error(
			report$warning_encountered(pcall, error, inputs),
			all_patterns(
				pcall, error,
				inputs$value, inputs$trait))

	}
)
