
return_a <- function (value, input, rules, pcall = NULL) {
	# 

	valid_pcall <- 
		!is.null(pcall) && (
		is.character(pcall) ||
		is.call(pcall))

	pcall <- if (valid_pcall) {
		deparse_to_string(pcall)
	} else {
		deparse_to_string( sys.call() )
	}

	if (missing(value)) {
		report$missing_value(pcall)
	}
	if (missing(traits)) {
		report$missing_input(pcall)
	}
	if (missing(rules)) {
		report$missing_rules(pcall)
	}
	if (!is.list(rules)) {
		report$input_not_list(pcall, input)
	}
	if (!is.list(rules)) {
		report$rules_not_list(pcall, rules)
	}

	(length(rules) == 0) ||
		check_rules(
			value,
			input
			validate_rules(rules, pcall),
			pcall)
}

validate_rules <- function (rules, pcall) {
	# check that the rules are a list of function pairs

	lapply(
		rules,
		function (rule_pair) {

			if (!is.list(rule_pair) ||
				!length(rule_pair) == 2) {
				
				report$rule_not_two_list(pcall, rule_pair)
			}
			if (!is.function( rule_pair[[1]] ) ||
				!is.function( rule_pair[[2]] )) {
				report$rule_not_functions(pcall, rule_pair)
			}
			rules
	})
}

check_rules <- function () {
	
}










