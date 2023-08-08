snippet src
	source("${0:file.R}", encoding = "UTF-8")

snippet pipe-
	$$ <-
	  $$ %>%
	  ${0:code}

snippet pipe
	${1:dataset} <-
	  ${1} %>%
	  ${0:code}

snippet aa-
	$$ <-
	  $$ |>
	  ${0:code}

snippet aa
	${1:dataset} <-
	  ${1} |>
	  ${0:code}
