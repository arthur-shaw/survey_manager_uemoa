
# PLACES THIS WOULD BE USEFUL:
# Table FC.1: single household response rate
# Table FC.8a: Module 6: single primary female decision-maker
# Table FC.8b: Module 6(M) (Men): single male decision-maker
# Table FC.9: Module 4  Results: multiple women of child-bearing age
# Table FC.10a: Module 5 Results: multiple children under 6


# define function

createResultDummy <- function(
	data, 		# input data
	resultVar, 	# result variable
	resultVal 	# result value
	) {

	# load necessary libraries
	library(dplyr)
	library(rlang)
	library(haven)

	# create variable names
	hasResult <- paste0("hasResult", resultVal)

	withDummy <<- data %>%
	mutate(!! hasResult := {{resultVar}} == resultVal)

}

moduleResponseRate <- function(
	data, 				# input data
	resultVar, 			# target variable (varname, unquoted)
	toGroup = FALSE, 	# whether to group: TRUE or FALSE
	groupVar = NULL, 	# grouping variable
	responseRate 		# RHS formula for response rate
	) {

	# load requisite libraries
	library(dplyr)
	library(stringr)
	library(purrr)
	library(rlang)

	# create column for each result

		# capture values
		responseVarValues <<- attr(data %>% pull({{resultVar}}), which = "labels")

			# TODO: confirm that values appear even in dset where those values aren't observed
			# think they do, since they come from Stata value labels

		# iterate over each level
		withDummies <<- purrr::map_dfc(.x = responseVarValues, .f = createResultDummy, data = data, resultVar = {{resultVar}})

	# compute counts for each column
	if (toGroup == FALSE) {

		countDummies <<- withDummies %>%
		summarize_at(vars(starts_with("hasResult")), sum, na.rm = TRUE)

	} else if (toGroup == TRUE) {

		countDummies <<- withDummies %>%
		group_by({{groupVar}}) %>%
		summarize_at(vars(starts_with("hasResult")), sum, na.rm = TRUE)

	}		

	# compute percentage for each column
	tableData <<- countDummies %>%
		
		# compute total responses
		mutate(numObs = rowSums(select(., starts_with("hasResult")), na.rm = TRUE)) %>%

		# compute percentage for matching target columns
		mutate_at(.vars = vars(starts_with("hasResult")), .funs = 
			list(perc = ~. / numObs)) %>%
		rename_at(.vars = vars(ends_with("_perc")), .funs = 
			list(~ paste0("perc_", gsub("_perc", "", .)))) %>%

	# compute response rate
	mutate(responseRate = {{responseRate}})

	# FOR SOME STRANG REASON `pull` DOESN'T WORK FOR LABELLED COLUMNS--BUT ONLY IN A FUNCTION
	# targetVar <- data %>% select({{resultVar}})
	# assign("labels", value = as_factor({{data}} %>% pull({{resultVar}})) %>% levels())
	# labels <<- as_factor({{data}} %>% pull({{resultVar}})) %>% levels()
	# qq_show(labels <<- as_factor(data %>% pull({{resultVar}})) %>% levels())
	# values <<- names(tableData %>% select(-team, -responseRate)) %>% str_extract(pattern = "[0-9]+")	

}

extractLabels <- function(data, resultVar) {

	# assign("labels", value = as_factor({{data}} %>% pull({{resultVar}})) %>% levels())
	varLabels <<- as_factor({{data}} %>% pull({{resultVar}})) %>% levels()

}

# # IMPLEMENT WITH EXAMPLE DATA

# # load libraries
# library(dplyr)
# library(haven)

# # injest data
# dataDir <- "C:/Users/wb393438/UEMOA/vague2/ehcvm-tri-automatique/donnees/fusionnees/"
# persons <- read_stata(file = paste0(dataDir, "membres.dta"), encoding = "UTF-8") %>%
# 	select(interview__id, membres__id, s01q01, s01q03b, AgeAnnee) %>%
# 	mutate(team = case_when(
# 		row_number() %in% c(1:500) ~ "Team 1",
# 		row_number() %in% c(501:1000) ~ "Team 2",
# 		row_number() %in% c(1001:1500) ~ "Team 3",
# 		row_number() >= 15-1 ~ "Team 4",
# 		))


# # compute stats by group
# groupStats <- moduleResponseRate(data = persons, resultVar = s01q03b, 
# 	toGroup = TRUE, groupVar = team, responseRate = (hasResult1 + hasResult2)/numObs)

# # compute stats overall
# overallStats <- moduleResponseRate(data = persons, resultVar = s01q03b, 
# 	toGroup = FALSE, responseRate = (hasResult1 + hasResult2)/numObs) %>%
# 	mutate(team = "All teams")

# # combine group and overall stats
# tableContents <- rbind(groupStats, overallStats)

# # compute total
# tableContents <- tableContents %>%
# 	mutate(numHeaped = rowSums(select(., starts_with("num"))))
# 	select(team, starts_with("perc_"), responseRate)




# create table with `gt`

# library(gt)
# library(haven)
# library(stringr)
# library(glue)

# create rules for renaming column names from variable names to "Code {codes}"
# labels <- as_factor(persons$s01q03b) %>% levels()
# codes <- names(tableContents %>% select(-team, -responseRate)) %>% str_extract(pattern = "[0-9]+")
# result <- glue("{valLabels} \n ({valCodes})")
# newColNames <- setNames(as.list(result), names(tableContents %>% select(-team, -responseRate)))

createResponseRateTable <- function(
	data, 				# input data
	resultVar, 			# target variable (varname, unquoted)
	toGroup = FALSE, 	# whether to group: TRUE or FALSE
	groupVar = NULL, 	# grouping variable
	responseRate, 		# RHS formula for response rate 
	title = "", 		# table title
	subtitle = "", 		# table subtitle
	groupText = "", 	# column label for group variable
	thresholdNote = "", # table note on threshold
	threshold = 0.95,  	# threshold for table
	respRateNote = "", 	# footnote explaining response rate
	tableColor = "#075e87"
	) {

	library(gt)
	library(glue)

	# convert string variable name into a symbol
	groupVar <- sym(groupVar)

	# compute stats by group
	groupStats <- moduleResponseRate(data = data, resultVar = {{resultVar}}, 
		toGroup = toGroup, groupVar = {{groupVar}} , responseRate = {{responseRate}}) %>%
		select({{groupVar}}, starts_with("perc_"), numObs, responseRate)

	# compute stats overall
	overallStats <- moduleResponseRate(data = data, resultVar = {{resultVar}}, 
		toGroup = FALSE, responseRate = {{responseRate}}) %>%
		mutate({{groupVar}} := "All teams") %>%
		select({{groupVar}}, starts_with("perc_"), numObs, responseRate)

	# combine group and overall stats
	tableContents <<- rbind(groupStats, overallStats)

	# extract response variable labels and values
	valLabels <<- as_factor({{data}} %>% pull({{resultVar}})) %>% levels()
	valCodes <<- names(tableContents %>% select(starts_with("perc_"))) %>% str_extract(pattern = "[0-9]+")

	# create mapping of old to new column names
	colText <<- glue("{valLabels} \n ({valCodes})")
	newColNames <<- setNames(as.list(colText), names(tableContents %>% select(-{{groupVar}}, -responseRate, -numObs)))

	# create table with `gt`
	tableContents %>% 
		gt(rowname_col = as_name(enquo(groupVar))) %>% 
		tab_stubhead(label = groupText) %>% 
		tab_header(
			title = title, 
			subtitle = subtitle) %>% 
		cols_label(.list = newColNames) %>%
		cols_label(
			responseRate = "Response rate",
			numObs = "Total number") %>%
		fmt_number(columns =  starts_with("perc_"), scale_by = 100) %>% # percentage with `%` at end
		fmt_number(columns = "responseRate", scale_by = 100) %>%
		tab_options(
			heading.background.color = tableColor,
			column_labels.background.color = tableColor) %>%
		tab_footnote(
			footnote = respRateNote,
			locations = cells_column_labels(
				columns = vars(responseRate))
			) %>%
		opt_footnote_marks(marks = "standard") %>%
		tab_source_note(source_note = thresholdNote) %>%
		tab_style(
			style = cell_text(
				color = "red",
				style = "italic"
				),
			locations = cells_data(
				columns = vars(responseRate),
				rows = responseRate < threshold
				)
			)

}



	# NOTES
	# More verbose to create a basic table
	# But has tons of functions for accessing, formatting, or tweaking parts of the table
	# `tab_options` is to tables what `theme` is to ggplot: tons of granular control

# create table with `flextable`

# library(flextable)

# tableContents %>%
# 	flextable() %>% 
# 	set_header_labels(values = newColNames) %>%
# 	bg(bg = "#075e87", part = "header") %>%
# 	color(color = "white", part = "header")

	# NOTES
	# no obvious way to present shares as percentages
	# nice themes for presenting like in Word (e.g., `theme_booktabs()` for Word, `theme_box` for Excel)
	# nice controls for adding borders like in Word


# create table with `kableExtra`


