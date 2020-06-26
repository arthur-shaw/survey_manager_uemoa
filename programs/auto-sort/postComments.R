post_var_comment <- function(
	interview__id,
	interview__key,
	issueVars,
	issueLoc,
	issueComment,	
	server,
	user,
	password,
	log_dir
) {

	# packages needed for this program 
	packagesNeeded <- c(
		"httr", 	# to talk with the server
		"glue", 	# to create easy-to-read messages more easily
		"readr" 	# to write failure info to disk
	)

	# identify and install those packages that are not already installed
	packagesToInstall <- packagesNeeded[!(packagesNeeded %in% installed.packages()[,"Package"])]
	if(length(packagesToInstall)) 
		install.packages(packagesToInstall, quiet = TRUE, 
			repos = 'https://cloud.r-project.org/', dep = TRUE)

	# load all needed packages
	lapply(packagesNeeded, library, character.only = TRUE)

	# formulate API call	
	base_url <- paste0("https://", server, ".mysurvey.solutions", 
		"/api/v1/interviews/", interview__id, 			# interview
		"/comment-by-variable/", issueVars	 			# variable
		)

	call <- httr::modify_url(url = base_url, 
		query = list(rosterVector = issueLoc, comment = issueComment))

	# make request
	response <- httr::POST(
		url = call, 
		authenticate(user = user, password = password),
		accept_json(),
		content_type_json()
		)

	# react to response

	status_code <- httr::status_code(response) 

	# if request failed, capture details on failure
	if (status_code != 200) {

		print(glue("
			Problem posting comment to interview {interview__id} for \\
			variable {issueVars} at location {issueLoc}
			"))

		# extract interview info and reason for failure
		failed_post_comment <- data.frame(
			interview__id = interview__id,
			status_code = status_code,
			reason = content(response),
			stringsAsFactors = FALSE
		)

		# write failure info to disk
		readr::write_csv(
			failed_post_comment, 
			path = paste0(log_dir, "failed_post_comment.csv"),
			append = TRUE
		)

	# if request succeeded, indicate success
	} else {

		print(glue(
			"Success posting comment to interview {interview__id} for \\ 
			variable {issueVars} at location {issueLoc}
			"))

	}

}





