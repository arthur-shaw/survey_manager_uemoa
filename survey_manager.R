# =============================================================================
# Load necessary libraries
# =============================================================================

# packages needed for this program 
packagesNeeded <- c(
	"shiny", 		# for interactive user interface
	"shinyjs", 		# for hiding/showing content using JS
	"shinyFiles", 	# for selecting project folder
	"fs",  			# for compiling list of accessible drives
	"waiter", 		# for loading screens
	"RStata", 		# for running Stata scripts from R
	"dplyr", 		# for data wrangling
	"stringr", 		# for string wrangling
	"purrr", 		# for iteration
	"rmarkdown", 	# for creating HTML reports from RMarkdown files
	"readr", 		# for injesting from text and for writing R and Stata programs line by line
	"readxl", 		# for injesting Excel data
	"writexl", 		# for writing data to Excel (e.g., LandPKS)
	"haven", 		# for injesting Stata data files
	"rhandsontable" # for capturing data interactively
)

# identify and install those packages that are not already installed
packagesToInstall <- packagesNeeded[!(packagesNeeded %in% installed.packages()[,"Package"])]
if(length(packagesToInstall)) 
	install.packages(packagesToInstall, quiet = TRUE, 
		repos = 'https://cloud.r-project.org/', dep = TRUE)

# load all needed packages
lapply(packagesNeeded, library, character.only = TRUE)

# =============================================================================
# Helper functions
# =============================================================================

# method for getting latest file modification date in each sub-directory
files_last_mod_date <- function(parent_dir, child_dir) {

	library(fs)
	library(dplyr)

	# create df of metadata on files in targe directory
	fs::dir_info(path = paste0(parent_dir, "/", child_dir, "/"), recurse = FALSE) %>%	
	# add directory in `child_dir` format for ease of reporting to end user
	mutate(dir = child_dir) %>%
	filter(type == "file") %>%
	group_by(dir) %>%
	summarize(files_last_mod = max(modification_time)) %>%
	ungroup()

}

# define writing function
writeToDisk <- function(path, content) {

	library(readr)

	if (!file.exists(path)) {
		toAppend = FALSE
	} else {
		toAppend = TRUE
	}
	readr::write_lines(x = content, path = path, append = toAppend)
}

# =============================================================================
# Define interface
# =============================================================================

dl_wait_screen <- tagList(
	spin_wave(), 
	h4("Téléchargement en cours...")
)

dl_alt_wait_screen <- tagList(
	spin_wave(), 
	h4("Faisant une requête auprès du serveur...")	
)

auto_sort_wait <- tagList(
	spin_pong(),
	h4("Exécutant le programme de rejet....")
)

report_wait_screen <- tagList(
	spin_cube_grid(),
	h4("Création de rapport en cours...")
)

ui <- tagList(

	useShinyjs(),
	use_waiter(), 

	navbarPage(title = "Survey manager",
		tabPanel(
			title = "Configurer", 
			value = "setup_tab",
			icon = icon("cog"),
				fluidPage(
  					radioButtons("have_setup", label = h2("Detenez vous déjà un fichier de configuration"),
    					choices = list("Oui" = 1, "Non" = 2)),
					conditionalPanel(condition = "input.have_setup == 1", 
						tags$p("Veuillez charger le ficher de configuration en le retrouvant sur votre ordinateur"),
						fileInput("setup_file", label = h2("Veuillez sélectionner le fichier de configuration"), accept = ".R")
					),
					conditionalPanel(condition = "input.have_setup == 2",
						
						# project folder
						h2("Dossier du projet"),
						shinyDirButton("proj_folder", "Parcourir dossiers", "Veuillez choisir un dossier", icon = icon("folder")),
						
						# Stata
						h2("Détails concernant Stata"),
						tags$p("Veuillez sélectionner le fichier d'exécution de Stata et le numéro de version."),
						tags$p("These details are needed to execute Stata .do files from this program."),
						shinyFilesButton("stata_exe", "Choisir Stata.exe", "Veuillez sélectionner un fichier", multiple = FALSE, icon = icon("file-code")),
						numericInput(inputId = "stata_version", label = "Spécifier le numéro de version de Stata", value = 14, min = 12, max = 16),
						
						# SuSo connection
						h2("Détails du serveur SuSo"),
						
						h3("Identifiants de connexion"),
						textInput(inputId = "site", label = "Site"),
						textInput(inputId = "user", label = "Nom d'utilisateur"),
						textInput(inputId = "password", label = "Mot de passe"),

						h3("Questionnaires"),
						tags$p("Spécifier le nom ou le motif regex pour identifier le(s) questionnaire(s) du type indiqué."),
						textInput(inputId = "hhold_pattern", label = "Ménage"),
						textInput(inputId = "call_back_pattern", label = "Entretien de rappel"),
						textInput(inputId = "mystery_pattern", label = "Entretien de répondant secret"),

						# Preload file details
						h3("Détails sur les fichiers de préchargement"),
						tags$p("Indiquer le nom du fichier. Inclure l'extension"),
						textInput(inputId = "sample_preload_file", label = "Échantillon", placeholder = "Format Stata"),
						textInput(inputId = "sample_preload_id", label = "ID ménage dans la base échantillon"),
						textInput(inputId = "hhold_preload_file", label = "Ménage", placeholder = "Format Stata"),
						textInput(inputId = "hhold_preload_id", label = "ID ménage dans la base ménage"),
						textInput(inputId = "member_preload_file", label = "Membres", placeholder = "Format Stata"),
						textInput(inputId = "member_preload_id", label = "ID ménage dans la base membre"),
						textInput(inputId = "int_lang_file", label = "Enquêteurs et langues parlées.", placeholder = "Format Excel"),

						# Main data files
						h2("Détails sur les données exportées"),
						tags$p("Nom de base. Veuillez inclure l'extension .dta dans le nom."), 
						textInput(inputId = "hhold_file", label = "Ménage", value = "COVID_19_Mali.dta"),
						textInput(inputId = "attempt_file", label = "Tentative d'appel", value = "tentatives.dta"),
						textInput(inputId = "number_file", label = "Numéros de téléphone", value = "numeros.dta"),
						textInput(inputId = "member_file", label = "Membres du ménage", value = "membres.dta"),

						h2("Détails sur le programme de rejet"),
						h3("Status de Survey Solutions"),
						selectInput(inputId = "suso_status", label = "Status des entretiens à passer en revue", 
							choices = c("Completed", "ApprovedBySupervisor"), 
							selected = c("Completed", "ApprovedBySupervisor"), multiple = TRUE, selectize = TRUE),
						h3("Statut indiqué par l'entretien"),
						textInput(inputId = "data_status", label = "Expression Stata décrivant les entretiens à valider", 
							"inlist(result, 1, 3)"),

						h2("Période de collecte"),
						dateRangeInput("survey_dates", label = "Survey period"),						

						# write parameters to file
						h3("Sauvegarder le fichier de configuration"),
						actionButton(inputId = "writeParams", label = "Sauvegarder", icon = icon("save"))	

					),
					h2("Charger le fichier de configuration"),
					actionButton("setup_load", label = "Charger", icon = icon("play-circle"))

				)

		), 
		tabPanel(
			title = "Télécharger",
			value = "get_data",
			icon = icon("file-download"),
			h2("Ménage"),
			actionButton("get_hhold_data", label = "Télécharger", icon = icon("home")),
			actionButton("check_hhold_data", label = "Vérifier", icon = icon("check-square")),
			actionButton("use_hhold_data", label = "Utiliser ces fichiers", icon = icon("play-circle")),
			verbatimTextOutput("hhold_data_found"),
			h2("Composition des équipes de collecte"),
			actionButton("get_teams_data", label = "Télécharger", icon = icon("users")),
			h2("Enregistrements sonores"),
			actionButton("get_audio", label = "Télécharger", icon = icon("file-audio")),
			actionButton("check_audio_data", label = "Vérifier", icon = icon("check-square")),
			actionButton("use_audio_data", label = "Utiliser ces fichiers", icon = icon("play-circle")),
			verbatimTextOutput("audio_files_found"),
		),
		navbarMenu("Affecter", icon = icon("exchange-alt"),
			tabPanel("Ménage", icon = icon("home"), value = "assign_hholds",
				rHandsontableOutput("allocation"),
				actionButton("make_hhold_assign", label = "Créer affectations", icon = icon("play-circle"))
			),
			tabPanel("Entretien de rappel", icon = icon("phone-square"), value = "assign_call_back",
				tags$p("A VENIR PROCHAINEMENT")		
			),
			tabPanel("Répondant secret", icon = icon("user-secret"), value = "assign_mystery",
				tags$p("A VENIR PROCHAINEMENT")
			)		
		),
		navbarMenu("Agir", icon = icon("tasks"), 
			tabPanel("Approuver/rejeter", icon = icon("laptop-code"), value = "action_auto_sort",
				h2("Exécuter"),
				checkboxInput("autosort_acts", label = "Approuver/rejeter lors de l'exécution", value = FALSE),
				actionButton("run_autosort", label = "Exécuter", icon = icon("code")), 
				h2("Créer un rapport"),
				tags$p("NB: Ce bouton ne marche pas encore"),
				actionButton("make_autosort_report", label = "Créer", icon = icon("chart-bar"))
			),
			tabPanel("Surveiller", icon = icon("table"), value = "action_fcheck",
				conditionalPanel(condition = "output.got_teams == 1",
					h2("Paramètres du rapport"),
					dateRangeInput("fcheck_dates", label = "Période", start = (Sys.Date() - 7)),
					# TODO: require `interviewer` df to exist
					selectInput("fcheck_group_var", label = "Niveau d'agrégation", 
						choices = c("team"), 
						multiple = FALSE),
					selectInput("fcheck_teams", label = "Équipe(s)", 
						choices =  "",
							# c("All teams", "team 1", "team 2", "team 3"),
						selected = "All teams",
						multiple = TRUE,
						selectize = TRUE),
					h2("Créer rapport"),
					actionButton("make_fieldcheck_report", label = "Créer", icon = icon("table"))
				),
				conditionalPanel(condition = "output.got_teams == 2",
					tags$p("Besoin de télécharger la compostion des équipes de collectes avant créer le rapport")
				)
			),
			tabPanel("Écouter", icon = icon("headphones"), value = "action_listen",
				tags$p("A VENIR PROCHAINEMENT")
			)

		)
	)

)

# =============================================================================
# Define server functions
# =============================================================================

server <- function(input, output, session) {

	# initialize Shiny app by hiding...
	# ...buttons
	shinyjs::hide("use_hhold_data")
	# ...assignment actions
	shinyjs::hide(selector = "li a[data-value=assign_call_back]")
	# ...other actions
	shinyjs::hide(selector = "li a[data-value=get_data]")
	shinyjs::hide(selector = "li a[data-value=assign_hholds]")
	shinyjs::hide(selector = "li a[data-value=action_auto_sort]")
	shinyjs::hide(selector = "li a[data-value=action_fcheck]")
	shinyjs::hide(selector = "li a[data-value=action_listen]")

	# selection of project folder
	volumes <- c(Home = fs::path_home(), "R Installation" = R.home(), getVolumes()())
	shinyDirChoose(input, "proj_folder", roots = volumes, session = session, restrictions = system.file(package = "base"))

	# selection of Stata installation
	shinyFileChoose(input, "stata_exe", roots = volumes, session = session)

	# save user-provided parameters to disk
	observeEvent(input$writeParams, {

		projDir <<- paste0(parseDirPath(volumes, input$proj_folder), "/")
		stata_loc <<- parseFilePaths(volumes, input$stata_exe)$datapath
		stata_windows <<- gsub(pattern = "/", replacement = "\\\\", stata_loc)
		stata_path <<- shQuote(tools::file_path_sans_ext(stata_windows))

		contentLines <<- c(
			"# Project folder",
			paste0("projDir 	<- ", '"', projDir, '"'),
			paste0("dataDir 	<- ", 'paste0(projDir, "/data/")'),
			paste0("progsDir 	<- ", 'paste0(projDir, "/programs/")'),
			paste0("rejectDir	<- ", 'paste0(progsDir, "/auto-sort/")'),
			paste0("fcheckDir	<- ", 'paste0(progsDir, "/monitoring-tables/")'),
			paste0("assignHHDir	<- ", 'paste0(progsDir, "/assign-hholds/")'),
			paste0("callBckDir	<- ", 'paste0(progsDir, "/assign-call-back/")'),
			paste0("mysteryDir	<- ", 'paste0(progsDir, "/assign-mystery/")'),
			"",
			"# Stata",
			paste0("stata_loc	<- ", '"', stata_loc, '"'),
			paste0('options("RStata.StataPath" = shQuote(tools::file_path_sans_ext("', stata_loc, '"))', ")"),
			paste0('options("RStata.StataVersion" = ', input$stata_version, ")"),
			"",
			"# SuSo server details", 
			paste0("site 		<- ", '"', input$site, '"'),
			paste0("user 		<- ", '"', input$user, '"'),
			paste0("password 	<- ", '"', input$password, '"'),
			"",
			"# SuSo survey details",
			paste0("hhold_pattern 		<- ", '"', input$hhold_pattern, '"'),
			paste0("call_back_pattern	<- ", '"', input$call_back_pattern, '"'),
			paste0("mystery_pattern 	<- ", '"', input$mystery_pattern, '"'),
			"",
			"# Preload file details",
			paste0("sample_preload_file	<- ", '"', input$sample_preload_file, '"'),
			paste0("sample_preload_id	<- ", '"', input$sample_preload_id, '"'),
			paste0("hhold_preload_file <- ", '"', input$hhold_preload_file, '"'),
			paste0("hhold_preload_id <- ", '"', input$hhold_preload_id, '"'),
			paste0("member_preload_file	<- ", '"', input$member_preload_file, '"'),
			paste0("member_preload_id	<- ", '"', input$member_preload_id, '"'),
			paste0("int_lang_file	<- ", '"', input$int_lang_file, '"'),
			"", 			
			"# Data details",
			paste0("hhold_file	<- ", '"', input$hhold_file, '"'),
			paste0("attempt_file <- ", '"', input$attempt_file, '"'),
			paste0("number_file	<- ", '"', input$number_file, '"'),
			paste0("member_file	<- ", '"', input$member_file, '"'),
			"",
			"# Data collection period",
			paste0("surveyStart	<- ", '"', input$survey_dates[1], '"'),
			paste0("surveyEnd	<- ", '"', input$survey_dates[2], '"'),
			""
		)

		# create file path
		outputPath <<- paste0(projDir, "setup_params.R")

		# if file exists at that path, delete it
		if (file.exists(outputPath)) {
			file.remove(outputPath)
		}

		# write file to disk line by line
		purrr::walk(.x = contentLines, .f = writeToDisk, path = outputPath)

		susoStatuses <- input$suso_status %>%
			str_replace(pattern = "Completed", replacement = "100") %>%
			str_replace(pattern = "ApprovedBySupervisor", replacement = "120") %>%
			paste( collapse = ", ")

		# write Stata parameters to file
		contentLinesStata <<- c(
			"* FOLDERS",
			paste0("local rootDir 			", '"', projDir, '"'),
			paste0("local dataDir			", '"`rootDir', "'", "/data/hhold/", '"'),
			paste0("local downloadDir 		", '"`dataDir', "'", "/downloaded/", '"'),
			paste0("local rawDir 			", '"`dataDir', "'", "/combined/", '"'),
			paste0("local constructedDir 	", '"`dataDir', "'", "/derived/", '"'),
			paste0("local resourceDir 		", 	'"`dataDir', "'", "/resource/", '"'),
			paste0("local progDir 			", 	'"`rootDir', "'", "/programs/auto-sort/", '"'),
			paste0("local resultsDir		", 	'"`rootDir', "'", "/outputs/auto-sort/results/", '"'),
			paste0("local logDir			", 	'"`rootDir', "'", "/outputs/auto-sort/logs/", '"'),
			"",
			"* FILES",
			"// user-input",
			paste0("local hholds 	", '"', input$hhold_file, '"'),
			paste0("local attempts 	", '"', input$attempt_file, '"'),
			paste0("local numbers 	", '"', input$number_file, '"'),
			paste0("local members 	", '"', input$member_file, '"'),
			"",	
			"// constructed",
			'local attributes 		"attributes.dta"',
			'local issues 			"issues.dta"',
			"",
			"* FILE PATHS",
			paste0("local attributesPath 	", '"', "`constructedDir'/`attributes'", '"'),
			paste0("local issuesPath 		", '"', "`constructedDir'/`issues'", '"'),
			"",
			"* IDENTIFY INTERVIEWS TO PROCESS",
			"// By SuSo status",
			paste0("local statusesToReject ", '"', susoStatuses, '"'),
			"",
			"// By data status",
			paste0("local completedInterview ", '`"', input$data_status, '"', "'"),
			"// maximum number of unanswered answers to allow",
			"local maxUnanswered = 0",
			"",
			"* RUN STATA AUTO-SORT PROGRAMS",
			paste0('include "`progDir', "'/runAll.do")
		)

		# create file output path
		outputPathStata <- paste0(projDir, "/programs/auto-sort/", "configure_and_run.do")

		# if file exists at that path, delete it
		if (file.exists(outputPathStata)) {
			file.remove(outputPathStata)
		}

		# write file to disk line by line
		purrr::walk(.x = contentLinesStata, .f = writeToDisk, path = outputPathStata)

		write_append_prog <- function(
			dir_root,
			dir_data,
			file_output
		) {

			# write Stata append instructions to file
			contentLinesAppend <- c(

				"* folders",
				paste0("local rootDir 			", '"', dir_root, '"'),
				paste0("local dataDir			", '"`rootDir', "'", dir_data, '"'), 		# directory relative to project root
																							# "/data/hhold/"
				paste0("local downloadDir 		", '"`dataDir', "'", "/downloaded/", '"'),
				paste0("local rawDir 			", '"`dataDir', "'", "/combined/", '"'),
				paste0("local progDir 			", 	'"`rootDir', "'", "/programs/auto-sort/", '"'),
				"",
				"* load append program",
				paste0("include ", '"', "`progDir'/appendAll.do", '"'),
				"",
				"* execute appendAll to append together same-named files from different template versions",
				"appendAll, 							///",
				paste0("	", "inputDir(", '"', "`downloadDir'", '") 		///	where to find downloaded data'),
				paste0("	", "outputDir(", '"', "`rawDir'", '") 		///	where to save appended data'),
				""
			)

			# create append file path
			outputPathAppend <- file_output # paste0(paste0(projDir, "/programs/"), "append_instructions.do")

			# if file exists at that path, delete it
			if (file.exists(outputPathAppend)) {
				file.remove(outputPathAppend)
			}

			# write file to disk line by line
			purrr::walk(.x = contentLinesAppend, .f = writeToDisk, path = outputPathAppend)

		}

		write_append_prog(
			dir_root = projDir, 
			dir_data = "/data/hhold/", 
			file_output = paste0(paste0(projDir, "/programs/"), "append_hhold.do"))

		write_append_prog(
			dir_root = projDir, 
			dir_data = "/data/call_back/", 
			file_output = paste0(paste0(projDir, "/programs/"), "append_call_back.do"))

		write_append_prog(
			dir_root = projDir, 
			dir_data = "/data/mystery/", 
			file_output = paste0(paste0(projDir, "/programs/"), "append_mystery.do"))

	})

	# load newly created setup file
	observeEvent(input$setup_file_new, {
		source(input$setup_file$datapath)
	})

	# load setup file
	observeEvent(input$setup_load, {
		if (input$have_setup == 1) {
			source(input$setup_file$datapath)
		} else if (input$have_setup == 2) {
			source(outputPath)
		}

		shinyjs::show(selector = "li a[data-value=get_data]")
	})

	# get household data from SuSo
	observeEvent(input$get_hhold_data, {

		waiter_show(html = dl_wait_screen)

		source(paste0(progsDir, "get_hhold_data.R"))

		stata(src = paste0(progsDir, "append_hhold.do"))
		
		waiter_hide()
		
		# assignment actions
		shinyjs::show(selector = "li a[data-value=assign_call_back]")

		# other actions
		shinyjs::show(selector = "li a[data-value=action_auto_sort]")
		shinyjs::show(selector = "li a[data-value=action_fcheck]")

	})

	# check dates of hhold data downloaded on local machine
	observeEvent(input$check_hhold_data, {

		# create hhold data download directory
		hholdDlDir <- paste0(dataDir, "hhold/downloaded/")

		# compile list of folders (each folder corresponding to a file)
		hholdDataDirs <- list.dirs(hholdDlDir, full.names = FALSE, recursive = FALSE)

		# compile last file modification time for each sub-directory
		hhold_last_mod_by_dir <- map_dfr(
			.x = hholdDataDirs,
			.f = ~ files_last_mod_date(parent_dir = hholdDlDir, .x),
		)

		output$hhold_data_found <- renderPrint({ hhold_last_mod_by_dir })

	})

	# enable use data action if data checked
	observeEvent(input$check_hhold_data, {
		shinyjs::show("use_hhold_data")
	})

	# enable actions tab if decide to use currently downloaded data
	observeEvent(input$use_hhold_data, {

		# assignment actions
		shinyjs::show(selector = "li a[data-value=assign_call_back]")

		# other actions
		shinyjs::show(selector = "li a[data-value=action_auto_sort]")
		shinyjs::show(selector = "li a[data-value=action_fcheck]")

	})

	# get audio data from SuSo
	observeEvent(input$get_audio, {

		waiter_show(html = dl_wait_screen)

		source(paste0(progsDir, "get_audio_data.R"))
	
		waiter_hide()
		
		# enable listening to audio
		shinyjs::show(selector = "li a[data-value=action_listen]")

	})

	# check dates of hhold data downloaded on local machine
	observeEvent(input$check_audio_data, {

		# create hhold data download directory
		audioDlDir <- paste0(dataDir, "audio/downloaded/")

		# compile list of folders (each folder corresponding to a file)
		audioDataDirs <- list.dirs(audioDlDir, full.names = FALSE, recursive = FALSE)

		# compile last file modification time for each sub-directory
		audio_last_mod_by_dir <- map_dfr(
			.x = audioDataDirs,
			.f = ~ files_last_mod_date(parent_dir = hholdDlDir, .x),
		)

		output$audio_files_found <- renderPrint({ audio_last_mod_by_dir })

	})

	# enable use data action if data checked
	observeEvent(input$check_audio_data, {
		shinyjs::show("use_audio_data")
	})

	# enable listen to audio if decide to use currently downloaded data
	observeEvent(input$use_audio_data, {

		shinyjs::show(selector = "li a[data-value=action_listen]")

	})

	# get field team composition data
	got_teams_data <- reactiveVal(NA)	# initialize to NA
	team_choices <- reactiveVal(data.frame(supervisor = "All teams"))
	observeEvent(input$get_teams_data, {
		source(paste0(progsDir, "get_team_composition.R"))
		print("Finished running team code")
		if (exists("interviewers")) {
			got_teams_data(TRUE) 			# update to TRUE if df exists
			team_choices(interviewers %>% select(supervisor) %>% distinct() %>% rbind("All teams"))
			shinyjs::show(selector = "li a[data-value=assign_hholds]")
		} else {
			got_teams_data(FALSE) 			# update to FALSE if df DNE after running code
		}
	})
	observe({
		updateSelectInput(session, "fcheck_teams", choices = team_choices(), selected = "All teams")
		updateSelectInput(session, "progress_teams", choices = team_choices(), selected = "All teams")
	})

	output$got_teams <- reactive({
		if_else(condition = got_teams_data() == TRUE,
			true = 1,
			false = 2,
			missing = 2)
	})
	outputOptions(output, "got_teams", suspendWhenHidden = FALSE)

	output$got_teams_print <- renderPrint({
		if_else(
			condition = got_teams_data(),
			true = "Data successfully received",
			false = "Data request failed",
			missing = "Data not requested yet"
		)
	})

	# get SuSo interviewer sync data
	observeEvent(input$get_suso_sync, {
		waiter_show(html = dl_alt_wait_screen)
		source(paste0(progsDir, "get_sync_data.R")) # TODO: make this a common component outside of cluster report prog dir
		waiter_hide()
	})

	# get community data from SuSo
	observeEvent(input$get_comm_data, {

		waiter_show(html = dl_wait_screen)

		# get data
		source(paste0(progsDir, "get_comm_data.R"))
	
		# append data
		stata(src = paste0(progsDir, "append_community.do"))

		waiter_hide()

	})

	# get education data from SuSo
	observeEvent(input$get_educ_data, {

		waiter_show(html = dl_wait_screen)

		# get data
		source(paste0(progsDir, "get_educ_data.R"))

		# append data
		stata(src = paste0(progsDir, "append_education.do"))
		
		waiter_hide()

	})

	# get education data from SuSo
	observeEvent(input$get_health_data, {

		waiter_show(html = dl_wait_screen)

		source(paste0(progsDir, "get_health_data.R"))

		# append data
		stata(src = paste0(progsDir, "append_health.do"))
		
		waiter_hide()

	})

	# get all data
	
	observeEvent(input$get_all_data, {

		waiter_show(html = dl_wait_screen)

		# household
		source(paste0(progsDir, "get_hhold_data.R"))
		stata(src = paste0(progsDir, "append_hhold.do"))

		# call-back
		# TODO: create this program
		# source(paste0(progsDir, "get_call_back_data.R"))
		# stata(src = paste0(progsDir, "append_call_back.do"))

		# mystery respondent
		# TODO: create this program
		# source(paste0(progsDir, "get_mystery_data.R"))
		# stata(src = paste0(progsDir, "append_mystery.do"))

		# team composition
		source(paste0(progsDir, "get_team_composition.R"))

		# audio
		source(paste0(progsDir, "get_audio_data.R"))

		waiter_hide()

		# enable action tabs

		# assignment actions
		shinyjs::show(selector = "li a[data-value=assign_hholds]")
		shinyjs::show(selector = "li a[data-value=assign_call_back]")
		shinyjs::show(selector = "li a[data-value=assign_mystery]")

		# other actions
		shinyjs::show(selector = "li a[data-value=action_auto_sort]")
		shinyjs::show(selector = "li a[data-value=action_fcheck]")
		shinyjs::show(selector = "li a[data-value=action_listen]")

	})

	# run auto-sort
	observeEvent(input$run_autosort, {
		
		waiter_show(html = auto_sort_wait)

		# run Stata programs for creating attributes and issues
		print("Starting execution of auto-sort")
		print("Starting Stata prog")
		stata(src = paste0(rejectDir, "configure_and_run.do"))

		# pass decision on auto-sort behavior from UI to script sourced below
		to_reject <<- input$autosort_acts

		# run R components of auto-sort once Stata has created outputs
		# that serve as R's inputs
		print("Preparing to run R prog")
		print(paste0("To reject is: ", to_reject))
		while(!file.exists(paste0(dataDir, "/hhold/derived/issues.dta"))) {
			print("Waiting for Stata process to finish")
			 Sys.sleep(1)
		}
		print("Running R program")
		source(paste0(rejectDir, "processInterviews.R"))

		waiter_hide()

	})

    output$allocation <- renderRHandsontable({

		allocation_table <- interviewers %>%
			filter(!str_detect(interviewer, "_interv$")) %>%
			arrange(interviewer) %>%
			select(interviewer) %>%
			# TODO: filter to non-archived interviewers
			mutate(num_to_assign = NA_real_)		
		
		rhandsontable(allocation_table) %>%
            hot_col("interviewer", readOnly = TRUE) %>%
            hot_table(highlightRow = TRUE)

    })

	observeEvent(input$make_hhold_assign, {

        data_entered <<- hot_to_r(input$allocation) %>%
            filter(!is.na(num_to_assign))
    
		source(paste0(progsDir, "assign-hholds/", "make_hhold_assignments.R"))

	})

	# create field check report
	observeEvent(input$make_fieldcheck_report, {
		
		waiter_show(html = report_wait_screen)

		fcheck_params = list(
			# TODO: Come back to this once have written to file
			
			# pass user-provided paramters to Rmd
			hhold_file = hhold_file,
			attempt_file = attempt_file,
			number_file = number_file,
			member_file = member_file,
			reportStart = input$fcheck_dates[1],
			reportEnd = input$fcheck_dates[2],
			supervisors = input$fcheck_teams,
			groupVar = input$fcheck_group_var,
			site = site,
			user = user,
			password = password
		)

		# create report using parameters above
		rmarkdown::render(
			input = 		paste0(fcheckDir, "report.Rmd"), 
			output_dir = 	paste0(projDir, "outputs/", "monitoring-tables/"), 
			output_file = 	paste0("report.html"), 
			params = fcheck_params,
			encoding = "UTF-8")

		waiter_hide()

	})

}

shinyApp(ui = ui, server = server)
