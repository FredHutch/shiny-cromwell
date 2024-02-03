RSCRIPT = Rscript --no-init-file
FILE_TARGET := "${FILE}"

run:
	${RSCRIPT} -e "options(shiny.autoreload = TRUE)" \
		-e "shiny::runApp(\"app\", launch.browser = TRUE)"

# use: `make style_file FILE=stuff.R`
# ("R/" is prepended); accepts 1 file only
style_file:
	${RSCRIPT} -e 'styler::style_file(${FILE_TARGET})'

pkg_deps:
	@${RSCRIPT} -e 'invisible(lapply(c("glue", "cli"), require, character.only = TRUE, quiet = TRUE))' \
		-e 'deps = renv::dependencies(quiet = TRUE)' \
		-e 'uniq_pkgs = sort(unique(deps$$Package))' \
		-e 'uniq_pkgs = uniq_pkgs[!grepl("^proofr$$|^rcromwell$$|^shinyauthr$$", uniq_pkgs)]' \
		-e 'uniq_pkgs = c("getwilds/proofr", "getwilds/rcromwell", "getwilds/shinyauthr@remote-api", uniq_pkgs)' \
		-e 'cat("\n")' \
		-e 'cli_alert_info("Found {length(uniq_pkgs)} packages")' \
		-e 'cli_alert_info("Here are the installation instructions:")' \
		-e "cli_code(glue('pak::pak(c({glue_collapse(double_quote(uniq_pkgs), sep = \", \")}))'))"
