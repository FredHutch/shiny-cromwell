RSCRIPT = Rscript --no-init-file
FILE_TARGET := "${FILE}"
DEPS := $(shell ${RSCRIPT} -e 'invisible(lapply(c("glue", "cli"), require, character.only = TRUE, quiet = TRUE))' -e 'deps = renv::dependencies(quiet = TRUE)' -e 'uniq_pkgs = sort(unique(deps$$Package))' -e 'uniq_pkgs = uniq_pkgs[!grepl("^proofr$$|^rcromwell$$", uniq_pkgs)]' -e 'cat(c("getwilds/proofr@v0.4.0", "getwilds/rcromwell@v3.3.0", uniq_pkgs), file="deps.txt", sep="\n")')

run:
	${RSCRIPT} -e "options(shiny.devmode = TRUE)" \
		-e "shiny::runApp(\"app\", launch.browser = TRUE, port = 4026)"

run_docker:
	docker build --platform linux/amd64 -t shiny-cromwell:app .
	docker run --rm -it -p 3838:3838 shiny-cromwell:app

# use: `make branch=inputs-viewer run_branch`
run_branch:
	docker pull nexus-registry.fredhutch.org/scicomp-nexus/shiny-cromwell:$(branch)
	docker run --rm -it -p 3838:3838 nexus-registry.fredhutch.org/scicomp-nexus/shiny-cromwell:$(branch)

# point the shiny app at the dev API
# use: `make branch=inputs-viewer run_branch_dev_api`
run_branch_dev_api:
	docker pull nexus-registry.fredhutch.org/scicomp-nexus/shiny-cromwell:$(branch)
	docker run -e PROOF_API_BASE_URL=https://proof-api-dev.fredhutch.org --rm -it -p 3838:3838 nexus-registry.fredhutch.org/scicomp-nexus/shiny-cromwell:$(branch)

# point the shiny app at a custom API instance
# use: `make branch=inputs-viewer url=http://gizmo666:2112 run_branch_custom_api`
run_branch_custom_api:
	docker pull nexus-registry.fredhutch.org/scicomp-nexus/shiny-cromwell:$(branch)
	docker run -e PROOF_API_BASE_URL=$(url) --rm -it -p 3838:3838 nexus-registry.fredhutch.org/scicomp-nexus/shiny-cromwell:$(branch)


# use: `make style_file FILE=stuff.R`
# accepts 1 file only
style_file:
	${RSCRIPT} -e 'styler::style_file(${FILE_TARGET})'

pkg_deps_cmd:
	@${RSCRIPT} -e 'invisible(lapply(c("glue", "cli"), require, character.only = TRUE, quiet = TRUE))' \
	-e 'uniq_pkgs = readLines("deps.txt")' \
	-e 'cli_alert_info("Found {length(uniq_pkgs)} packages")' \
	-e 'cli_alert_info("Here are the installation instructions:")' \
	-e "cli_code(glue('pak::pak(c({glue_collapse(double_quote(uniq_pkgs), sep = \", \")}))'))"

pkg_deps_install:
	@${RSCRIPT} -e 'invisible(lapply(c("glue", "cli"), require, character.only = TRUE, quiet = TRUE))' \
	-e 'uniq_pkgs = readLines("deps.txt")' \
	-e 'cli_alert_info("Found {length(uniq_pkgs)} packages")' \
	-e 'cli_alert_info(glue("{glue_collapse(uniq_pkgs, sep = \", \")}"))' \
	-e 'cli_alert_info("Installing them using pak:")' \
	-e "pak::pak(eval(parse(text = glue('c({glue_collapse(double_quote(uniq_pkgs), sep = \", \")})'))))"
