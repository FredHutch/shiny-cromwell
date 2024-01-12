RSCRIPT = Rscript --no-init-file
FILE_TARGET := "${FILE}"

run:
	${RSCRIPT} -e "options(shiny.autoreload = TRUE)" \
		-e "shiny::runApp(\"app\", launch.browser = TRUE)"

# use: `make style_file FILE=stuff.R`
# ("R/" is prepended); accepts 1 file only
style_file:
	${RSCRIPT} -e 'styler::style_file(${FILE_TARGET})'
