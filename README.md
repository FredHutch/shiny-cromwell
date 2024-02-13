# shiny-cromwell

Shiny app for interacting with the Fred Hutch instances of Cromwell.

The app lives at a URL only accessible to Fred Hutch folks.

This app can also be run on your local machine for any reason, e.g., if you notice memory limitations in the deployed version or you want to run it at another institution.

## Running the app

### Without Docker

You can run this Shiny app locally. First you'll need to install required packages:

(note: run `make pkg_deps_cmd` to update the below code block)

```r
pak::pak(c("getwilds/proofr@v0.2", "getwilds/rcromwell@v3.2.0", "base64enc", "cookies", "DBI", "dplyr", "DT", "ggplot2", "glue", "jsonlite", "lubridate", "magrittr", "purrr", "RColorBrewer", "rlang", "RSQLite", "shiny", "shinyBS", "shinydashboard", "shinydashboardPlus", "shinyFeedback", "shinyjs", "shinyWidgets", "tibble", "uuid"))
```

And the above yourself in R. 

Or you can run `pkg_deps_install` which should find and install required packages all in one step.

After installing required packages run the app. There's a few different ways to do that:

1. If you are familiar with using make commands, run `make run`
2. If you don't or can't use make, you can run it on the command line like `Rscript -e 'shiny::runApp("app", launch.browser = TRUE)'`
3. Inside of R you can run `shiny::runApp("app", launch.browser = TRUE)'`

All of these should open the Shiny app in your default browser. If that does not happen get in touch!

### With Docker

The make command `make run_docker` will attempt to run the app with Docker. You'll need Docker installed and running for this to work. 

Note that CTRL+C doesn't work to kill the container after running `make run_docker`. Yo have to use a separate terminal window or Docker Desktop to kill the container (basically the docker process doesn't receive the signal to kill its process).

Also note that docker commands can be specific to the host operating system, so if you run into errors you may need to modify the docker commands.
