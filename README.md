# shiny-cromwell

Shiny app for interacting with the Fred Hutch instances of Cromwell.

The app lives at a URL only accessible to Fred Hutch folks.

This app can also be run on your local machine for any reason, e.g., if you notice memory limitations in the deployed version or you want to run it at another institution.

## Running the app

You can run this Shiny app locally. First you'll need to install required packages:

(note: run `make pkg_deps` to update the below code block)

```r
pak::pak(c("getwilds/proofr@v0.2", "getwilds/rcromwell@v3.2.0", "cookies", "data.table", "DBI", "dplyr", "DT", "glue", "httr", "jsonlite", "lubridate", "markdown", "purrr", "RColorBrewer", "rlang", "RSQLite", "shiny", "shinyBS", "shinydashboard", "shinydashboardPlus", "shinyFeedback", "shinyjs", "shinyWidgets", "tidyverse", "uuid"))
```

After installing required packages run the app. There's a few different ways to do that:

1. If you are familiar with using make commands, run `make run`
2. If you don't or can't use make, you can run it on the command line like `Rscript -e 'shiny::runApp("app", launch.browser = TRUE)'`
3. Inside of R you can run `shiny::runApp("app", launch.browser = TRUE)'`

All of these should open the Shiny app in your default browser. If that does not happen get in touch!
