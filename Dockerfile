FROM fredhutch/r-shiny-server-base:4.3.2

RUN apt-get update -y && apt-get install -y libssh-dev

RUN R -q -e 'install.packages(c("ellipsis"), repos="https://cran.rstudio.com/")'
RUN R -q -e 'install.packages(c("shiny"), repos="https://cran.rstudio.com/")'
RUN R -q -e 'install.packages(c("shinyFeedback", "shinyWidgets", "shinydashboard", "shinydashboardPlus", "ssh", "remotes", "markdown", "lubridate", "jsonlite", "dplyr", "DT", "glue", "httr", "purrr", "RColorBrewer", "rlang", "shinyBS", "shinyjs", "tidyverse", "uuid", "memoise"), repos="https://cran.r-project.org")'

RUN R -q -e "remotes::install_github('getwilds/proofr@v0.2')"

RUN R -q -e "remotes::install_github('getwilds/rcromwell@v3.2.0')"




ADD check.R /tmp/

RUN R -f /tmp/check.R --args ellipsis shiny shinyWidgets shinydashboard shinydashboardPlus ssh remotes markdown lubridate jsonlite rcromwell DT tidyverse RColorBrewer glue shinyBS shinyjs shinyFeedback rmarkdown proofr httr dplyr purrr rlang uuid memoise

RUN rm -rf /srv/shiny-server/
ADD app/. /srv/shiny-server/

CMD /usr/bin/shiny-server
