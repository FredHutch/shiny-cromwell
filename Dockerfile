FROM fredhutch/r-shiny-server-base:4.3.2

RUN apt-get update -y && apt-get install -y libssh-dev

RUN R -q -e 'install.packages(c("ellipsis"), repos="https://cran.rstudio.com/")'
RUN R -q -e 'install.packages(c("shiny"), repos="https://cran.rstudio.com/")'
RUN R -q -e 'install.packages(c("shinyFeedback", "shinyWidgets", "shinydashboard", "shinydashboardPlus", "ssh", "paws", "remotes", "markdown", "lubridate", "jsonlite", "cookies", "dplyr", "RSQLite", "DBI", "data.table", "DT", "glue", "httr", "purrr", "RColorBrewer", "rlang", "shinyBS", "shinyjs", "tidyverse", "uuid", "base64enc"), repos="https://cran.r-project.org")'

RUN R -q -e "remotes::install_github('getwilds/proofr@v0.2')"

RUN R -q -e "remotes::install_github('getwilds/rcromwell@v3.2.0')"


ADD .my.cnf /root/

ADD check.R /tmp/

RUN R -f /tmp/check.R --args ellipsis shiny shinyWidgets shinydashboard shinydashboardPlus ssh paws remotes markdown lubridate jsonlite rcromwell DT tidyverse RColorBrewer glue shinyBS shinyjs shinyFeedback rmarkdown proofr httr cookies dplyr RSQLite DBI purrr data.table rlang uuid base64enc

RUN rm -rf /srv/shiny-server/
ADD app/. /srv/shiny-server/

CMD /usr/bin/shiny-server
