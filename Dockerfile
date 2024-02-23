FROM fredhutch/r-shiny-server-base:4.3.2

RUN apt-get update -y && apt-get install -y libssh-dev

RUN R -q -e 'install.packages(c("ellipsis"), repos="https://cran.rstudio.com/")'
RUN R -q -e 'install.packages(c("shiny"), repos="https://cran.rstudio.com/")'
RUN R -q -e 'install.packages(c("shinyFeedback", "shinyWidgets", "shinydashboard", "shinydashboardPlus", "ssh", "remotes", "markdown", "lubridate", "jsonlite", "dplyr", "DT", "glue", "httr", "purrr", "RColorBrewer", "rlang", "shinyBS", "shinyjs", "tidyverse", "uuid", "memoise", "rclipboard", "shinyvalidate", "shinylogs", "testhat"), repos="https://cran.r-project.org")'

RUN R -q -e "remotes::install_github('getwilds/proofr@v0.2')"

RUN R -q -e "remotes::install_github('getwilds/rcromwell@v3.2.0')"

RUN rm -rf /srv/shiny-server/
COPY app/ /srv/shiny-server/

ENV APPLICATION_LOGS_TO_STDOUT=true
ENV SHINY_LOG_STDERR=1

CMD /usr/bin/shiny-server


