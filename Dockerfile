FROM fredhutch/r-shiny-server-base:4.3.2

RUN apt-get update -y && apt-get install -y libssh-dev

RUN R -q -e 'install.packages(c("ellipsis"), repos="https://cran.rstudio.com/")'
RUN R -q -e 'install.packages(c("shiny"), repos="https://cran.rstudio.com/")'
RUN R -q -e 'install.packages(c("shinyWidgets", "shinydashboard", "ssh", "paws", "remotes", "markdown", "lubridate", "jsonlite"), repos="https://cran.r-project.org")'

RUN echo remove this

RUN R -q -e "remotes::install_github('getwilds/rcromwell@v2.0.2')"

RUN R -q -e "remotes::install_github('getwilds/proofr@v0.1')"


ADD check.R /tmp/

RUN RUN R -f /tmp/check.R --args ellipsis shiny shinyWidgets shinydashboard ssh paws remotes markdown lubridate jsonlite rcromwell DT tidyverse RColorBrewer glue shinyBS shinyjs shinyFeedback rmarkdown proofr httr 

RUN rm -rf /srv/shiny-server/
ADD app/. /srv/shiny-server/
EXPOSE 3838
