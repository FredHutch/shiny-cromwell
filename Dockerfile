FROM fredhutch/r-shiny-server-base

RUN apt-get update -y && apt-get install -y libssh-dev

RUN R -q -e 'install.packages(c("ellipsis"), repos="https://cran.rstudio.com/")'
RUN R -q -e 'install.packages(c("shiny"), repos="https://cran.rstudio.com/")'
RUN R -q -e 'install.packages(c("shinyWidgets", "shinydashboard", "ssh", "paws", "remotes", "markdown", "lubridate", "jsonlite"), repos="https://cran.r-project.org")'

RUN R -q -e "remotes::install_github('FredHutch/fh.wdlR@v2.0.2')"

RUN rm -rf /srv/shiny-server/
ADD app/. /srv/shiny-server/
EXPOSE 3838

