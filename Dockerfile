FROM fredhutch/r-shiny-server-base

RUN apt-get update -y && apt-get install -y libssh-dev

RUN R -q -e 'install.packages(c("shiny", "shinyWidgets", "shinydashboard", "ssh", "paws", "remotes", "markdown"), repos="https://cran.r-project.org")'

RUN R -q -e "remotes::install_github('FredHutch/fh.wdlR')"

RUN rm -rf /srv/shiny-server/
ADD app/. /srv/shiny-server/
EXPOSE 3838

