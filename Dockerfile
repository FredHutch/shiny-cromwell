FROM fredhutch/r-shiny-server-base:4.3.2

RUN apt-get update -y && apt-get install -y libssh-dev

RUN R -q -e 'install.packages(c("ellipsis"), repos="https://cran.rstudio.com/")'
RUN R -q -e 'install.packages(c("shiny"), repos="https://cran.rstudio.com/")'
RUN R -q -e 'install.packages(c("shinyFeedback", "shinyWidgets", "shinydashboard", "shinydashboardPlus", "ssh", "remotes", "markdown", "lubridate", "jsonlite", "dplyr", "DT", "glue", "httr", "purrr", "RColorBrewer", "rlang", "shinyBS", "shinyjs", "tidyverse", "uuid", "memoise", "rclipboard", "shinyvalidate", "shinylogs", "testhat"), repos="https://cran.r-project.org")'

RUN R -q -e "remotes::install_github('getwilds/proofr@v0.2')"

RUN R -q -e "remotes::install_github('getwilds/rcromwell@v3.2.1')"

RUN rm -rf /srv/shiny-server/
COPY app/ /srv/shiny-server/
# COPY ./shiny-server.conf /etc/shiny-server/

ARG CI_COMMIT_BRANCH
ARG CI_COMMIT_SHA
ARG CI_COMMIT_SHORT_SHA
ARG CI_COMMIT_TIMESTAMP

ENV CI_COMMIT_BRANCH=$CI_COMMIT_BRANCH
ENV CI_COMMIT_SHA=$CI_COMMIT_SHA
ENV CI_COMMIT_SHORT_SHA=$CI_COMMIT_SHORT_SHA
ENV CI_COMMIT_TIMESTAMP=$CI_COMMIT_TIMESTAMP

RUN echo "export CI_COMMIT_BRANCH=$CI_COMMIT_BRANCH" >> /home/shiny/.bashrc
RUN echo "export CI_COMMIT_SHA=$CI_COMMIT_SHA" >> /home/shiny/.bashrc
RUN echo "export CI_COMMIT_SHORT_SHA=$CI_COMMIT_SHORT_SHA" >> /home/shiny/.bashrc
RUN echo "export CI_COMMIT_TIMESTAMP=$CI_COMMIT_TIMESTAMP" >> /home/shiny/.bashrc

RUN chown shiny:shiny /home/shiny/.bashrc

ENV APPLICATION_LOGS_TO_STDOUT=true
ENV SHINY_LOG_STDERR=1

WORKDIR /srv/shiny-server/
CMD R -f start.R
# CMD /usr/bin/shiny-server


