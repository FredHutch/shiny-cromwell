FROM fredhutch/r-shiny-server-base:4.3.2 AS base

RUN apt-get update -y && apt-get install -y libssh-dev

RUN R -q -e "install.packages('pak', repos = c(CRAN = 'https://cloud.r-project.org'))"
RUN R -q -e "pak::pak('renv')"

WORKDIR /app

RUN mkdir -p renv
COPY renv.lock renv.lock
COPY .Rprofile .Rprofile
COPY renv/activate.R renv/activate.R
COPY renv/settings.json renv/settings.json

RUN mkdir renv/.cache
ENV RENV_PATHS_CACHE renv/.cache

RUN R -q -e "renv::restore()"

FROM base

WORKDIR /app
COPY --from=base /app .

RUN rm -rf /srv/shiny-server/
ADD app/. /srv/shiny-server/
EXPOSE 3838
