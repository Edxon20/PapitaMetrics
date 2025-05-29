FROM rocker/shiny-verse:latest

RUN apt-get update -q && \
    apt-get install -y --no-install-recommends git && \
    rm -rf /var/lib/apt/lists/*


COPY app.R /srv/shiny-server/
