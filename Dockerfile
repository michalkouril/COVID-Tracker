FROM rocker/shiny-verse:3.6.1

RUN R -e "install.packages(c('openxlsx', 'dplyr', 'ggplot2', 'plotly', 'shinyWidgets', 'shinydashboard', 'shinythemes', 'scales', 'stringr'))"

COPY shiny-server.conf /etc/shiny-server/
COPY . /srv/shiny-server

