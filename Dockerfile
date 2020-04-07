FROM rocker/shiny-verse:3.6.1

RUN R -e "install.packages(c('openxlsx', 'dplyr', 'ggplot2', 'plotly', 'shinyWidgets', 'shinydashboard', 'shinythemes', 'scales', 'stringr', 'httr'))"

COPY shiny-server.conf /etc/shiny-server/
COPY . /srv/shiny-server
RUN chown -R shiny:shiny /srv/shiny-server/data
RUN chmod -R 755 /srv/shiny-server/data
