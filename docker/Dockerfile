FROM openanalytics/r-base

MAINTAINER Garrett Dancik 

# system libraries of general use
RUN apt-get update && apt-get install -y \
    sudo \
    pandoc \
    pandoc-citeproc \
    libcurl4-gnutls-dev \
    libcairo2-dev \
    libxt-dev \
    libssl-dev \
    libssh2-1-dev \
    libssl1.0.0 \
    libmysqlclient-dev \
    && apt-get clean \
    && rm -rf /var/lib/apt/lists/ \
    && rm -rf /tmp/downloaded_packages/ /tmp/*.rds

# basic shiny functionality
RUN R -e "install.packages(c('shiny', 'DT', 'DBI', 'RMariaDB', 'ggplot2', 'plotly', 'stringr', 'shinyBS', 'shinydashboard', 'shinyjs', 'shinycssloaders', 'syuzhet', 'rclipboard'), repos='https://cloud.r-project.org/')" \
    && rm -rf /tmp/downloaded_packages/ /tmp/*.rds

# copy the app to the image
RUN mkdir /root/CPP
COPY DCAST /root/CPP

COPY Rprofile.site /usr/lib/R/etc
COPY .my.cnf /root/

EXPOSE 3838

CMD ["R", "-e", "shiny::runApp('/root/CPP')"]

