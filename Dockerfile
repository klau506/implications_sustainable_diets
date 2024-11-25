# Start with the base image
FROM rocker/r-ver:4.1.0

ENV R_LIBS_USER="/usr/local/lib/R/site-library"
RUN chmod a+w /usr/local/lib/R/site-library

# Install required system libraries
RUN apt-get update \
  && apt-get install -y \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev \
    libharfbuzz-dev \
    libfribidi-dev \
    git

# install pkgs
RUN apt-get update
RUN R -e "install.packages('remotes')"
RUN R -e "install.packages('markdown')"
RUN R -e "install.packages('readr')"
RUN R -e "install.packages('rlang')"
RUN R -e "install.packages('readr')"
RUN R -e "install.packages('data.table')"
RUN R -e "install.packages('magrittr')"
RUN R -e "install.packages('dplyr')"
RUN R -e "install.packages('tibble')"
RUN R -e "install.packages('tidyr')"
RUN R -e "install.packages('ggplot2')"
RUN R -e "install.packages('stringr')"
RUN R -e "install.packages('here')"
RUN R -e "remotes::install_github('JGCRI/gcamdata')"
RUN R -e "install.packages('tidyverse')"
RUN R -e "install.packages('ggtext')"
RUN R -e "install.packages('ggdist')"
RUN R -e "install.packages('glue')"
RUN R -e "install.packages('patchwork')"
RUN R -e "install.packages('magick')"
RUN R -e "install.packages('viridis')"
RUN R -e "install.packages('ggforce')"
RUN R -e "install.packages('cowplot')"
RUN R -e "install.packages('ggpubr')"

# clone repo
RUN apt-get install -y git
RUN git clone -b diets_v1 --single-branch https://github.com/klau506/implications_sustainable_diets /root/implications_sustainable_diets

# working directory
WORKDIR /app/
COPY . /app

RUN addgroup --system app \
    && adduser --system --ingroup app app

CMD ["R"]
