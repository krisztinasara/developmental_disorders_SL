# Use the official rocker image for R (change version as needed)
FROM rocker/r-ver:4.3.3

# Install required R packages (add any you want here)
RUN R -e "install.packages(c('tidyverse', 'devtools', 'languageserver'), repos='http://cran.rstudio.com/')"

# Install system dependencies (optional, add as needed)
RUN apt-get update && \
    apt-get install -y \
    git \
    curl \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev

# Set default shell
SHELL ["/bin/bash", "-c"]

# Set the default working directory
WORKDIR /workspaces/${{REPO_NAME}}