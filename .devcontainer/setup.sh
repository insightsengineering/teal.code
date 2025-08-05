#!/bin/bash

# R Package Installation Script for teal.code development
set -e

echo "Setting up R environment for teal.code development..."

# Update package list
sudo apt-get update

# Install additional system dependencies if needed
sudo apt-get install -y \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev \
    libgit2-dev \
    libharfbuzz-dev \
    libfribidi-dev \
    libfreetype6-dev \
    libpng-dev \
    libtiff5-dev \
    libjpeg-dev

# Install essential R packages
echo "Installing R packages..."
R -e "
# Set CRAN mirror
options(repos = c(CRAN = 'https://cran.rstudio.com/'))

# Install essential packages for development
install.packages(c(
  'devtools',
  'testthat', 
  'roxygen2',
  'pkgdown',
  'remotes',
  'renv',
  'usethis',
  'dplyr',
  'random.cdisc.data',
  'nestcolor'
), dependencies = TRUE)

# Try to install teal.data if available
tryCatch({
  install.packages('teal.data')
}, error = function(e) {
  message('teal.data package not available from CRAN, will need to install from GitHub')
})
"

# Install the current package in development mode
echo "Installing teal.code package dependencies..."
R -e "
if (file.exists('DESCRIPTION')) {
  devtools::install_deps(dependencies = TRUE)
}
"

echo "R environment setup completed!"
