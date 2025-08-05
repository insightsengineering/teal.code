# Dev Container Setup for teal.code

This directory contains the development container configuration for the teal.code R package.

## What gets installed automatically:

1. **Base Environment**: Ubuntu 24.04 with R 4.3.3 (via rocker/verse image)
2. **System Dependencies**: All required libraries for R package development
3. **R Packages**: 
   - Core development packages: devtools, testthat, roxygen2, pkgdown, remotes, renv, usethis
   - Project-specific packages: dplyr, random.cdisc.data, nestcolor
   - Dependencies for teal.code package

## Persistence:

- **R packages** are installed to `/usr/local/lib/R/site-library` which is mounted to `.devcontainer/r-packages/` on your host
- This means packages will persist between container rebuilds
- The first setup may take a few minutes, but subsequent starts will be faster

## Usage:

1. Open this repository in VS Code
2. When prompted, click "Reopen in Container" or use Command Palette > "Dev Containers: Reopen in Container"
3. Wait for the container to build and setup script to complete
4. Start developing!

## Customization:

- Modify `setup.sh` to add additional R packages or system dependencies
- Update `devcontainer.json` to change VS Code extensions or settings
- The `.Rprofile` is configured to work optimally with this setup

## Troubleshooting:

If you encounter issues:
1. Rebuild the container: Command Palette > "Dev Containers: Rebuild Container"
2. Check the setup script logs in the terminal
3. Ensure Docker has sufficient resources (4GB+ RAM recommended)
