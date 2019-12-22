#!/bin/bash

# This file installs the packaged shiny apps.
# Also it will install different package version to 
# different R libraries to host different versions of 
# each shiny app if needed

# make sure this file has proper execution rights:
#   sudo -i  # run as root on ubuntu
#   /bin/bash ./deploy.sh

# Also make sure that GITHUB_PAT environment var is set
# (e.g. in .Renviron) to privat repos. github_install is smart
# as it will use this var to access private repos.


#-------------------------------------------------
#           install shiny app packages
#-------------------------------------------------

# latest versions go to global R lib.  older tagged releases go ti custom libs.
# by this we can host older versions of the shiny app
# at the same time by specifying the lib when starting the app

# R library for earlier versions:
mkdir -p /usr/share/R/rlib  # the production code library. no dev versions

### gridsampler ###
R -e 'devtools::install_github("markheckmann/gridsampler")'  # latest version
R -e 'withr::with_libpaths(new = "/usr/share/R/rlib", devtools::install_github("markheckmann/gridsampler", ref = "v0.6"))'

### OpenRepGrid.app ###
R -e 'devtools::install_github("markheckmann/OpenRepGrid.app")'  # latest version
R -e 'withr::with_libpaths(new = "/usr/share/R/rlib", devtools::install_github("markheckmann/OpenRepGrid.app", ref = "v0.2"))'

### OpenRepGrid.app ###
R -e 'devtools::install_github("markheckmann/OpenRepGrid.ic")'  # latest version
R -e 'withr::with_libpaths(new = "/usr/share/R/rlib", devtools::install_github("markheckmann/OpenRepGrid.ic"))'

### QuestorPro (private repo) ###
R -e 'devtools::install_github("markheckmann/questorpro")'  # latest version
R -e 'withr::with_libpaths(new = "/usr/share/R/rlib", devtools::install_github("markheckmann/questorpro"))'

