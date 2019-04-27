#!/bin/bash

# This file installs the packaged shiny apps.
# Also it will install different package version to 
# different R libraries

# make sure this file has proper execution rights:
#   sudo chmod a+x deploy.sh
#   sudo /bin/bash ./deploy.sh


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
R -e 'withr::with_libpaths(new = "/usr/share/R/rlib", devtools::install_github("markheckmann/gridsampler", branch = "v.05"))'
