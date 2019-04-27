# latest released version of shiny app
# installed in special R lib

library(shiny)
options(shiny.sanitize.errors = FALSE)
package <- "gridsampler"
rlib <- "/usr/share/R/rlib"
  
# add custom library with production version on top of libpath
.libPaths(new = c(rlib, .libPaths()) )
library(gridsampler)

inst <- rownames(installed.packages())
if (package %in% inst) {
  appDir <- system.file("shiny", package = package)
  setwd(appDir)
  shiny::shinyAppDir(".")
  
} else {
  server <- function(input, output){}
    ui <- shiny::fluidPage(
      shiny::titlePanel(paste0("Package '", package, "' was not found."))
  )
  shiny::shinyApp(ui = ui, server = server)
}
 
