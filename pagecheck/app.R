# latest release version of shiny app
# installed in custom R lib

library(shiny)
options(shiny.sanitize.errors = FALSE)
package <- "QuestorPro"
rlib <- "/usr/share/R/rlib"
library(QuestorPro)
# options(questorpro.login = TRUE)

# add custom library with production version on top of libpath
.libPaths(new = c(rlib, .libPaths()) )

inst <- rownames(installed.packages())
if (package %in% inst) {
  appDir <- system.file("pagecheck", package = package)
  setwd(appDir)
  shiny::shinyAppDir(".")
} else {
  server <- function(input, output){}
    ui <- shiny::fluidPage(
      shiny::titlePanel(paste0("Package '", package, "' was not found."))
  )
  shiny::shinyApp(ui = ui, server = server)
}
 
