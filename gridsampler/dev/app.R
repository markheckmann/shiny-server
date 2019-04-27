package <- "gridsampler"

inst <- rownames(installed.packages())
if (package %in% inst) {
  
  appDir <- system.file("shiny", package = package)
  setwd(appDir)
  shiny::shinyAppDir(".")
  
} else {
  server <- function(input, output){}
    ui <- fluidPage(
      titlePanel(paste0("Package '", package, "' was not found."))
  )
  shiny::shinyApp(ui = ui, server = server)
}
 

