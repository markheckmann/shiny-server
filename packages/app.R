# List packages installed and accessible for shiny server as table

library(DT)

server <- function(input, output) {
  
  output$table <- renderDataTable({
    input$btn
    inst <- installed.packages()
    df <- as.data.frame(inst)
    rownames(df) <- NULL
    df[, c("Package", "Version", "LibPath")]
  })
  
  
  output$libpath <- renderPrint({
    .libPaths() 
  })
  
}


ui <- fluidPage(
  titlePanel("Installed packages"),
  
  sidebarLayout(
    sidebarPanel(
      actionButton("btn", "Reload")
    ),
    mainPanel(
      verbatimTextOutput("libpath"),
      wellPanel(
        dataTableOutput("table")
      )
    )
  )
)

shiny::shinyApp(ui = ui, server = server)

