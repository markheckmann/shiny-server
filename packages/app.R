# List packages installed and accessible for shiny server as table

server <- function(input, output) {
  
  output$table <- renderDataTable({
    input$btn
    inst <- installed.packages()
    data.frame(package = inst[  , c(1,3)])
  })
}

ui <- fluidPage(
  titlePanel("Installed packages"),
  
  sidebarLayout(
    sidebarPanel(
      actionButton("btn", "Reload")
    ),
    mainPanel(
      wellPanel(
        dataTableOutput("table")
      )
    )
  )
)

shiny::shinyApp(ui = ui, server = server)
