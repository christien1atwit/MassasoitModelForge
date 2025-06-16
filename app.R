library(shiny)
library(reticulate)
library(DT)

# Check if Python is available and load required Python modules
use_python("C:\\Users\\TEKOWNER\\AppData\\Local\\Programs\\Python\\Python312\\python.exe")

# UI definition
ui <- fluidPage(
  titlePanel("Massasoit Model Forge"),
  sidebarLayout(
    sidebarPanel(
      fileInput("file1", "Choose Excel File", accept = ".xlsx"),
      actionButton("loadData", "Load Data")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Data", DTOutput("dataTable")),
        tabPanel("Summary", verbatimTextOutput("summary"))
      )
    )
  )
)

# Server logic
server <- function(input, output, session) {
  # Reactive value to store the loaded data
  data <- reactiveVal(NULL)
  
  # Load data when button is clicked
  observeEvent(input$loadData, {
    req(input$file1)
    
    tryCatch({
      # Read the Excel file
      df <- readxl::read_excel(input$file1$datapath)
      data(df)
      
      # Show success message
      showNotification("Data loaded successfully!", type = "message")
    }, error = function(e) {
      showNotification(paste("Error:", e$message), type = "error")
    })
  })
  
  # Display the data table
  output$dataTable <- renderDT({
    req(data())
    datatable(data())
  })
  
  # Display summary statistics
  output$summary <- renderPrint({
    req(data())
    summary(data())
  })
}

# Run the application
shinyApp(ui = ui, server = server)
