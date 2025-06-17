library(shiny)
library(reticulate)
library(DT)

# Check if Python is available and load required Python modules
use_python("C:\\Users\\TEKOWNER\\AppData\\Local\\Programs\\Python\\Python313\\python.exe")

# Import Python utilities
py_utils <- import_from_path("python_utils", path = ".")
data_utils <- py_utils$data_utils

# UI definition
ui <- fluidPage(
  titlePanel("Massasoit Model Forge"),
  sidebarLayout(
    sidebarPanel(
      radioButtons("dataSource", "Choose data source:",
                   choices = c("Use base file" = "base",
                               "Upload your own file" = "upload"),
                   selected = "base"),
      
      # Conditional panel for base file selection
      conditionalPanel(
        condition = "input.dataSource == 'base'",
        selectInput("baseFile", "Select base file:",
                    choices = list.files("Base_Data_Files", pattern = "\\.xlsx$", full.names = FALSE),
                    selected = NULL)
      ),
      
      # Conditional panel for file upload
      conditionalPanel(
        condition = "input.dataSource == 'upload'",
        fileInput("file1", "Choose Excel File", accept = ".xlsx")
      ),
      
      actionButton("loadData", "Load Data")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Data", DTOutput("dataTable")),
        tabPanel("Summary", verbatimTextOutput("summary")),
        tabPanel("Data Info", verbatimTextOutput("dataInfo"))
      )
    )
  )
)

# Server logic
server <- function(input, output, session) {
  # Reactive value to store the loaded data
  data <- reactiveVal(NULL)
  
  # Update base file dropdown when files change
  observe({
    updateSelectInput(session, "baseFile", 
                      choices = list.files("Base_Data_Files", pattern = "\\.xlsx$", full.names = FALSE))
  })
  
  # Load data when button is clicked
  observeEvent(input$loadData, {
    tryCatch({
      if (input$dataSource == "base" && !is.null(input$baseFile)) {
        # Read from base file
        file_path <- file.path("Base_Data_Files", input$baseFile)
        if (file.exists(file_path)) {
          df <- readxl::read_excel(file_path)
          data(df)
          showNotification(paste("Loaded base file:", input$baseFile), type = "message")
        } else {
          stop("Selected base file not found")
        }
      } else if (input$dataSource == "upload" && !is.null(input$file1)) {
        # Read from uploaded file
        df <- readxl::read_excel(input$file1$datapath)
        data(df)
        showNotification("Uploaded file loaded successfully!", type = "message")
      } else {
        stop("Please select a file or upload one")
      }
    }, error = function(e) {
      showNotification(paste("Error:", e$message), type = "error")
    })
  })
  
  # Display the data table
  output$dataTable <- renderDT({
    req(data())
    datatable(data())
  })
  
  # Display summary statistics using Python
  output$summary <- renderPrint({
    req(data())
    
    # Convert R data to Python pandas DataFrame
    py_run_string("import pandas as pd")
    py$df <- r_to_py(data())
    
    # Get summary using Python function
    summary_result <- data_utils$get_data_summary(py$df)
    
    # Format and display the summary
    cat("Data Summary\n")
    cat("===========\n\n")
    
    cat("Number of rows:", summary_result$num_rows, "\n")
    cat("Number of columns:", summary_result$num_columns, "\n\n")
    
    cat("Column names:\n")
    cat(paste(" - ", summary_result$column_names, collapse = "\n"), "\n\n")
    
    cat("Data types:\n")
    for (col in names(summary_result$data_types)) {
      cat(" - ", col, ": ", summary_result$data_types[[col]], "\n")
    }
    cat("\n")
    
    if (length(summary_result$missing_values) > 0) {
      cat("Missing values:\n")
      for (col in names(summary_result$missing_values)) {
        if (summary_result$missing_values[[col]] > 0) {
          cat(" - ", col, ": ", summary_result$missing_values[[col]], "\n")
        }
      }
    } else {
      cat("No missing values found.\n")
    }
  })
}

# Run the application
shinyApp(ui = ui, server = server)
