library(shiny)
library(reticulate)
library(DT)
library(readr)
library(readxl)

# Function to install Python dependencies from requirements.txt
install_python_deps <- function() {
  if (!file.exists("requirements.txt")) {
    message("requirements.txt not found. Creating with default dependencies...")
    default_reqs <- c(
      "pandas>=1.3.0",
      "numpy>=1.21.0",
      "openpyxl>=3.0.7",
      "matplotlib>=3.4.0",
      "scikit-learn>=1.0.0",
      "scipy>=1.8.0",
      "python-dateutil>=2.8.2",
      "pytz>=2020.1"
    )
    writeLines(default_reqs, "requirements.txt")
  }
  
  message("Installing/updating Python dependencies from requirements.txt...")
  reqs <- readLines("requirements.txt")
  reqs <- reqs[!grepl("^\\s*#", reqs)]  # Remove comments
  reqs <- trimws(reqs[reqs != ""])  # Remove empty lines and trim whitespace
  
  for (pkg in reqs) {
    tryCatch({
      message("Installing ", pkg, "...")
      py_install(pkg, pip = TRUE)
    }, error = function(e) {
      warning("Failed to install ", pkg, ": ", conditionMessage(e))
    })
  }
}

# Install Python dependencies
suppressWarnings({
  tryCatch({
    install_python_deps()
  }, error = function(e) {
    message("Warning: Could not install Python dependencies: ", conditionMessage(e))
    message("Please install them manually using: pip install -r requirements.txt")
  })
})

# Check if Python is available and load required Python modules
tryCatch({
  if (!py_available(initialize = TRUE)) {
    stop("Python is not available. Please install Python and ensure it's in your PATH.")
  }
  
  # Try to import Python utilities
  if (!file.exists("python_utils")) {
    stop("python_utils directory not found. Please ensure it exists in the app directory.")
  }
  py_utils <- import_from_path("python_utils", path = ".")
  data_utils <- py_utils$data_utils
}, error = function(e) {
  stop("Error initializing Python: ", conditionMessage(e))
})

# UI definition with custom CSS
ui <- tagList(
  tags$head(
    tags$style(HTML("
      .well {
        background-color: #f9f9f9;
        border: 1px solid #ddd;
        border-radius: 4px;
        padding: 10px;
        margin-bottom: 20px;
      }
      .btn-primary {
        background-color: #337ab7;
        border-color: #2e6da4;
      }
      .btn-primary:hover {
        background-color: #286090;
        border-color: #204d74;
      }
      .file-input-label {
        font-weight: bold;
        margin-bottom: 10px;
        display: block;
      }
      .file-input-info {
        margin-top: 5px;
        font-size: 0.9em;
        color: #666;
      }
      .shiny-input-container {
        margin-bottom: 10px;
      }
    "))
  ),

  fluidPage(
    titlePanel("Massasoit Model Forge"),
    sidebarLayout(
      sidebarPanel(
        radioButtons("dataSource", "Choose data source:",
                     choices = c("Use base file" = "base",
                                 "Upload your own file" = "upload",
                                 "Online Databases" = "api"),
                     selected = "base"),

        # Conditional panel for base file selection
        conditionalPanel(
          condition = "input.dataSource == 'base'",
          selectInput(
            "baseFile",
            "Select base file:",
            choices = list.files(
              "Base_Data_Files",
              pattern = "\\.(xlsx|csv)$",
              full.names = FALSE),
            selected = NULL)
        ),

        # Conditional panel for file upload
        conditionalPanel(
          condition = "input.dataSource == 'upload'",
          fileInput("file1", 
                   label = span("Choose File(s)", 
                              class = "file-input-label"),
                   multiple = TRUE,
                   accept = c(".xlsx", ".xls", ".csv"),
                   buttonLabel = "Browse..."),
          div("Select one or more Excel (.xlsx, .xls) or CSV (.csv) files", 
              class = "file-input-info")
        ),
        
        # Conditional panel for online databases
        conditionalPanel(
          condition = "input.dataSource == 'api'",
          selectInput("apiSource", "Select Data Source:",
                      choices = c("Traffic", "Visual Crossing")),
          # Placeholder for API-specific parameters
          uiOutput("apiParams")
        ),

        actionButton("loadData", "Load Data"),

        # Analysis type selection
        selectInput(
          "analysisType",
          "Select Analysis Type:",
          choices = list(
            "-- Select Analysis Type --" = "",
            "Parametric/Semi-parametric" = list(
              "GAM(M)s" = "gamm",
              "GLM(M)s" = "glmm",
              "Logistic Regression" = "logistic",
              "ANOVA" = "anova",
              "Linear Regression" = "linear",
              "Generalized Estimating Equations" = "gee",
              "Negative Binomial Regression" = "negbin"
            ),
            "Non-parametric" = list(
              "GWR (Geographically Weighted Regression)" = "gwr",
              "Goodness of Fit, Chi-squared test" = "chisq",
              "Mann-Whitney U test" = "mannwhitney",
              "Kruskal-Wallis test" = "kruskal",
              "Zero Inflated Model" = "zeroinfl",
              "Hurdle Model" = "hurdle",
              "Sign test" = "signtest",
              "Wilcoxon Signed-Rank test" = "wilcoxon",
              "Spearman's Rank Correlation" = "spearman",
              "Permutation signed rank test" = "permtest"
            )
          ),
          selected = ""
        ),

        # Dynamic UI for analysis parameters
        uiOutput("analysisParams"),

        # Action button to run the selected analysis
        actionButton("runAnalysis", "Run Analysis", class = "btn-primary")
      ),
      mainPanel(
        tabsetPanel(
          id = "mainTabs",
          tabPanel("Data", DTOutput("dataTable")),
          tabPanel("Summary", verbatimTextOutput("summary")),
          tabPanel("Analysis Results", 
                   verbatimTextOutput("analysisResults"),
                   plotOutput("analysisPlot"))
        )
      )
    )
  )
)

# Server logic
server <- function(input, output, session) {
  # Reactive values
  analysis_results <- reactiveValues(
    result = NULL,
    plot = NULL
  )
  
  # Store merged data from multiple files
  merged_data <- reactiveVal(NULL)
  
  # API parameters UI
  output$apiParams <- renderUI({
    req(input$apiSource)
    
    if (input$apiSource == "Traffic") {
      tagList(
        textInput("trafficLocation", "Location:", placeholder = "e.g., Boston, MA"),
        dateRangeInput("trafficDates", "Date Range:", 
                      start = Sys.Date() - 30, 
                      end = Sys.Date())
      )
    } else if (input$apiSource == "Visual Crossing") {
      tagList(
        textInput("vcLocation", "Location:", placeholder = "e.g., Boston, MA"),
        dateRangeInput("vcDates", "Date Range:", 
                      start = Sys.Date() - 30, 
                      end = Sys.Date()),
        selectInput("vcUnitGroup", "Unit System:", 
                    choices = c("Metric" = "metric", "US" = "us"))
      )
    }
  })
  
  # Dynamic UI for analysis parameters
  output$analysisParams <- renderUI({
    req(input$analysisType)
    
    if (input$analysisType == "") return(NULL)
    
    tagList(
      # Common parameters for most analyses
      selectInput("responseVar", "Response Variable:", 
                  choices = names(data())),
      
      # Conditional parameters based on analysis type
      if (input$analysisType %in% c("linear", "logistic", "glmm", "gamm", "negbin")) {
        selectInput("predictorVars", "Predictor Variables:", 
                    choices = names(data()),
                    multiple = TRUE)
      },
      
      if (input$analysisType %in% c("anova", "kruskal")) {
        selectInput("groupVar", "Grouping Variable:", 
                    choices = names(data()))
      },
      
      if (input$analysisType %in% c("chisq")) {
        tagList(
          selectInput("observedVar", "Observed Variable:", 
                      choices = names(data())),
          numericInput("expectedProbs", "Expected Probabilities (comma-separated):", 
                      value = "", 
                      placeholder = "Leave empty for uniform distribution")
        )
      } else if (input$analysisType %in% c("spearman", "pearson")) {
        tagList(
          selectInput("var1", "Variable 1:", choices = names(data())),
          selectInput("var2", "Variable 2:", choices = names(data()))
        )
      }
    )
  })
  
  # Run analysis when the run button is clicked
  observeEvent(input$runAnalysis, {
    req(data(), input$analysisType)
    
    tryCatch({
      # This is where we'll implement the actual analysis functions
      # For now, we'll just show a message
      showNotification(paste("Running", input$analysisType, "analysis..."), 
                      type = "message")
      
      # Store a simple result for demonstration
      analysis_results$result <- paste("Results for", input$analysisType, "analysis will appear here.")
      
    }, error = function(e) {
      showNotification(paste("Error in analysis:", e$message), 
                      type = "error")
    })
  })
  
  # Display analysis results
  output$analysisResults <- renderPrint({
    req(analysis_results$result)
    cat(analysis_results$result)
  })
  
  # Display analysis plot
  output$analysisPlot <- renderPlot({
    # Placeholder for analysis plots
    if (!is.null(analysis_results$plot)) {
      analysis_results$plot
    } else {
      plot(1, 1, type = "n", xlab = "", ylab = "", axes = FALSE)
      text(1, 1, "Plot will appear here", cex = 1.5)
    }
  })
  
  # Reactive value to store the loaded data
  data <- reactiveVal(NULL)
  
  # Helper function to read different file types
  read_data_file <- function(file_path, file_name) {
    if (grepl("\\.xlsx?$", file_name, ignore.case = TRUE)) {
      df <- readxl::read_excel(file_path)
    } else if (grepl("\\.csv$", file_name, ignore.case = TRUE)) {
      df <- readr::read_csv(file_path, show_col_types = FALSE)
    } else {
      stop("Unsupported file format")
    }
    return(df)
  }
  
  # Load data when button is clicked
  observeEvent(input$loadData, {
    tryCatch({
      if (input$dataSource == "base" && !is.null(input$baseFile)) {
        # Load single base file
        file_path <- file.path("Base_Data_Files", input$baseFile)
        df <- read_data_file(file_path, input$baseFile)
        data(df)
        merged_data(NULL)  # Reset merged data
        showNotification(paste("Loaded base file:", input$baseFile), 
                        type = "message")
        
      } else if (input$dataSource == "upload" && !is.null(input$file1)) {
        # Handle multiple file uploads
        files <- input$file1
        dfs <- list()
        
        # Read all files
        for (i in seq_len(nrow(files))) {
          df <- read_data_file(files$datapath[i], files$name[i])
          dfs[[files$name[i]]] <- df
        }
        
        # If only one file, use it directly
        if (length(dfs) == 1) {
          data(dfs[[1]])
          merged_data(NULL)
          showNotification("File loaded successfully!", type = "message")
          return()
        }
        
        # For multiple files, check row counts
        row_counts <- sapply(dfs, nrow)
        if (length(unique(row_counts)) > 1) {
          showModal(modalDialog(
            title = "Incompatible Data",
            "The selected files have different numbers of rows and cannot be merged.",
            easyClose = TRUE,
            footer = modalButton("OK")
          ))
          return()
        }
        
        # Merge data frames by columns
        merged_df <- do.call(cbind, dfs)
        data(merged_df)
        merged_data(merged_df)
        showNotification(
          paste("Successfully merged", length(dfs), "files with", 
                nrow(merged_df), "rows and", ncol(merged_df), "columns"),
          type = "message"
        )
        
      } else if (input$dataSource == "api") {
        # Placeholder for API data loading
        showNotification("API integration will be implemented here", 
                        type = "message")
        return(NULL)
      }
    }, error = function(e) {
      showNotification(paste("Error loading data:", e$message), 
                      type = "error")
    })
  })
  
  # Data table output
  output$dataTable <- renderDT({
    df <- data()
    req(df)
    datatable(df, 
              options = list(scrollX = TRUE, 
                           pageLength = 10,
                           lengthMenu = c(5, 10, 15, 20)))
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
