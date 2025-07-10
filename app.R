library(shiny)
library(shinyjs) # For JavaScript operations in Shiny
library(reticulate)
library(DT)
library(readxl) # For reading Excel files
library(lme4)   # For GLMMs
library(mgcv)   # For GAMs/GAMMs
library(MASS)   # For Negative Binomial Regression (glm.nb)
library(pscl)   # For Zero-Inflated and Hurdle models
library(geepack) # For GEE
library(spgwr) # For GWR
library(readr)
library(readxl)

# Rscript -e "shiny::runApp('app.R', host = '0.0.0.0', port = 8000, launch.browser = TRUE)"

# Initialize shinyjs
shinyjs::useShinyjs()

conda_env_name <- "MassasoitModelForge_env"

# ------------------ App Miniconda setup ------------------- #

message(
  "Miniconda is assumed to be installed and ready at path:",
  reticulate::miniconda_path()
)

message(
        paste(
              "Attempting to create/check Conda environment with name:",
              conda_env_name))

# 2. Check if the Conda environment exists, create it if not.
if (!(conda_env_name %in% reticulate::conda_list()$name)) {
  message(paste("Creating Conda environment:", conda_env_name, "..."))
  reticulate::conda_create(
                           envname = conda_env_name,
                           packages = c(paste0("python=", "3.10")))
  message(paste("Conda environment", conda_env_name, "created."))
} else {
  message(paste("Conda environment", conda_env_name, "already exists."))
}

reticulate::use_condaenv(conda_env_name, required = TRUE)

# Install Python dependencies
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
}
suppressWarnings({
  tryCatch({
    install_python_deps()
  }, error = function(e) {
    message(
            "Warning: Could not install Python dependencies: ",
            conditionMessage(e))
    message(
            "Please install manually using: pip install -r requirements.txt")
  })
})

# Check if Python is available and load required Python modules
tryCatch({
  if (!py_available(initialize = TRUE)) {
    stop(
         "Python is not available. Please install \
          Python and ensure it's in your PATH.")
  }

  if (!file.exists("python_utils")) {
    stop("python_utils directory not found. Please \
    ensure it exists in the app directory.")
  }
  py_utils <- import_from_path("python_utils", path = ".")
  data_utils <- py_utils$data_utils
}, error = function(e) {
  stop("Error initializing Python: ", conditionMessage(e))
})

# UI definition with custom CSS
ui <- fluidPage(
  useShinyjs(),
  tags$head(
    # Include the external CSS file
    tags$link(rel = "stylesheet", type = "text/css", href = "app_design.css"),
    # Add favicon
    tags$link(rel = "icon", type = "image/png", href = "favicon.ico")
  ),

  # Landing Page
  div(
    id = "landingPage",
    class = "landing-page",
    div(
      class = "landing-content",
      h1("Welcome to the Massasoit Model Forge", class = "landing-title"),
      div(class = "button-container",
        actionButton("enterAppBtn", "Enter Application", class = "app-btn"),
        actionButton("aboutBtn", "About Us", class = "app-btn")
      )
    )
  ),

  # About Page
  div(
    id = "aboutPage",
    class = "page",
    style = "display: none;
             position: fixed;
             top: 0;
             left: 0;
             width: 100%;
             height: 100%;
             background: url('Ian_Background_Image.jpg') 
                 no-repeat center center fixed;
             background-size: cover;
             overflow-y: auto;",
    # Dark overlay
    div(
      style = "position: fixed;
      top: 0; left: 0;
      right: 0;
      bottom: 0;
      background: rgba(0, 0, 0, 0.5);
      z-index: 1;"
    ),
    # Content wrapper
    div(style = "position: relative; z-index: 2; min-height: 100%;",
      # Header with title and logo
      div(class = "app-header",
        div(class = "header-left",
          actionLink(
            "appTitleLink",
            "Massasoit Model Forge",
            class = "app-title-link"
          )
        ),
        div(class = "header-right",
          a(
            href = "https://massasoit.edu/",
            target = "_blank",
            img(
              src = "STEMlogowithBackground.png",
              class = "stem-logo",
              alt = "Massasoit STEM"
            )
          )
        )
      ),
      # Main content
      div(
        class = "about-container",
        style = "max-width: 800px;
        margin: 30px auto;
        padding: 40px;
        background: rgba(255, 255, 255, 0.95);
        border-radius: 10px;
        box-shadow: 0 8px 25px rgba(0, 0, 0, 0.2);
        position: relative;
        z-index: 2;",
        # About Us Section
        h2("Who We Are"),
        p("  \t     We’re Sammy Olsen and Ian Handy, data scientists who got \
        our start at Massasoit Community College. This app began as a \
        tool for a very specific purpose: to help make sense of over a \
        decade’s worth of wild bee research in preparation for the \
        national Ecological Society of America conference in 2025."),
        p("   \t    As interns in the Massasoit STEM Research Program, \
        we worked with field data that was messy, complex, and deeply \
        important. We wanted to build a tool that not only helped us run \
        our own statistical models, but also made advanced data science \
        techniques accessible to researchers like us— community college \
        students, interns, field biologists, and anyone working with data \
        outside of a traditional research institution. We saw how messy and \
        overwhelming data could be, \
        especially when you’re just getting started. \
        Our goal was to make something that not \
        only handles the complexity, but \
        actually helps people ", em("understand"), " it."),
        p("  \t    We believe in open science, \
        transparency, and user developed \
        software. Massasoit Model Forge reflects that belief. Over time, \
        that idea grew into the tool you're using now. It’s built by students, \
        for students, but designed to be powerful enough for anyone."),

        # Why We Built This Section
        h3("Why We Built This"),
        p("As community college students, we found that the tools for advanced \
        statistical analysis were either too expensive, opaque, or complex for \
        many in education and research. We \
        built this app to show that real science \
        can happen anywhere, when you give people the tools to do it."),

        h3(" "),
        p("Our mission is twofold:"),
        tags$ul(
          tags$li("To create transparent, open-source tools that bring \
          the power of modern statistical modeling to everyone, across \
          disciplines."),
          tags$li("To legitimize and elevate the research of community college \
          students, whose work is often undervalued and overlooked, despite \
          its scientific rigor.")
        ),
        p("Massasoit Model Forge is a reflection of the values we hold dear: \
        accessibility, reproducibility, and \
        scientific curiosity. We built this \
        for our peers, our mentors, and anyone doing research without a huge \
        lab budget or institutional access. We’re proud of where we came from, \
        and excited about where this project can go."),

        # What It Does Section
        h3("What It Does"),
        p("Massasoit Model Forge is an open-source statistical modeling \
          application built with R Shiny and hosted through Posit Connect. \
          It integrates both R and Python \
          (via the ",
          span(
            "reticulate",
            style = "font-family: 'Courier New', monospace;"
          ),
          " package) to \
          give users access to a broad set of tools for analyzing datasets— \
          without needing advanced programming skills or thousand dollar \
          software."),
        p("The app was originally built to support our lab’s ongoing \
          research on wild bee populations, where we needed a flexible tool \
          that could accommodate non-parametric, real-world ecological data. \
          It has since evolved into a general purpose modeling environment \
          that allows users to:"),
        tags$ul(
          tags$li("Upload and examine structured data files. (CSV, Excel)"),
          tags$li("Run both parametric (e.g., GLMs, linear regression, ANOVA) \
          and non-parametric (e.g., \
          mixed models, chi-squared tests) analyses."),
          tags$li("Explore and export model diagnostics, summaries, and data \
          visualizations without writing code.")
        ),
        h4("All through a guided interface with built in interpretability \
        and error-checking!"),
        p("We’ll continue to expand the app’s capabilities, documentation, \
        and educational use cases. If you're working with data in an \
        under-resourced setting, this tool was built with you in mind. \
        We’re still learning. We’re still \
        building. And we’re glad you’re here."),

        # Contact Information
        h3("Contact Information"),
        p("How to reach the team."),
        tags$ul(
          tags$li("Github: spamolsen"),
          tags$li("Github: vyndyctyv")
        )
      )
    )
  ),

  # Main Application Content
  div(
    id = "mainApp",
    class = "page",
    # Header with title and logo
    div(class = "app-header",
      div(class = "header-left",
        actionLink(
          "appTitleLink",
          "Massasoit Model Forge",
          class = "app-title-link"
        )
      ),
      div(class = "header-right",
        a(href = "https://massasoit.edu/", target = "_blank",
          img(
            src = "STEMlogowithBackground.png",
            class = "stem-logo",
            alt = "Massasoit STEM"
          )
        )
      )
    ),
    sidebarLayout(
      sidebarPanel(
        tabsetPanel(
          id = "sidebarTabs",
          tabPanel(
            "Data",
            radioButtons(
                         "dataSource",
                         "Choose data source:",
                         choices = c(
                                     "Use base file" = "base",
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
            # Add JavaScript to switch to Analyze tab when Load Data is clicked
            tags$script(HTML("
              $(document).on('shiny:inputchanged', function(event) {
              if (event.name === 'loadData' && event.value > 0) {
                setTimeout(function() {
                $('a[data-value=\"Analyze\"]').tab('show');
                }, 300);
              }
              });
            "))
          ),
          tabPanel(
            "Analyze",
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
                  "GWR (Geographically \
                  Weighted Regression)" = "gwr", # Requires spatial data
                  "Goodness of Fit, Chi-squared test" = "chisq",
                  "Mann-Whitney U test" = "mannwhitney",
                  "Kruskal-Wallis test" = "kruskal",
                  "Zero Inflated Model" = "zeroinfl",
                  "Hurdle Model" = "hurdle",
                  "Sign test" = "signtest",
                  "Wilcoxon Signed-Rank test" = "wilcoxon",
                  "Spearman's Rank Correlation" = "spearman",
                  "Permutation signed \
                  rank test" = "permtest" # Placeholder!!!!
                )
              ),
              selected = ""
            ),

            # Dynamic UI for analysis parameters
            uiOutput("analysisParams"),

            # Action button to run the selected analysis
            actionButton("runAnalysis", "Run Analysis", class = "btn-primary")
          )
        )
      ),
      mainPanel(
        tabsetPanel(
          id = "mainTabs",
          tabPanel("Data", DTOutput("dataTable")),
          tabPanel("Summary", verbatimTextOutput("summary")),
          tabPanel("Analysis Results",
                   plotOutput("analysisPlot"),
                   verbatimTextOutput("analysisResults"))

        )
      )
    )
  )
)

# Server logic
server <- function(input, output, session) {
  # Initialize app - show only landing page initially
  shinyjs::runjs("$('#landingPage').addClass('page').show();")
  shinyjs::runjs("$('#mainApp, #aboutPage').addClass('page').hide();")

  # Navigation button handlers
  observeEvent(input$aboutBtn, {
    navigateToPage("about")
  })

  observeEvent(input$enterAppBtn, {
    navigateToPage("app")
  })

  observeEvent(input$appTitleLink, {
    if (appState$currentPage != "landing") {
      navigateToPage("landing")
    }
  })

  # Initialize app state
  appState <- reactiveValues(
    currentPage = "landing" # Can be "landing", "app", or "about"
  )

  # Navigation functions
  navigateToPage <- function(page) {
    # Hide all pages
    shinyjs::runjs("$('.page').hide();")

    # Show the selected page
    if (page == "app") {
      shinyjs::runjs("$('#mainApp').show();")
      appState$currentPage <- "app"
    } else if (page == "about") {
      shinyjs::runjs("$('#aboutPage').show();")
      appState$currentPage <- "about"
    } else {
      shinyjs::runjs("$('#landingPage').show();")
      appState$currentPage <- "landing"
    }

    # Force a redraw to ensure the background image loads
    shinyjs::runjs(
      "setTimeout(function() { 
        $(window).trigger('resize'); 
      }, 50);
      "
    )
  }

  # Navigation observers
  observeEvent(input$enterAppBtn, {
    navigateToPage("app")
  })

  observeEvent(input$aboutBtn, {
    navigateToPage("about")
  })

  # Handle title click to navigate back
  # to the landing page
  observeEvent(input$appTitleLink, {
    navigateToPage("landing")
  }, ignoreInit = TRUE)

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

  # Reactive value to store the loaded data
  data <- reactiveVal(NULL)

  # Helper function to read different file types with robust type handling
  read_data_file <- function(file_path, file_name) {
    # Function to safely convert columns to appropriate types
    clean_and_convert <- function(df) {
      # Convert all columns to character first to avoid type coercion warnings
      df[] <- lapply(df, as.character)

      # Function to guess and convert column types
      convert_column <- function(x) {
        # Remove any non-numeric characters from potential numeric columns
        clean_x <- gsub("[^0-9.-]", "", x)

        # Try to convert to numeric if possible
        num_x <- suppressWarnings(as.numeric(clean_x))
        if (!all(is.na(num_x)) && !all(is.na(x) | x == "")) {
          return(num_x)
        }

        # Check for logical values
        if (all(tolower(x) %in% c("true", "false", "t", "f", "", NA))) {
          return(as.logical(x))
        }

        # Check for dates (simple check)
        if (any(grepl("\\d{1,4}[-/]\\d{1,2}[-/]\\d{1,4}", x, ignore.case = TRUE))) {
          date_x <- as.Date(x, optional = TRUE)
          if (!all(is.na(date_x))) {
            return(date_x)
          }
        }

        # Return as character if no other type fits
        return(x)
      }

      # Apply conversion to each column
      df[] <- lapply(df, function(col) {
        # Skip if column is already in a good format
        if (is.numeric(col) || is.logical(col) || inherits(col, "Date")) {
          return(col)
        }
        convert_column(col)
      })

      return(df)
    }

    # Read the file with appropriate function
    df <- tryCatch({
      if (grepl("\\.xlsx?$", file_name, ignore.case = TRUE)) {
        # For Excel files, read all as text first to avoid type guessing issues
        suppressWarnings({
          df <- readxl::read_excel(file_path, col_types = "text")
        })
      } else if (grepl("\\.csv$", file_name, ignore.case = TRUE)) {
        # For CSV files, read all as character first
        df <- readr::read_csv(file_path, col_types = cols(.default = col_character()), 
                             show_col_types = FALSE)
      } else {
        stop("Unsupported file format. Please use .xlsx, .xls, or .csv files.")
      }

      # Clean and convert column types
      df <- clean_and_convert(df)

      # Process with Python utilities if available
      if (exists("data_utils")) {
        py_df <- r_to_py(df)
        df <- data_utils$append_coord(py_df)
        py_df <- r_to_py(df)
        df <- data_utils$clean_column_names(py_df)
      }

      df
    },
    error = function(e) {
      showNotification(paste("Error reading file:", e$message), type = "error")
      return(NULL)
    })
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

  output$summary <- renderPrint({
    req(data())
    df <- data()
    cat("Data Summary\n")
    cat("===========\n\n")
      cat("Number of rows:", nrow(df), "\n")
      cat("Number of columns:", ncol(df), "\n\n")
      cat("Column names:\n")
      cat(paste(" -", names(df)), sep = "\n")
      cat("\n\n")
      cat("Data types:\n")
      for (col in names(df)) {
        cat(" -", col, ":", class(df[[col]])[1], "\n")
      }
      cat("\n")
      missing_vals <- sapply(df, function(x) sum(is.na(x)))
      if (any(missing_vals > 0)) {
        cat("Missing values:\n")
        for (col in names(missing_vals)) {
          if (missing_vals[[col]] > 0) {
        cat(" -", col, ":", missing_vals[[col]], "\n")
          }
        }
      } else {
        cat("No missing values found.\n")
      }
      cat("\n")
      cat("First few rows of data:\n")
      print(utils::head(df, 5))
})

  output$analysisParams <- renderUI({
    req(input$analysisType)
    req(data()) 

    if (input$analysisType == "") return(NULL)

    # Get data and calculate non-NA counts
    df <- data()
    non_na_counts <- sapply(df, function(x) sum(!is.na(x)))
    total_rows <- nrow(df)
    
    # This is my attempt at right-aligning da N values
    css_rules <- lapply(seq_along(non_na_counts), function(i) {
      glue::glue(
        ".selectize-dropdown-content .option[data-value='{names(non_na_counts)[i]}']::after {{
          content: 'N = {non_na_counts[i]} / {total_rows}';
          float: right;
          color: #777;
          margin-left: 10px;
        }}
        .selectize-dropdown-content .option[data-value='{names(non_na_counts)[i]}']:hover::after {{
          color: #000;
        }}
        .selectize-dropdown-content .option[data-value='{names(non_na_counts)[i]}'].active::after {{
          color: #000;
        }}
        .selectize-dropdown-content .option[data-value='{names(non_na_counts)[i]}'].selected::after {{
          color: #fff;
        }}"
      )
    }) %>% paste(collapse = "\n")

    # Format variable names (without N values in the text, they'll be added via CSS)
    format_vars <- function(vars) {
      setNames(vars, vars)
    }

    all_data_cols <- format_vars(names(df))
    num_data_cols <- format_vars(names(df)[sapply(df, is.numeric)])
    char_data_cols <- format_vars(names(df)[sapply(df, is.character)])

    # Include the CSS in the UI
    tagList(
      tags$head(tags$style(HTML(css_rules))),
      # Common parameters for most analyses
      if (input$analysisType %in% c("linear", "logistic", "glmm", "gamm", "negbin", "anova", "kruskal",
                                    "gee", "zeroinfl", "hurdle", "wilcoxon", "signtest", "mannwhitney")) {
        selectizeInput("responseVar", "Response Variable:",
                     choices = num_data_cols,
                     options = list(render = I(
                       '{
                         item: function(item, escape) { 
                           return "<div>" + escape(item.label) + "</div>"; 
                         }
                       }'
                     )))
      },

      if (input$analysisType %in% c("linear", "logistic", "glmm", "gamm", "negbin", "gee", "zeroinfl", "hurdle")) {
        selectizeInput("predictorVars", "Predictor Variables:",
                     choices = num_data_cols,
                     multiple = TRUE,
                     options = list(
                       render = I('{
                         item: function(item, escape) { 
                           return "<div>" + escape(item.label) + "</div>"; 
                         }
                       }')
                     ))
      },

      if (input$analysisType %in% c("glmm", "gamm")) {
        selectizeInput("randomEffect", "Random Effects (e.g., (1|group) or (predictor|group)):",
                       choices = char_data_cols,
                       multiple = TRUE,
                       options = list(create = TRUE, placeholder = "Type or select for random effects",
                       render = I('{
                         item: function(item, escape) { 
                           return "<div style=\"text-align:right\">" + escape(item.label) + "</div>"; 
                         }
                       }')
                     ))
      },

      if (input$analysisType %in% c("anova", "kruskal", "mannwhitney", "wilcoxon", "signtest")) {
        selectizeInput("groupVar", "Grouping Variable:",
                     choices = char_data_cols,
                     options = list(render = I(
                       '{
                         item: function(item, escape) { 
                           return "<div>" + escape(item.label) + "</div>"; 
                         }
                       }'
                     )))
      },

      if (input$analysisType == "logistic") {
        selectizeInput("logisticFamily", "Family for Logistic Regression:",
                     choices = c("binomial", "quasibinomial"),
                     selected = "binomial")
      },

      if (input$analysisType == "glmm" || input$analysisType == "gee") {
        selectizeInput("glmmFamily", "Family for GLMM/GEE:",
                     choices = c("binomial", "poisson", "gaussian", "Gamma", "inverse.gaussian", "quasibinomial", "quasipoisson"),
                     selected = "poisson")
      },

      if (input$analysisType == "chisq") {
        tagList(
          selectizeInput("chisqVar", "Variable for Chi-squared Test:",
                        choices = all_data_cols,
                        options = list(render = I(
                          '{
                            item: function(item, escape) { 
                              return "<div>" + escape(item.label) + "</div>"; 
                            }
                          }'
                        ))),
          textInput("expectedProbs", "Expected Probabilities (comma-separated, optional):",
                    value = "",
                    placeholder = "e.g., 0.25, 0.75"),
          helpText("Leave empty for uniform distribution, or provide probabilities matching levels.")
        )
      },

      if (input$analysisType %in% c("spearman", "pearson")) { # Pearson added as a common correlation
        tagList(
          selectizeInput("var1", "Variable 1:",
                       choices = all_data_cols,
                       options = list(render = I(
                         '{
                           item: function(item, escape) { 
                             return "<div>" + escape(item.label) + "</div>"; 
                           }
                         }'
                       ))),
          selectizeInput("var2", "Variable 2:",
                       choices = all_data_cols,
                       options = list(render = I(
                         '{
                           item: function(item, escape) { 
                             return "<div>" + escape(item.label) + "</div>"; 
                           }
                         }'
                       )))
        )
      },
      # Add more specific parameters as needed for other models
      if (input$analysisType == "permtest") {
        helpText("Permutation signed rank test requires a specific implementation (e.g., from 'coin' package).")
      }
    )
  })


  # Run analysis when the run button is clicked
  observeEvent(input$runAnalysis, {
    req(data(), input$analysisType)

    # Reset results and plot on new analysis run
    analysis_results$result <- NULL
    analysis_results$plot <- NULL

    df <- data()

    tryCatch({
      showNotification(paste("Running", input$analysisType, "analysis..."), type = "message")

      current_analysis_result <- list(summary = "No results yet.", plot = NULL)

      if (input$analysisType == "linear") {
        req(input$responseVar, input$predictorVars)
        formula_str <- paste(input$responseVar, "~", paste(input$predictorVars, collapse = " + "))
        model <- lm(as.formula(formula_str), data = df)
        current_analysis_result$summary <- summary(model)
        current_analysis_result$plot <- function() {
          # If only one predictor, plot regression line
          if (length(input$predictorVars) == 1) {
            predictor <- input$predictorVars[1]
            plot(df[[predictor]], df[[input$responseVar]],
                 xlab = predictor, ylab = input$responseVar,
                 main = paste("Linear Regression:", input$responseVar, "vs", predictor))
            abline(model, col = "blue", lwd = 2)
          } else {
           # For multiple predictors, show diagnostic plots
           par(mfrow = c(2,2))
           plot(model)
           par(mfrow = c(1,1))
          }
        }
      } else if (input$analysisType == "logistic") {
        req(input$responseVar, input$predictorVars)
        formula_str <- paste(input$responseVar, "~", paste(input$predictorVars, collapse = " + "))
        model <- glm(as.formula(formula_str), family = input$logisticFamily, data = df)
        current_analysis_result$summary <- summary(model)
        current_analysis_result$plot <- function() {
          # Example: Plotting predicted probabilities vs. a predictor
          if (length(input$predictorVars) > 0) {
            predictor_to_plot <- input$predictorVars[1]
            plot(df[[predictor_to_plot]], predict(model, type = "response"),
                 xlab = predictor_to_plot, ylab = "Predicted Probability",
                 main = "Logistic Regression: Predicted Probabilities")
            points(df[[predictor_to_plot]], df[[input$responseVar]], col = "red", pch = 16) # Actual values
          } else {
            plot(1,1, type = "n", main = "No plot available for this configuration")
          }
        }
      } else if (input$analysisType == "glmm") {
        req(input$responseVar, input$predictorVars, input$randomEffect)
        # Construct formula with fixed and random effects
        fixed_formula_str <- paste(input$responseVar, "~", paste(input$predictorVars, collapse = " + "))
        random_formula_str <- paste(fixed_formula_str, "+", paste(input$randomEffect, collapse = " + "))

        model <- glmer(as.formula(random_formula_str), family = input$glmmFamily, data = df)
        current_analysis_result$summary <- summary(model)
        current_analysis_result$plot <- function() {
          plot(model, main = "GLMM Residuals vs. Fitted")
        }
      } else if (input$analysisType == "gamm") {
        req(input$responseVar, input$predictorVars)
        # GAM formula can be complex, for simplicity, using s() for smoothing on all predictors
        # Users might need more control here for specific smooth terms
        formula_str_parts <- lapply(input$predictorVars, function(v) paste0("s(", v, ")"))
        formula_str <- paste(input$responseVar, "~", paste(formula_str_parts, collapse = " + "))

        # If random effects are selected, add them to the GAMM formula
        if (!is.null(input$randomEffect) && length(input$randomEffect) > 0) {
            random_formula_str <- paste(input$randomEffect, collapse = " + ")
            formula_str <- paste(formula_str, "+", random_formula_str)
        }

        model <- gam(as.formula(formula_str), data = df)
        current_analysis_result$summary <- summary(model)
        current_analysis_result$plot <- function() {
          plot(model, pages = 1, main = "GAM Smooth Terms") # Plots smooth terms
        }
      } else if (input$analysisType == "anova") {
        req(input$responseVar, input$groupVar)
        formula_str <- paste(input$responseVar, "~", input$groupVar)
        model <- aov(as.formula(formula_str), data = df)
        current_analysis_result$summary <- summary(model)
        current_analysis_result$plot <- function() {
          boxplot(as.formula(formula_str), data = df,
                  main = paste("ANOVA: ", input$responseVar, " by ", input$groupVar),
                  xlab = input$groupVar, ylab = input$responseVar)
        }
      } else if (input$analysisType == "gee") {
        req(input$responseVar, input$predictorVars, input$randomEffect) # randomEffect for GEE's id
        formula_str <- paste(input$responseVar, "~", paste(input$predictorVars, collapse = " + "))
        # GEE requires an 'id' variable for clustering
        if (length(input$randomEffect) == 1) {
          model <- geeglm(as.formula(formula_str), id = df[[input$randomEffect]],
                          family = input$glmmFamily, data = df)
          current_analysis_result$summary <- summary(model)
          current_analysis_result$plot <- function() {
            plot(1,1, type = "n", main = "GEE plots often involve residuals or predictions, needs custom implementation.")
          }
        } else {
          stop("For GEE, please select exactly one random effect variable to serve as the 'id' for clustering.")
        }
      } else if (input$analysisType == "negbin") {
        req(input$responseVar, input$predictorVars)
        formula_str <- paste(input$responseVar, "~", paste(input$predictorVars, collapse = " + "))
        model <- glm.nb(as.formula(formula_str), data = df)
        current_analysis_result$summary <- summary(model)
        current_analysis_result$plot <- function() {
          plot(model, main = "Negative Binomial Regression Diagnostics")
        }
      } else if (input$analysisType == "chisq") {
        req(input$chisqVar)
        observed_counts <- table(df[[input$chisqVar]])
        expected_probs <- NULL
        if (!is.null(input$expectedProbs) && input$expectedProbs != "") {
          expected_probs <- as.numeric(unlist(strsplit(input$expectedProbs, ",")))
          if (sum(expected_probs) != 1 && !is.na(sum(expected_probs))) {
            showNotification("Warning: Expected probabilities do not sum to 1. Will be normalized.", type = "warning")
            expected_probs <- expected_probs / sum(expected_probs)
          }
          if (length(expected_probs) != length(observed_counts)) {
            stop("Number of expected probabilities must match the number of levels in the observed variable.")
          }
        }
        test_result <- chisq.test(observed_counts, p = expected_probs)
        current_analysis_result$summary <- test_result
        current_analysis_result$plot <- function() {
          barplot(observed_counts, main = "Observed Counts for Chi-squared Test",
                  ylab = "Count", xlab = input$chisqVar)
          if (!is.null(expected_probs)) {
            expected_counts <- sum(observed_counts) * expected_probs
            barplot(expected_counts, main = "Expected Counts for Chi-squared Test",
                    ylab = "Count", xlab = input$chisqVar, add = TRUE, density = 20)
          }
        }
      } else if (input$analysisType == "mannwhitney") {
        req(input$responseVar, input$groupVar)
        group_levels <- unique(df[[input$groupVar]])
        if (length(group_levels) != 2) {
          stop("Mann-Whitney U test requires exactly two groups.")
        }
        formula_str <- paste(input$responseVar, "~", input$groupVar)
        test_result <- wilcox.test(as.formula(formula_str), data = df)
        current_analysis_result$summary <- test_result
        current_analysis_result$plot <- function() {
          boxplot(as.formula(formula_str), data = df,
                  main = paste("Mann-Whitney U Test: ", input$responseVar, " by ", input$groupVar),
                  xlab = input$groupVar, ylab = input$responseVar)
        }
      } else if (input$analysisType == "kruskal") {
        req(input$responseVar, input$groupVar)
        formula_str <- paste(input$responseVar, "~", input$groupVar)
        test_result <- kruskal.test(as.formula(formula_str), data = df)
        current_analysis_result$summary <- test_result
        current_analysis_result$plot <- function() {
          boxplot(as.formula(formula_str), data = df,
                  main = paste("Kruskal-Wallis Test: ", input$responseVar, " by ", input$groupVar),
                  xlab = input$groupVar, ylab = input$responseVar)
        }
      } else if (input$analysisType == "zeroinfl") {
        req(input$responseVar, input$predictorVars)
        formula_str <- paste(input$responseVar, "~", paste(input$predictorVars, collapse = " + "))
        model <- zeroinfl(as.formula(formula_str), data = df)
        current_analysis_result$summary <- summary(model)
        current_analysis_result$plot <- function() {
          plot(fitted(model), residuals(model),
               xlab = "Fitted Values", ylab = "Residuals",
               main = "Zero-Inflated Model: Residuals vs. Fitted")
          abline(h = 0, col = "red")
        }
      } else if (input$analysisType == "hurdle") {
        req(input$responseVar, input$predictorVars)
        formula_str <- paste(input$responseVar, "~", paste(input$predictorVars, collapse = " + "))
        model <- hurdle(as.formula(formula_str), data = df)
        current_analysis_result$summary <- summary(model)
        current_analysis_result$plot <- function() {
          plot(fitted(model), residuals(model),
               xlab = "Fitted Values", ylab = "Residuals",
               main = "Hurdle Model: Residuals vs. Fitted")
          abline(h = 0, col = "red")
        }
      } else if (input$analysisType == "signtest") {
        req(input$responseVar, input$groupVar)
        # Sign test typically for paired data or comparing median to a value.
        # For simplicity, assuming a paired-like comparison for now.
        # You'll need to define how 'paired' data is structured for this.
        # Example: comparing two groups, assumes equal sample size for simplicity.
        # A more robust implementation would check for paired samples.
        group_levels <- unique(df[[input$groupVar]])
        if (length(group_levels) != 2) {
          stop("Sign test requires exactly two groups for comparison.")
        }
        # Get data for each group
        group1_data <- df[[input$responseVar]][df[[input$groupVar]] == group_levels[1]]
        group2_data <- df[[input$responseVar]][df[[input$groupVar]] == group_levels[2]]

        # Ensure same length for paired test, or define how to handle unpaired.
        if (length(group1_data) != length(group2_data)) {
          stop("For a basic sign test, the two groups must have the same number of observations.")
        }

        differences <- group1_data - group2_data
        positive_diffs <- sum(differences > 0)
        negative_diffs <- sum(differences < 0)
        zeros <- sum(differences == 0)
        n_effective <- length(differences) - zeros # Exclude zeros

        # Binomial test for number of positive differences
        test_result <- binom.test(positive_diffs, n_effective, p = 0.5)

        current_analysis_result$summary <- list(
          "Sign Test Result" = test_result,
          "Positive Differences" = positive_diffs,
          "Negative Differences" = negative_diffs,
          "Zero Differences" = zeros
        )
        current_analysis_result$plot <- function() {
          hist(differences, main = "Distribution of Differences for Sign Test",
               xlab = "Differences", ylab = "Frequency", breaks = 10)
          abline(v = 0, col = "red", lty = 2)
        }
      } else if (input$analysisType == "wilcoxon") {
        req(input$responseVar, input$groupVar)
        # Wilcoxon Signed-Rank Test is for paired data, similar to sign test
        # Wilcoxon Rank-Sum Test (Mann-Whitney U) is for unpaired.
        # Assuming you mean Wilcoxon Rank-Sum test for two groups here.
        # If paired, you'd need to modify data input.
        formula_str <- paste(input$responseVar, "~", input$groupVar)
        test_result <- wilcox.test(as.formula(formula_str), data = df)
        current_analysis_result$summary <- test_result
        current_analysis_result$plot <- function() {
          boxplot(as.formula(formula_str), data = df,
                  main = paste("Wilcoxon Rank-Sum Test: ", input$responseVar, " by ", input$groupVar),
                  xlab = input$groupVar, ylab = input$responseVar)
        }
      } else if (input$analysisType == "spearman" || input$analysisType == "pearson") {
        req(input$var1, input$var2)
        method <- ifelse(input$analysisType == "spearman", "spearman", "pearson")
        correlation_result <- cor.test(df[[input$var1]], df[[input$var2]], method = method)
        current_analysis_result$summary <- correlation_result
        current_analysis_result$plot <- function() {
          plot(df[[input$var1]], df[[input$var2]],
               xlab = input$var1, ylab = input$var2,
               main = paste(tools::toTitleCase(method), "Correlation Plot"))
          abline(lm(df[[input$var2]] ~ df[[input$var1]]), col = "blue") # Add regression line
        }
      } else if (input$analysisType == "permtest") {
          current_analysis_result$summary <- "Permutation signed rank test is complex and requires specific packages/implementations. This is a placeholder."
          current_analysis_result$plot <- function() {
            plot(1,1, type = "n", main = "No plot for permutation test placeholder.")
          }
      } else if (input$analysisType == "gwr") {
          current_analysis_result$summary <- "Geographically Weighted Regression (GWR) requires spatial data (sf or sp objects) and specific packages (e.g., spgwr). Data needs to be in a spatial format first."
          current_analysis_result$plot <- function() {
            plot(1,1, type = "n", main = "GWR requires spatial data visualization.")
          }
      } else {
        current_analysis_result$summary <- "Analysis type not yet implemented or selected."
        current_analysis_result$plot <- NULL
      }

      analysis_results$result <- current_analysis_result$summary
      analysis_results$plot <- current_analysis_result$plot

      updateTabsetPanel(session, "mainTabs", selected = "Analysis Results")

    }, error = function(e) {
      showNotification(paste("Error running analysis:", e$message), type = "error")
      analysis_results$result <- paste("Error: ", e$message)
      analysis_results$plot <- NULL
    })
  })

  # Display analysis results
  output$analysisResults <- renderPrint({
    req(analysis_results$result)
    analysis_results$result
  })

  # Display analysis plot
  output$analysisPlot <- renderPlot({
    if (!is.null(analysis_results$plot)) {
      analysis_results$plot()
    } else {
      plot(1, 1, type = "n", xlab = "", ylab = "", axes = FALSE)
      text(1, 1, "Plot will appear here", cex = 1.5)
    }
  })
}

# Run the application
shinyApp(ui = ui, server = server)