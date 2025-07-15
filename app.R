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
library(ggplot2)


# in R studio, write and run a file with the following code:
# install.packages(c("shiny", "shinyjs", "reticulate", "DT", "readxl", "lme4", "mgcv", "MASS", "pscl", "geepack", "spgwr", "readr", "ggplot2"))
# library(reticulate)
# reticulate::install_miniconda()  # If not already installed
# reticulate::conda_create("MassasoitModelForge_env")
# reticulate::use_condaenv("MassasoitModelForge_env")

# # Activate the conda environment
# reticulate::use_condaenv("MassasoitModelForge_env", required = TRUE)

# # Install required Python packages
# reticulate::py_install(c("pandas", "numpy", "scipy", "scikit-learn"), envname = "MassasoitModelForge_env")

# # Verify the installation
# reticulate::py_module_available("pandas")

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




##########################################################################

######################     Analysis Functions           ###################

##########################################################################



# Function to read and clean data (Can be expanded for more than logistic regression)
read_data_file <- function(file_path, file_name) {
  # ... existing code ...
  
  # After loading and cleaning data
  df <- clean_and_convert(df)
  
  # Identify suitable logistic response variables
  suitable_logistic_vars <- sapply(names(df), function(col) {
    is_logistic_response(df, col)
  })
  
  # Store suitable variables in reactive value
  suitable_response_vars <- reactiveVal()
  suitable_response_vars(names(df)[suitable_logistic_vars])
  
  return(df)
}





# Linear Regression Analysis
run_linear_analysis <- function(df, response_var, predictor_vars) {
  formula_str <- paste(response_var, "~", paste(predictor_vars, collapse = " + "))
  model <- lm(as.formula(formula_str), data = df)
  
  list(
    summary = summary(model),
    plot = function() {
      if (length(predictor_vars) == 1) {
        plot(df[[predictor_vars[1]]], df[[response_var]],
             xlab = predictor_vars[1], 
             ylab = response_var,
             main = paste("Linear Regression:", response_var, "vs", predictor_vars[1]))
        abline(model, col = "blue", lwd = 2)
      } else {
        par(mfrow = c(2, 2))
        plot(model)
        par(mfrow = c(1, 1))
      }
    }
  )
}



# # Logistic Regression Analysis
# run_logistic_analysis <- function(df, response_var, predictor_vars, family = "binomial") {
#   formula_str <- paste(response_var, "~", paste(predictor_vars, collapse = " + "))
#   model <- glm(as.formula(formula_str), family = family, data = df)
  
#   list(
#     summary = summary(model),
#     plot = function() {
#       if (length(predictor_vars) > 0) {
#         predictor_to_plot <- predictor_vars[1]
#         plot(df[[predictor_to_plot]], predict(model, type = "response"),
#              xlab = predictor_to_plot, 
#              ylab = "Predicted Probability",
#              main = "Logistic Regression: Predicted Probabilities")
#         points(df[[predictor_to_plot]], df[[response_var]], 
#                col = "red", pch = 16)
#       } else {
#         plot(1, 1, type = "n", 
#              main = "No plot available for this configuration")
#       }
#     }
#   )
# }

# Function to identify and categorize suitable logistic response variables
is_logistic_response <- function(df, col_name) {
  col <- df[[col_name]]
  
  # Check if column is numeric or logical
  if (!is.numeric(col) && !is.logical(col)) {
    return("unsuitable")
  }
  
  # For numeric columns
  if (is.numeric(col)) {
    # Remove NA values for checking
    col <- na.omit(col)
    
    # Check if all values are either 0 or 1
    if (all(col %in% c(0, 1))) {
      return("binary")
    }
    
    # Check if all values are proportions (0 <= x <= 1)
    if (all(col >= 0 & col <= 1)) {
      return("proportion")
    }
    
    # Check if column has only non-zero values of same sign
    non_zero <- col[col != 0]
    zero <- col[col == 0]
    if (length(non_zero) > 0 && length(zero) > 0) {
      # Check if all non-zero values are positive or all negative
      if (all(non_zero > 0) || all(non_zero < 0)) {
        return("convertible")
      }
    }
    
    return("unsuitable")
  }
  
  # For logical columns, they're automatically suitable
  return("binary")
}

# Function to convert convertible variables to binary
convert_to_binary <- function(df, col_name) {
  col <- df[[col_name]]
  
  # Convert non-zero values to 1, keep zeros as 0
  df[[col_name]] <- ifelse(col != 0, 1, 0)
  return(df)
}

# Add this function before the analysis dispatch in the server section
run_logistic_analysis <- function(df, response_var, predictor_vars, family = "binomial") {
  # Convert convertible variables before analysis
  var_type <- is_logistic_response(df, response_var)
  if (var_type == "convertible") {
    df <- convert_to_binary(df, response_var)
  }
  
  # Prepare formula
  formula_str <- paste(response_var, "~", paste(predictor_vars, collapse = " + "))
  
  # Run logistic regression with proper error handling
  tryCatch({
    model <- glm(as.formula(formula_str), family = family, data = df)
    
    # Create summary and plot
    model_summary <- summary(model)
    
    # Create plot function that will be called by Shiny
    plot_func <- function() {
      if (length(predictor_vars) > 0) {
        predictor_to_plot <- predictor_vars[1]
        plot_data <- data.frame(
          x = df[[predictor_to_plot]],
          y = df[[response_var]],
          predicted = predict(model, type = "response")
        )
        
        # Create base R plot
        plot(
          plot_data$x,
          plot_data$predicted,
          type = "l",
          col = "blue",
          lwd = 2,
          xlab = predictor_to_plot,
          ylab = "Predicted Probability",
          main = "Logistic Regression: Predicted Probabilities"
        )
        points(plot_data$x, plot_data$y, col = "red", pch = 16)
      } else {
        plot(1, 1, type = "n", 
             main = "No plot available for this configuration",
             xlab = "", ylab = "")
      }
    }
    
    list(
      summary = model_summary,
      plot = plot_func
    )
  }, error = function(e) {
    showNotification(paste("Error running logistic regression:", e$message), type = "error")
    NULL
  })
}

# ANOVA Analysis
run_anova_analysis <- function(df, response_var, group_var) {
  formula_str <- paste(response_var, "~", group_var)
  model <- aov(as.formula(formula_str), data = df)
  
  list(
    summary = summary(model),
    plot = function() {
      boxplot(as.formula(formula_str), data = df,
              main = paste("ANOVA: ", response_var, " by ", group_var),
              xlab = group_var, 
              ylab = response_var)
    }
  )
}



# GLMM Analysis
run_gee_analysis <- function(df, response_var, predictor_vars, cluster_id, family = "poisson") {
  formula_str <- paste0(response_var, " ~ ", paste(predictor_vars, collapse = " + "))
  
  # For Poisson family, convert response variable to integer if needed
  if (family == "poisson") {
    df[[response_var]] <- as.integer(round(df[[response_var]]))
  }
  
  # Create GEE model using geepack
  gee_model <- geepack::geeglm(
    as.formula(formula_str),
    family = family,
    data = df,
    id = as.factor(df[[cluster_id]]),  # Ensure cluster ID is treated as factor
    corstr = "exchangeable"
  )
  
  list(
    summary = summary(gee_model),
    plot = function() {
      # Get fitted values and residuals
      fitted <- fitted(gee_model)
      residuals <- residuals(gee_model)
      
      # Create plot
      plot(fitted, residuals,
           xlab = "Fitted values",
           ylab = "Residuals",
           main = "GEE Residuals vs. Fitted",
           pch = 16, col = "blue")
      abline(h = 0, lty = 2)
    }
  )
}






#######################################################################



# UI definition with custom CSS
ui <- fluidPage(
  useShinyjs(),
  tags$head(
    # Prevent caching of CSS
    HTML('<meta http-equiv="Cache-Control" content="no-cache, no-store, must-revalidate" />
         <meta http-equiv="Pragma" content="no-cache" />
         <meta http-equiv="Expires" content="0" />
         <meta http-equiv="Content-Type" content="text/html; charset=utf-8"/>'),
    # Include the external CSS file with version parameter
    tags$link(rel = "stylesheet", type = "text/css", 
             href = paste0("app_design.css?", as.integer(Sys.time()))),
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
             overflow-y: auto;
             padding: 0;
             margin: 0;",
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
    div(style = "position: relative; z-index: 2; min-height: 100%; padding-top: 80px;",
      # Header with title and logo
      div(class = "app-header",
        style = "position: fixed; top: 0; left: 0; right: 0; z-index: 1000;"
      ,
        div(class = "header-left",
          actionLink(
            "appTitleLink_about",
            "Massasoit Model Forge",
            class = "app-title-link"
          )
        ),
        div(class = "header-right",
          a(
            href = "https://massasoitstem.com/",
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
        p("  \t     We're ",
          tags$a(id = "sammyLink", onclick = "event.preventDefault();", "Sammy Olsen", style = "color: #3498db; text-decoration: underline; cursor: pointer;"),
          " and ",
          tags$a(id = "ianLink", onclick = "event.preventDefault();", "Ian Handy", style = "color: #3498db; text-decoration: underline; cursor: pointer;"),
          ", data scientists who got \
        our start at Massasoit Community College. This app began as a \
        tool for a very specific purpose: to help make sense of over a \
        decade's worth of wild bee research in preparation for the \
        national Ecological Society of America conference in 2025."),
        
        # Popup Modals
        tags$div(id = "sammyModal", class = "modal",
          tags$div(class = "modal-content",
            tags$span(class = "close", "×"),
            h3("Sammy Olsen"),
            p("Data Scientist and co-creator of Massasoit Model Forge."),
            p("Github: spamolsen")
          )
        ),
        
        tags$div(id = "ianModal", class = "modal",
          tags$div(class = "modal-content",
            tags$span(class = "close", "×"),
            h3("Ian Handy"),
            p("Data Scientist and co-creator of Massasoit Model Forge."),
            p("Github: vyndyctyv")
          )
        ),
        
        # JavaScript for modals
        tags$script(HTML(
          "// Get the modals
          var sammyModal = document.getElementById('sammyModal');
          var ianModal = document.getElementById('ianModal');
          
          // Get the links that open the modals
          var sammyLink = document.getElementById('sammyLink');
          var ianLink = document.getElementById('ianLink');
          
          // Get the <span> elements that close the modals
          var spans = document.getElementsByClassName('close');
          
          // When the user clicks on a link, open the corresponding modal
          sammyLink.onclick = function() { sammyModal.style.display = 'block'; }
          ianLink.onclick = function() { ianModal.style.display = 'block'; }
          
          // When the user clicks on <span> (x), close the modal
          for (var i = 0; i < spans.length; i++) {
            spans[i].onclick = function() {
              sammyModal.style.display = 'none';
              ianModal.style.display = 'none';
            }
          }
          
          // When the user clicks anywhere outside of the modal, close it
          window.onclick = function(event) {
            if (event.target == sammyModal || event.target == ianModal) {
              sammyModal.style.display = 'none';
              ianModal.style.display = 'none';
            }
          }
          ")),
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
        p("Click on our names above to learn more about us or reach out through GitHub."),
        tags$ul(
          tags$li(tags$a(href = "https://github.com/spamolsen", target = "_blank", "GitHub: spamolsen")),
          tags$li(tags$a(href = "https://github.com/vyndyctyv", target = "_blank", "GitHub: vyndyctyv"))
        )
      )
    )
  ),

  # Main Application Content
  div(
    id = "mainApp",
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
          a(href = "https://massasoitstem.com/", target = "_blank",
            img(
              src = "STEMlogowithBackground.png",
              class = "stem-logo",
              alt = "Massasoit STEM"
            )
          )
        )
      ),

      
      # Main content row with sidebar and results
      # Transparent spacer div for layout
      div(style = "height: 40px; margin: 20px 0;"),
      
      div(class = "container-fluid",
        div(class = "row",
          # Sidebar with data/analysis controls (left side)
          div(class = "col-md-4",
            div(class = "sidebar-panel", style = "background-color: white; padding: 15px; border-radius: 5px; box-shadow: 0 2px 4px rgba(0,0,0,0.1);",
              tabsetPanel(
                id = "sidebarTabs",
                tabPanel(
                  "Data",
                  div(class = "form-group",
                    radioButtons(
                      "dataSource",
                      "Choose data source:",
                      choices = c(
                        "Use base file" = "base",
                        "Upload your own file" = "upload",
                        "Online Databases" = "api"
                      ),
                      selected = "base"
                    )
                  ),

                  # Conditional panel for base file selection
                  conditionalPanel(
                    condition = "input.dataSource == 'base'",
                    div(class = "form-group",
                      selectInput(
                        "baseFile",
                        "Select base file:",
                        choices = list.files(
                          "Base_Data_Files",
                          pattern = "\\.(xlsx|csv)$",
                          full.names = FALSE
                        ),
                        selected = NULL
                      )
                    )
                  ),

                  # Conditional panel for file upload
                  conditionalPanel(
                    condition = "input.dataSource == 'upload'",
                    div(class = "form-group",
                      fileInput(
                        "file1",
                        label = span("Choose File(s)", class = "file-input-label"),
                        multiple = TRUE,
                        accept = c(".xlsx", ".xls", ".csv"),
                        buttonLabel = "Browse..."
                      ),
                      div(
                        "Select one or more Excel (.xlsx, .xls) or CSV (.csv) files",
                        class = "file-input-info"
                      )
                    )
                  ),

                  # Conditional panel for online databases
                  conditionalPanel(
                    condition = "input.dataSource == 'api'",
                    div(class = "form-group",
                      selectInput(
                        "apiSource", 
                        "Select Data Source:",
                        choices = c("Traffic", "Visual Crossing")
                      ),
                      # Placeholder for API-specific parameters
                      uiOutput("apiParams")
                    )
                  ),
                  
                  actionButton("loadData", "Load Data", class = "btn-primary"),
                  
                  # Add JavaScript to switch to Analyze tab when Load Data is clicked
                  tags$script(HTML("
                    $(document).on('shiny:inputchanged', function(event) {
                      if (event.name === 'loadData' && event.value > 0) {
                        setTimeout(function() {
                          $('a[data-value=\"Analyze\"]').tab('show');
                        }, 300);
                      }
                    });
                  ")),
                  style = "margin-top: 10px;"
                ),
                
                tabPanel(
                  "Analyze",
                  div(class = "form-group",
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
                    )
                  ),

                  # Dynamic UI for analysis parameters
                  uiOutput("analysisParams"),

                  # Action button to run the selected analysis
                  actionButton("runAnalysis", "Run Analysis", class = "btn-primary")
                )
              )
            )
          ),
          
          # Main content area with results tabs (right side)
          div(class = "col-md-8",
            div(class = "main-panel", style = "background-color: white; padding: 15px; border-radius: 5px; box-shadow: 0 2px 4px rgba(0,0,0,0.1);",
              tabsetPanel(
                id = "mainTabs",
                tabPanel(
                  "View File",
                  div(class = "table-responsive",
                    DTOutput("dataTable")
                  )
                ),
                
                tabPanel("Data Summary",
                  div(class = "summary-output",
                    verbatimTextOutput("summary")
                  )
                ),
                
                tabPanel("Analysis Results",
                  conditionalPanel(
                    condition = "output.analysisPlot_available == true",
                    div(class = "plot-container",
                      plotOutput("analysisPlot", height = "500px")
                    )
                  ),
                  
                  conditionalPanel(
                    condition = "output.analysisPlot_available == false",
                    div(class = "alert alert-info",
                      "No plot available for this analysis type or an error occurred."
                    )
                  ),
                  
                  div(class = "results-output",
                    verbatimTextOutput("analysisResults")
                  )
                )
              )
            )
          )
        )
      )
    )
  )
)

# Server logic
server <- function(input, output, session) {
  # Function to safely convert columns to appropriate types
  clean_and_convert <- function(df) {
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

  # Reactive value to store suitable response variables for logistic regression
  suitable_response_vars <- reactiveVal(NULL)

  # Update suitable response variables when data is loaded
  observeEvent(input$loadData, {
    if (!is.null(data())) {
      df <- data()
      suitable_response_vars(names(df)[sapply(names(df), function(col) {
        is_logistic_response(df, col)
      })])
    }
  })

  # Reactive value to store suitable variables
  suitable_vars <- reactiveVal(NULL)
  suitable_vars_types <- reactiveVal(NULL)

  # Update suitable variables when data is loaded
  observeEvent(input$loadData, {
    if (!is.null(data())) {
      df <- data()
      
      # Get all variable types
      var_types <- lapply(names(df), function(col) {
        is_logistic_response(df, col)
      })
      names(var_types) <- names(df)
      suitable_vars_types(var_types)
      
      # Get only suitable variables
      suitable_vars(names(df)[sapply(var_types, function(x) x != "unsuitable")])
    }
  })

  # Function to format variable names with conversion info
  format_variable_name <- function(var_name, var_type) {
    if (var_type == "convertible") {
      return(paste0(var_name, " <span style='color: red;'>Will be Converted</span>"))
    }
    return(var_name)
  }

  # Function to format variable names with conversion info
  format_variable_name <- function(var_name, var_type) {
    if (var_type == "convertible") {
      return(paste0(var_name, " <span style='color: red;'>Will be Converted</span>"))
    }
    return(var_name)
  }

  # Render response variable selector with conversion info
  output$responseVarSelector <- renderUI({
    req(data(), input$analysisType)
    
    if (input$analysisType == "logistic") {
      # Get all variable types
      var_types <- lapply(names(data()), function(col) {
        is_logistic_response(data(), col)
      })
      names(var_types) <- names(data())
      
      # Get suitable variables
      suitable_vars_list <- names(data())[sapply(var_types, function(x) x != "unsuitable")]
      
      # Create choices with conversion info
      choices <- setNames(
        sapply(suitable_vars_list, function(var) {
          format_variable_name(var, var_types[[var]])
        }),
        suitable_vars_list
      )
      
      selectInput(
        "responseVar",
        "Response Variable:",
        choices = choices,
        selected = NULL
      )
    } else if (input$analysisType != "") {
      selectInput(
        "responseVar",
        "Response Variable:",
        choices = names(data()),
        selected = NULL
      )
    }
  })

  # ... rest of existing server code ...
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

  observeEvent(input$appTitleLink_about, {
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
        
        # Read the file with appropriate function
        if (grepl("\\.xlsx?$", input$baseFile, ignore.case = TRUE)) {
          # For Excel files, read all as text first to avoid type guessing issues
          df <- readxl::read_excel(file_path, col_types = "text")
        } else if (grepl("\\.csv$", input$baseFile, ignore.case = TRUE)) {
          # For CSV files, read all as character first
          df <- readr::read_csv(file_path, col_types = cols(.default = col_character()), 
                               show_col_types = FALSE)
        } else {
          stop("Unsupported file format. Please use .xlsx, .xls, or .csv files.")
        }
        
        # Clean and convert column types
        df <- clean_and_convert(df)
        
        # Store the data
        data(df)
        
        # Update suitable variables and types
        var_types <- lapply(names(df), function(col) {
          is_logistic_response(df, col)
        })
        names(var_types) <- names(df)
        suitable_vars_types(var_types)
        suitable_vars(names(df)[sapply(var_types, function(x) x != "unsuitable")])
      } else if (input$dataSource == "upload" && !is.null(input$file1)) {
        # Load uploaded files
        uploaded_files <- input$file1
        df_list <- list()
        
        for (file in uploaded_files) {
          if (grepl("\\.xlsx?$", file$name, ignore.case = TRUE)) {
            df <- readxl::read_excel(file$datapath, col_types = "text")
          } else if (grepl("\\.csv$", file$name, ignore.case = TRUE)) {
            df <- readr::read_csv(file$datapath, col_types = cols(.default = col_character()), 
                                 show_col_types = FALSE)
          } else {
            stop("Unsupported file format. Please use .xlsx, .xls, or .csv files.")
          }
          df_list[[file$name]] <- df
        }
        
        # Merge all uploaded files
        if (length(df_list) > 0) {
          df <- Reduce(function(x, y) merge(x, y, all = TRUE), df_list)
          
          # Clean and convert column types after merge
          df <- clean_and_convert(df)
          
          # Store the data
          data(df)
          
          # Update suitable variables and types
          var_types <- lapply(names(df), function(col) {
            is_logistic_response(df, col)
          })
          names(var_types) <- names(df)
          suitable_vars_types(var_types)
          suitable_vars(names(df)[sapply(var_types, function(x) x != "unsuitable")])
        }
      } else if (input$dataSource == "api") {
        # Load from API
        if (input$apiSource == "Traffic") {
          # Traffic API implementation
          df <- data.frame(
            Location = input$trafficLocation,
            StartDate = input$trafficDates[1],
            EndDate = input$trafficDates[2]
          )
        } else if (input$apiSource == "Visual Crossing") {
          # Visual Crossing API implementation
          df <- data.frame(
            Location = input$vcLocation,
            StartDate = input$vcDates[1],
            EndDate = input$vcDates[2],
            UnitGroup = input$vcUnitGroup
          )
        }
        
        # Clean and convert column types
        df <- clean_and_convert(df)
        
        # Store the data
        data(df)
        
        # Update suitable variables and types
        var_types <- lapply(names(df), function(col) {
          is_logistic_response(df, col)
        })
        names(var_types) <- names(df)
        suitable_vars_types(var_types)
        suitable_vars(names(df)[sapply(var_types, function(x) x != "unsuitable")])
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



##########################################################################

####################### Function to handle variable conversion before analysis
prepare_response_variable <- function(df, var_name) {
  var_type <- suitable_vars_types()[[var_name]]
  
  if (var_type == "convertible") {
    df <- convert_to_binary(df, var_name)
  }
  
  return(df)
}

##########################################################################

######################     Code for analysis           ###################

##########################################################################



    # Format variable names (without N values in the text, they'll be added via CSS)
    format_vars <- function(vars) {
      setNames(vars, vars)
    }

    all_data_cols <- format_vars(names(df))
    num_data_cols <- format_vars(names(df)[sapply(df, is.numeric)])
    char_data_cols <- format_vars(names(df)[sapply(df, is.character)])
    logistic_cols <- format_vars(names(df)[sapply(names(df), function(col) is_logistic_response(df, col) != "unsuitable")])

    # Include the CSS in the UI
    tagList(
      tags$head(tags$style(HTML(css_rules))),
      # Common parameters for most analyses
      if (input$analysisType %in% c("linear", "glmm", "gamm", "negbin", "anova", "kruskal",
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

      if (input$analysisType == "logistic") {
        selectizeInput("responseVar", "Response Variable:",
                     choices = logistic_cols,
                     options = list(render = I(
                       '{
                         item: function(item, escape) { 
                           return "<div>" + escape(item.label) + "</div>"; 
                         }
                       }'
                     )))
      },

      if (input$analysisType %in% c("linear","logistic", "glmm", "gamm", "negbin", "gee", "zeroinfl", "hurdle")) {
        selectizeInput("predictorVars", "Predictor Variables:",
                     choices = all_data_cols,
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
        selectizeInput("groupVar", "Grouping Variables:",
                     choices = char_data_cols,
                     multiple = TRUE,
                     options = list(
                       render = I('{
                         item: function(item, escape) { 
                           return "<div>" + escape(item.label) + "</div>"; 
                         }
                       }'),
                       delimiter = "+"
                     ))
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

      if (input$analysisType == "gee") {
        # Get categorical variables for cluster ID
        categorical_vars <- format_vars(names(df)[sapply(df, function(x) is.factor(x) || is.character(x))])
        
        selectizeInput("clusterID", "Cluster/Group ID:",
                       choices = categorical_vars,
                       options = list(render = I(
                         '{
                           item: function(item, escape) { 
                             return "<div>" + escape(item.label) + "</div>"; 
                           }
                         }'
                       )))
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
    showNotification(paste("Running", input$analysisType, "analysis..."), 
                    type = "message")
    
    # Dispatch to the appropriate analysis function
    model_result <- switch(
      input$analysisType,
      "linear" = run_linear_analysis(
        df, 
        input$responseVar, 
        input$predictorVars
      ),
      "logistic" = run_logistic_analysis(
        df, 
        input$responseVar, 
        input$predictorVars,
        input$logisticFamily
      ),
      "anova" = run_anova_analysis(
        df,
        input$responseVar,
        input$groupVar
      ),
      "glmm" = run_glmm_analysis(
        df,
        input$responseVar,
        input$predictorVars,
        input$randomEffect,
        input$glmmFamily
      ),
      "gee" = run_gee_analysis(
        df,
        input$responseVar,
        input$predictorVars,
        input$clusterID,
        input$glmmFamily
      ),
      # Add other analysis types here
      NULL
    )
    
    # Store results if successful
    if (!is.null(model_result)) {
      analysis_results$result <- model_result$summary
      analysis_results$plot <- model_result$plot
      
      # Switch to results tab after successful analysis
      updateTabsetPanel(session, "mainTabs", selected = "Analysis Results")
    } else {
      stop("Unsupported analysis type or missing parameters")
    }
    
  }, error = function(e) {
    showNotification(paste("Error in analysis:", e$message), 
                    type = "error")
  })
})

  # Reactive to track if plot is available
  output$analysisPlot_available <- reactive({
    !is.null(analysis_results$plot)
  })
  outputOptions(output, "analysisPlot_available", suspendWhenHidden = FALSE)

  # Display analysis results
output$analysisResults <- renderPrint({
  if (is.null(analysis_results$result)) {
    return("Running analysis...")
  }
  analysis_results$result
})

  # Display analysis plot
  output$analysisPlot <- renderPlot({
    req(analysis_results$plot)
    tryCatch({
      analysis_results$plot()
    }, error = function(e) {
      plot(1, 1, type = "n", main = "Error generating plot",
           xlab = "", ylab = "", axes = FALSE)
      text(1, 1, paste("Plot error:", e$message), cex = 1, col = "red")
    })
  })
}

# Run the application
shinyApp(ui = ui, server = server)