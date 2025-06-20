# Add R to PATH
$env:Path += ";C:\Program Files\R\R-4.4.2\bin\x64"

# Activate Python virtual environment
.\venv\Scripts\Activate.ps1

# Set Python path for reticulate
$env:RETICULATE_PYTHON = ".\venv\Scripts\python.exe"

# Install required R packages if not already installed
Write-Host "Checking for required R packages..." -ForegroundColor Yellow
Rscript -e "if(!require('shiny')) install.packages('shiny', repos='https://cran.rstudio.com/')"
Rscript -e "if(!require('reticulate')) install.packages('reticulate', repos='https://cran.rstudio.com/')"
Rscript -e "if(!require('DT')) install.packages('DT', repos='https://cran.rstudio.com/')"

# Run the Shiny app
Write-Host "Starting Shiny app..." -ForegroundColor Green
Rscript -e "shiny::runApp('app.R', port=8080, launch.browser=TRUE)"
