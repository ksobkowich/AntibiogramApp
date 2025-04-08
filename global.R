# Load Libraries ----------------------------------------------------------
library(AMR)
library(arrow)
library(data.table)
library(DT)
library(fresh)
library(plotly)
library(scales)
library(shiny)
library(shinyalert)
library(shinydashboard)
library(shinydashboardPlus)
library(shinyjs)
library(shinyWidgets)
library(tools)
library(vroom)
library(typedjs)
library(readxl)
library(foreach)
library(doParallel)
library(shinyBS)
library(jsonlite)
library(stringdist)
library(colorspace)
library(tidyverse)
library(zoo)
library(renv)
library(nanoparquet)
library(shinycssloaders)
library(quarto)
library(webshot2)

# renv::init()

# Run this line and select option 2 for dockerizing.
# renv::snapshot()

# renv::deactivate()

# Source Files ------------------------------------------------------------
#Modules
source("Modules/abPage.R")
source("Modules/importDataModule.R")
source("Modules/filterPanelModule.R")

#Functions
source("Functions/dataCleaningFunction.R")
source("Functions/columnDetectFunctions.R")
source("Functions/parseWideColumns.R")

#Data
awareList <- read.csv("./Data/2023AwareClassifications.csv")

# Increase maximum data size ----------------------------------------------
options(shiny.maxRequestSize = 1000 * 1024^2)
