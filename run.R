# Точка входа: запуск проекта DFI

## Установим рабочую директорию на папку проекта (где лежит этот файл)
this_file <- tryCatch(normalizePath(sys.frame(1)$ofile), error = function(e) NA)
if (!is.na(this_file)) {
  setwd(dirname(this_file))
}

options(repos = c(CRAN = "https://cloud.r-project.org"))

# ensure required packages are installed
ensure_packages <- function(pkgs) {
  installed <- rownames(installed.packages())
  to_install <- setdiff(pkgs, installed)
  if (length(to_install) > 0) {
    install.packages(to_install, dependencies = TRUE)
  }
}

required_packages <- c(
  "readr","lubridate","dplyr","data.table","ggplot2","stringr","rsample",
  "scorecard","ModelMetrics","modelsummary","tidypredict","precrec","caret","tornado",
  "tidyverse","bootStepAIC","readxl","glmnet","openxlsx","doParallel","foreach","car","httr2","jsonlite"
)

ensure_packages(required_packages)

# 1) Загрузить конфиг
source('config.R')

# 2) Создать папки для данных и результатов
if (!dir.exists('data')) dir.create('data', recursive = TRUE)
if (!dir.exists(OUTPUT_DIR)) dir.create(OUTPUT_DIR, recursive = TRUE)

# 3) Запустить основной скрипт (использует относительные пути)
source(file.path('src', 'DFI_v2.R'))


