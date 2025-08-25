# server.R
library(shiny)
library(readxl)
library(tidyverse)
library(mclust)
library(moments)
library(shinyjs)
library(car)
library(refineR)
library(shinyFiles)
library(shinyWidgets)
library(bslib)
library(ggplot2)
library(future)
library(future.apply)

# Set up parallel processing plan once for efficiency.
plan(multisession)

# A shared utility function to guess column names based on common keywords.
# This avoids code duplication across the different server modules.
guess_column <- function(cols_available, common_names) {
  for (name in common_names) {
    match_idx <- grep(paste0("^", name, "$"), cols_available, ignore.case = TRUE)
    if (length(match_idx) > 0) {
      return(cols_available[match_idx[1]])
    }
  }
  return("")
}

# A shared utility function to filter data based on gender and age ranges.
# This function is now used by both the Main and Parallel Analysis modules.
filter_data <- function(data, gender_choice, age_min, age_max, col_gender, col_age) {
  if (col_age == "") {
    stop("Age column not found in data.")
  }
  
  filtered_data <- data %>%
    filter(!!rlang::sym(col_age) >= age_min & !!rlang::sym(col_age) <= age_max)
  
  if (col_gender != "" && col_gender %in% names(data)) {
    filtered_data <- filtered_data %>%
      mutate(Gender_Standardized = case_when(
        grepl("male|m|man|jongen(s)?|heren|mannelijk(e)?", !!rlang::sym(col_gender), ignore.case = TRUE) ~ "Male",
        grepl("female|f|vrouw(en)?|v|meisje(s)?|dame|mevr|vrouwelijke", !!rlang::sym(col_gender), ignore.case = TRUE) ~ "Female",
        TRUE ~ "Other"
      ))
    
    if (gender_choice != "Both") {
      filtered_data <- filtered_data %>%
        filter(Gender_Standardized == case_when(
          gender_choice == "M" ~ "Male",
          gender_choice == "F" ~ "Female",
          TRUE ~ NA_character_
        )) %>%
        filter(!is.na(Gender_Standardized))
    }
  } else {
    if (gender_choice %in% c("M", "F")) {
      return(data[FALSE, ])
    } else {
      filtered_data <- filtered_data %>%
        mutate(Gender_Standardized = "Combined")
    }
  }
  
  return(filtered_data)
}

source("server_main.R")
source("server_gmm.R")
source("server_parallel.R")

server <- function(input, output, session) {
  
  # --- Reactive Values for State Management ---
  data_reactive <- reactiveVal(NULL)
  gmm_uploaded_data_rv <- reactiveVal(NULL)
  gmm_processed_data_rv <- reactiveVal(NULL)
  gmm_transformation_details_rv <- reactiveVal(list(male_hgb_transformed = FALSE, female_hgb_transformed = FALSE))
  selected_dir_reactive <- reactiveVal(NULL)
  message_rv <- reactiveVal(list(type = "", text = ""))
  analysis_running_rv <- reactiveVal(FALSE)
  parallel_data_rv <- reactiveVal(NULL)
  parallel_results_rv <- reactiveVal(list())
  parallel_message_rv <- reactiveVal(list(type = "", text = ""))
  
  # Renders alert-style messages
  renderMessageUI <- function(rv) {
    renderUI({
      msg <- rv()
      if (is.null(msg) || msg$text == "") {
        return(NULL)
      }
      class_name <- switch(msg$type,
                           "error" = "alert alert-danger",
                           "success" = "alert alert-success",
                           "warning" = "alert alert-warning",
                           "info" = "alert alert-info",
                           "alert alert-secondary")
      div(class = class_name, msg$text)
    })
  }
  
  output$app_message <- renderMessageUI(message_rv)
  output$main_message <- renderMessageUI(message_rv)
  output$parallel_message <- renderMessageUI(parallel_message_rv)
  
  # Observer that prevents switching tabs when an analysis is running
  observeEvent(input$tabs, {
    if (!analysis_running_rv()) {
      message_rv(list(type = "", text = ""))
    } else {
      message_rv(list(text = "Tab switch blocked: An analysis is currently running. Please wait or reset the analysis.", type = "warning"))
    }
  })
  
  observeEvent(input$tab_switch_blocked, {
    if (analysis_running_rv()) {
      message_rv(list(text = "Cannot switch tabs while an analysis is running. Please wait or reset the analysis.", type = "warning"))
    }
  })
  
  mainServer(input, output, session, data_reactive, selected_dir_reactive, message_rv, analysis_running_rv)
  gmmServer(input, output, session, gmm_uploaded_data_rv, gmm_processed_data_rv, gmm_transformation_details_rv, message_rv, analysis_running_rv)
  parallelServer(input, output, session, parallel_data_rv, parallel_results_rv, parallel_message_rv, analysis_running_rv)
}