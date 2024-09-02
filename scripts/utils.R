# Load packages
library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(bslib)
library(bsicons)
library(shinycssloaders)
library(readxl)
library(dplyr)
library(tidyr)
library(zoo)
library(baseline)
library(MESS)
library(pracma) 
library(ggpmisc) 
library(ggrepel)
library(ggplot2)
library(plotly)
library(ggpubr)
library(openxlsx)




#assign upload data to data frame
# Define a function to load and process the data
load_and_process_data <- function(file_path) {

  raw_data <- read_excel(file_path, col_names = TRUE)
  
  # updates names in case the template is wrong
  colnames(raw_data) <- c("Date", "Date", "Label", "NO", "Unit")
  
  #remove unused columns
  raw_data <- raw_data %>% 
    dplyr::select(NO)
  
  #add time column with sampling rate of 4Hz
  raw_data$Time <- seq(0, (nrow(raw_data)/4-0.25), 0.25)
  
  #add time column with sampling rate of 4Hz
  raw_data$Time_Min <- as.numeric(raw_data$Time/60)
  
  # Return the processed data frame
  return(raw_data)
}

