# PROJECT:  
# AUTHOR:   J. Lara | USAID
# PURPOSE:  
# REF ID:   749cb784 
# LICENSE:  MIT
# DATE:     2024-10-01
# UPDATED: 

# DEPENDENCIES ------------------------------------------------------------


library(tidyverse)
library(sismar)
library(janitor)
library(glue)
library(openxlsx)
library(readxl)
library(fs)
library(googlesheets4)
library(glamr)
load_secrets()


# GLOBAL VARIABLES --------------------------------------------------------


input_folder <- "Data/population"
input_sheets <- c(as.character(c(2023:2050)))

ref_id <- "749cb784"


# LOAD DATA ------------------------------------------------------------------


file_inventory <- dir(input_folder,
                      full.name = TRUE,
                      pattern = "*.xlsx")


# MUNGE -------------------------------------------------------------------


df <- process_pop_ine(file_inventory, 
                      input_sheets, 
                      age_level = "Exact")


# WRITE OUT -------------------------------------------------------------------


write_csv(df, 
          glue("Dataout/ine_demproj_corrected.csv")
)
