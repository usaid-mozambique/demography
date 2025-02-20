# PROJECT:  
# AUTHOR:   J. Lara | USAID
# PURPOSE:  
# REF ID:   46302dc5 
# LICENSE:  MIT
# DATE:     2025-02-20
# UPDATED: 

# DEPENDENCIES ------------------------------------------------------------

library(tidyverse)
library(gagglr)
library(janitor)
library(glue)
library(readxl)


# GLOBAL VARIABLES --------------------------------------------------------


# FUCTIONS ----------------------------------------------------------------

process_wpp <- function(file, sheet) {
  
  df <- read_excel(path = file,  ### Corrected argument
                   sheet = sheet,  ### Fixed reference
                   skip = 16) |> 
    filter(Type == "Country/Area") |> 
    pivot_longer(cols = `0`:`100+`, 
                 names_to = "age", 
                 values_to = "value") |> 
    select(c(Variant, 
             country = `Region, subregion, country or area *`, 
             Year, 
             age, 
             value)) |> 
    clean_names() |> 
    filter(country != "NOTES")  ### Remove "NOTES" from the country column
  
  return(df)
}



process_all_sheets <- function(file) {
  sheets <- excel_sheets(file)  # Get all sheet names
  sheets <- setdiff(sheets, "NOTES")
  sheet_data <- map(sheets, ~ process_wpp(file, .x), .progress = TRUE)  # Apply function to each sheet
  names(sheet_data) <- sheets  # Assign sheet names to the list
  return(sheet_data)  # Returns a list of data frames
  
}


# LOAD DATA ------------------------------------------------------------------
  
  df <- read_excel("Data/WPP2024_POP_F01_1_POPULATION_SINGLE_AGE_BOTH_SEXES.xlsx", 
                   sheet = "Estimates", skip = 16)

# MUNGE -------------------------------------------------------------------
  
  df1 <- df |> 
    filter(Type == "Country/Area") |> 
    pivot_longer(cols = `0`:`100+`, names_to = "age", values_to = "value") |> 
    select(c(Variant, 
             country = `Region, subregion, country or area *`, 
             Year, 
             age, 
             value)) |> 
    clean_names()

# VISUALIZE ---------------------------------------------------------------

df1 |> 
    filter(country == "Mozambique") |> 
    mutate(value = as.numeric(value)) |> 
    group_by(year) |>
    summarize(value = sum(value), .groups = 'drop') |> 
    ggplot(aes(x = year, y = value)) +
    geom_line()
  

# INTERATE ----------------------------------------------------------------

  file_path <- "Data/WPP2024_POP_F01_1_POPULATION_SINGLE_AGE_BOTH_SEXES.xlsx"
  result_list <- process_all_sheets(file_path)
  final_df <- bind_rows(result_list, .id = "sheet_name")
  
  write_csv(final_df,
            "Dataout/wpp_population.csv",
            na = "")
  