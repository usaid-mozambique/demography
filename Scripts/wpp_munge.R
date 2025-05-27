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


parse_pop_wpp <- function(file, sheet, geo) {
  
  df <- readxl::read_excel(path = file,
                           sheet = sheet,
                           skip = 16)
  
  df <- df |>
    dplyr::filter(Type == "Country/Area") |>
    tidyr::pivot_longer(cols = `0`:`100+`,
                        names_to = "age",
                        values_to = "value") |>
    dplyr::select(c(pais = `Region, subregion, country or area *`,
                    pais_iso = `ISO3 Alpha-code`,
                    variante = Variant,
                    periodo = Year,
                    idade = age,
                    valor = value)) |>
    # janitor::clean_names() |>
    dplyr::filter(pais == geo,
                  pais != "NOTES")
  
  return(df)
  
}

process_pop_wpp <- function(file, geo) {
  # Check if the file exists
  if (!file.exists(file)) {
    stop("File does not exist.")
  }
  
  # Get all sheet names
  sheets <- readxl::excel_sheets(file)

  # Remove "NOTES" sheet if it exists
  sheets <- setdiff(sheets, "NOTES")
  # print(paste("Sheet names to process:", paste(sheets, collapse = ", ")))
  
  # Check if there are any sheets left to process
  if (length(sheets) == 0) {
    stop("No sheets to process.")
  }
  
  # Apply function to each sheet
  sheet_data <- purrr::map(sheets, ~ {
    print(paste("Processing sheet:", .x))
    parse_pop_wpp(file, .x, geo)
  })
  
  # Combine all data frames into one
  combined_df <- dplyr::bind_rows(sheet_data, .id = "sheet_name")
  
  # Return the combined data frame
  return(combined_df)
}


# Example usage
file_path <- "Data/WPP2024_POP_F01_1_POPULATION_SINGLE_AGE_BOTH_SEXES.xlsx"
df <- process_pop_wpp(file = file_path, geo = "Mozambique")








# MUNGE -------------------------------------------------------------------
  
df <- readxl::read_excel(path = file_path,
                         sheet = "Estimates",
                         skip = 16)

  df1 <- df |> 
    filter(Type == "Country/Area") |> 
    pivot_longer(cols = `0`:`100+`, names_to = "age", values_to = "value") |> 
    select(c(Variant, 
             country = `Region, subregion, country or area *`, 
             Year, 
             age, 
             value)) |> 
  filter(country == "Mozambique") |> 
    clean_names()

# VISUALIZE ---------------------------------------------------------------

df1 |> 
    filter(country == "Mozambique") |> 
    mutate(value = as.numeric(value)) |> 
    group_by(year) |>
    summarize(value = sum(value), .groups = 'drop') |> 
    ggplot(aes(x = year, y = value)) +
    geom_line()
  

# ITERATE ----------------------------------------------------------------

  file_path <- "Data/WPP2024_POP_F01_1_POPULATION_SINGLE_AGE_BOTH_SEXES.xlsx"
  result_list <- process_pop_wpp(file_path, geo = "Mozambique")
  final_df <- bind_rows(result_list, .id = "sheet_name")
  
  write_csv(final_df,
            "Dataout/wpp_population.csv",
            na = "")
  