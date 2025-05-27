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
library(ggthemes)
library(scales)


# GLOBAL VARIABLES --------------------------------------------------------

file_path <- "Data/WPP2024_POP_F01_1_POPULATION_SINGLE_AGE_BOTH_SEXES.xlsx"

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


# PROCESS -----------------------------------------------------------------


df <- process_pop_wpp(file = file_path, geo = "Mozambique")


# MUNGE -------------------------------------------------------------------


df1 <- df %>% 
  filter(between(periodo, 2023, 2075),
         variante %in% c(
           "Low",
           "Medium",
           "High",
           "No change"
         )) %>% 
  mutate(valor = as.numeric(valor),
         periodo = as.integer(periodo),
         variante = factor(variante, levels = c("No change", "High", "Medium", "Low"))) %>% 
  group_by(variante, periodo) %>% 
  summarize(valor = sum(valor), .groups = "drop") %>% 
  mutate(valor_k = round(valor / 1000, 1))


df_last <- df1 %>%
  filter(periodo == max(periodo))

# VISUALIZE ---------------------------------------------------------------

ggplot(df1, aes(periodo, valor_k, color = variante)) +
  geom_line(linewidth = 1) +
  geom_text(
    data = df_last,
    aes(label = variante),
    hjust = -0.1,
    vjust = -.7,
    size = 3.5
  ) +
  geom_text(
    data = df_last,
    aes(label = valor_k),
    hjust = -0.15,
    vjust = .7,
    size = 3.5
  ) +
  xlim(min(df1$periodo), max(df1$periodo) + 1) + # Give space for label
  theme_fivethirtyeight() +
  scale_x_continuous(
    limits = c(min(df1$periodo), max(df1$periodo) + 4),
    breaks = seq(2025, 2080, by = 5)
  ) +
  scale_y_continuous(
    limits = c(30, 150),
    breaks = seq(30, 150, by = 10)
  ) +
  labs(
    title = "Mozambique Demographic Projections (2024-2075)",
    subtitle = "The most conservative WPP model predicts that the Mozambique population will \ndouble by 2075. If current demographic patterns persist, the population will \nquadrupal in size.",
    caption = "Source: UN 2024 World Population Prospects (https://population.un.org/wpp/)",
    x = "Year",
    y = "Population (millions)"
  ) +
  theme(
    plot.caption = element_text(hjust = 0, face = "italic"),
    legend.position = "none",
    legend.direction = "vertical",
    axis.title.y = element_text(size = 12),
    axis.title.x = element_text(size = 12),
    axis.text.x = element_text(size = 10.5, angle = 45, hjust = 1)
  )
