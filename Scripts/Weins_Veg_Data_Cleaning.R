# ##############################################################################
# Title: Weins Pole Vegetation Data Cleaning
# Author: Will Harrod
# Date Created: 2026-10-13
################################################################################

################################################################################
# 1) Prep ######################################################################
################################################################################

# 1.1) Add packages and data ---------------------------------------------------

# Clear environments
rm(list = ls())

# Load Packages
library(tidyverse)
library(fs)

# 1.2) File directories (Change these for your device) -------------------------

# Directory where the validation summaries are held
csv_dir <-  "/home/will/NCSU/R_Code/Timbermill_Covariates/Data"

# Add the Data 
veg_svy_raw <- read_csv(path(csv_dir, "Timbermill_Vegetation_Form_0.csv"))
veg_dat_raw <- read_csv(path(csv_dir, "start_scores_1.csv"))

# View the data 
glimpse(veg_svy_raw)
glimpse(veg_dat_raw)

################################################################################
# 2) Clean the Surveys #########################################################
################################################################################

# View the survey data 
glimpse(veg_svy_raw)

# Initial Clean
veg_svy_clean <- veg_svy_raw |> 
  # Rename Columns 
  rename(
    Collector = `Data Collector`,
    Recorder = `Data Recorder`,
    Plot.ID = `Plot ID`,
    Point.ID = `Point ID`
         ) |> 
  # Fix some columns
  mutate(
    Date = str_remove(Date, " .*"),
    Date = mdy(Date),
    Editor = str_remove(Editor, "_NCSU"),
    Editor = case_when(
      Editor == "dbnewber" ~ "Dakota",
      Editor == "dwbarefo" ~ "Darcy",
      Editor == "wdharrod" ~ "Will",
      Editor =="bcbolton" ~ "Bryce",
      TRUE ~ Editor
    ),
    Plot.Type = str_extract(Plot.ID, "d{2}")
  ) |> 
  # Sort and name rows
  arrange(Plot.ID, Point.ID) |> 
  rowid_to_column() |> 
  # Select relevant columns
  select(
    rowid,
    Plot.ID,
    Point.ID,
    Date,
    Collector,
    Recorder,
    Editor,
    GlobalID
    )

# View again
glimpse(veg_svy_clean)

# How many points per plot?
veg_svy_clean |> 
  count(Plot.ID) |> 
  arrange(Plot.ID) |> 
  print(n = Inf)

# Who enetered the data?
veg_svy_clean |> 
  count(Editor)

# What dates were surveyed entered?
veg_svy_clean |> 
  count(Date) |> 
  arrange(Date)

# View the whole thing 
veg_svy_clean |> 
  print(n = Inf)

# Any duplicates?
veg_svy_clean |> 
  count(Plot.ID, Point.ID) |> 
  filter(n > 1)

# List of rows to remove
drop_rows <- c(1, 113, 139)

# Drop those rows and rename the camera point ID on the other duplicate
veg_svy_flt <- veg_svy_clean |> 
  filter(!rowid %in% drop_rows) |> 
  mutate(Point.ID = case_when(
    rowid == 30 ~ "CT05",
    TRUE ~ Point.ID
  ))
# View
glimpse(veg_svy_flt)

################################################################################
# 3) Clean the Veg Data ########################################################
################################################################################

# View the data 
glimpse(veg_dat_raw)

# Initial Clean
veg_dat_clean <- veg_dat_raw |> 
  # Rename Columns 
  rename(
    Hit.Number = `Measurment Number`,
    Notes = `Notes about this Measurement`
  ) |> 
  rename_with(
    .cols =  starts_with("Band"),
    .fn = ~ paste0("B", str_extract(., "\\d+"))
                ) |> 
  # Fix some columns
  mutate(
    Hit.Number = paste0("H", Hit.Number)
  ) |> 
  # Select relevant columns
  select(
    Hit.Number,
    Groundcover,
    starts_with("B"),
    ParentGlobalID
  ) |> 
  rename(GlobalID = ParentGlobalID) 
# View again
glimpse(veg_dat_clean)

# What cover classes do we see?
veg_dat_clean |> 
  pivot_longer(cols = starts_with("B"), names_to = "Band", values_to = "Cover.Type") |> 
  distinct(Cover.Type) |> 
  print(n = Inf)

################################################################################
# 4) Combine and View ##########################################################
################################################################################

# Combine the two data types 
veg_dat_full <- veg_svy_clean |> 
  right_join(veg_dat_clean, by = "GlobalID") |> 
  select(-GlobalID, - rowid) |> 
  pivot_longer(cols = starts_with("B"), names_to = "Band", values_to = "Cover.Type") |> 
  filter(!is.na(Cover.Type))
# View 
glimpse(veg_dat_full)

# What are the cover types?
veg_dat_full |> 
  distinct(Cover.Type) |> 
  print(n = Inf)

# Remove the NAs and Test Data 
veg_dat_full_flt <-  veg_dat_full |> 
  filter(
    !Cover.Type %in% c("more tests", "These are just for a test", "ahrdf", "dsG") &
    !is.na(Cover.Type)
    ) |> 
  # Clean thee cover classes
  mutate(
    Cover.Type = str_replace(Cover.Type, "w", "W"),
    Cover.Type = str_replace(Cover.Type, "v", "V"),
    Cover.Type = str_remove_all(Cover.Type, " "),
    Cover.Type = str_remove_all(Cover.Type, ","),
         )
# View
glimpse(veg_dat_full_flt)

# What are the cover types after the cleaning?
veg_dat_full_flt |> 
  distinct(Cover.Type) |> 
  print(n = Inf)

# Aggrigate accross 