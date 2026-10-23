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
csv_dir <-  "/home/will/NCSU/R_Code/Covariate_Management/Data"

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
      TRUE ~ Editor
    ),
    Plot.Type = str_extract(Plot.ID, "d{2}")
  ) |> 
  # Select relevant columns
  select(
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
  arrange(Plot.ID, Point.ID) |> 
  print(n = Inf)


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
  select(-GlobalID) |> 
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
    )
# View
glimpse(veg_dat_full_flt)

# Aggrigate accross 