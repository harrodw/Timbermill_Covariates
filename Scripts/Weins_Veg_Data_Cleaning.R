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
library(tidytext)

# 1.2) File directories (Change these for your device) -------------------------

# Directory where the validation summaries are held
csv_dir <-  "/home/will/NCSU/R_Code/Timbermill_Covariates/Data"

# View the available data
basename(dir_ls(csv_dir))

# Add the Data 
veg_svy_raw <- read_csv(path(csv_dir, "Timbermill_Vegetation_Form_0.csv"))
veg_dat_raw <- read_csv(path(csv_dir, "start_scores_1.csv"))

# View the data 
glimpse(veg_svy_raw)
glimpse(veg_dat_raw)

################################################################################
# 2) Clean the Surveys #########################################################
################################################################################

# 2.1) Clean the survey data ---------------------------------------------------

# View the survey data 
glimpse(veg_svy_raw)

# Initial Clean
veg_svy_clean <- veg_svy_raw |> 
  # Rename Columns 
  rename(
    Collector = `Data Collector`,
    Recorder = `Data Recorder`,
    plot.id = `Plot ID`,
    point.id = `Point ID`
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
    Plot.Class = str_extract(plot.id, "d{2}")
  ) |> 
  # Sort and name rows
  arrange(plot.id, point.id) |> 
  rowid_to_column() |> 
  # Select relevant columns
  select(
    rowid,
    plot.id,
    point.id,
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
  count(plot.id) |> 
  arrange(plot.id) |> 
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
  count(plot.id, point.id) |> 
  filter(n > 1)

# List of rows to remove
drop_rows <- c(1, 113, 139)

# Drop those rows and rename the camera point ID on the other duplicate
veg_svy_flt <- veg_svy_clean |> 
  filter(!rowid %in% drop_rows) |> 
  mutate(point.id = case_when(
    rowid == 30 ~ "CT05",
    TRUE ~ point.id
  ))
# View
glimpse(veg_svy_flt)

# 2.2) Combine with the camera trap updated names ------------------------------

# Read in the camera trap data 
cam_trp_meta_raw <- read_csv(path(csv_dir, "camera_trap_metadata_2026.csv"))
# View
glimpse(cam_trp_meta_raw)
cam_trp_meta_raw |> count(plot.id, plot.type) |> print(n = Inf)

# Clean the metadata
cam_trp_meta <-  cam_trp_meta_raw |> 
  rename(new.plot.id = plot.id) |> 
  # Calculate the old names 
  mutate(
    plot.id = str_replace(new.plot.id, "RO", "RE"),
    point.id = str_remove(point.id, "[A-Z]{2}\\d{2}_")
  ) |> 
  # select the columns we need
  select(plot.id, new.plot.id, point.id, plot.type, plot.id, x, y)
  
# View after the cleaning 
glimpse(cam_trp_meta)
cam_trp_meta |> count(plot.type) |> print(n = Inf)
cam_trp_meta |> count(plot.id, new.plot.id) |> print(n = Inf)
cam_trp_meta |> count(plot.id, point.id) |> filter(n > 1)

# Join with the veg 
veg_svy_info <- veg_svy_flt |> 
  left_join(cam_trp_meta, by = c("plot.id", "point.id")) |> 
  mutate(
  new.plot.id = case_when(
    plot.type == "Reference Opening" ~ str_replace(plot.id, "RE", "RO"),
    TRUE ~ plot.id
    )) |> 
  select(-plot.id) |> 
  rename(plot.id = new.plot.id)
# View
glimpse(veg_svy_info)

################################################################################
# 3) Clean the Veg Data ########################################################
################################################################################

# View the data 
glimpse(veg_dat_raw)

# Initial Clean
veg_dat_clean <- veg_dat_raw |> 
  # Rename Columns 
  rename(
    Drop.Number = `Measurment Number`,
    Notes = `Notes about this Measurement`
  ) |> 
  rename_with(
    .cols =  starts_with("Band"),
    .fn = ~ paste0("B", str_extract(., "\\d+"))
                ) |> 
  # Fix some columns
  mutate(
    Drop.Number = paste0("D", Drop.Number)
  ) |> 
  # Select relevant columns
  select(
    Drop.Number,
    Groundcover,
    starts_with("B"),
    ParentGlobalID
  ) |> 
  rename(GlobalID = ParentGlobalID) 
# View again
glimpse(veg_dat_clean)

# What cover classes do we see?
veg_dat_clean |> 
  pivot_longer(cols = starts_with("B"), names_to = "Band", values_to = "Cover.Class") |> 
  distinct(Cover.Class) |> 
  print(n = Inf)

################################################################################
# 4) Combine and View ##########################################################
################################################################################

# 4.1) Combine surveys and hits and clean --------------------------------------

# Combine the two data types 
veg_dat_full <- veg_svy_info |> 
  right_join(veg_dat_clean, by = "GlobalID") |> 
  select(-GlobalID, - rowid) |> 
  pivot_longer(cols = starts_with("B"), names_to = "Band", values_to = "Cover.Class") 
  # filter(!is.na(Cover.Class))
# View 
glimpse(veg_dat_full)

# See some random band cover classes 
veg_dat_full |> 
  select(point.id, Drop.Number, Band, Cover.Class, Groundcover) |> 
  slice_sample(n = 50) |> 
  print(n = Inf)

# What are the cover types?
veg_dat_full |> 
  distinct(Cover.Class) |> 
  print(n = Inf)

# Remove the NAs and Test Data 
veg_dat_full_flt <-  veg_dat_full |> 
  filter(!Cover.Class %in% c("more tests", "These are just for a test", "ahrdf", "dsG", "10")) |> 
  # Clean thee cover classes
  mutate(
    Cover.Class = replace_na(Cover.Class, "N"),
    Cover.Class = str_replace(Cover.Class, "w", "W"),
    Cover.Class = str_replace(Cover.Class, "v", "V"),
    Cover.Class = str_remove_all(Cover.Class, " "),
    Cover.Class = str_remove_all(Cover.Class, ","),
         ) 
  
# View
glimpse(veg_dat_full_flt)

# What are the cover types after the cleaning?
veg_dat_full_flt |> 
  count(Cover.Class) |> 
  arrange(-n) |> 
  print(n = Inf)


# 4.2) Make a dataframe for the percent cover in each cover class, then combine-------
# N = nothing
# W = woody
# S = switchcane
# F = forb
# G = grass
# V = vine
# W = standing water
# D = dead wood
# L = litter

# View again
glimpse(veg_dat_full_flt)

# Function to calculate cover 
cover_cal <- function(dat, cover_class){

  # Find the total number of Hits at each point
  hit_count <- dat |> 
    distinct(plot.id, point.id, plot.type, Date, Drop.Number, x, y) |> 
    group_by(plot.id, point.id, plot.type, Date, x, y) |> 
    summarise(n.drops = n(), .groups = "drop") |> 
    filter(n.drops >= 10)
  
  # find the number of hits positive for that cover class 
  cvr <- dat |> 
    filter(str_detect(Cover.Class, cover_class)) |> 
    distinct(plot.id, point.id, plot.type, Date, Drop.Number, x, y) |> 
    group_by(plot.id, point.id, plot.type, Date, x, y) |> 
    summarise(n.cvr = n(), .groups = "drop") |> 
    rename_with(
      ~ paste0(cover_class, ".cvr", recycle0 = TRUE),
        starts_with("n.cvr"),
    ) 
  
  # Combine and calculate 
  cvr_pct <- hit_count |> 
    left_join(cvr, by = c("plot.id", "point.id", "plot.type", "Date", "x", "y")) |> 
    mutate(across(.cols = ends_with(".cvr"), .fns = ~ replace_na(., 0))) |> 
    mutate(across(.cols = ends_with(".cvr"), .fns = ~ . / n.drops))

}


# calculate forb cover
wood_cvr <- cover_cal(dat = veg_dat_full_flt, cover_class = "W")
forb_cvr <- cover_cal(dat = veg_dat_full_flt, cover_class = "F")
grass_cvr <- cover_cal(dat = veg_dat_full_flt, cover_class = "G")
cane_cvr <- cover_cal(dat = veg_dat_full_flt, cover_class = "S")
vine_cvr <- cover_cal(dat = veg_dat_full_flt, cover_class = "V")
# View one
glimpse(wood_cvr)
wood_cvr |> count(plot.id, point.id) |> filter(n > 1)

# Groundcover ------------------------------------------------------------------

# What is the groundcover breakdown
veg_dat_full_flt |> count(Groundcover)

# Calculate most common groundcover
ground_cvr <- veg_dat_full_flt |> 
  distinct(plot.id, point.id, plot.type, Date, Drop.Number, Groundcover, x, y) |> 
  group_by(plot.id, point.id, plot.type, Date, x, y) |>
  count(Groundcover, sort = TRUE) |> 
  slice(1) |> 
  ungroup() |> 
  select(-n) |> 
  rename(gc = Groundcover)
# View
glimpse(ground_cvr)

# Calculate percent standing water  --------------------------------------------

# Find the total number of Hits at each point
hit_count <- veg_dat_full_flt |> 
  distinct(plot.id, point.id, plot.type, Date, Drop.Number, x, y) |> 
  group_by(plot.id, point.id, plot.type, Date, x, y) |> 
  summarise(n.drops = n(), .groups = "drop") |> 
  filter(n.drops >= 10)

veg_dat_full_flt |> count(Groundcover)

# find the number of hits positive for that cover class 
h2o_cvr <- veg_dat_full_flt |> 
  # Convert the bareground under the turbines to water
  mutate(Groundcover = case_when(Groundcover == "B" & str_detect(plot.id, "TO") ~ "W",
                                 TRUE ~ Groundcover)) |> 
  filter(str_detect(Groundcover, "W")) |> 
  distinct(plot.id, point.id, plot.type, Date, Drop.Number, x, y) |> 
  group_by(plot.id, point.id, plot.type, Date, x, y) |> 
  summarise(h2o.cvr = n(), .groups = "drop") 

# Combine and calculate 
h2o_pct <- hit_count |> 
  left_join(h2o_cvr, by = c("plot.id", "point.id", "plot.type", "Date", "x", "y")) |> 
  mutate(h2o.cvr = replace_na(h2o.cvr, 0)) |> 
  mutate(h2o.cvr = h2o.cvr / n.drops)
# View
glimpse(h2o_pct)
hist(h2o_pct$h2o.cvr)

# --------------------------------------------------------------------------

# Calculate percent of drops with no hits --------------------------------------

# Find the drops with no cover in any class
blank_cvr <- veg_dat_full_flt |> 
  mutate(bare = case_when(Cover.Class == "N" ~ 0, TRUE ~ 1)) |> 
  distinct(plot.id, point.id, bare, Cover.Class, plot.type, Date, Drop.Number, x, y) |> 
  group_by(plot.id, point.id, plot.type, Drop.Number, Date, x, y) |> 
  mutate(bare.total = sum(bare)) |> 
  ungroup() |> 
  filter(bare.total < 1) |> 
  select(-bare, -bare.total) |> 
  group_by(plot.id, point.id, plot.type, Date, x, y) |> 
  summarise(N.cvr = n(), .groups = "drop") |> 
  right_join(hit_count, by = c("plot.id", "point.id", "plot.type", "Date", "x", "y")) |> 
  mutate(N.cvr = N.cvr / n.drops)
# View
glimpse(blank_cvr)

# ------------------------------------------------------------------------------

# Function to join data 
join_cvr <- function(dat1, dat2){
  joined_cvr <-  left_join(dat1, dat2, by = c("plot.id", "point.id", "plot.type", "Date", "n.drops", "x", "y"))
  return(joined_cvr)
}

# Combine
full_cvr_all <- wood_cvr |> 
  join_cvr(forb_cvr) |> 
  join_cvr(grass_cvr) |>
  join_cvr(cane_cvr) |> 
  join_cvr(vine_cvr) |>
  join_cvr(blank_cvr) |> 
  join_cvr(h2o_pct) |> 
  left_join(ground_cvr, by = c("plot.id", "point.id", "plot.type", "Date", "x", "y")) |> 
  rename(date = Date) |> 
  select(plot.id, point.id, plot.type, date, n.drops, ends_with(".cvr"), gc, x, y)

# View
glimpse(full_cvr_all)
full_cvr_all |> filter(is.na(plot.id))
full_cvr_all |> filter(is.na(plot.type))

# Assign the corect plot type to the missing plots 
full_cvr_plt_typ <- full_cvr_all |> 
  mutate(
    plot.type = case_when(
      is.na(plot.type) & str_detect(plot.id, "IF") ~ "Interior Forest",
      is.na(plot.type) & str_detect(plot.id, "RE") ~ "Reference Edge",
      is.na(plot.type) & str_detect(plot.id, "RO") ~ "Reference Opening",
      is.na(plot.type) & str_detect(plot.id, "TE") ~ "Turbine Edge",
      is.na(plot.type) & str_detect(plot.id, "TO") ~ "Turbine Opening",
      TRUE ~ plot.type
    )) |> 
  mutate(across(.cols = ends_with(".cvr"), .fns = ~ replace_na(., 0)))
# View
glimpse(full_cvr_plt_typ)
full_cvr_plt_typ |> filter(is.na(plot.id))
full_cvr_plt_typ |> filter(is.na(plot.type))

# ----------------------------------------------------------------------------
# FLAGGED! Drop the surveyes of points where we did not collect data ---------
# ----------------------------------------------------------------------------
full_cvr <-  full_cvr_plt_typ |>  filter(
  !is.na(plot.id) 
  # & !is.na(plot.type)
  )

# Save 
write_csv(full_cvr, path(csv_dir, "percent_cover.csv"))

################################################################################
# 5) Plots #####################################################################
################################################################################

# Path to the figures
fig_path <- "/home/will/NCSU/Figures"

# 5.1) average percent cover ---------------------------------------------------

# Palette
plot_pal <- c(
  "Interior Forest" = "forestgreen", 
  "Reference Edge" = "darkolivegreen2", 
  "Reference Opening" = "goldenrod3",
  "Turbine Edge" = "violetred4",
  "Turbine Opening" = "darkorchid4"
)


# Prep the data for plotting
veg_pct_plt_dat <- full_cvr |> 
  pivot_longer(cols = ends_with("cvr"), names_to = "cvr.class", values_to = "point.cvr") |> 
  group_by(plot.id, plot.type, cvr.class) |> 
  summarise(plot.cvr = mean(point.cvr), .groups = "drop") |> 
  mutate(
    plot.id.cvr = paste(plot.id, cvr.class, sep = "."),
    plot.id.cvr = factor(plot.id.cvr),
    plot.id.cvr = fct_reorder(plot.id.cvr, plot.cvr, .desc = TRUE, .na_rm = TRUE),
    cvr.class = case_when(
      cvr.class == "F.cvr" ~ "Forbs",
      cvr.class == "G.cvr" ~ "Grass",
      cvr.class == "W.cvr" ~ "Woody",
      cvr.class == "S.cvr" ~ "Switchcane",
      cvr.class == "V.cvr" ~ "Vine",
      cvr.class == "N.cvr" ~ "No Vegetation",
      cvr.class == "h2o.cvr" ~ "Water",
      TRUE ~ cvr.class
    )) |> 
  arrange(plot.id) |> 
  # OPTIONAL: Remove a cover class
  filter(!cvr.class %in% c("Water"))
  
# View the data 
glimpse(veg_pct_plt_dat)
veg_pct_plt_dat |> count(plot.id) |>  print(n = Inf)
veg_pct_plt_dat |> count(plot.type) |>  print(n = Inf)
veg_pct_plt_dat |> filter(cvr.class == "No Vegetation") |> print(n = 100)

# Make a plot of average cover at each plot type 
veg_pct_plt <- veg_pct_plt_dat |> 
  ggplot() +
  geom_col(aes(x = plot.id.cvr, y = plot.cvr, fill = plot.type, col = plot.type)) +
  scale_color_manual(values = plot_pal) +
  scale_fill_manual(values = plot_pal) +
  scale_y_continuous(
    labels = scales::percent,
    expand = expansion(mult = c(0, 0.05))
  ) +
  labs(
    fill = "Plot Type",
    color = "Plot Type",
    y = "Percent Cover",
    x = "Individual Sensor Location"
  ) +
  theme_classic() +
  theme(
    plot.title = element_blank(),
    axis.title.y = element_text(size = 16),
    axis.title.x = element_text(size = 16),
    axis.text.x = element_blank(),
    axis.text.y = element_text(size = 14),
    legend.text = element_text(size = 16),
    legend.position = "right",
    legend.title = element_text(size = 16),
    strip.text = element_text(size = 14),
  ) +
  facet_wrap(vars(cvr.class), scales = "free_x")

# View 
veg_pct_plt

# Save the plot
ggsave(
  veg_pct_plt,
  file = path(fig_path, "veg_cover_plot.png"),
  width = 200,
  height = 150,
  units = "mm",
  dpi = 300)

# 5.1) Veg cover Pie Chart -----------------------------------------------------

# Palette
veg_pal <- c(
  "Forbs" = "forestgreen", 
  "Grass" = "goldenrod",
  "Switchcane" = "yellow4",
  "Vine" = "darkolivegreen2",
  "Woody" = "chocolate4",
  "No Vegetation" = "gray75"
  )

# Prep the data for plotting
veg_pct_pie_dat <- full_cvr |> 
  pivot_longer(cols = ends_with("cvr"), names_to = "cvr.class", values_to = "point.cvr") |> 
  filter(cvr.class != "h2o.cvr") |> 
  group_by(plot.type, cvr.class) |> 
  summarise(mean.plt.cvr = mean(point.cvr), .groups = "drop") |> 
  group_by(plot.type) |> 
  mutate(mean.plt.cvr = mean.plt.cvr / sum(mean.plt.cvr)) |> 
  ungroup() |> 
  mutate(
    plot.type.cvr = paste(plot.type, cvr.class, sep = "."),
    cvr.class = case_when(
      cvr.class == "F.cvr" ~ "Forbs",
      cvr.class == "G.cvr" ~ "Grass",
      cvr.class == "W.cvr" ~ "Woody",
      cvr.class == "S.cvr" ~ "Switchcane",
      cvr.class == "V.cvr" ~ "Vine",
      cvr.class == "N.cvr" ~ "No Vegetation",
      TRUE ~ cvr.class
    )) 

# View the data 
glimpse(veg_pct_pie_dat)
veg_pct_pie_dat |> print(n = Inf)

# Make the pie chart
veg_pct_pie <-  veg_pct_pie_dat |> 
  # filter(mean.plt.cvr > 0) |> 
  ggplot(aes(x = "", y = mean.plt.cvr, fill = cvr.class, col = cvr.class)) +
  geom_col(width = 1) +
  scale_color_manual(values = veg_pal) +
  scale_fill_manual(values = veg_pal) +
  coord_polar("y", start = 0) +
  # labs(title = "Average Vegetation By Plot Type") +
  guides(fill = guide_legend(ncol = 1, nrow = 5)) +
  labs(
    fill = "Cover Class",
    color = "Cover Class"
  ) +
  theme_classic() +
  theme(
    plot.title = element_blank(),
    axis.title.y = element_blank(),
    axis.title.x = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    legend.text = element_text(size = 14),
    legend.position = "right",
    legend.title = element_text(size = 14),
    strip.text = element_text(size = 12),
  ) +
  facet_wrap(vars(plot.type), scales = "free_x")

# View 
veg_pct_pie

# Save the plot
ggsave(
  veg_pct_pie,
  file = path(fig_path, "veg_cover_piechart.png"),
  width = 200,
  height = 125,
  units = "mm",
  dpi = 300)
