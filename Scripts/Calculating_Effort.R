################################################################################
# Title: Sensor Effort
# Creator: Will Harrod
# Created: 2026-07-23
# Description: Calculating the number of survey days for camera traps and ARUs
################################################################################

################################################################################
# 1: Prep ######################################################################
################################################################################

# Clear environments
rm(list = ls())

# Add packages
library(tidyverse)
library(fs)

# Directories
bird_frog_dir <- "G:/Bird_Anuran_ARUs"
bat_dir <- "G:/Bat_ARUs"

################################################################################
# 2: Bird and Anuran ARU Effort ################################################
################################################################################

# List the ARUS
aru_ls <- dir_ls(bird_frog_dir)
n_aru <- length(aru_ls)

# View
aru_ls
n_aru

# View info for the first aru
aru_info <- aru_ls[4] |>
  dir_info() |>
  mutate(file = basename(path)) |>
  mutate(file.name = str_remove_all(file, "SMM2-\\d{2}_")) |>
  mutate(file.name = str_remove_all(file.name, "\\.wav")) |>
  mutate(file.name = str_remove_all(file.name, "_")) |> 
  mutate(date.time = parse_date_time(file.name, orders = "ymd%H%M%S")) |> 
  select(date.time, size, file)

# View
glimpse(aru_info)
head(aru_info)
# what is the range of file sizes
aru_info |> 
  count(size) |> 
  arrange(size) |> 
  mutate(size = as.numeric(str_remove_all(as.character(size), "M")))

# Large recordings are over 100 MB
lrg_fil_sze <-  fs_bytes("100M")

# Blank data frame to store the number of morning recordings 
rec_info <- tibble(
)

# Loop over the ARUs and extract the relevant information
for(i in 1:n_aru){

  # Define a single ARU
  aru <- aru_ls[i]
  plot <- basename(aru)
  
  # List the recordings made by that ARU by size and extract the important information
  recs <- aru|>
    dir_info() |>
    mutate(file = basename(path),
           Plot = plot) |>
    mutate(file.name = str_remove_all(file, "SMM2-\\d{2}_")) |>
    mutate(file.name = str_remove_all(file.name, "\\.wav")) |>
    mutate(file.name = str_remove_all(file.name, "_")) |> 
    mutate(date.time = parse_date_time(file.name, orders = "ymd%H%M%S")) |> 
    # mutate(size = as.numeric(str_remove_all(as.character(size), "M"))) |> 
    mutate(Morning = case_when(
      size > lrg_fil_sze ~ 1,
      size <= lrg_fil_sze ~ 0
    )) |> 
    select(Plot, date.time, Morning, size, file)
  
  # Combine with the others
  rec_info <- bind_rows(rec_info, recs)
  
  # Message
  message("Finished extracting info for  ", plot, " Plot ", i, " out of ", n_aru)
}

# View
glimpse(rec_info)

# Where are the NA's?
rec_info |> 
  filter(if_any(everything(), is.na))

# Counts of morning vs night surveys
rec_info |> 
  count(Morning)

# Unique nights surveyed for frogs 
rec_nights <-  rec_info |> 
  filter(Morning == 0) |> 
  mutate(date = as_date(date.time)) |> 
  count(date) 

# View unique nights
rec_nights |> 
  print(n = Inf)

# View the total number of nights
nrow(rec_nights)

# total minutes of nighttime recording
rec_info |> 
  filter(Morning == 0) |> 
  nrow() * 5
  
# Total days 
rec_info |> 
  filter(Morning == 0) |> 
  nrow() / (12 * 24)

# Unique days surveyed for birds
rec_days <-  rec_info |> 
  filter(Morning == 1) |> 
  mutate(date = as_date(date.time)) |> 
  count(date) 

# View unique nights
rec_days |> 
  print(n = Inf)

# View the total number of days
nrow(rec_days)

# total minutes of daytime recording
rec_info |> 
  filter(Morning == 1) |> 
  nrow() * 60 

# Total days 
rec_info |> 
  filter(Morning == 1) |> 
  nrow() / 24

# Average number of days recording
rec_info |> 
  mutate(Date = as_date(date.time)) |> 
  distinct(Plot, Date) |> 
  group_by(Plot) |> 
  reframe(Plot, Days.Active = n()) |> 
  distinct() |> 
  arrange(Days.Active) |> 
  print(n = Inf)

# Save the output
write.csv(rec_info, "Data/bird_frog_aru_effort.csv")

################################################################################
# 3: Bat ARU Effort ############################################################
################################################################################

# Read in the summary data 
uARU_info <-  read.csv("Data/uARU_info.csv")
# View
glimpse(uARU_info)

# Extract what I need 
uARU_info_clean <- uARU_info |> 
  select(Plot.ID, Date.Deployed, Date.Pulled, Days.Deployed, Days.Active)
# View
glimpse(uARU_info_clean)

# min, max, mean number of days active
uARU_info_clean |> 
  drop_na(Days.Active) |> 
  reframe(min = min(Days.Active), max = max(Days.Active), mean = mean(Days.Active)) |> 
  distinct()


# List the Plots
uARU_ls <- dir_ls(bat_dir)
uARU_rec_ls <- dir_ls(uARU_ls, type = "directory")
uARU_plots <-  path_file(path_dir(uARU_rec_ls))
n_uARU <- length(uARU_ls)
n_rec_uARU <-  length(uARU_rec_ls)

# View
uARU_ls
uARU_rec_ls
uARU_plots
n_uARU
n_rec_uARU

# View some files
dir_ls(path(uARU_ls[1], "Data"))

# Make a storage dataframe
uARU_rec_cnt <- tibble(
  Plot = uARU_plots,
  dir = uARU_rec_ls
) |> 
  rowwise() |> 
  mutate(n.Recs = length(dir_ls(dir, glob = "*.wav"))) |> 
  ungroup()


# View
glimpse(uARU_rec_cnt)

# View the number of recordings 
uARU_rec_cnt |> 
  reframe(min = min(n.Recs), max = max(n.Recs), mean = mean(n.Recs), total = sum(n.Recs)) |> 
  distinct()

################################################################################
# 4: AHDriFT Effort ############################################################
################################################################################

# Add in the effort data 
ahdrift <- read.csv("Data/AHDriFT_Effort.csv")

# View
glimpse(ahdrift)

# Days Active
ahdrift_days <-  ahdrift |>
  select(
    Plot, Hut.ID, 
    D1.Days.Active, D2.Days.Active, D3.Days.Active, D4.Days.Active
         ) |> 
  pivot_longer(cols = ends_with("Days.Active"), names_to = "Deployment", values_to = "Deployment.Length") |> 
  mutate(Deployment = str_remove_all(Deployment, ".Days.Active"))

# Status 
ahdrift_status <- ahdrift |> 
  select(
    Plot, Hut.ID, 
    D1.Status, D2.Status, D3.Status, D4.Status
  ) |> 
  pivot_longer(cols = ends_with("Status"), names_to = "Deployment", values_to = "Status") |> 
  mutate(Deployment = str_remove_all(Deployment, ".Status"))

# Settings and combine
ahdrift_lng <-  ahdrift |> 
  select(
    Plot, Hut.ID, 
    D1.Settings, D2.Settings, D3.Settings, D4.Settings
  ) |> 
  pivot_longer(cols = ends_with("Settings"), names_to = "Deployment", values_to = "Settings") |> 
  mutate(Deployment = str_remove_all(Deployment, ".Settings")) |> 
  left_join(ahdrift_status, by = c("Plot", "Hut.ID", "Deployment")) |> 
  left_join(ahdrift_days, by = c("Plot", "Hut.ID", "Deployment")) |> 
  mutate() |> 
  drop_na(Deployment.Length)

# View
glimpse(ahdrift_lng)

# Hut fates
ahdrift_lng |> 
  count(Status)

# Total days by settings
ahdrift_lng |> 
  group_by(Settings) |> 
  reframe(Settings, Days.Active = sum(Deployment.Length)) |> 
  distinct() |> 
  print(n = Inf)

# Min, max, and mean days active among cameras that died
ahdrift_lng |> 
  filter(Status == "Battery Failure") |>
  group_by(Plot, Hut.ID, Settings) |> 
  reframe(Plot, Hut.ID, Settings, Days.Active = sum(Deployment.Length)) |> 
  distinct() |> 
  group_by(Settings) |>
  reframe(
          Settings,
          min.days = min(Days.Active),
          max.Days = max(Days.Active),
          mean.Days = mean(Days.Active),
          n.Dead = n()
          ) |>
  mutate(prop.dead = case_when(Settings == "1-min TL" ~ n.Dead / 50,
                              Settings == "5-min TL" ~ n.Dead / 100)) |>
  distinct() |>
  print(n = Inf)

# Min, max, and mean days active among all cameras 
ahdrift_lng |> 
  group_by(Plot, Hut.ID, Settings) |> 
  reframe(Plot, Hut.ID, Settings, Days.Active = sum(Deployment.Length)) |> 
  distinct() |> 
  group_by(Settings) |>
  reframe(
    Settings,
    min.days = min(Days.Active),
    max.Days = max(Days.Active),
    mean.Days = mean(Days.Active)
  ) |> 
  distinct() |>
  print(n = Inf)

################################################################################
# 5: Camera Trap Effort ########################################################
################################################################################

# Read in the camera trap data 
cam_trp <- read.csv("Data/CT_Effort.csv")
# View
glimpse(cam_trp)

# Min, mean, max, and total trap days 
cam_trp |> 
  filter(Total.Days.Active > 0) |> 
  reframe(min = min(Total.Days.Active), max = max(Total.Days.Active), mean = mean(Total.Days.Active), total = sum(Total.Days.Active))

# Counts by days active 
cam_trp |> 
  count(Total.Days.Active) 

# Percent of cameras by deployment length
cam_trp |> 
  mutate(dpl.lng = case_when(Total.Days.Active < 24 ~ "Under 24 Days",
                             Total.Days.Active >= 24 ~ "Over 24 days" )) |> 
  group_by(dpl.lng) |> 
  reframe(dpl.lng, count = n(), percent = n()/nrow(cam_trp) * 100) |> 
  distinct()
