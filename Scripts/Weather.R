###############################################################################
# Title: Weather Data 
# Author: Will Harrod
# Date Created: 2026-11-11
# Weather data was obtained from the Open Metro historic weather api
################################################################################

################################################################################
# 1) Prep ######################################################################
################################################################################

# 1.1) Add packages ------------------------------------------------------------

# Clear environments
rm(list = ls())

# Load Packages
library(tidyverse)
library(fs)
library(tuneR)

# 1.2) File directories (Change these for your device) -------------------------

# Directory where the raw audio files are held 
csv_dir <-  "/home/will/NCSU/R_Code/Timbermill_Covariates/Data"

# View the available data
basename(dir_ls(csv_dir))

# Read in the weather data
weather_raw <- read_csv(path(csv_dir, "open-meteo-36.15N76.56W6m.csv"))

# View
glimpse(weather_raw)

# 1.3) Rename columns ----------------------------------------------------------

# Change the names so they are easier to code 
weather <-  weather_raw |> 
  rename(
    date.time = time,
    temp.c = `temperature_2m (°C)`,
    wind.10m.ms = `wind_speed_10m (m/s)`,
    wind.100m.ms = `wind_speed_100m (m/s)`,
    dew.point.c = `dew_point_2m (°C)`,
    humidity.pct = `relative_humidity_2m (%)`,
    rain.mm = `rain (mm)`,
    showers.mm = `showers (mm)`,
    clouds.pct = `cloud_cover (%)`,
    vpd.kpa = `vapour_pressure_deficit (kPa)`,
    wbt.c = `wet_bulb_temperature_2m (°C)`,
    uv = `uv_index ()`,
    is.day = `is_day ()`
  ) |> 
  mutate(
    date = date(date.time),
    hour = hour(date.time)
         ) |> 
  relocate(c("date", "hour", "is.day"), .before = date.time) |> 
  # Remove the daily summaries 
  filter(!is.na(is.day))

# View again
glimpse(weather)

# save 
write_csv(weather, (path(csv_dir, "weather_data.csv")))
                                                
################################################################################
# 2) Rain over time ############################################################
################################################################################

# 2.1 Find out when it rained --------------------------------------------------

# View the rain on day 1
weather |> 
  filter(date %in% weather$date[1:30]) |> 
  select(date, hour, rain.mm) |> 
  print(n = Inf)

# Claculate daily rainfall
daily_rain <- weather |> 
  group_by(date) |> 
  summarise(
    total.rain.mm = sum(rain.mm)
  )

# Plot daily total rain over time 
rain_plot <- daily_rain |> 
  ggplot() +
    geom_col(aes(x = date, y = total.rain.mm), col = "blue2") +
    theme_classic() +
  labs(y = "Daily Precipitation (mm)") +
  theme(
    plot.title = element_text(size = 14),
    axis.title.y = element_text(size = 14),
    axis.title.x = element_text(size = 14),
    axis.text.x = element_text(size = 14),,
    axis.text.y = element_text(size = 14),
    legend.text = element_text(size = 14),
    legend.position = "right",
    legend.title = element_text(size = 14),
    strip.text = element_text(size = 12),
  ) 
rain_plot

# Find the days where it rained at least 25 mm
rainy_days <- daily_rain |> 
  filter(total.rain.mm >= 20) |> 
  arrange(-total.rain.mm)

# View
rainy_days |> print(n = Inf)

# Pull the hours from those days 
rain_day_hours <- weather |> 
  filter(date %in% rainy_days$date) |> 
  select(date, hour, date.time, rain.mm) |> 
  arrange(date.time)

# View 
rain_day_hours |> print(n = Inf)

# Rain cutoff (1mm)
rain_cutoff <-  1

# Pull out only the rainy hours
rainy_hours <- weather |> 
  filter(rain.mm > rain_cutoff) |> 
  mutate(
    date = str_remove_all(as.character(date), "-"),
    date.hour = paste(date, hour, "-")
    ) |> 
  select(date, hour, date.hour, rain.mm) 
  
# View
glimpse(rainy_hours)

# Plot these by date 
hour_date_plot <- rainy_hours |> 
  ggplot() +
  geom_smooth(aes(x = date, y = rain.mm), col = "blue2", se = FALSE) +
  geom_point(aes(x = date, y = rain.mm), col = "blue2") +
  theme_classic()
hour_date_plot

# 2.2) Find the audio files from those rainy hours -----------------------------

# Path to the audio files 
audio_dir <- "/media/will/Timbermill/Bird_Anuran_ARUs"

# List of ARU ddirectories 
aru_dirs <- dir_ls(audio_dir)
n_arus <- length(aru_dirs)

# View 
aru_dirs
n_arus

# Storage object for rain recordings
rain_recs <- tibble(
  plot = NA_character_,
  date = NA_character_,
  hour = NA_integer_,
  date.hour = NA_character_,
  rain.mm = NA_real_
)

# Loop over the audio files and pull out the recordings from those hours 
for(i in 1:n_aru){
  
  # Define an ARU 
  aru <- aru_dirs[i]
  
  # list the dates 
  aru_info_tmp <- tibble(
    file.path = dir_ls(aru)
  ) |>  
  mutate(
    plot = str_extract(file.path, "[A-Z]{2}\d{2}"),
    date = str_remove_all(str_extract(file.path, "_\d{8}_"), "_"),
    time = str_remove_all(str_extract(file.path, "_\d{6}_"), "_"),
    hour = as.integer(str_sub(time, start = 1, end = 2)),
    minte = as.integer(str_sub(time, start = 3, end = 4)),
    second = as.integer(str_sub(time, start = 5, end = 6)),
    date.hour = paste(date, hour, "-")
  ) |> 
  select(-time) |> 
  filter(date.hour %in% rainy_hours$date.hour) |> 
  left_join(rainy_hours, by = c("date", "hour", "date.hour")) |> 
  mutate(new.file.basename = paste0(plot, "_", date, "_", time, "_Rrain.mm"))
  
  # Combine 
  rain_recs <- bind_rows(rain_recs, aru_info_tmp) 
  
}

# View
glimpse(rain_recs)

# Save 
write_csv(rain_recs, "rain_recording_info.csv")

# 2.3) Chop up the rainy audio files for validation ----------------------------

# Function to move .wav files
move_wav <-  function(seconds, in_file, out_dir, new_name){
  
  # Define the start end times as well as the filepath
  start_sec <- select_secs
  end_sec <- start_sec + samp_length
  
  # Read in the wav
  wav <-  readWave(
    filename = in_file,
    from = start_sec,
    to = end_sec
    )
  
  # Save that wave file 
  writeWave(
    object = wav,
    filename = path(out_dir, paste0(new_wav_name, "_S", start_sec, "_E", end_sec, ".wav")) 
  )
  
}

# Function to chop up audio date 
# Note: this pulls from only the first 5 minutes of each hour
snip_rain <- function(dat, n_samp){
  
  # Find the audio file
  file <- dat |> pull(file) 
  
  # basename of the new wave file
  new_wav_name <- dat |> pull(new.basename)
  
  # select seconds to pull from 
  select_secs |> slice_sample(n = n_samp)
  
  # slice and copy the wav files 
  map(
    .x = , 
    .f = move_wav(
      seconds = select_secs,
      in_file = file,
      out_dir = out_dir,
      new_name = new_wav_name
       )
     )
  
}

# sample length
samp_length <- 3

# A list of possible seconds the pull could start from
select_secs <- tibble(start.second = seq(from = 0, to = 300 - samp_length))

# number of samples for each hour of rain data at each ARU
n_samp_total <- 1000
n_samp <- ceiling(n_samp_total / nrow(rain_recs))
n_samp

# Output directory
out_dir <- "/home/will/NCSU/Data/BirdNet_Validations/2026/MinConf_025_Shiny/Rain"

# map the custom function onto our selected rain snippets 
map(
  .x = rain_recs, 
  .f = snip_rain(
    n_samp = n_samp
    )
  )
