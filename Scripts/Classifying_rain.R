###############################################################################
# Title: Classifying Rain Events 
# Author: Will Harrod
# Date Created: 2026-11-01
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
library(remotes)
remotes::install_git("https://github.com/Cdevenish/hardRain.git")
# library(hardRain)

# 1.2) File directories (Change these for your device) -------------------------

# Directory where the raw audio files are held 
csv_dir <-  "/home/will/NCSU/R_Code/Timbermill_Covariates/Data"

# View the avilable data
dir_ls(csv_dir)

################################################################################
# 2) Classify Rain #############################################################
################################################################################