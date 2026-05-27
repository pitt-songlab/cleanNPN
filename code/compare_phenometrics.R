
#Exploratory Data Analysis
#Written by Brianna Shepherd
#Updated: May 27, 2026

#load packages
library(tidyverse)
library(dplyr)
library(readr)
library(ggplot2)
library(maps)
library(patchwork)

#Import phenometrics data for ACER 2025 flowers
ACER_FL_phenometrics_2025 <- read_csv("alldata/input/NPN/flower_phenometrics/Acer/2025.csv")
View(ACER_FL_phenometrics_2025)

#Import 2025 Acer data with calculated FLS
ACRU_FLS_2025 <- read_csv("alldata/intermediate/ACRU_FLS_2025.csv")
View(ACRU_FLS_2025)


