###### create_dataset.R ######
# Author: Nathaniel A. Dewey #
# Created: 3/2/2020          #
# Updated: 3/4/2020          #
##############################

### Setup Environment

rm(list=ls())

library(tidyverse)

### Define Functions

# Read CSV files with BOM and NUL problems
# https://stackoverflow.com/questions/30251576/reading-a-non-standard-csv-file-into-r
read.csvX = function(file, encoding="UTF-16LE", sep=",", header=T, stringsAsFactors=T) {
  csvLines = readLines(file, encoding=encoding, skipNul=T, warn=F)
  # Remove BOM (ÿþ) from first line
  if (substr(csvLines[[1]], 1, 2) == "ÿþ") {
    csvLines[[1]] = substr(csvLines[[1]], 3, nchar(csvLines[[1]]))
  }
  csvLines = csvLines[csvLines != ""]
  if (length(csvLines) == 0) {
    warning("Empty file")
    return(NULL)
  }
  csvData = read.csv(text=paste(csvLines, collapse="\n"), sep=sep, header=header, stringsAsFactors=stringsAsFactors)
  return(csvData)
}

### Import source datasets

mdse_report_card <- read.csvX("source/2019_Accountability_Schools.csv", sep="\t", stringsAsFactors=F)
mdse_attendance <- read.csvX("source/Attendance_2019.csv", stringsAsFactors=F)
mdse_mcap <- read.csvX("source/MCAP_ELA_MATH_2019.csv", stringsAsFactors=F) # long on test subject and grade level
mdse_special_services <- read.csvX("source/Special_Services_2019.csv", stringsAsFactors=F)

### Plan final dataset

# Variables:
# - schid (int)
# - schname (chr)
# - chronic_absentee_pct (num)
# - mcap_ela_proficient_pct (num)
# - mcap_math_proficient_pct (num)
# - star_rating (int)
# - points_earned (int)
# - farms_per (num)
# Observations: High schools in Baltimore City with traditional management

### Munge source datasets

## Report card (canonical)
mdse_report_card <- mdse_report_card %>%
  rename(schid = School.Number, schname = School.Name, star_rating = Star.Rating, points_earned=Total.Points.Earned.Percentage) %>%
  filter(LSS.Number == 30) %>% # Baltimore City
  select(schid, schname, star_rating, points_earned)

## Attendance

mdse_attendance <- mdse_attendance %>%
  rename(schid = School.Number, chronic_absentee_pct = Chronic.Absentee.Pct) %>%
  filter(LSS.Number == 30, schid != "A") %>% # Baltimore City
  mutate(schid = as.integer(schid))

schids_with_nonstandard_gradebands <- filter(mdse_attendance, School.Type == "All Students") %>% pull(schid)

mdse_attendance <- mdse_attendance %>%
  filter(
    !(schid %in% schids_with_nonstandard_gradebands) | School.Type == "All Students"
  ) %>% select(schid, chronic_absentee_pct)

## MCAP

mdse_mcap <- mdse_mcap %>%
  rename(schid = School.Number) %>%
  filter(LSS.Number == 30, schid != "A") %>% # Baltimore City
  select(schid, Assessment, Proficient.Pct) %>%
  mutate(schid = as.integer(schid)) %>%
  pivot_wider(schid, names_from=Assessment, values_from=Proficient.Pct)

## Special Services

mdse_special_services <- mdse_special_services %>%
  rename(schid = School.Number, farms_per = FARMS.Pct) %>%
  filter(LSS.Number == 30, schid != "A") %>% # Baltimore City
  mutate(schid = as.integer(schid))

schids_with_nonstandard_gradebands <- filter(mdse_special_services, School.Type == "All") %>% pull(schid)
  
mdse_special_services <- mdse_special_services %>%
  filter(
    !(schid %in% schids_with_nonstandard_gradebands) | School.Type == "All"
  ) %>% select(schid, farms_per)

### Merge source datasets into final dataset

analytic <- mdse_report_card %>%
  left_join(mdse_mcap, by="schid") %>%
  left_join(mdse_attendance, by="schid") %>%
  left_join(mdse_special_services, by="schid")

### Save final dataset
save(analytic, file="data/analytic.RData")
