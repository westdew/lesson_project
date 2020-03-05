###### create_dataset.R ######
# Author: Nathaniel A. Dewey #
# Created: 3/2/2020          #
# Updated: 3/4/2020          #
##############################

### Setup Environment

rm(list=ls())

library(tidyverse)
library(readxl)

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

mdse_schools <- read_excel("source/School_Directory_2019.xlsx", sheet="School_Directory_2019")
mdse_report_card <- read.csvX("source/2019_Accountability_Schools.csv", sep="\t", stringsAsFactors=F)
mdse_attendance <- read.csvX("source/Attendance_2019.csv", stringsAsFactors=F)
mdse_mcap <- read.csvX("source/MCAP_ELA_MATH_2019.csv", stringsAsFactors=F) # long on test subject and grade level
mdse_special_services <- read.csvX("source/Special_Services_2019.csv", stringsAsFactors=F)

### Plan final dataset

# Variables:
# - schid (int)
# - schname (chr)
# - schtype (chr)
# - chronic_absentee_pct (num)
# - mcap_ela10_proficient_pct (num)
# - mcap_algebra_proficient_pct (num)
# - star_rating (int)
# - points_earned (int)
# - farms_per (num)
# Observations: High schools in Baltimore City (not special education or alternative schools)

### Munge source datasets

## School Directory (canonical)

mdse_schools <- mdse_schools %>%
  rename(schid = `School Number`, schname = `School Name`, schtype = `School Type`) %>%
  filter(
    `LSS Number` == 30, # Baltimore City
    schtype %in% c("H", "MH", "EMH") # high school
  ) %>% 
  mutate(schid = as.integer(schid)) %>%
  select(schid, schname, schtype)

## Report card

mdse_report_card <- mdse_report_card %>%
  rename(schid = School.Number, star_rating = Star.Rating, points_earned=Total.Points.Earned.Percentage) %>%
  filter(LSS.Number == 30) %>% # Baltimore City
  select(schid, star_rating, points_earned)

## Attendance

mdse_attendance <- mdse_attendance %>%
  rename(schid = School.Number, chronic_absentee_pct = Chronic.Absentee.Pct) %>%
  filter(
    LSS.Number == 30, # Baltimore City
    schid != "A", # not aggregate
    School.Type == "High" # high school
  ) %>% 
  mutate(schid = as.integer(schid)) %>%
  select(schid, chronic_absentee_pct)

## MCAP

mdse_mcap <- mdse_mcap %>%
  rename(schid = School.Number) %>%
  filter(
    LSS.Number == 30, # Baltimore City
    schid != "A", # not aggregate
    Assessment == "Algebra 1" | Assessment == "English/Language Arts Grade 10" # only include high school tests
  ) %>%
  select(schid, Assessment, Proficient.Pct) %>%
  mutate(schid = as.integer(schid)) %>%
  pivot_wider(schid, names_from=Assessment, values_from=Proficient.Pct) %>%
  rename(mcap_algebra_proficient_pct = "Algebra 1", mcap_ela10_proficient_pct = "English/Language Arts Grade 10")

## Special Services

mdse_special_services <- mdse_special_services %>%
  rename(schid = School.Number, farms_per = FARMS.Pct) %>%
  filter(
    LSS.Number == 30, # Baltimore City
    schid != "A", # not aggregate
    School.Type == "High" # high school
  ) %>%
  mutate(schid = as.integer(schid)) %>%
  select(schid, farms_per)

### Merge source datasets into final dataset

analytic <- mdse_schools %>%
  left_join(mdse_report_card, by="schid") %>%
  left_join(mdse_mcap, by="schid") %>%
  left_join(mdse_attendance, by="schid") %>%
  left_join(mdse_special_services, by="schid")

analytic <- filter(analytic, !(schid %in% c(177, 301, 307, 884))) # remove special education and alternative schools

### Save final dataset
save(analytic, file="data/analytic.RData")
