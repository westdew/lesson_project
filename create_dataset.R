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

## Attendance
  
## MCAP

## Special Services

### Merge source datasets into final dataset

### Save final dataset