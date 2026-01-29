## @author Edwin Garcia
## Data Analysis on Avian Populations
## R version 4.5.1 (2025-06-13)

## Clear working directory
rm(list = ls())

## Load all necessary libraries
library("readxl")
library(tidyverse)
library(dplyr)

## Import all avian monitoring data from the National Capital Region Network (NCRN)
NCRNobsv <- bind_rows(read_csv("data/raw/National Capital Region Network (2007-2024)/ncrn_birds_forest_R1_BirdDataStandard.csv"),
                      read_csv(("data/raw/National Capital Region Network (2007-2024)/ncrn_birds_grass_R1_BirdDataStandard.csv")))

## View data to make sure it was imported correctly
view(NCRNobsv)
head(NCRNobsv)

## Detect any missing values in the data sets
is.na(NCRNobsv)

## Look like all data points for columns:
## SubUnitCode, SubUnitName, NoiseLevelUnit, Temp_Recorded, EventNotes, ExcludeNote
## ObservationNote, ExcludeReason
## are missing values.

## Double check that this is the case
## Each line will return all observations that have data filled in
## for each of these columns
NCRNobsv[complete.cases(NCRNobsv$SubUnitCode),]
NCRNobsv[complete.cases(NCRNobsv$SubUnitName),]
NCRNobsv[complete.cases(NCRNobsv$NoiseLevelUnit),]
NCRNobsv[complete.cases(NCRNobsv$Temp_Recorded),]
NCRNobsv[complete.cases(NCRNobsv$EventNotes),]
NCRNobsv[complete.cases(NCRNobsv$ExcludeNote),]
NCRNobsv[complete.cases(NCRNobsv$ObservationNote),]
NCRNobsv[complete.cases(NCRNobsv$ExcludeReason),]

## Given these columns are completely empty,
## it is best to completely remove them from the data set
## This will select and keep all columns that are not all missing values
## 2 represents that this lambda function will be applied to the columns of NCRNobsv
NCRNobsv <- NCRNobsv[!apply(NCRNobsv, 2, function(x) all(is.na(x)))]

## Want to organize the data set such that all individual observations are 
## grouped together by year and by individual point count stations
NCRNcount <- count(NCRNobsv, PointGroupName, EventYear, CommonName, name = "Total Observed") %>%
  pivot_wider(id_cols = c(`EventYear`, `PointGroupName`), names_from = CommonName, values_from = `Total Observed`, values_fill = 0) %>%
  mutate(Total = rowSums(NCRNcount[, 3:187], na.rm = TRUE))

## Every row now contains information on the year the data was taken from, 
## from which point count station, and how many birds of a particular
## species were observed there, including a complete total of all birds observed
view(NCRNcount)
head(NCRNcount)

## Finding the Shannon-Weiner Index (H), which measure diversity in a specific point count station
## Finding Hill's N1 diversity index, which is an easier to interpret diversity index, formulated as e^H
startIndex = 3      ## Start and end indices for columns storing bird count data
endIndex = 187 
rows = nrow(NCRNcount)
NCRNcount$H <- 0              ## endIndex + 2
NCRNcount$N1 <- 0             ## endIndex + 3
NCRNcount$numSpecies <- 0     ## endIndex + 4
for (i in 1:rows) {
  currRow = NCRNcount[i,]
  total = currRow[endIndex + 1]
  numSpecies <- 0
  N1 <- 1
  for (j in startIndex:endIndex) {
    if (currRow[j] != 0) {
      numSpecies <- numSpecies + 1
      pi <- (currRow[j] /  total)
      N1 <- N1 * (pi ^ pi)
    }
  }
  N1 <- 1 / N1
  NCRNcount[i, endIndex + 3] <- N1
  NCRNcount[i, endIndex + 2] <- log(N1)
  NCRNcount[i, endIndex + 4] <- numSpecies
}

view(NCRNcount)








