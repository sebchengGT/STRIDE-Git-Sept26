library(tidyverse)
library(DT)
library(dplyr)
library(shiny)
library(shinydashboard)
library(bslib)
library(bsicons)
library(leaflet)
library(htmltools)
library(thematic)
library(arrow)
library(forcats)
library(fontawesome)
library(shinyjs)
library(shinyauthr)
library(httr)
library(scales)
library(tidytext)
library(ggplot2)
library(plotly)
library(readr)
library(geojsonio)
library(shinyWidgets)
library(later)
library(googlesheets4)
library(DBI)
library(RPostgres)
library(pool)
library(reactable)
library(reactablefmtr)


# HROD Data Upload
sheet_url <- "https://docs.google.com/spreadsheets/d/1e3ni50Jcv3sBAkG8TbwBt4v7kjjMRSVvf9gxtcMiqjU/edit?gid=0#gid=0"
SHEET_ID <- "https://docs.google.com/spreadsheets/d/1x9D8xfXtkT1Mbr4M4We7I9sUqoj42X4SFX9N9hu24wM/edit?gid=0#gid=0"
SHEET_NAME <- "Sheet1" # Assuming the data is on the first tab
school_data <- reactiveVal(NULL)
df <- read_parquet("School-Level-v2.parquet") # per Level Data
uni <- read_parquet("School-Unique-v2.parquet") # School-level Data
IndALL <- read_parquet("IndDistance.ALL2.parquet") # Industry Distances
ind <- read_parquet("SHS-Industry.parquet") # Industry Coordinates
# Clean Sector names
ind <- ind %>%
  mutate(
    Sector = case_when(
      is.na(Sector) | Sector == "#N/A" ~ NA_character_,
      str_detect(Sector, regex("Agri", ignore_case = TRUE)) ~ "Agriculture and Agri-business",
      str_detect(Sector, regex("Business", ignore_case = TRUE)) ~ "Business and Finance",
      str_detect(Sector, regex("Hospitality|Tourism", ignore_case = TRUE)) ~ "Hospitality and Tourism",
      str_detect(Sector, regex("Manufacturing|Engineeri", ignore_case = TRUE)) ~ "Manufacturing and Engineering",
      str_detect(Sector, regex("Professional|Service", ignore_case = TRUE)) ~ "Professional/Private Services",
      str_detect(Sector, regex("Public", ignore_case = TRUE)) ~ "Public Administration",
      TRUE ~ Sector  # leave unchanged if no match
    )
  )
SDO <- read_parquet("SDOFill.parquet") # SDO and Regional Filling-up Rate
DBMProp <- read.csv("DBM-Proposal.csv") # Teacher Shortage Data

# EFD Data Upload
EFDDB <- read.csv("EFD-DataBuilder-2025.csv")
EFDMP <- read_parquet("EFD-Masterlist.parquet")
EFD_Projects <- read.csv("EFD-ProgramsList-Aug2025.csv") %>% mutate(Allocation = as.numeric(Allocation)) %>% mutate(Completion = as.numeric(Completion)) %>% filter(FundingYear >= 2020)
LMS <- read_parquet("EFD-LMS-GIDCA-NSBI2023.parquet") %>%
  mutate(
    Region = case_when(Region == "Region IV-B" ~ "MIMAROPA", TRUE ~ Region),
    With_Shortage = case_when(Estimated_CL_Shortage > 0 ~ 1, TRUE ~ 0)
  ) %>%
  left_join(
    uni %>% select(
      SchoolID,
      Latitude,
      Longitude,
      Legislative.District,
      Municipality,
      Barangay
    ),
    by = c("School_ID" = "SchoolID")
  )


geojson_data <- geojson_read("gadm41_PHL_1.json", what = "sp")
geojson_table <- as.data.frame(geojson_data)
regprov <- read.csv("RegProv.Congestion.csv")
geojson_table <- left_join(geojson_table,regprov, by="NAME_1")
buildablecsv <- read.csv("Buildable_LatLong.csv")
# Clean column names: remove line breaks, extra spaces
names(buildablecsv) <- gsub("[\r\n]", " ", names(buildablecsv))
names(buildablecsv) <- trimws(names(buildablecsv))
# Ensure lat/long are numeric
buildablecsv$Latitude <- as.numeric(buildablecsv$Latitude)
buildablecsv$Longitude <- as.numeric(buildablecsv$Longitude)


# CLOUD Data Upload
cloud <- read_parquet("Cloud_Consolidated.parquet")
cloud_v2 <- read_parquet("Cloud_Consolidated_v2.parquet")
cloud_v3 <- read_parquet("Cloud_Consolidated_v3.parquet")

#Data Explorer 
ThirdLevel <- read.csv("2025-Third Level Officials DepEd-cleaned.csv", stringsAsFactors = FALSE)

# dfGMIS data
dfGMIS <- read.csv("GMIS-FillingUpPerPosition-2025.csv")

# Get unique positions for the dropdown
all_positions <- c("All Positions" = "All", 
                   sort(unique(as.character(dfGMIS$Position))))

# Calculate overall totals
overall_totals <- dfGMIS %>%
  summarise(
    Total.Filled = sum(Total.Filled, na.rm = TRUE),
    Total.Unfilled = sum(Total.Unfilled, na.rm = TRUE)
  )

# end of dfGMIS data

user_base <- tibble::tibble(
  user = c("iamdeped", "depedadmin"),
  password = c("deped123", "stride123"), # In a real app, use hashed passwords
  password_hash = sapply(c("deped123", "stride123"), sodium::password_store), # Hashed passwords
  permissions = c("admin", "standard"),
  name = c("User One", "User Two")
)

SERVICE_ACCOUNT_FILE <- "service_account.json" 
