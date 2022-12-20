### ----------------------------- ###
### --- Clean FISH CZECH Dryver --- ### 
### ----------------------------- ###


# date written: 02.05.22
# date last modified: 02.05.22
# Project: Evaluating European Broad River Types for Diatoms, Fish and Macrophytes
# Purpose: Clean Czech FISH Dryver Data 
# Notes: 


# setup -----------------------------------------------

options("box.path" = "~/R/box_modules/")

library(data.table)
library(magrittr)
#library(jjmisc)
library(mapview)
library(tidyr)
library(mapview)
box::use(readxl[read_excel],
         lubridate[ymd, year, month],
         stringdist[amatch],
         sf = sf,
         dp = dplyr,
         box/dfm)

# load data -------------------------------------------
bio <- read_excel("data/fish/original_data/czech_dryver/raw/CZECH FISH DATA - DRYVER PROJECT_Paril.xlsx")
taxontable <- readRDS("data/fish/2022-04-27_taxontable_fish.rds")
typologies <- readRDS("data/all_typologies.rds")

# prepare data ----------------------------------------

setDT(bio)

var.coords    <- bio[1,-1]
var.site.name <- bio[2,-1]
var.date      <- bio[3,-1]
var.date      <- bio[7,-1]

sites <- data.table(
        original_site_name = var.site.name,
        date               = as.Date(as.numeric(var.date), origin = "1899-12-30")
)

unique(month(sites$date))

# - no month in focal months