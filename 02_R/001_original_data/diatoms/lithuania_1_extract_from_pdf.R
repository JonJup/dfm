### ----------------------------------------------- ###
### --- Extract Lithuanian Diatom data from PDF --- ### 
### ----------------------------------------------- ###

# -------------------------------
# date written: 28.03.22
# date last modified: 29.03.22
# Project: Evaluating European Broad River Types for Diatoms, Fish and Macrophytes
# Purpose:
# Notes: 
# -------------------------------

# TEST 
# BOX 
# Dont Repeat Yourself  
# AUTOMATE 
# DOCUMENT 
# NO BORKEN WINDOWS 
# Design by Contract 
# Fail fast/ defensive programming 

# setup -----------------------------------------------
library(pdftools)
library(magrittr)
library(stringr)
library(data.table)
# load data -------------------------------------------

for (i in 2:1169){#
        print(paste("## STARTING PAGE", i))
        i.pdf <- pdf_ocr_text(
                pdf = "data/diatoms/original_data/lithuania_monitoring/raw/elektroninio dokumento nuorasas.pdf", 
                pages = i)
        i.pdf %<>% strsplit("\n")
        i.pdf <- i.pdf[[1]]
        if (i == 2){
                i.pdf <- i.pdf[-c(1:4)]   
                all_dt <- list()
        }
        i.pdf |> 
        lapply(function(x) stringr::str_remove(string = x, pattern = "Phytobenthos")) |> 
                lapply(function(x) stringr::str_trim(string = x)) -> 
                i.pdf
        
        # - extract site id from i.pdf
        i.site_id <- 
                i.pdf |> 
                lapply(function(x) str_extract(x, pattern = "^[A-Z]+[0-9]+")) |> 
                unlist()
        # - remove site id from i.pdf
        for (k in 1:uniqueN(i.site_id)){
                
                i.pdf %<>%
                        lapply(function(x) stringr::str_remove(string = x, pattern = unique(i.site_id)[k])) %>%
                        lapply(function(x) stringr::str_trim(string = x))
                
        }
        
        # - extract Y Coordinate 
        i.y_coord <- 
                i.pdf |> 
                lapply(function(x) str_extract(x, pattern = "\\ [0-9]+$")) |> 
                lapply(function(x) str_trim(x)) |> 
                #lapply(as.numeric) |> 
                unlist()
        
        # - remove Y Coordinate from i.pdf
        for (k in 1:uniqueN(i.y_coord)){
                
                i.pdf %<>%
                        lapply(function(x) stringr::str_remove_all(string = x, pattern = unique(i.y_coord)[k])) %>%
                        lapply(function(x) stringr::str_trim(string = x))
                
        }
        
        # - extract X Coordinate 
        i.x_coord <- 
                i.pdf |> 
                lapply(function(x) str_extract(x, pattern = "\\ [0-9]+$")) |> 
                lapply(function(x) str_trim(x)) |> 
                #lapply(as.numeric) |> 
                unlist()
        
        # - remove X Coordinate from i.pdf
        for (k in 1:uniqueN(i.x_coord)){
                
                i.pdf %<>%
                        lapply(function(x) stringr::str_remove_all(string = x, pattern = unique(i.x_coord)[k])) %>%
                        lapply(function(x) stringr::str_trim(string = x))
                
        }
        
        # - the rest is the site name 
        i.site_name <- unlist(i.pdf)
        
        i.dt <- data.table(site_id = i.site_id, 
                           site_name = i.site_name, 
                           x.coord   = i.x_coord,
                           y.coord   = i.y_coord)
        all_dt[[i-1]] <- i.dt
        rm(list = ls()[grepl(pattern = "^i\\.", x = ls())])
}
all_dt_bound <- rbindlist(all_dt)



# second half  ----------------------------------------------------------------------
for (i in 1170:2337){#
        print(paste("## STARTING PAGE", i))
        i.pdf <- pdf_ocr_text(
                pdf = "data/diatoms/original_data/lithuania_monitoring/raw/elektroninio dokumento nuorasas.pdf", 
                pages = i)
        i.pdf %<>% strsplit("\n")
        i.pdf <- i.pdf[[1]]
        if (i == 1170){
                i.pdf <- i.pdf[-c(1:3)]   
                all_dt2 <- list()
        }
        # - extract date from i.pdf
        i.date <- 
                i.pdf |> 
                lapply(function(x) str_extract(x, pattern = "^[0-9]+/[0-9]+/[0-9]+\\ ")) |> 
                lapply(function(x) str_trim(x)) |> 
                unlist() 
        # - remove date from i.pdf
        for (k in 1:uniqueN(i.date)){
                
                i.pdf %<>%
                        lapply(function(x) stringr::str_remove(string = x, pattern = unique(i.date)[k])) %>%
                        lapply(function(x) stringr::str_trim(string = x))
                
        }
        # - extract abundance from i.pdf 
        # - some strings end with a point that messes up the pattern recognituon.
        # - So first we remove the point. 
        i.pdf  <- 
                i.pdf|> 
                lapply(function(x) str_remove(x, pattern = "\\.$")) |> 
                lapply(function(x) str_trim(x)) |> 
                unlist() 
        
        i.abudance <- 
                i.pdf |> 
                lapply(function(x) str_extract(x, pattern = "[0-9]+$")) |> 
                lapply(function(x) str_trim(x)) |> 
                unlist() 
        
        # - drop NA from unique vector
        i.uni <- unique(i.abudance)
        i.uni <- i.uni[which(!is.na(i.uni))]
        # - remove abundance from i.pdf
        for (k in 1:length(i.uni)){
                
                i.pdf %<>%
                        lapply(function(x) stringr::str_remove_all(string = x, pattern = i.uni[k])) %>%
                        lapply(function(x) stringr::str_trim(string = x))
                
        }
        # - the rest is the taxon
        i.taxon <- unlist(i.pdf)
        
        i.dt <- data.table(date = i.date, 
                           taxon = i.taxon, 
                           abundance   = i.abudance)
        all_dt2[[i-1169]] <- i.dt
        rm(list = ls()[grepl(pattern = "^i\\.", x = ls())])
}

all_dt_bound2<- rbindlist(all_dt2)
all_dt_bound2$date <- lubridate::mdy(all_dt_bound2$date)
month(all_dt_bound2$date) |> hist()

all_dt_b3 <- bind_cols(all_dt_bound, all_dt_bound2)

quicksave <- list(all_dt_bound, all_dt_bound2)

saveRDS(quicksave, "data/diatoms/original_data/lithuania_monitoring/quicksave.rds")
data <- readRDS("data/diatoms/original_data/lithuania_monitoring/quicksave.rds")

data1 <- data[[1]]
data2 <- data[[2]]

data1[, site_id2 := .GRP, by = "site_id"]
data2[, date_id := .GRP, by = "date"]

data2$date_id
data1$site_id2

## second try 


pdf <- pdf_text("data/diatoms/original_data/lithuania_monitoring/raw/elektroninio dokumento nuorasas.pdf")
pdf2 <- strsplit(pdf, "\n")
saveRDS(pdf2, "data/diatoms/original_data/lithuania_monitoring/split_pdf.rds")
pdf2 <- readRDS("data/diatoms/original_data/lithuania_monitoring/split_pdf.rds")


pdf3 <- pdf2[-1]
pdf3[[1]] <- pdf3[[1]][-c(1:4)]
pdf3[[1168]]
pdf3[1169]
half1 <- pdf3[1:1168]

half1 <- unlist(half1)
# - identify trailing lines. Site names sometmes take up two lines
trail_id <- which(!str_detect(half1, "Phytobenthos"))
half1 <- half1[-trail_id]

half1 %<>% str_remove("Phytobenthos") %>% str_trim()
site_id <- 
        half1 |> 
        str_extract(pattern = "^[A-Z]+[0-9]+") 
half1 %<>% str_remove(pattern = paste(unique(site_id), collapse = "|")) %>% str_trim()
y.coord <- 
        half1 |> 
        str_extract(pattern = "\\ [0-9]+$") %>% str_trim()
# - remove Y Coordinate from i.pdf
half1 %<>% str_remove(pattern = paste(unique(y.coord), collapse = "|")) %>% str_trim()
x.coord <- 
        half1 |> 
        str_extract(pattern = "\\ [0-9]+$") %>% str_trim()
half1 %<>% str_remove(pattern = paste(unique(x.coord), collapse = "|")) %>% str_trim()


#- try to match pages 
