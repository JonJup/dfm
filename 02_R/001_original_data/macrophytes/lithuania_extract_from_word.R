

# - the file was downloaded from https://aaa.lrv.lt/lt/veiklos-sritys/vanduo/upes-ezerai-ir-tvenkiniai/valstybinis-upiu-ezeru-ir-tvenkiniu-monitoringas/makrofitu-tyrimai
library(docxtractr)
library(data.table)
library(stringr)


doc <- read_docx("~/../Downloads/II DALIS ATASKAITA.docx")

sites1 <- docx_extract_tbl(doc, 
                 tbl_number = 5)
sites2 <- docx_extract_tbl(doc, 
                 tbl_number = 6)
sites3 <- docx_extract_tbl(doc, 
                 tbl_number = 7)
sites <- rbindlist(list(sites1, sites2, sites3))

# - read data 

docx_extract_tbl(doc, 
                 tbl_number = 17)

tables <- c(8,11,15, 19, 23, 27,31, 35, 39, 43, 47, 51, 55, 59, 64, 68, 71, 75, 
            79, 83, 89, 93, 97, 101, 105, 109, 113, 117, 121, 125, 129, 133, 137, 
            141, 145, 149, 155, 159, 163, 167, 171, 175, 178, 182, 186, 190, 194, 
            198, 202, 206, 210, 214, 218, 222, 229, 233, 237, 241, 245, 249, 253, 
            257, 261, 265, 269, 273)

for (i in tables){
        
        if (i == 8) {
                counter =  1
                ls_out <- list()
        } else {
                counter =  counter + 1
        }
               
        i.increment1 <- 1  
        i.increment2 <- 2 
        
        i.d1 <- docx_extract_tbl(doc, tbl_number = i)
        i.d2 <- docx_extract_tbl(doc, tbl_number = i + 1, header = FALSE)
        i.d3 <- docx_extract_tbl(doc, tbl_number = i + 2)
        # - modify data 
        i.coord <- i.d1[1,3]
        i.x.coord <- 
                str_split(i.coord, ";") |> 
                {\(x) x[[1]][1]}() |> 
                str_remove("X") |> 
                str_replace(",", ".") |> 
                as.numeric()
        i.y.coord <- 
                str_split(i.coord, ";") |> 
                {\(x) x[[1]][2]}() |> 
                str_remove("Y") |> 
                str_replace(",", ".") |> 
                as.numeric()
        i.site_id <- as.character(i.d1[4,2])
        i.date <- lubridate::dmy(i.d2[1,3])
        i.taxa <- unlist(i.d3[-c(1:3),1]) |> unname()
        if (any(str_detect(i.taxa, "\\(Subm\\.\\)"))){
                i.taxa <- i.taxa[-which(str_detect(i.taxa, "\\(Subm\\.\\)"))]
        }
        if (any(str_detect(i.taxa, "\\(Fl\\.\\)"))){
                i.taxa <- i.taxa[-which(str_detect(i.taxa, "\\(Fl\\.\\)"))]
        }
        
        i.final <- data.table(
                site_id = i.site_id,
                date    = i.date,
                x.coord = i.x.coord,
                y.coord = i.y.coord,
                taxon   = i.taxa
        )
        
        ls_out[[counter]] <- i.final
        rm(list = ls()[grepl(pattern = "^i\\.", x = ls())])
}; 

dt_out <- rbindlist(ls_out)
saveRDS(dt_out, "data/macrophytes/original_data/lithuania_macrophytes/lith_data.rds")

