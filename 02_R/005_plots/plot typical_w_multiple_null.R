## -- plot indval results with multiple null models

# created: 16.12.22
# modified: 16.12.22
# project : Evaluating European Broad River Types for Diatoms, Fish and Macrophytes
# purpose : visualize the results of the Typical Assemblages analysis

# setup -----------------------------------------------------------------------------

library(pacman)
p_load(ggdist, ggplot2, dplyr, data.table, magrittr, wesanderson, stringr, vegan)

### log files ---- 
sink(file = paste0("02_R/999_log_files/typical_w_multiple_nulls", Sys.Date(), "_", "log.txt"))
rstudioapi::documentPath()
sessionInfo()
sink(file = NULL)
### --------------


# functions -------------------------------------------------------------------------

plot_fun <- function(res){
        
        vlinevar1 <- data[typology_system == "null_model1_type" & taxonomic_level == res] %>% unique(by = c("typology_system", "taxonomic_level", "taxon","type1", "type2"))
        vlinevar1[, jaccard_similarity := mean(jaccard_similarity), by = c("typology_system", "taxon", "taxonomic_level")]
        vlinevar1 %<>% unique(by =  c("typology_system", "taxon", "taxonomic_level"))
        
        vlinevar2 <- data[typology_system == "null_model2_type" & taxonomic_level == res] %>% unique(by = c("typology_system", "taxonomic_level", "taxon","type1", "type2"))
        vlinevar2[, jaccard_similarity := mean(jaccard_similarity), by = c("typology_system", "taxon", "taxonomic_level")]
        vlinevar2 %<>% unique(by =  c("typology_system", "taxon", "taxonomic_level"))
        
        vlinevar3 <- data[typology_system == "null_model3_type" & taxonomic_level == res] %>% unique(by = c("typology_system", "taxonomic_level", "taxon","type1", "type2"))
        vlinevar3[, jaccard_similarity := mean(jaccard_similarity), by = c("typology_system", "taxon", "taxonomic_level")]
        vlinevar3 %<>% unique(by =  c("typology_system", "taxon", "taxonomic_level"))
        
        vlinevar4 <- data[typology_system == "null_model4_type" & taxonomic_level == res] %>% unique(by = c("typology_system", "taxonomic_level", "taxon","type1", "type2"))
        vlinevar4[, jaccard_similarity := mean(jaccard_similarity), by = c("typology_system", "taxon", "taxonomic_level")]
        vlinevar4 %<>% unique(by =  c("typology_system", "taxon", "taxonomic_level"))
        
        
        out_plot <- 
                data |>  
                filter(!str_detect(typology_system, "null_model"), taxonomic_level == res) |> 
                unique(by = c("taxonomic_level", "taxon","typology_system", "type1", "type2")) |> 
                ggplot(aes(y=jaccard_similarity, x=typology_system)) + 
                geom_boxplot(aes(fill = typology_system), alpha = 1,size = .8) +
                geom_hline(aes(yintercept = jaccard_similarity), data = vlinevar1, lty = "dashed", linewidth = 1, col = "red") + 
                geom_hline(aes(yintercept = jaccard_similarity), data = vlinevar2, lty = "dashed", linewidth = 1) + 
                geom_hline(aes(yintercept = jaccard_similarity), data = vlinevar3, lty = "dashed", linewidth = 1) + 
                geom_hline(aes(yintercept = jaccard_similarity), data = vlinevar4, lty = "dashed", linewidth = 1) + 
                xlab(NULL) + 
                ylab("Jaccard Similarity") + 
                facet_grid(.~taxon) + 
                scale_color_manual(values = pal, guide = "none") +
                scale_fill_manual(values = pal) + 
                theme(panel.background = element_rect(fill = "white"),
                      panel.grid.major.x = element_line(colour = "grey89"),
                      axis.ticks = element_blank(),
                      axis.text.x = element_blank(), #element_text(color = pal), # size = 14, lineheight = .9
                      legend.key = element_blank(),
                      panel.border = element_blank(),
                      panel.spacing.y = unit(0,"line")) 
        return(out_plot)
}

# load data -------------------------------------------------------------------------

fish <- readRDS("01_data/004_results/fish_typical_2022-06-15.rds")
diat <- readRDS("01_data/004_results/diatoms_typical_2022-06-16.rds")
maph <- readRDS("01_data/004_results/macrophytes_typical_2022-12-14.rds")
null <- readRDS("01_data/004_results/all_typical_null_model2022-12-14.rds")

# prepare data ----------------------------------------------------------------------

#  define color palette
pal <- wes_palette("Darjeeling1")[c(1,2,3,4,5,6)]

fish$taxon = "fish"
diat$taxon = "diatom"
maph$taxon = "macrophyte"

data <- rbindlist(list(diat, fish, maph, null), use.names = TRUE)

#- drop alternative null models
ädata <- data[!typology_system %in% c("null_model2_type", "null_model3_type", "null_model4_type")]

data$typology_system <- factor(data$typology_system, levels = c("brt", "bgr", "enz", "few", "ife", "null_model1_type", "null_model2_type", "null_model3_type", "null_model4_type")) 


res.dia <- data[taxonomic_level == "species" & taxon == "macrophyte" & typology_system == "brt"]
arrange(res.dia, jaccard_similarity)
#res.dia[, median(jaccard_similarity), by = "typology_system"]


# create plots ----------------------------------------------------------------------
plot_fun("species"); ggsave(filename = paste0("04_fig/002_results/004_typical/", Sys.Date(),"_typical_boxplot_w_multiple_null_species.tiff"))
plot_fun("genus");   ggsave(filename = paste0("04_fig/002_results/004_typical/", Sys.Date(),"_typical_boxplot_w_multiple_null_genus.tiff"))
plot_fun("family");  ggsave(filename = paste0("04_fig/002_results/004_typical/", Sys.Date(),"_typical_boxplot_w_multiple_null_family.tiff"))
#


# NMDS plot  ------------------------------------------------------------------------

## distance table diatoms 

create_distance_table <- function(x){
        
        N <- uniqueN(x$type1)
        out <- matrix(data = 666, nrow = N, ncol = N)
        diag(out) <- 1 
        for (i in 1:nrow(x)){
                out[x$id1[i], x$id2[i]] <- x$jaccard_similarity[i]
        }
        out <- 1 - out
        out <- as.dist(out)
        
}


for (i in c("diatom", "fish", "macrophyte")) {
        for (k in c(unique(data$typology_system))) {
                if (k == "null model") next()
                for (l in c("species", "genus", "family")) {
                        # create subset
                        
                        
                        
                        i.dt <-
                                data[taxon == i & typology_system == k & taxonomic_level == l]
                        i.dt[, c("type1", "type2") := .(
                                as.character(type1), 
                                as.character(type2)
                        )]
                        # - create type IDs 
                        i.dt[, c("id1", "id2") := .(as.numeric(as.factor(type1)), as.numeric(as.factor(type2)))]
                        
                        # assign id to types 
                        i.id.table <- copy(i.dt)
                        i.id.table <- i.id.table[, c("type2", "id2")]
                        i.id.table <- unique(i.id.table, by = c("type2"))
                        
                        i.dist <- create_distance_table(i.dt)
                        i.mds  <- metaMDS(i.dist)
                        i.mds <- as.data.table(i.mds$points)
                        i.mds$type <- i.id.table$type2
                        
                        i.out <- 
                                ggplot(i.mds, aes(MDS1, MDS2)) + 
                                geom_label(aes(label = type)) + 
                                ggtitle(paste(i, k , l))
                        ggsave(filename = paste0("fig/results/typical/typical_nmds_",i,"_",k,"_",l,"_", Sys.Date(),".tiff"), i.out)
                        rm(list = ls()[grepl("^i\\.", ls())])
                }
        }
}

