## -- Inspect results classification strength 

# created:  21.02.22
# modified: 21.02.22
# project : Evaluating European Broad River Types for Diatoms, Fish and Macrophytes
# purpose : Inspect the results of the classification strength analysis. 

# setup -----------------------------------------------------------------------------
library(pacman)
p_load(rstudioapi, 
       data.table,
       ggplot2,
       magrittr,
       dplyr,
       wesanderson,
       RColorBrewer,
       stringr)
# - color palettes 
pal <- wes_palette("Darjeeling1")[c(1,2,3,4,5)]
myPalette <- colorRampPalette(rev(brewer.pal(11, "RdBu")))

### log files ---- 
sink(file = paste0("02_R/999_log_files/inspect_results_classification_strength_", Sys.Date(), "_", "log.txt"))
documentPath()
sessionInfo()
sink(file = NULL)
### --------------


# functions -------------------------------------------------------------------------
cs_plot <- function(x){
        x%<>%arrange(difference)
        out <- 
                ggplot(x) +  
                geom_linerange(aes(y=type, xmin = between_type, xmax = within_type, col = difference),
                               lwd = 3) + 
                ylab(NULL) + 
                #facet_wrap(.~typology, scale = "free")+
                scale_colour_gradientn(colours = myPalette(100), limits=c(min(x$difference),max(x$difference))) + 
                theme(panel.background = element_rect(fill = "white"),
                      panel.grid.major.x = element_line(colour = "grey89"),
                      axis.ticks = element_blank(),
                      #axis.text.x = element_blank(), #element_text(color = pal), # size = 14, lineheight = .9
                      legend.key = element_blank(),
                      panel.border = element_blank(),
                      panel.spacing.y = unit(0,"line")) 
        
        return(out)
}


# load data -------------------------------------------------------------------------
cs <- readRDS("01_data/004_results/2022-12-16_classification_strength.rds")

# - fix typo in column name
cs %<>% rename(typology = typlogy)


# mean values  ----------------------------------------------------------------------
# for taxa across typology systems 
cs |> 
        filter(resolution == "species") |> 
        filter(!str_detect(typology, "null")) |> 
        group_by(taxon) |> 
        summarize(mean = mean(classification_strength))
# for typology systems across typology taxa 
cs |> 
        filter(resolution == "species") |>
        group_by(typology) |> 
        summarize(mean = mean(classification_strength))

# 
cs |> 
        filter(resolution == "species") |> 
        filter(taxon == "macrophytes") |> 
        unique(by = "typology")

# overall mean 
cs[taxon == "fish" & resolution == "species" & typology == "ife"]
cs[, cas := round(classification_strength, 3)]
cs[taxon == "macrophytes" & resolution == "species", c("typology", "cas")] |> unique(by = "typology")

cs |>   
        mutate(resolution = factor(resolution, levels = c("species", "genus", "family"))) |> 
        ggplot(aes(x = resolution, y = classification_strength, group = taxon)) + 
        geom_line(aes(col = taxon)) +
        facet_wrap(.~typology)

cs %<>% mutate(difference = within_type - between_type)
cs_dia <- filter(cs, taxon == "diatom" & resolution == "genus", typology != "null_model_type")
cs_fis <- filter(cs, taxon == "fish" & resolution == "genus", typology != "null_model_type")
cs_mac <- filter(cs, taxon == "macrophytes" & resolution == "genus", typology != "null_model_type")

cs_dia1 <- arrange(cs_dia, between_type)
cs_dia2 <- arrange(cs_dia, desc(within_type))
cs_dia3 <- arrange(cs_dia, difference)
cs_fis1 <- arrange(cs_fis, between_type)
cs_fis2 <- arrange(cs_fis, desc(within_type))
cs_fis3 <- arrange(cs_fis, difference)
cs_mac1 <- arrange(cs_mac, between_type)
cs_mac2 <- arrange(cs_mac, desc(within_type))
cs_mac3 <- arrange(cs_mac, difference)

cs_dia1 %<>% mutate(type = factor(type, levels = unique(cs_dia1$type)))
cs_dia2 %<>% mutate(type = factor(type, levels = unique(cs_dia2$type)))
cs_dia3 %<>% mutate(type = factor(type, levels = unique(cs_dia3$type)))
cs_fis1 %<>% mutate(type = factor(type, levels = unique(cs_fis1$type)))
cs_fis2 %<>% mutate(type = factor(type, levels = unique(cs_fis2$type)))
cs_fis3 %<>% mutate(type = factor(type, levels = unique(cs_fis3$type)))
cs_mac1 %<>% mutate(type = factor(type, levels = unique(cs_mac1$type)))
cs_mac2 %<>% mutate(type = factor(type, levels = unique(cs_mac2$type)))
cs_mac3 %<>% mutate(type = factor(type, levels = unique(cs_mac3$type)))


cs_plot(cs_dia1) + facet_wrap(.~typology, scales="free") + ggtitle("diatoms")
