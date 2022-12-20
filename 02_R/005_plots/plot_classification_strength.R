## -- Plot classification strength 

# created: 15.06.22
# modified: 16.12.22
# project : Evaluating European Broad River Types for Diatoms, Fish and Macrophytes
# purpose : visualize the results of the classification strength analysis
# last change: plot macrophytes including bryophytes

# setup -----------------------------------------------------------------------------
pacman::p_load(data.table, dplyr, ggplot2, tidyr, magrittr, wesanderson, RColorBrewer, stringr)

### log files ---- 
sink(file = paste0("02_R/999_log_files/classification_strength_plot", Sys.Date(), "_", "log.txt"))
rstudioapi::documentPath()
sessionInfo()
sink(file = NULL)
### --------------

## functions ---- 
cs_plot <- function(x){
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
                      axis.text.x = element_blank(), #element_text(color = pal), # size = 14, lineheight = .9
                      legend.key = element_blank(),
                      panel.border = element_blank(),
                      panel.spacing.y = unit(0,"line")) 
        
        return(out)
}



# load data -------------------------------------------------------------------------
cs <- readRDS("01_data/004_results/2022-12-16_classification_strength.rds")

# - fix typo in column name
cs %<>% rename(typology = typlogy)

# - color palettes 
pal <- wes_palette("Darjeeling1")[c(1,2,3,4,5)]
myPalette <- colorRampPalette(rev(brewer.pal(11, "RdBu")))

# classification strength plots -----------------------------------------------------

shapes = c(21,22,23)

## DIATOMS 
vlinevar <- cs[typology == "null_model1_type" & resolution == "species"]
vlinevar %<>% unique(by = c("typology", "resolution", "taxon"))
out_plot <- cs |>  
        filter(!str_detect(typology, "null_model"), resolution == "species") |> 
        unique(by = c("typology", "resolution", "taxon")) |> 
        ggplot(aes(y=classification_strength, x=typology)) + 
        
        #geom_bar(size = 4, width = 0.1) +
        geom_col(aes(fill = typology), alpha = .8) +
        geom_hline(aes(yintercept = classification_strength), data = vlinevar, lty = "dashed", linewidth = 1) + 
        xlab(NULL) + 
        ylab("Classification strength") + 
        facet_grid(.~taxon) + 
        scale_color_manual(values = pal, guide = "none") +
        scale_fill_manual(values = pal) + 
        scale_shape_manual(values = shapes, guide = "none") + 
        theme(panel.background = element_rect(fill = "white"),
              panel.grid.major.x = element_line(colour = "grey89"),
              axis.ticks = element_blank(),
              axis.text.x = element_blank(), #element_text(color = pal), # size = 14, lineheight = .9
              legend.key = element_blank(),
              panel.border = element_blank(),
              panel.spacing.y = unit(0,"line")) 
ggsave(filename = paste0("04_fig/002_results/002_classification_strength/cs_all_", Sys.Date(), ".tiff"))

        # within and between plots ----------------------------------------------------------
cs %<>% mutate(difference = within_type - between_type)
cs_dia <- filter(cs, taxon == "diatom" & resolution == "genus"     , !str_detect(typology, "null_model"))
cs_fis <- filter(cs, taxon == "fish" & resolution == "genus"       , !str_detect(typology, "null_model"))
cs_mac <- filter(cs, taxon == "macrophytes" & resolution == "genus", !str_detect(typology, "null_model"))

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

ggsave(filename = paste0("04_fig/002_results/002_classification_strength/cs_wb_dia1_", Sys.Date(),".tiff"), plot = cs_plot(cs_dia1))
ggsave(filename = paste0("04_fig/002_results/002_classification_strength/cs_wb_dia2_", Sys.Date(), ".tiff"), plot = cs_plot(cs_dia2))
ggsave(filename = paste0("04_fig/002_results/002_classification_strength/cs_wb_dia3_", Sys.Date(), ".tiff"), plot = cs_plot(cs_dia3))
ggsave(filename = paste0("04_fig/002_results/002_classification_strength/cs_wb_fis1_", Sys.Date(), ".tiff"), plot = cs_plot(cs_fis1))
ggsave(filename = paste0("04_fig/002_results/002_classification_strength/cs_wb_fis2_", Sys.Date(), ".tiff"), plot = cs_plot(cs_fis2))
ggsave(filename = paste0("04_fig/002_results/002_classification_strength/cs_wb_fis3_", Sys.Date(), ".tiff"), plot = cs_plot(cs_fis3))
ggsave(filename = paste0("04_fig/002_results/002_classification_strength/cs_wb_mac1_", Sys.Date(), ".tiff"), plot = cs_plot(cs_mac1))
ggsave(filename = paste0("04_fig/002_results/002_classification_strength/cs_wb_mac2_", Sys.Date(), ".tiff"), plot = cs_plot(cs_mac2))
ggsave(filename = paste0("04_fig/002_results/002_classification_strength/cs_wb_mac3_", Sys.Date(), ".tiff"), plot = cs_plot(cs_mac3))




# old code --------------------------------------------------------------------------
# # ---- 
# 
# cs |>  
#         filter(taxon == "fish" & resolution == "species") |> 
#         pivot_longer(cols = c("within_type", "between_type"), names_to = "sim_type", values_to = "similarity") |> 
#         ggplot(aes(x=similarity,y=type)) + 
#         geom_point(aes(fill = sim_type), shape = 21) + 
#         facet_wrap(.~typlogy, scales = "free")+
#         theme(legend.position = "none")
# cs |>  
#         filter(taxon == "macrophytes" & resolution == "species") |> 
#         pivot_longer(cols = c("within_type", "between_type"), names_to = "sim_type", values_to = "similarity") |> 
#         ggplot(aes(x=similarity,y=type)) + 
#         geom_point(aes(fill = sim_type), shape = 21) + 
#         facet_wrap(.~typlogy, scales = "free")+
#         theme(legend.position = "none")
# 




