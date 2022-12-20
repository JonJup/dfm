# —————————————————————————— #
# ——— Create taxon table ——— # 
# —————————————————————————— #

# ———————————————————————————————————
# date first written: 10-11-21
# date last modified: 10-11-21
# Project: Evaluating European Broad River Types for Diatoms, Fish and Macrophytes
# Purpose: Create the taxon tables 
# ————————————————

library(data.table)

taxontable <- data.table(
        original_name = "placeholder",
        species       = "placeholder",
        genus         = "placeholder",
        family        = "placeholder",
        order         = "placeholder",
        subclass      = "placeholder",
        class         = "placeholder",
        phylum        = "placeholder",
        kingdom       = "placeholder",
        clean         = TRUE
        )

saveRDS(taxontable, "data/diatoms/211110_taxontable_diatoms.rds")
saveRDS(taxontable, "data/fish/211110_taxontable_fish.rds")
saveRDS(taxontable, "data/macrophytes/211110_taxontable_macrophytes.rds")

