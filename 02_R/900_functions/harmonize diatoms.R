## --- functions to harmonize diatoms --- ### 



# append_to_tt ----------------------------------------------------------------------
append_to_tt <- function(x,y){
        add <- taxontable[original_name == x]
        add$original_name <- y
        taxontable <- rbindlist(list(taxontable, add))   
        return(taxontable)
}

# check_fwb -------------------------------------------------------------------------
check_fwb <- 
        function(x){
                x %in% dia1$taxon_old
        }

# get_fwb ---------------------------------------------------------------------------
get_fwb <- 
        function(x){
                dia1 |>
                        filter(taxon_old == x) |>
                        {\(x) x[,1]}() |>
                        as.character()
                }

# get_entry -------------------------------------------------------------------------

add_entry_tt <- 
        function(x, add_to){
                i.gen <- stringr::word(x,1)
                ## genus already in taxontable?
                i.gen.pres <- i.gen %in% taxontable$genus
                ## yes? then get info
                if(i.gen.pres){
                        add               <- taxontable[genus == i.gen, ][1,]
                        add$species       <- NA
                        add$original_name <- i.tu
                        add$fixed_name    <- x
                        if (str_detect(x, "\\ "))
                                add$species <- add$fixed_name
                        add_to <- rbindlist(list(add_to, add))
                } else {
                        print(paste(i,": new_genus"))
                }
                return(
                        add_to
                )
        }

# new_genus -------------------------------------------------------------------------

new_genus <- function(
        ori, fix, spe = NA, gen, fam, ord, cla, phy, kin
){ 
        
        if (cla %in% c("Coscinodiscophyceae", "Bacillariophyceae", "Mediophyceae", "Bacillariophyta classis incertae sedis")){
                phy = "Bacillariophyta"
                kin = "Chromista"
        }
            
        add <- data.table(
                original_name = ori,
                fixed_name = fix,
                species = spe, 
                genus = gen,
                family = fam, 
                order = ord, 
                class = cla,
                phylum = phy,
                kingdom = kin
        )
        taxontable <- rbindlist(list(taxontable, add))
        return(taxontable)
        
        }

# new_entry -------------------------------------------------------------------------

new_entry <- function(ori, fix, spe = NULL, gen = NULL){
        
        if (is.null(spe))
                spe <- fix 
        if (is.null(gen))
                gen <- word(fix,1)
        
        # create new table 
        add <- data.table(original_name = ori,
                          fixed_name = fix,
                          species = spe, 
                          genus = gen)
        
        ## check that genus is in taxontable 
        if (!add$gen %in% taxontable$genus){
                stop(paste("genus", add$gen, "not in taxontable. Use new_genus() instead."))
        }
        
        ## extract higher taxa from taxontable. Save in variable to 
        ## check number of unique entries  
        higher_tax <- taxontable[genus == add$gen]
        fam_var    <- unique(higher_tax$family)
        ord_var    <- unique(higher_tax$order)
        cla_var    <- unique(higher_tax$class)
        phy_var    <- unique(higher_tax$phylum)
        kin_var    <- unique(higher_tax$kingdom)
        
        if (length(fam_var) > 1 ) {stop(print("More than one family") )}
        if (length(ord_var) > 1 ) {stop(print("More than one order")  )}
        if (length(cla_var) > 1 ) {stop(print("More than one class")  )}
        if (length(phy_var) > 1 ) {stop(print("More than one phylum") )}
        if (length(kin_var) > 1 ) {stop(print("More than one kingdom"))} 
        
        ## add higher taxonomic lelves
        
        add$family  <- fam_var
        add$order   <- ord_var
        add$class   <- cla_var
        add$phylum  <- phy_var
        add$kingdom <- kin_var
        
        ## add clean variable 
        # add$clean = TRUE
        
        ## combine with previous taxontable
        taxontable <- rbindlist(list(taxontable, add))
        ## return output 
        return(taxontable)
}
# check taxon table -----------------------------------------------------------------

check_taxon_table <- function(x){
        one <- any(duplicated(x$original_name))
        uspe <- unique(x$species) |> na.omit()
        prob_spe <- c()
        for (i in seq_along(uspe)){
                if (x[species == uspe[i], uniqueN(genus)] != 1){
                        prob_spe[length(prob_spe) + 1] <- uspe[i] 
                }
        }
        ugen <- unique(x$genus) |> na.omit()
        prob_gen <- c()
        for (i in seq_along(uspe)){
                if (x[genus == ugen[i], uniqueN(family)] != 1){
                        prob_gen[length(prob_gen) + 1] <- ugen[i] 
                }
        }
        ufam <- unique(x$family) |> na.omit()
        prob_fam <- c()
        for (i in seq_along(ufam)){
                if (x[family == ufam[i], uniqueN(order)] != 1){
                        prob_fam[length(prob_fam) + 1] <- ufam[i] 
                }
        }
        uord <- unique(x$order) |> na.omit()
        prob_ord <- c()
        for (i in seq_along(uord)){
                if (x[order == uord[i], uniqueN(class)] != 1){
                        prob_ord[length(prob_ord) + 1] <- uord[i] 
                }
        }
        
        out <- list(
                one, 
                prob_spe, 
                prob_gen,
                prob_fam,
                prob_ord
        )
        
        return(out)
}


# find point ------------------------------------------------------------------------
find_point <- function(b){
        column <- ceiling(b/nrow(distances2))
        row  <- b - (column-1) * nrow(distances2)
        c(row, column)
}


# merge sites -----------------------------------------------------------------------
merge_sites <- function(data, site_id1, site_id2){
        xname <- unique(pull(data[site_id == site_id1, c("original_site_name")]))
        data[site_id == site_id2, original_site_name := xname]
        data[site_id == site_id2, site_id := site_id1]
        return(data)
}



# old -------------------------------------------------------------------------------
# check_ssv <- function(x){i.tax %in% dia2$taxon}
# get_ssv   <- function(x){
#         i.id <- which(dia2$taxon == x) 
#         i.use <- dia2$usage[i.id]
#         if (i.use == "synonym"){
#                 i.new <- dia2$new[i.id]
#                 i.id2 <- which(dia2$old == i.new)
#                 y <- dia2$taxon[i.id2]
#         } else if (i.use == "accepted"){
#                 y <- x 
#         } else {
#                 print(paste("loop", i, " —— new usage"))
#                 break()
#         }
#         y
# }
# check_omn <- function(x){
#         any(stringr::str_detect(dia3$taxon, pattern = i.tax))
# }
# get_omn   <- function(x){
#         i.id <- which(stringr::str_detect(dia3$taxon, x)) 
#         i.use <- dia3$new[i.id]
#         if (is.na(i.use)){
#                 y <- readline(prompt = dia3$taxon[i.id])
#         } else {
#                 i.use2 <- readline(prompt = i.use)
#                 i.id2 <- which(dia3$old == i.use2)
#                 y <- dia3$taxon[i.id2]
#                 y <- readline(prompt = y)
#         }
#         y
# }