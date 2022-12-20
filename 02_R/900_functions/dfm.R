## --- functions to harmonize diatoms --- ### 

# append_to_tt ----------------------------------------------------------------------
#' @param original_name: name you want to add to table 
#' @param fixed_name: original name for taxon with same final name 
#' @param data_from: where can we find fixed name? 
#' @param data_to: what table should the result be added to

#' @export
append_to_tt <- function(original_name, fixed_name, data_from, data_to){
        
        add.id <- which(data_from$original_name == fixed_name)
        
        add <- data_from[add.id, ]
        
        add$original_name <- original_name
        
        out <- rbind(data_to, add)
        
        return(out)
}

# check_fwb -------------------------------------------------------------------------
#' @export
check_fwb <- 
        function(x, dia1){
                x %in% dia1$taxon_old
        }

# get_fwb ---------------------------------------------------------------------------
#' @export
get_fwb <- 
        function(x, data){
                data |>
                        dplyr::filter(taxon_old == x) |>
                        {\(x) x[,1]}() |>
                        as.character()
        }

# get_entry -------------------------------------------------------------------------
#' @export
add_entry_tt <- 
        function(x, data, data2){
                i.gen <- stringr::word(x,1)
                ## genus already in taxontable?
                i.gen.pres <- i.gen %in% data$genus
                ## yes? then get info
                if(i.gen.pres){
                        add <- data[genus == i.gen, ][1,]
                        add$species <- NA
                        add$original_name <- data2
                        add$fixed_name <- x
                        if (stringr::str_detect(x, "\\ "))
                                add$species <- add$fixed_name
                        data <- data.table::rbindlist(list(data, add))
                } else {
                        print(paste(i,": new_genus"))
                }
                return(
                        data
                )
        }

# new_genus -------------------------------------------------------------------------
#' @export
new_genus <- function(
        ori, fix, spe = NA, gen, fam, ord, cla, phy, kin
){ 
        
        if (cla %in% c("Coscinodiscophyceae", "Bacillariophyceae", "Mediophyceae", "Bacillariophyta classis incertae sedis")){
                phy = "Bacillariophyta"
                kin = "Chromista"
        }
        
        add <- data.table::data.table(
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
        taxontable <- data.table::rbindlist(list(taxontable, add))
        return(taxontable)
        
}

# new_entry -------------------------------------------------------------------------
#' @export
new_entry <- function(ori, fix, spe = NULL, gen = NULL, tt = taxontable, tt_a = taxontable_add){
        
        if (is.null(spe))
                spe <- fix 
        if (is.null(gen))
                gen <- stringr::word(fix,1)
        
        # create new table 
        add <-  data.table::data.table(
                original_name = ori,
                fixed_name = fix,
                species = spe,
                genus = gen
        )
        
        ## check that genus is in taxontable 
        if (!add$gen %in% tt$genus){
                stop(paste("genus", add$gen, "not in taxontable. Use new_genus() instead."))
        }
        
        ## extract higher taxa from taxontable. Save in variable to 
        ## check number of unique entries  
        higher_tax <- tt[genus == add$gen]
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
        #add$clean = TRUE
        
        ## combine with previous taxontable
        tt_a <-  data.table::rbindlist(list(tt_a, add))
        ## return output 
        return(tt_a)
}

# new_entry_nd ----------------------------------------------------------------------
## new entry for non-diatom taxa
## same as new_entry but without the fix argument 

#' @export
new_entry_nd <- function(ori, spe, gen = NULL, taxontable){
        
        if (is.null(gen))
                gen <- stringr::word(spe,1)
        
        # create new table 
        add <-  data.table::data.table(original_name = ori,
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
        scl_var    <- unique(higher_tax$subclass)
        cla_var    <- unique(higher_tax$class)
        phy_var    <- unique(higher_tax$phylum)
        kin_var    <- unique(higher_tax$kingdom)
        
        if (length(fam_var) > 1 ) {stop(print("More than one family") )}
        if (length(ord_var) > 1 ) {stop(print("More than one order")  )}
        if (length(scl_var) > 1 ) {stop(print("More than one subclass")  )}
        if (length(cla_var) > 1 ) {stop(print("More than one class")  )}
        if (length(phy_var) > 1 ) {stop(print("More than one phylum") )}
        if (length(kin_var) > 1 ) {stop(print("More than one kingdom"))} 
        
        ## add higher taxonomic lelves
        
        add$family  <- fam_var
        add$order   <- ord_var
        add$subclass <- scl_var
        add$class   <- cla_var
        add$phylum  <- phy_var
        add$kingdom <- kin_var
        
        ## add clean variable 
        add$clean = TRUE
        
        ##
        if("taxon_state" %in% names(taxontable)){
                add$taxon_state <- NA
        }
        
        ## combine with previous taxontable
        taxontable <-  data.table::rbindlist(list(taxontable, add))
        ## return output 
        return(taxontable)
}


# check taxon table -----------------------------------------------------------------
#' @export
check_taxon_table <- function(x){
        
        one <- any(duplicated(x$original_name))
        uspe <- unique(x$species)
        if (any(is.na(uspe)))
                uspe <- uspe[-which(is.na(uspe))]
        prob_spe <- c()
        for (i in seq_along(uspe)){
                if (x[species == uspe[i], data.table::uniqueN(genus)] != 1){
                        prob_spe[length(prob_spe) + 1] <- uspe[i] 
                }
        }
        ugen <- unique(x$genus)
        if (any(is.na(ugen)))
                ugen <- ugen[-which(is.na(ugen))]
        prob_gen <- c()
        for (i in seq_along(uspe)){
                if (x[genus == ugen[i],  data.table::uniqueN(family)] != 1){
                        prob_gen[length(prob_gen) + 1] <- ugen[i] 
                }
        }
        ufam <- unique(x$family) 
        if (any(is.na(ufam)))
                ufam <- ufam[-which(is.na(ufam))]
        prob_fam <- c()
        for (i in seq_along(ufam)){
                if (x[family == ufam[i],  data.table::uniqueN(order)] != 1){
                        prob_fam[length(prob_fam) + 1] <- ufam[i] 
                }
        }
        uord <- unique(x$order) 
        if (any(is.na(uord)))
                uord <- uord[-which(is.na(uord))]
        prob_ord <- c()
        for (i in seq_along(uord)){
                if (x[order == uord[i], data.table::uniqueN(class)] != 1){
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
#' @export
find_point <- function(point_id,distane_matrix){
        column <- ceiling(point_id/nrow(distane_matrix))
        row  <- point_id - (column-1) * nrow(distane_matrix)
        c(row, column)
}


# merge sites1 -----------------------------------------------------------------------
#' @export
merge_sites_on_name <- function(data, site_id1, site_id2){
        xname <- unique(pull(data[site_id == site_id1, c("original_site_name")]))
        data[site_id == site_id2, original_site_name := xname]
        data[site_id == site_id2, site_id := site_id1]
        return(data)
}
# merge sites2 -----------------------------------------------------------------------
#' @export
merge_sites_on_id <- function(data, id, sample_id_ending){
        
        xdata <- data[id]
        if (uniqueN(xdata$site_id) != 1){
                data[site_id == xdata$site_id[1], site_id := xdata$site_id[2]]
                data[, gr_sample_id := paste0("site_", site_id, "_date_", date_id, sample_id_ending)]     
        }

        return(data)

}

# update taxonomy macrophytes -------------------------------------------------------
#' @export
update_taxonomy_macrophytes <- function(TU, taxontable_arg) {
        fill_new_table <- character(length(TU))
        taxontable_new <- data.frame(original_name = TU, species = fill_new_table, 
                                     genus = fill_new_table, family = fill_new_table, order = fill_new_table, 
                                     subclass = fill_new_table, class = fill_new_table, phylum = fill_new_table, 
                                     kingdom = fill_new_table, clean = FALSE, taxon_state = fill_new_table)
        taxontable <- rbind(taxontable_arg, taxontable_new)
        for (i in seq_along(TU)) {
                i.id <- which(taxontable$original_name == TU[i])
                if (taxontable$clean[i.id]) 
                        (next)()
                i.co <- {
                        function(x) x[[1]]
                }(taxize::classification(TU[i], db = "gbif"))
                if (all(is.na(i.co))) 
                        (next)()
                taxontable$species[i.id] <- ifelse("species" %in% i.co$rank, 
                                                   i.co$name[which(i.co$rank == "species")], NA)
                taxontable$genus[i.id] <- ifelse("genus" %in% i.co$rank, 
                                                 i.co$name[which(i.co$rank == "genus")], NA)
                taxontable$family[i.id] <- ifelse("family" %in% i.co$rank, 
                                                  i.co$name[which(i.co$rank == "family")], NA)
                taxontable$order[i.id] <- ifelse("order" %in% i.co$rank, 
                                                 i.co$name[which(i.co$rank == "order")], NA)
                taxontable$subclass[i.id] <- ifelse("subclass" %in% 
                                                            i.co$rank, i.co$name[which(i.co$rank == "subclass")], 
                                                    NA)
                taxontable$class[i.id] <- ifelse("class" %in% i.co$rank, 
                                                 i.co$name[which(i.co$rank == "class")], NA)
                taxontable$phylum[i.id] <- ifelse("phylum" %in% i.co$rank, 
                                                  i.co$name[which(i.co$rank == "phylum")], NA)
                taxontable$kingdom[i.id] <- ifelse("kingdom" %in% i.co$rank, 
                                                   i.co$name[which(i.co$rank == "kingdom")], NA)
                taxontable$clean[i.id] <- TRUE
                rm(i.co)
                rm(i.id)
                gc()
        }
        return(taxontable)
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