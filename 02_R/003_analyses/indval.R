### ---------------------- ###
### --- compute indval --- ### 
### ---------------------- ###

# -------------------------------
# date written: 10.03.22
# date last modified: 14.12.22 
# Project: Evaluating European Broad River Types for Diatoms, Fish and Macrophytes
# Purpose: Compute indval
# Notes: 
# last changes: adapt to four null model
# -------------------------------

# functions --------------------------------------------------------------------------
indval_fun1 <- 
        function(typology){
                x1 <- lapply(data,
                       function(x)
                               multipatt(
                                       x[, -c(1:10)],
                                       cluster = unlist(x[,(typology), with = FALSE]),
                                       func = "IndVal.g",
                                       duleg = TRUE
                               ))    
                
}

indval_fun2 <- 
        function(y){
                out <- 
                        y |> 
                        lapply(function(x) x$sign)|> 
                        lapply(function(x) setDT(x))|> 
                        lapply(function(x) x[p.value <=0.05])|> 
                        lapply(function(x) nrow(x))     |> 
                        unlist()
                return(out)
        }

indval_fun3 <- 
        function(t){
                out <- 
                        t |> 
                        lapply(function(x) x$sign)|> 
                        lapply(function(x) mean(x$p.value, na.rm = TRUE)) |> 
                        unlist()
                        
        }


r.brt  <- indval_fun1("brt")
r2.brt <- indval_fun2(r.brt)
r3.brt <- indval_fun3(r.brt)

r.ife  <- indval_fun1("ife")
r2.ife <- indval_fun2(r.ife)
r3.ife <- indval_fun3(r.ife)

r.bgr  <- indval_fun1("bgr")
r2.bgr <- indval_fun2(r.bgr)
r3.bgr <- indval_fun3(r.bgr)

r.few  <- indval_fun1("few")
r2.few <- indval_fun2(r.few)
r3.few <- indval_fun3(r.few)

r.enz <- indval_fun1("enz")
r2.enz <- indval_fun2(r.enz)
r3.enz <- indval_fun3(r.enz)


results_indval <- 
        data.table(
                taxon = taxon, 
                typology = rep(c("brt", "ife", "bgr", "feow", "enz"), each = 4),
                taxoonomic.resolution = rep(c("species", "genus", "family", "order"), times = 5),
                n.indicator = c(r2.brt, r2.ife, r2.bgr, r2.few, r2.enz),
                mean_p = c(r3.brt, r3.ife, r3.bgr, r3.few, r3.enz)
        )

