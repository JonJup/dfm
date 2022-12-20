newest_sample <- function(x) {
        
        # - restrict to focal month
        x <- x[lubridate::month(date) %in% 5:9]
        x[, sampling.events := uniqueN(gr_sample_id), by = "site_id"]
        x[, newest_date := max(date), by = "site_id"]
        x <- x[sampling.events == 1 | date == newest_date]  
        x[, newest_date := NULL]
        return(x)
}


        
 