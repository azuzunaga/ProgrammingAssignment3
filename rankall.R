rankall <- function(outcome, num = "best") {
        source("simpleCap.R")
        
        ## Read outcome data
        dat <- read.csv("outcome-of-care-measures.csv", 
                        na.strings = "Not Available", stringsAsFactors = FALSE)
        
        ## Check that outcome is valid
        # State and outcome list
        states <- unique(dat$State)
        outcomes <- c("heart attack", "heart failure", "pneumonia")
        
        # Check outcome name validity
        if (!(outcome %in% outcomes)){
                stop("Invalid outcome")
        }
        
        ## Return hospital name in that state with lowest 30-day death
        ## rate
        outcome <- make.names(simpleCap(outcome))
        
        foo <- dat[c(2, 7, 11, 17, 23)]
        
        colind <- grep(outcome, names(foo), fixed = TRUE)
        
        best <- foo[, c(1, 2, colind)]
        
        best <- best[complete.cases(best), ]
        
        best <- best[order(best[, 2], best[, 3], best[, 1], decreasing = FALSE), ]
        
        ## Return hospital name in that state with the given rank
        splitbest <- split(best, best$State)
        
        if (num == "best") {
                num <- 1
                sub1 <- lapply(splitbest, function(x) x[num, 1:2])
                data.frame(hospital = unlist(sapply(sub1, "[", 1)), 
                           state = names(sub1), row.names = NULL)
                
        } else if (num == "worst") {
                sub1 <- lapply(splitbest, function(x) x[nrow(x), 1:2])
                data.frame(hospital = unlist(sapply(sub1, "[", 1)), 
                           state = names(sub1), row.names = NULL)
        } else {
                num <- num
                sub1 <- lapply(splitbest, function(x) x[num, 1:2])
                data.frame(hospital = unlist(sapply(sub1, "[", 1)), 
                           state = names(sub1), row.names = NULL)
        }
        

        
        ## 30-day death rate
        

        ## For each state, find the hospital of the given rank
        ## Return a data frame with the hospital names and the
        ## (abbreviated) state name
}