rankhospital <- function(state, outcome, num = "best") {
        source("simpleCap.R")
        
        ## Read outcome data
        dat <- read.csv("outcome-of-care-measures.csv", 
                        na.strings = "Not Available", stringsAsFactors = FALSE)
        
        ## Check that state and outcome are valid
        # State and outcome list
        states <- unique(dat$State)
        outcomes <- c("heart attack", "heart failure", "pneumonia")
        
        # Check state name validity
        if (!(toupper(state) %in% states)) {
                stop("Invalid state")                
        }
        
        # Check outcome name validity
        if (!(outcome %in% outcomes)){
                stop("Invalid outcome")
        }
        
        ## Return hospital name in that state with lowest 30-day death
        ## rate
        outcome <- make.names(simpleCap(outcome))
        
        foo <- dat[c(2, 7, 11, 17, 23)]
        
        colind <- grep(outcome, names(foo), fixed = TRUE)
        
        best <- foo[foo$State == state, c(1, 2, colind)]
        
        best <- best[complete.cases(best), ]
        
        best <- best[order(best[, 2], best[, 3], best[, 1], decreasing = FALSE), ]
        
        
        ## Return hospital name in that state with the given rank
        ## 30-day death rate
        
        if (num == "best") {
                print(best[1, 1])
        } else if (num == "worst") {
                print(best[nrow(best), 1])
        } else {
                print(best[num, 1])
        }
}
