best <- function(state, outcome) {
        ## Read outcome data
        df <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
        ## Check that state and outcome are valid
        if(!any(state == state.abb)) {
                stop("invalid state")
        }
        if(!any(outcome == c("heart attack", "heart failure", "pneumonia"))) {
                stop("invalid outcome")
        }
        ##Return hospital name in that state with lowest 30-day death rate
        switch(outcome, "heart attack" = {
                col = 11
                }, "heart failure" = {
                col = 17
                }, "pneumonia" = {
                col = 23
                })
        data_state <- df[df$State == state, c(2, col)]
        data_state[which.min(data_state[, 2]), 1]
}       
                       