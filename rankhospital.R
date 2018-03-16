rankhospital <- function(state, outcome, num = "best") {
        ## Read outcome data
        df <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
        ## Check that state and outcome are valid
        if(!any(state == state.abb)) {
                stop("invalid state")
        }
        if(!any(outcome == c("heart attack", "heart failure", "pneumonia"))) {
                stop("invalid outcome")
        }
        ## Return hospital name in that state with the given rank
        ## 30-day death rate
        switch(outcome, "heart attack" = {
                col = 11
        }, "heart failure" = {
                col = 17
        }, "pneumonia" = {
                col = 23
        })
        data_state <- df[df$State == state, c(2, col)]
        data_state[, 2] <- as.numeric(data_state[, 2])
        rank_list <- data_state[order(data_state[, 2], data_state[, 1]), ]
                if (num == "best") {
                        rank_list[1, 1]
                } else if (num == "worst") { 
                        rank_list[sum(!is.na(rank_list[, 2])), 1]
                } else if (num <= (nrow(rank_list))) { 
                        rank_list[num, 1]
                } else if (num > (nrow(rank_list))) { 
                                        c(NA)
                }
}
