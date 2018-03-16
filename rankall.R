rankall <- function(outcome, num = "best") {
        ## Read outcome data
        df <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
        ## Check that state and outcome are valid
        if(!any(outcome == c("heart attack", "heart failure", "pneumonia"))) {
                stop("invalid outcome")
        }
        ## For each state, find the hospital of the given rank
        ## Return a data frame with the hospital names and the (abbreviated) 
        ## state name
        switch(outcome, "heart attack" = {
                col = 11
        }, "heart failure" = {
                col = 17
        }, "pneumonia" = {
                col = 23
        })
        d = data.frame(hospital=rep(0), state=rep(0))
        sa <- sort(state.abb)
        dc <- c("DC")
        sa_dc <- c(sa[1:7], dc, sa[8:50])
        for (i in sa_dc) {
                data_state <- df[df$State == i, c(2, 7, col)]
                data_state[, 3] <- as.numeric(data_state[, 3])
                rank_list <- data_state[order(data_state[, 3], data_state[, 2]), ]
                h_name <- if (num == "best") {
                        rank_list[1, 1]
                } else if (num == "worst") { 
                        rank_list[sum(!is.na(rank_list[, 2])), 1]
                } else if (num <= (nrow(rank_list))) {
                        rank_list[num, 1]
                } else if (num > (nrow(rank_list))) { 
                        c(NA)
                } 
                d[i, ] = c(h_name, i)
        }
        d[-1,]
}