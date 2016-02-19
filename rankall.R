rankall <- function(outcome, num = "best") {
        ## Read outcome data
        input <- read.csv("outcome-of-care-measures.csv", stringsAsFactors = F)
        
        possible_outcome <- c("heart attack", "heart failure", "pneumonia")
        if(!outcome %in% possible_outcome) {
                stop("invalid outcome")
        }
        final_frame <- data.frame()
        for(i in sort(unique(input$State))) {
                Outcome <- input[input$State == i,]
                outcome_selector <- switch(outcome,
                                           "heart attack" = 11,
                                           "heart failure" = 17,
                                           "pneumonia" = 23)
                hospital_frame <- Outcome[,c(2,outcome_selector)]
                names(hospital_frame) <- c("name", "outcome")
                hospital_frame <- hospital_frame[hospital_frame$outcome != "Not Available" ,]
                hospital_frame$outcome <- suppressWarnings(as.numeric(hospital_frame$outcome))
                hospital_frame <- hospital_frame[order(hospital_frame$outcome, hospital_frame$name),]
                hospital_frame$rank <- 1:nrow(hospital_frame)
                
                mu <- if (num == "best") {1
                } else if (num == "worst") {nrow(hospital_frame)
                } else {num}
        
                if(mu > nrow(hospital_frame)) {
                        name <- NA
                } else {
                        name <- hospital_frame$name[mu]
                }

                
                temp_frame <- data_frame(name,i)
                final_frame <- rbind(final_frame, temp_frame)
                
        }
        names(final_frame) <- c("hospital", "state")
        return(final_frame)
}