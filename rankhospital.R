rankhospital <- function(state, outcome, num = "best"){
      Outcome <- read.csv("outcome-of-care-measures.csv", stringsAsFactors = F)
      if(!state %in% Outcome$State ) {
            stop("invalide state")
      }
      possible_outcome <- c("heart attack", "heart failure", "pneumonia")
      if(!outcome %in% possible_outcome) {
            stop("invalid outcome")
      }
      
      Outcome <- Outcome[Outcome$State == state,]
      outcome_selector <- switch(outcome,
                                 "heart attack" = 11,
                                 "heart failure" = 17,
                                 "pneumonia" = 23)
      
      hospital_frame <- Outcome[,c(2,outcome_selector)]
      names(hospital_frame) <- c("name", "outcome")
      hospital_frame <- hospital_frame[hospital_frame$outcome != "Not Available" ,]
      hospital_frame$outcome <- as.numeric(hospital_frame$outcome)
      hospital_frame <- hospital_frame[order(hospital_frame$outcome, hospital_frame$name),]
      #hospital_frame$rank <- 1:nrow(hospital_frame)
      
      
      mu <- if (num == "best") {1
      } else if (num == "worst") {nrow(hospital_frame)
      } else {num}
 
      if(mu > nrow(hospital_frame)) {return(NA)}
      else{return(hospital_frame$name[mu])}
}