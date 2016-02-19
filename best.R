best <- function(state, outcome){
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
                                  "heart attack" = 13,
                                  "heart failure" = 19,
                                  "pneumonia" = 25)
      outcome_selection <- Outcome[,outcome_selector]
      hospital_names <- Outcome$Hospital.Name[outcome_selection != "Not Available"]
      outcome_selection <- as.numeric(outcome_selection[outcome_selection != "Not Available"])
      return(sort(hospital_names[which(outcome_selection==min(outcome_selection))])[1])
      
}

