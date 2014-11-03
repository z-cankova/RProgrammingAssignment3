best <- function(state, outcome) {
      
      ## Read outcome data
      outcome_data <- read.csv("outcome-of-care-measures.csv")
      mortality_data <- data.frame("Hospital" = outcome_data[,2], 
                                   "State" = outcome_data[,7],
                                   "HA_Mortality" = as.numeric(as.character(outcome_data[,11])),
                                   "HF_Mortality" = as.numeric(as.character(outcome_data[,17])),
                                   "P_Mortality" = as.numeric(as.character(outcome_data[,23])))
      
      ## Check that state and outcome are valid
      ## Return hospital name in that state with lowest 30-day death rate
      mort_data_by_state <- mortality_data[which(mortality_data$State == state), ]
      
      if(nrow(mort_data_by_state) == 0) {
            stop("invalid state")
      }
      
      if(outcome == "heart attack") {
            min_HA_mortality <- min(mort_data_by_state$HA_Mortality, na.rm = TRUE)
            min_HA_mort_hospitals <- mort_data_by_state[which(mort_data_by_state$HA_Mortality == min_HA_mortality), "Hospital"]
            sorted_min_HA_mort_hospitals <- min_HA_mort_hospitals[order(min_HA_mort_hospitals)]
            return(as.character(sorted_min_HA_mort_hospitals[1]))
      }
      
      else if(outcome == "heart failure") {
            min_HF_mortality <- min(mort_data_by_state$HF_Mortality, na.rm = TRUE)
            min_HF_mort_hospitals <- mort_data_by_state[which(mort_data_by_state$HF_Mortality == min_HF_mortality), "Hospital"]
            sorted_min_HF_mort_hospitals <- min_HF_mort_hospitals[order(min_HF_mort_hospitals)]
            return(as.character(sorted_min_HF_mort_hospitals[1]))
      }
      
      else if(outcome == "pneumonia") {
            min_P_mortality <- min(mort_data_by_state$P_Mortality, na.rm = TRUE)
            min_P_mort_hospitals <- mort_data_by_state[which(mort_data_by_state$P_Mortality == min_P_mortality), "Hospital"]
            sorted_min_P_mort_hospitals <- min_P_mort_hospitals[order(min_P_mort_hospitals)]
            return(as.character(sorted_min_P_mort_hospitals[1]))
      }
      
      else {
            stop("invalid outcome")
      }
}
