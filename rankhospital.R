rankhospital <- function(state, outcome, num) {
      
      ## Read outcome data
      outcome_data <- read.csv("outcome-of-care-measures.csv")
      mortality_data <- data.frame("Hospital" = outcome_data[,2], 
                                   "State" = outcome_data[,7],
                                   "HA_Mortality" = as.numeric(as.character(outcome_data[,11])),
                                   "HF_Mortality" = as.numeric(as.character(outcome_data[,17])),
                                   "P_Mortality" = as.numeric(as.character(outcome_data[,23])))
      
      ## Check that state and outcome are valid
      ## Return hospital name in that state with the given rank 30-day death rate
      mort_data_by_state <- mortality_data[which(mortality_data$State == state), ]
      
      if(nrow(mort_data_by_state) == 0) {
            stop("invalid state")
      }
      
      if(outcome == "heart attack") {
            HA_mort_hopsitals <- na.omit(data.frame("Hospital" = mort_data_by_state$Hospital,
                                                    "HA_Mortality" = mort_data_by_state$HA_Mortality))
            order_HA_mortality <- order(HA_mort_hopsitals$HA_Mortality, HA_mort_hopsitals$Hospital)
            min_HA_mort_hospitals <- HA_mort_hopsitals[order_HA_mortality, "Hospital", ]
                        
            if(num == "best") {
                  return(as.character(min_HA_mort_hospitals[1]))
            }
            
            else if(num == "worst") {
                  return(as.character(min_HA_mort_hospitals[length(min_HA_mort_hospitals)]))
            }
            
            else if(num > 0 & num <= length(min_HA_mort_hospitals)) {
                  return(as.character(min_HA_mort_hospitals[num]))
            }
            
            else {
                  return(NA)
            }
      }
      
      else if(outcome == "heart failure") {
            HF_mort_hopsitals <- na.omit(data.frame("Hospital" = mort_data_by_state$Hospital,
                                                    "HF_Mortality" = mort_data_by_state$HF_Mortality))
            order_HF_mortality <- order(HF_mort_hopsitals$HF_Mortality, HF_mort_hopsitals$Hospital)
            min_HF_mort_hospitals <- HF_mort_hopsitals[order_HF_mortality, "Hospital", ]
            
            if(num == "best") {
                  return(as.character(min_HF_mort_hospitals[1]))
            }
            
            else if(num == "worst") {
                  return(as.character(min_HF_mort_hospitals[length(min_HF_mort_hospitals)]))
            }
            
            else if(num > 0 & num <= length(min_HF_mort_hospitals)) {
                  return(as.character(min_HF_mort_hospitals[num]))
            }
            
            else {
                  return(NA)
            }
      }
      
      else if(outcome == "pneumonia") {
            P_mort_hopsitals <- na.omit(data.frame("Hospital" = mort_data_by_state$Hospital,
                                                   "P_Mortality" = mort_data_by_state$P_Mortality))
            order_P_mortality <- order(P_mort_hopsitals$P_Mortality, P_mort_hopsitals$Hospital)
            min_P_mort_hospitals <- P_mort_hopsitals[order_P_mortality, "Hospital", ]
                        
            if(num == "best") {
                  return(as.character(min_P_mort_hospitals[1]))
            }
            
            else if(num == "worst") {
                  return(as.character(min_P_mort_hospitals[length(min_P_mort_hospitals)]))
            }
            
            else if(num > 0 & num <= length(min_P_mort_hospitals)) {
                  return(as.character(min_P_mort_hospitals[num]))
            }
            
            else {
                  return(NA)
            }
      }
      
      else {
            stop("invalid outcome")
      }
}