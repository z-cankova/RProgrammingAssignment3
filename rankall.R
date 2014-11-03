rankall <- function(outcome, num = "best") {
      
      ## Read outcome data
      outcome_data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
      mortality_data <- data.frame("Hospital" = outcome_data[,2], 
                                   "State" = outcome_data[,7],
                                   "HA_Mortality" = as.numeric(outcome_data[,11]),
                                   "HF_Mortality" = as.numeric(outcome_data[,17]),
                                   "P_Mortality" = as.numeric(outcome_data[,23]))
      
      ## Check that state and outcome are valid
      ## For each state, find the hospital of the given rank
      ## Return a data frame with the hospital names and the (abbreviated) state name
      hospital_rank_data <- data.frame()
      states <- levels(mortality_data$State)
      
      for(i in 1:length(states)) {
            mort_data_by_state <- mortality_data[which(mortality_data$State == states[i]), ]
            
            if(nrow(mort_data_by_state) == 0) {
                  hospital_rank_data <- rbind(hospital_rank_data, data.frame("hospital" = NA, 
                                                                             "state" = states[i], 
                                                                             row.names = states[i]))
            }
            
            if(outcome == "heart attack") {
                  HA_mort_hopsitals <- na.omit(data.frame("Hospital" = mort_data_by_state$Hospital,
                                                          "HA_Mortality" = mort_data_by_state$HA_Mortality))
                  order_HA_mortality <- order(HA_mort_hopsitals$HA_Mortality, HA_mort_hopsitals$Hospital)
                  min_HA_mort_hospitals <- HA_mort_hopsitals[order_HA_mortality, "Hospital", ]
                  
                  if(num == "best") {
                        hospital_rank_data <- rbind(hospital_rank_data,
                                                    data.frame("hospital" = min_HA_mort_hospitals[1], 
                                                               "state" = states[i], 
                                                               row.names = states[i]))
                  }
                  
                  else if(num == "worst") {
                        hospital_rank_data <- rbind(hospital_rank_data,
                                                    data.frame("hospital" = min_HA_mort_hospitals[length(min_HA_mort_hospitals)], 
                                                               "state" = states[i], 
                                                               row.names = states[i]))
                  }
                  
                  else if(num > 0 & num <= length(min_HA_mort_hospitals)) {
                        hospital_rank_data <- rbind(hospital_rank_data, data.frame("hospital" = min_HA_mort_hospitals[num], 
                                                                                   "state" = states[i], 
                                                                                   row.names = states[i]))
                  }
                  
                  else {
                        hospital_rank_data <- rbind(hospital_rank_data, data.frame("hospital" = NA, 
                                                                                   "state" = states[i], 
                                                                                   row.names = states[i]))
                  }
            }
            
            else if(outcome == "heart failure") {
                  HF_mort_hopsitals <- na.omit(data.frame("Hospital" = mort_data_by_state$Hospital,
                                                          "HF_Mortality" = mort_data_by_state$HF_Mortality))
                  order_HF_mortality <- order(HF_mort_hopsitals$HF_Mortality, HF_mort_hopsitals$Hospital)
                  min_HF_mort_hospitals <- HF_mort_hopsitals[order_HF_mortality, "Hospital", ]
                                    
                  if(num == "best") {
                        hospital_rank_data <- rbind(hospital_rank_data,
                                                    data.frame("hospital" = min_HF_mort_hospitals[1], 
                                                               "state" = states[i], 
                                                               row.names = states[i]))
                  }
                  
                  else if(num == "worst") {
                        hospital_rank_data <- rbind(hospital_rank_data,
                                                    data.frame("hospital" = min_HF_mort_hospitals[length(min_HF_mort_hospitals)], 
                                                               "state" = states[i], 
                                                               row.names = states[i]))
                  }
                  
                  else if(num > 0 & num <= length(min_HF_mort_hospitals)) {
                        hospital_rank_data <- rbind(hospital_rank_data, data.frame("hospital" = min_HF_mort_hospitals[num], 
                                                                                   "state" = states[i], 
                                                                                   row.names = states[i]))
                  }
                  
                  else {
                        hospital_rank_data <- rbind(hospital_rank_data, data.frame("hospital" = NA, 
                                                                                   "state" = states[i], 
                                                                                   row.names = states[i]))
                  }
            }
            
            else if(outcome == "pneumonia") {
                  P_mort_hopsitals <- na.omit(data.frame("Hospital" = mort_data_by_state$Hospital,
                                                          "P_Mortality" = mort_data_by_state$P_Mortality))
                  order_P_mortality <- order(P_mort_hopsitals$P_Mortality, P_mort_hopsitals$Hospital)
                  min_P_mort_hospitals <- P_mort_hopsitals[order_P_mortality, "Hospital", ]
                                    
                  if(num == "best") {
                        hospital_rank_data <- rbind(hospital_rank_data,
                                                    data.frame("hospital" = min_P_mort_hospitals[1], 
                                                               "state" = states[i], 
                                                               row.names = states[i]))
                  }
                  
                  else if(num == "worst") {
                        hospital_rank_data <- rbind(hospital_rank_data,
                                                    data.frame("hospital" = min_P_mort_hospitals[length(min_P_mort_hospitals)], 
                                                               "state" = states[i], 
                                                               row.names = states[i]))
                  }
                  
                  else if(num > 0 & num <= length(min_P_mort_hospitals)) {
                        hospital_rank_data <- rbind(hospital_rank_data, data.frame("hospital" = min_P_mort_hospitals[num], 
                                                                                   "state" = states[i], 
                                                                                   row.names = states[i]))
                  }
                  
                  else {
                        hospital_rank_data <- rbind(hospital_rank_data, data.frame("hospital" = NA, 
                                                                                   "state" = states[i], 
                                                                                   row.names = states[i]))
                  }
            }
            
            else {
                  stop("invalid outcome")
            }
      }
      
      return(hospital_rank_data)
}