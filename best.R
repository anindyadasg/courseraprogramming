best <- function(state, outcome ){
  setwd("~/Desktop/Coursera R Programming/rprog-data-ProgAssignment3-data")
  state <- as.character(state)
  outcome <- as.character(outcome)
  outcome_master <- matrix(data=c("heart attack","heart failure","pneumonia"),nrow=3,ncol=1)
  pos_outcome <- match(outcome,outcome_master,nomatch=0)
  err_msg1 <- c('Error in best("')
  err_msg2 <- c('", "')
  err_msg3 <- c('") : invalid state')
  err_msg4 <- c('") : invalid outcome')
  if ( pos_outcome == 0) {
    stop(err_msg1,state,err_msg2,outcome,err_msg4)
    geterrmessage()
    }
  outcomeofcare <- read.csv("outcome-of-care-measures.csv",header=TRUE,sep=",",na.strings="Not Available",stringsAsFactors = FALSE)
  state_factor <- as.factor(outcomeofcare$State)
  state_factor_level <- levels(state_factor)
  pos_state <- match(state,state_factor_level,nomatch = 0)
  if (pos_state > 0 ){
    if (outcome == "heart attack"){
      outcomeofcare_final <- outcomeofcare[complete.cases(outcomeofcare[11]),]
      outcomeofcare_final <- outcomeofcare_final[c(2,7,11)]
      outcomeofcare_final_state <- outcomeofcare_final[which(outcomeofcare_final$State == state),]
      names(outcomeofcare_final_state)[1] <- "Hosp"
      names(outcomeofcare_final_state)[3] <- "MR"
      y <- outcomeofcare_final_state
      st <- y[order(y$MR,y$Hosp),]
    }
    if (outcome == "heart failure"){
      outcomeofcare_final <- outcomeofcare[complete.cases(outcomeofcare[17]),]
      outcomeofcare_final <- outcomeofcare_final[c(2,7,17)]
      outcomeofcare_final_state <- outcomeofcare_final[which(outcomeofcare_final$State == state),]
      names(outcomeofcare_final_state)[1] <- "Hosp"
      names(outcomeofcare_final_state)[3] <- "MR"
      y <- outcomeofcare_final_state
      st <- y[order(y$MR,y$Hosp),]
    }
    if (outcome == "pneumonia"){
      outcomeofcare_final <- outcomeofcare[complete.cases(outcomeofcare[23]),]
      outcomeofcare_final <- outcomeofcare_final[c(2,7,23)]
      outcomeofcare_final_state <- outcomeofcare_final[which(outcomeofcare_final$State == state),]
      names(outcomeofcare_final_state)[1] <- "Hosp"
      names(outcomeofcare_final_state)[3] <- "MR"
      y <- outcomeofcare_final_state
      st <- y[order(y$MR,y$Hosp),]
    }
  }
    else{
      stop(err_msg1,state,err_msg2,outcome,err_msg3)
      geterrmessage()
    }
return(st$Hosp[1])
}