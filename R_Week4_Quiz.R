### WK_4_QUIZ_1 ###


##Question 1
#What is produced at the end of this snippet of R code?
set.seed(1)
rpois(5, 2)
# Because the `set.seed()' function is used, `rpois()' will always output the same vector in this code.


##Question 2
#What R function can be used to generate standard Normal random variables?
# Answer:
# rnorm


##Question 3
#When simulating data, why is using the set.seed() function important? Select all that apply.
# Answer:
# It ensures that the sequence of random numbers starts in a specific place and is therefore reproducible.


##Question 4
#Which function can be used to evaluate the inverse cumulative distribution function for the Poisson distribution?
# Answer:
# qpois
# Probability distribution functions beginning with the `q' prefix are used to evaluate the quantile (inverse cumulative distribution) function.


##Question 5
#What does the following code do?
set.seed(10)
x <- rep(0:1, each = 5)
e <- rnorm(10, 0, 20)
y <- 0.5 + 2 * x + e
order(y)
y2 <- sort(y)
# Answer:
# Generate data from a Normal linear model


##Question 6
#What R function can be used to generate Binomial random variables?
# Answer:
# rbinom


##Question 7
#What aspect of the R runtime does the profiler keep track of when an R expression is evaluated?
# Answer:
# the function call stack


##Question 8
#Consider the following R code
library(datasets)
Rprof()
fit <- lm(y ~ x1 + x2)
Rprof(NULL)
#(Assume that y, x1, and x2 are present in the workspace.) 
# Without running the code, 
# what percentage of the run time is spent in the 'lm' function, based on the 'by.total' method of normalization shown in 'summaryRprof()'?
# Answer:
# 100%
# When using `by.total' normalization, the top-level function (in this case, `lm()') always takes 100% of the time.


##Question 9
#When using 'system.time()', what is the user time?
# Answer:
# It is the time spent by the CPU evaluating an expression


##Question 10
#If a computer has more than one available processor and R is able to take advantage of that, 
#then which of the following is true when using 'system.time()'?
# Answer:
# elapsed time may be smaller than user time




### WK_4_QUIZ_2 ###


#########################################################################################################################################################
### PART 1 ###
# Plot the 30-day mortality rates for heart attack
outcome <- read.csv("./rprog_data_ProgAssignment3-data/outcome-of-care-measures.csv", colClasses = "character")
head(outcome)
outcome[, 11] <- as.numeric(outcome[, 11])
## You may get a warning about NAs being introduced; that is okay
hist(outcome[, 11])


#########################################################################################################################################################
### PART 2 ###
best <- function(state, outcome) {
        
        # reading outcome data
        OutComeofCare <- data.frame(read.csv("./rprog_data_ProgAssignment3-data/outcome-of-care-measures.csv", stringsAsFactors = FALSE))
        
        # checking for valid entries
        if (is.element(state, OutComeofCare[, 7]) == FALSE) { stop("invalid state") }
        if (is.element(outcome, c("heart attack", "heart failure", "pneumonia")) == FALSE) { stop("invalid outcome") }
        
        # assigning column number to the given outcome
        if (outcome == "heart attack") { cl <- 11 } 
        if (outcome == "heart failure") { cl <- 17 }
        if (outcome == "pneumonia") { cl <- 23 }
        
        
        # formatting data.frame
        df_ld <- OutComeofCare[OutComeofCare[, 7] == state, c(2, 7, cl)]
        df_ld <- df_ld[order(df_ld[, 1]), ]
        df_ld <- df_ld[order(as.numeric(df_ld[, 3])), ]
        
        # returning the lowest death rate for given outcome
        return (df_ld[1, 1])
}

# Question 1
best("SC", "heart attack")

# Question 2
best("NY", "pneumonia")

# Question 3
best("AK", "pneumonia")



#########################################################################################################################################################
### PART 3 ###
rankhospital <- function(state, outcome, num = "best") {
        
        # reading outcome data
        OutComeofCare <- data.frame(read.csv("./rprog_data_ProgAssignment3-data/outcome-of-care-measures.csv", stringsAsFactors = FALSE))
        
        # checking for valid entries
        if (is.element(state, OutComeofCare[, 7]) == FALSE) { stop("invalid state") }
        if (is.element(outcome, c("heart attack", "heart failure", "pneumonia")) == FALSE) { stop("invalid outcome") }
        
        # assigning column number to the given outcome
        if (outcome == "heart attack") { cl <- 11 }
        if (outcome == "heart failure") { cl <- 17 }
        if (outcome == "pneumonia") { cl <- 23 }
        
        # formatting data.frame
        df_ld <- OutComeofCare[OutComeofCare[, 7] == state, c(2, 7, cl)]
        df_ld <- df_ld[order(df_ld[, 1]), ]
        df_ld <- df_ld[order(as.numeric(df_ld[, 3])), ]
        df_ld$Rank = rank((as.numeric(df_ld[, 3])), na.last = TRUE, ties.method = c("first"))
        df_ld <- df_ld[df_ld[, 3] != "Not Available", ]
        
        if (num == "worst") {
                df_last <- tail(df_ld, 1)
                return (df_last[1, 1])
        } else if (num > 0 & num <= nrow(df_ld)) { df_ld[match(num, df_ld[, 4]), 1] 
        } else if (num == "best") { return (df_ld[1, 1]) 
        } else { return (NA) }
}

# Question 4
rankhospital("NC", "heart attack", "worst")

# Question 5
rankhospital("WA", "heart attack", 7)

# QUestion 6
rankhospital("TX", "pneumonia", 10)

# Question 7
rankhospital("NY", "heart attack", 7)



#########################################################################################################################################################
### PART 4 ###
rankall <- function(outcome, num = "best") {
        
        # reading outcome data
        OutComeofCare <- data.frame(read.csv("./rprog_data_ProgAssignment3-data/outcome-of-care-measures.csv", stringsAsFactors = FALSE))
        state.list <- unique(OutComeofCare[, 7])
        state.list <- sort(state.list, decreasing = FALSE)
        state.vect <- c()
        hospital.vect <- c()
        
        # assigning column number to the given outcome
        if (outcome == "heart attack") { cl <- 11 }
        if (outcome == "heart failure") { cl <- 17 }
        if (outcome == "pneumonia") { cl <- 23 }
        
        # checking for valid entries
        if (is.element(outcome, c("heart attack", "heart failure", "pneumonia")) == FALSE) { stop("invalid outcome") }
        
        ## For each state, find the hospital of the given rank
        for (i in state.list) {
                df_ld <- OutComeofCare[OutComeofCare[, 7] == i, c(2, 7, cl)]
                df_ld <- df_ld[order(df_ld[, 1]), ]
                df_ld <- df_ld[order(as.numeric(df_ld[, 3])), ]
                df_ld$Rank = rank((as.numeric(df_ld[, 3])), na.last = TRUE, ties.method = c("first"))
                
                if (num == "worst") {
                        df_ld_NA <- df_ld[df_ld[, 3] != "Not Available", ]
                        df_last <- tail(df_ld_NA, 1)
                        hospital.vect <- c(hospital.vect, df_last[1, 1])
                        state.vect <- c(state.vect, i)
                        
                } else if (num == "best") { 
                        hospital.vect <- c(hospital.vect, df_ld[1, 1]) 
                        state.vect <- c(state.vect, i)
                        
                } else if (num > 0 & num <= nrow(df_ld)) {
                        hospital.vect <- c(hospital.vect, df_ld[match(num, df_ld[, 4]), 1])
                        state.vect <- c(state.vect, i)
                        
                } else {
                        hospital.vect <- c(hospital.vect, NA)
                        state.vect <- c(state.vect, i)
                }
        }
        df_final <- data.frame("hospital" = hospital.vect, "state" = state.vect)
        return (df_final)
}

# Question 8
r <- rankall("heart attack", 4)
as.character(subset(r, state == "HI")$hospital)


# Question 9
r <- rankall("pneumonia", "worst")
as.character(subset(r, state == "NJ")$hospital)


# Question 10
r <- rankall("heart failure", 10)
as.character(subset(r, state == "NV")$hospital)



