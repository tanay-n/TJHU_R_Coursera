### WK_2_QUIZ_1 ###


##Question 1
cube <- function(x, n) {
        x^3
}
#What is the result of running in R after defining this function?
cube(3)
# Because 'n' is not evaluated, it is not needed even though it is a formal argument.


##Question 2
#The following code will produce a warning in R.
x <- 1:10
if(x > 5) {
        x <- 0
}
#Why?
# Answer:
# 'x' is a vector of length 10 and 'if' can only test a single logical statement.


##Question 3
f <- function(x) {
        g <- function(y) {
                y + z
        }
        z <- 4
        x + g(x)
}
#If I then run in R
z <- 10
f(3)
#What value is returned?


##Question 4
x <- 5
y <- if(x < 3) {
        NA
} else {
        10
}
#What is the value of 'y' after evaluating this expression?


##Question 5
#Consider the following R function
h <- function(x, y = NULL, d = 3L) {
        z <- cbind(x, d)
        if(!is.null(y))
                z <- z + y
        else
                z <- z + f
        g <- x + y / z
        if(d == 3L)
                return(g)
        g <- g + 10
        g
}
#Which symbol in the above function is a free variable?
# Answer: 
# f


##Question 6
#What is an environment in R?
# Answer:
# a collection of symbol/value pairs


##Question 7
#The R language uses what type of scoping rule for resolving free variables?
# Answer:
# lexical scoping


##Question 8
#How are free variables in R functions resolved?
# Answer:
# The values of free variables are searched for in the environment in which the function was defined


##Question 9
#What is one of the consequences of the scoping rules used in R?
# Answer:
# All objects must be stored in memory


##Question 10
#In R, what is the parent frame?
# Answer:
# It is the environment in which a function was called 




### WK_2_QUIZ_2 ###

#########################################################################################
# Function 1

# my version
pollutantmean <- function(directory, pollutant, id = 1:332) {
        
        if (pollutant == "sulfate") {
                cl <- 2
        } else if (pollutant == "nitrate") {
                cl <- 3       
        }
        
        summ <- 0
        count <- 0
        
        for(number in id) {
                if ((number >= 1) & (number <= 9)) {
                        d_name1 <- paste (directory,"/00",number,".csv",sep = "")
                        file1 <- read.csv(d_name1)
                        df1 <- data.frame(file1)
                        x1 <- df1[complete.cases(df1[, cl]),]
                        count <- count + nrow(x1[cl])
                        summ <- summ + sum(x1[cl])
                        
                } else if ((number >= 10) & (number <= 99)) {
                        d_name2 <- paste (directory,"/0",number,".csv",sep = "")
                        file2 <- read.csv(d_name2)
                        df2 <- data.frame(file2)
                        x2 <- df2[complete.cases(df2[, cl]),]
                        count <- count + nrow(x2[cl])
                        summ <- summ + sum(x2[cl])
                        
                } else if (number >= 100) {
                        d_name3 <- paste (directory,"/",number,".csv",sep = "")
                        file3 <- read.csv(d_name3)
                        no_na_val <- file3[!is.na(file3[, pollutant]), pollutant]
                        summ <- summ + sum(no_na_val)
                        count <- count + length(no_na_val)
                }
        }
        avg <- summ/count
        return (avg)
        return ()
}

# Question 1
pollutantmean("specdata", "sulfate", 1:10)

# Question 2
pollutantmean("specdata", "nitrate", 70:72)
#pollutantmean("specdata", "nitrate", 23)

# Question 3
pollutantmean("specdata", "sulfate", 34)

# Question 4
pollutantmean("specdata", "nitrate")


# version 2
### from https://github.com/cynthia0611/Coursera-R-Programming/blob/master/Week-2/pollutantmean.R ###
pollutantmean2 <- function(directory, pollutant, id = 1:332) {
        if(grep("specdata", directory) == 1) {
                directory <- ("./specdata/")
        }
        
        mean_vector <- c()
        
        all_files <- as.character( list.files(directory) )
        file_paths <- paste(directory, all_files, sep="")
        
        for (i in id) {
                current_file <- read.csv(file_paths[i], header=T, sep=",")
                na_removed <- current_file[!is.na(current_file[, pollutant]), pollutant]
                mean_vector <- c(mean_vector, na_removed)
                }
        
        result <- mean(mean_vector)
        return(result)
}
### from https://github.com/cynthia0611/Coursera-R-Programming/blob/master/Week-2/pollutantmean.R ###


######################################################################################### 
# Function 2

complete <- function(directory, id) {
        
        # checking for correct directory
        if (grep("specdata", directory) == 1) {
                directory <- "./specdata/"
        } else {
                return (NULL)
        }
        
        # store all file names as list and applying 'character' coercion
        fileslist <- as.character(list.files(directory))
        
        # creating empty vectors to store the results
        id_number <- c() 
        nobs <- c()
        
        # running a loop to get the results for each file id
        for (number in id) {
                file <- read.csv(paste(directory, fileslist[number], sep = ""))
                final <- file[complete.cases(file),]
                count <- nrow(final)
                nobs <- c(nobs, count)
                id_number <- c(id_number, number)
        }
        
        results <- data.frame("id" = id_number, "nobs" = nobs)
        return (results)
}


# Question 5
cc <- complete("specdata", c(6, 10, 20, 34, 100, 200, 310))
print(cc$nobs)

# Question 6
cc <- complete("specdata", 54)
print(cc$nobs)

# Question 7
set.seed(42)
cc <- complete("specdata", 332:1)
use <- sample(332, 10)
print(cc[use, "nobs"])


#########################################################################################
# Function 3
corr <- function(directory, threshold = 0) {
        
        if (grep("specdata", directory) == 1) {
                directory <- "./specdata/"
        }
        
        files <- list.files(directory)
        len <- length(files)
        id <- c(1:len)
        results <- c()
        
        for (i in id) {
                d_name <- paste(directory, files[i], sep = "")
                file <- read.csv(d_name)
                df <- data.frame(file)
                
                df_final <- df[complete.cases(df), ]
                n_complete <- df_final[, 3]
                s_complete <- df_final[, 2]
                
                count <- nrow(df_final)
                
                if (count > threshold) {
                        correlation <- cor(n_complete, s_complete)
                        results <- c(results, correlation)
                } else if (count < threshold) { next }
        }
        return (results)
}


# Question 8
cr <- corr("specdata")                
cr <- sort(cr)                
set.seed(868)                
out <- round(cr[sample(length(cr), 5)], 4)
print(out)


# Question 9
cr <- corr("specdata", 129)                
cr <- sort(cr)                
n <- length(cr)                
set.seed(197)                
out <- c(n, round(cr[sample(n, 5)], 4))
print(out)


# QUestion 10
cr <- corr("specdata", 2000)                
n <- length(cr)                
cr <- corr("specdata", 1000)                
cr <- sort(cr)
print(c(n, round(cr, 4)))


