
### WK_1_QUIZ_1 ###

##Question 1
#The R language is a dialect of which of the following programming languages?
# Answer: 
# 'S'
# R is a dialect of the S language which was developed at Bell Labs.


##Question 2
#The definition of free software consists of four freedoms (freedoms 0 through 3). 
#Which of the following is NOT one of the freedoms that are part of the definition? Select all that apply.
# Answer:
# The freedom to prevent users from using the software for undesirable purposes.
# The freedom to restrict access to the source code for the software.
# The freedom to sell the software for any price


##Question 3
#In R the following are all atomic data types EXCEPT: (Select all that apply)
# Answer:
# data frame
# table
# list
# matrix
# array


##Question 4
#If I execute the expression x <- 4 in R, what is the class of the object `x' as determined by the `class()' function?
x <- 4
class(x)


##Question 5
#What is the class of the object defined by the expression x <- c(4, "a", TRUE)?
x <- c(4, "a", TRUE)
class(x)
# The character class is the "lowest common denominator" here and so all elements will be coerced into that class.


##Question 6
#If I have two vectors x <- c(1,3, 5) and y <- c(3, 2, 10), what is produced by the expression rbind(x, y)?
x <- c(1,3, 5) 
y <- c(3, 2, 10)
rbind(x, y)
# The 'rbind' function treats vectors as if they were rows of a matrix. 
# It then takes those vectors and binds them together row-wise to create a matrix.


##QUestion 7
#A key property of vectors in R is that
# Answer:
# elements of a vector all must be of the same class


##Question 8
#Suppose I have a list defined as x <- list(2, "a", "b", TRUE). What does x[[1]] give me? Select all that apply.
# Answer:
# a numeric vector of length 1.
# a numeric vector containing the element 2.


##Question 9
#Suppose I have a vector x <- 1:4 and y <- 2:3. What is produced by the expression x + y?
x <- 1:4
y <- 2:3
x + y


##Question 10
#Suppose I have a vector x <- c(17, 14, 4, 5, 13, 12, 10) and I want to set all elements of this vector that are greater than 10 to be equal to 4. 
#What R code achieves this? Select all that apply.
x <- c(17, 14, 4, 5, 13, 12, 10)
x[x >= 11] <- 4
x[x > 10] <- 4


##Question 11
#Use the Week 1 Quiz Data Set to answer questions 11-20.
#In the dataset provided for this Quiz, what are the column names of the dataset?
# Answer:
# pulling the names of all the columns
hw1 <- read.csv('hw1_data.csv')
names(hw1)


##Question 12
#Extract the first 2 rows of the data frame and print them to the console. What does the output look like?
# Answer:
# extracting the first two rows and ALL columns
hw1[c(1:2),]


##Question 13
#How many observations (i.e. rows) are in this data frame?
# Answer:
# for the total number of rows or data entries
nrow(hw1)


##Question 14
#Extract the last 2 rows of the data frame and print them to the console. What does the output look like?
# Answer:
# extracting the last two rows of the data table 
tail(hw1,2)


##Question 15
#What is the value of Ozone in the 47th row?
# Answer:
# pulling a specified row
hw1[47,]


##Question 16
#How many missing values are in the Ozone column of this data frame?
# Answer:
sub1 <- subset(hw1, is.na(Ozone))
nrow(sub1)


##Question 17
#What is the mean of the Ozone column in this dataset? Exclude missing values (coded as NA) from this calculation.
# Answer:
sub2 <- subset(hw1, !is.na(Ozone), select = Ozone)
apply(sub2, 2, mean)


##Question 18
#Extract the subset of rows of the data frame where Ozone values are above 31 and Temp values are above 90. 
#What is the mean of Solar.R in this subset?
# Answer:
sub3 <- subset(hw1, Ozone > 31 & Temp > 90, select = Solar.R)
apply(sub3, 2, mean)


##Question 19
#What is the mean of "Temp" when "Month" is equal to 6?
# Answer:
sub4 <- subset(hw1, Month == 6, select = Temp)
apply(sub4, 2, mean)


##Question 20
#What was the maximum ozone value in the month of May (i.e. Month is equal to 5)?
# Answer:
sub5 <- subset(hw1, Month == 5 & !is.na(Ozone), select = Ozone)
apply(sub5, 2, max)
