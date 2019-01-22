### WK_3_QUIZ_1 ###


##Question 1
# Take a look at the 'iris' dataset that comes with R. The data can be loaded with the code:
library(datasets)
data(iris)
# A description of the dataset can be found by running
?iris
# There will be an object called 'iris' in your workspace. 
# In this dataset, what is the mean of 'Sepal.Length' for the species virginica? 
# Please round your answer to the nearest whole number.
names(iris)
mean_iris <- tapply(iris$Sepal.Length, iris[, "Species"] == "virginica", mean)
mean_iris


##Question 2
# Continuing with the 'iris' dataset from the previous Question, 
# what R code returns a vector of the means of the variables 'Sepal.Length', 'Sepal.Width', 'Petal.Length', and 'Petal.Width'?
mean_cols <- apply(iris[, 1:4], 2, mean)
mean_cols


##Question 3
# Load the 'mtcars' dataset in R with the following code:
library(datasets)
data(mtcars)
# There will be an object names 'mtcars' in your workspace. You can find some information about the dataset by running
?mtcars
# my analysis
names(mtcars)
unique(mtcars[, 2])
tapply(mtcars[, 1], mtcars[, 2], mean)
# Answer:
with(mtcars, tapply(mpg, cyl, mean))
tapply(mtcars$mpg, mtcars$cyl, mean) 
sapply(split(mtcars$mpg, mtcars$cyl), mean)


##Question 4
# Continuing with the 'mtcars' dataset from the previous Question, 
# what is the absolute difference between the average horsepower of 4-cylinder cars and the average horsepower of 8-cylinder cars?
df <- data.frame(tapply(mtcars$hp, mtcars$cyl, mean))
df[1,] - df[3,]


# Question 5
# If you run
debug(ls)
# what happens when you next call the 'ls' function?
ls() # call this in the console and you will be taken into the browser or "BROWSE[2]>"
# Answer:
# Execution of 'ls' will suspend at the beginning of the function and you will be in the browser.
