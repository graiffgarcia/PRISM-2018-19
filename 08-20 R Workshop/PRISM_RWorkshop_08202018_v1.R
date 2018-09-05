########################################
########################################
### PRISM Workshop Series, Fall 2018
### Intro and Refresher to R Workshop
### Authors: Benjamin Campbell
###          Ricardo Graiff Garcia
### Date of Presentation: 8/20/2018
### Notes: This is the only file from the 
###        workshop, self-contained.
###        Data files are imported
###        however.  
########################################
########################################

########################################
########################################
##### TOP MATTER
########################################
########################################

########################################
### WHAT IS R?
########################################

# R is a programming language designed for computational statistics. It is
# free and open source (FOSS), and it has a large and very active 
# developer community. R is an offshoot of the S language created at Bell Labs
# as a programming language that could be used by statisticians, who largely 
# don't want to learn Fortran (punch cards, anyone?).

# R is exceptionally powerful and on the cutting edge.  It is increasingly
# the standard in academia, Political Science, and is commonly used
# in business.  

# RStudio, which you are seeing right now, is not synonymous with R! 

########################################
### WHY USE R?
########################################

# R is the closest we have to a lingua franca of statistical computing.
# There are many good reasons for that. To me, it's enough that R is the only
# large project of its kind that is FOSS. Stata and SAS are proprietary;
# SPSS uses the R and Python languages but the software itself is proprietary;
# Julia is FOSS and it's pretty cool but it's still a very "young" language with
# a relatively small community of developers.

# It is also highly flexible, you can do almost anything you'll need to
# do, full stack.  If you can't find functionality to do it, you can 
# program it yourself. 

# R is great with graphics.

# You know exactly what is going on underneath the hood.

# There are increasingly reporting tools built in (knitr, R markdown)

########################################
### HOW DOES R WORK? 
########################################

# Object oriented programming.

# Easily replicable code, not point and click like SPSS or Stata.

# Can execute R code in the console or in RStudio's source editor.

# Scripting allows you to save code and manipulate it.

# Scripts should contain code, annotations, and header information
  # (This is a script!)

# In R Studio: file -> new file -> R script

# R, like most programming languages, works from a "working directory"
# which is pre-set or you define to a local folder.  This is where
# R will look for files when importing, or save files when exporting.  

# getwd()
# setwd("/Users/Ricardo/Dropbox/PRISM")

# RStudio has a really nifty "projects" feature that saves your working state
# in a directory of your choice. when you open a project in RStudio, it will
# reload all the tabs you had open *and* every object in your environment.

########################################
########################################
##### BASE R
########################################
########################################

# R comes loaded with its own set of "base" operations or functions, and
# with these, you can do most anything. many people pride themselves
# on doing as much as they can with base R. 

# the simplest thing you can do in R is arithmetic. for example, 
# we can use R to find the mean of 3 and 5:
(3 + 5) / 2 

# the second simplest thing you can do in R is assign values to variables.
# we can create a variable called x whose value is the number 3 like this:
x <- 3

# you can think of these variables, or objects, as "nouns", and functions as the
# "verbs" of the language.

# this creates an object called x that equals the number 3. you can do anything
# with it that you can do with the number 3:
x + 3
x + x
(x + 5) / 2

# let's call x by name -- this shows us the value of x on the console.
x


########################################

# back to the arithmetic thing! hidden in (3+5)/2 are two important concepts: 
# object types and functions.

# first, object types. when you ask R to calculate three plus five and then
# divide the result by two, it has to do a series of things: first, it stores
# the numbers 3, 5, and 2 in memory. then, it calculates the sum, and stores
# the number 8 in memory. then, it retrieves the number 2 from memory, divides
# 8 by 2, stores the number 4 in memory, and "prints" the number 4.
# (printing is what we call it when the console spits out a result)

# why am I telling you this?

# everything you do in R is an *operation* applied to an *object*. 
# objects come in different types, or *classes*, e.g., numeric, character, 
# logical, list, matrix, factor, data.frame.
# the way you *declare* your object in your code will tell R what class
# you want it to be, and not every operation will work with every class.
# for example, this is how we define the character 3:
"3"

# can we do math with that? the computer should be smart enough to figure out
# that numbers between quotes are numbers, right?
"3" + "5"

# that's weird. why does that happen?

# the way the character 3 is stored in memory isn't "special" just because 3
# is a number. the character 3 and the character A follow the same rules.
# once you tell R that 3 is a character, you can't add something to it, even
# if that something is a number:
"3" + 5

# how do we find the class of an object? it couldn't be easier:
class("3")
class(3)

########################################

# so that's numerics and characters. let's look at logical (aka Boolean) 
# objects. what does this expression return?
3 > 5

# is that the word FALSE (aka, a character object)?
class(3 > 5)

# it's the "logical" class! TRUE and FALSE are the units of Boolean algebra,
# and I won't dwell on it too much but you can do crazy cool things with it --
# like calculating the sum of 3 and 5, or creating the operating system I'm
# working on right now. 

# there are all sorts of comparisons you can do -- and an extremely simple
# syntax for them:
3 > 5
3 >= 5 # >= means greater than or equal to
3 < 5
3 <= 5
3 == 5 # you compare two objects with two equal signs (not one!!)
3 != 5 # != means not equal to ("!" means "not" in R's Boolean syntax)

# the Boolean operator AND is represented in R by &,
# and the operator OR is represented by |.

# for example, 3 > 5 evaluates to FALSE and 3 < 5 evaluates to TRUE. what does
# FALSE & TRUE equal?

(3 > 5) & (3 < 5)
TRUE & TRUE
FALSE & TRUE
FALSE & FALSE

# how about FALSE | (or) TRUE?
(3 > 5) | (3 < 5)
TRUE | TRUE
FALSE | TRUE
FALSE | FALSE

# Boolean algebra also has a "not" operator; in R that's an exclamation point:
!TRUE
!FALSE

# and the "xor" operator, which is an "exclusive or": it equals TRUE
# if and only if you have one TRUE and one FALSE.
xor(TRUE, TRUE)
xor(TRUE, FALSE)
xor(FALSE, FALSE)

# THIS IS REALLY IMPORTANT. R uses T and F as "shorthand" for TRUE and FALSE:
T & F

# YOU SHOULD *****NEVER***** USE THAT. NEVER EVER EVER EVER EVER EVER EVER EVER.

# why not?

# let's assign another value to a variable. this time, instead of the number 3,
# my value is going to be the logical FALSE. and instead of x, I want to use a 
# capital T.

# surely, R will stop me from doing something this stupid, right?
T <- FALSE

# it looks like it went through? but surely, R won't do actual Boolean algebra
# with my monstrosity... right? let's compare:
TRUE & TRUE
T & TRUE

# we can also compare if the two are equal using two equals signs like before:
T == TRUE
T == FALSE

# if you ever, for any reason at all, ever, whatsoever, find yourself with a
# variable called T (or F) that equals *anything* (a character, a number),
# any Boolean operations you run with T or F will come out wrong. this can break
# *so* many things, especially if you regularly use T and F as short for
# TRUE and FALSE.

# can you screw up the actual object TRUE? turns out you cannot:
TRUE <- FALSE
TRUE <- 3

# one last thing on Booleans: you may want to know if a number 
# is in a set of numbers, or if a character is in a set of characters.
# you can do that with the special command %in%, whose syntax works like
# an arithmetic operator:

myname <- "ricardo"
prism <- c("ricardo", "ben")
myname %in% prism
"ricardo" %in% prism 

# %in% returns a logical object, and as such it can be used in Boolean algebra:
"ben" %in% prism & "prisms" %in% prism

# the "not" operator works with %in% as well:
!("ricardo" %in% prism)

########################################

# one quick one: R has date and time capabilities. you can find your computer's
# date like this:
Sys.Date()

# and the time...
Sys.time()

# dates in R are stored in UNIX format, meaning that the current day is actually
# an integer that counts the number of days since January 1, 1970. which brings
# us to this: you can *convert* different object types in R, as long as that
# conversion makes logical sense. for example:
as.numeric(Sys.Date())

# this means we can also do arithmetic with dates:
Sys.Date() + 365
Sys.Date() - as.numeric(Sys.Date())

# we can use as.Date to *explicitly* create a date object and ask R how old
# I am, in days:
# (I say explicitly because if you just write "1990-05-25" between quotes,
# R will think you want to create a character object)
Sys.Date() - as.Date("1990-05-25")

# and we can do things that don't make that much sense. for example, what is the
# mean between today and the day I was born?
today <- Sys.Date()
birthday <- as.Date("1990-05-25")

mean(c(today, birthday))

# there's not much we can do with that information, but it makes sense that R
# will let you calculate it once you know that these dates are just integers.

########################################

# R also has ways of combining many different values into one object.
# for example, there's the vector, which you create with the c() operator:
c(1, 2, 3)

# by the way: you can do the same thing in R with a colon, which creates a 
# sequence from the first number to the second, increasing by 1:
c(1:3)

# you can do all kinds of things with vectors, like adding them to a number:
c(1, 2, 3) + 1

# ...or to another vector.
c(1, 2, 3) + c(1, 2, 3)

# you can find the first object in a vector with square brackets:
vector <- c(10, 20, 30)
vector[1]

# one caveat: every element in a vector must be the same class.
# if you try to trick R, it will coerce elements for you:
c(1, "2", 3)

# when you do operations on vectors, R will "recycle" vectors for you if they
# are not the same size. this is how we can add c(1, 2, 3) + 1 and get (2, 3, 4)
# back. before adding, R "recycles" 1 to match the length of the longer vector.
# for example:
# (note that R will give you a warning, but it will do the operation anyway!)
c(1, 2, 3) + c(10, 0)

########################################

# R also has matrices, created by passing a vector to the matrix() function:
# (ncol tells us how many columns the matrix has. by default, R creates a "tall"
# matrix, with one column)
the_matrix <- matrix(c(1, 2, 3, 4, 5, 6, 7, 8), ncol = 2)

# you can do matrix algebra in R (which has saved my life many times):
the_matrix + 2

# the multiplication operator is elementwise:
the_matrix * the_matrix

# you can multiply matrices with this funny %*% operator:
the_matrix %*% the_matrix

# but of course, these two aren't conformable. you have to transpose one side:
the_matrix %*% t(the_matrix)
t(the_matrix) %*% the_matrix

# you can create variables directly from these expressions:
matrix_reloaded <- the_matrix %*% t(the_matrix)

# you can also modify variables "in place" (this is true for everything, not
# just matrices):
the_matrix <- the_matrix * the_matrix

########################################

# R also has data frames, which is how most data objects are stored:
df <- data.frame(matrix_reloaded)

# now, instead of rows and columns like a matrix, this object has "observations"
# and "variables".
# the one I created just now is a boring example, 
# so let's load a real data frame to look at. this is a dataset of Argentine
# legislators (as evidenced by that super helpful file name):
df <- read.table("~/OSU/SP16/Quant 2/Replication/Argentina legislator data.tab", 
  sep="\t", header = TRUE)

# you can access specific "cells" in a data frame with square brackets, like we
# accessed the values in the c(10, 20, 30) vector. data frames are indexed by
# row and then column:
df[3, 12]

# we can look at all of one observation's values by leaving the column blank:
df[3,]

# and we can look at all the values of a variable by leaving the row blank:
df[,12]

# if we know the name of the variable we want, we can also use the $ operator:
df$urbanization

# remember: certain operations only work with certain object types. can you find
# the mean of a data frame?
mean(df)

# you can find the mean of a variable, though:
mean(df[,12])
mean(df$urbanization)

########################################

# one last object type: R has *factor* objects, which are useful for categorical 
# variables. factor objects have a limited number ofunique values (aka *levels*)
# that usually are labeled and sometimes are ordered.

# for example, here is a vector of the 12 months of the year:
months <- c(1:12)

# that makes complete sense, right? 12 months, numbered from 1 to 12. but that's
# still a *numeric* variable.
months
class(months)

#this means we can do arithmetic with months. below you can see that
# January plus March equals April.
months[1] + months[3]

# so that's not very good...
# we can transform our months variable into a factor like this.
# (note the optional argument *ordered*, which we set to TRUE because, ofc,
# the months of the year follow a temporal order)
months <- factor(months, ordered = TRUE)

# now, whenever you call the first value of the variable, it shows you all
# the *levels* of the variable:
months[1]

# now, we can't add January and March:
months[1] + months[3]

# but we still have numbers from 1 to 12, as you can see with levels():
levels(months)

# that's kind of dumb... we can fix that with levels() though:
levels(months) <- c("January", "February", "March", "April", "May", "June",
                    "July", "August", "September", "October", "November",
                    "December")

# so now, what is the first value of the months vector?
months[1]

# factors are great for plotting. to show you why, I'm going to create some 
# fake data. FIRST, the fake data will not include the factor object, but only
# the CHARACTERS with the names of the months.
fake_df1 <- data.frame(month = sample(levels(months), 100, replace = TRUE),
                      y = sample(c(1:1000000), 100, replace = TRUE))

# so here's a basic plot with ggplot, which we'll look at again in a minute:
library(ggplot2)
ggplot(data = fake_df1, aes(month, y)) + geom_boxplot()

# look what happens to the x axis there -- it's in alphabetical order! if you
# have an ordered variable, this will ruin your plot. how about we replace
# the character vector with the factors?
fake_df2 <- data.frame(month = sample(months, 100, replace = TRUE),
                       y = sample(c(1:1000000), 100, replace = TRUE))

ggplot(data = fake_df2, aes(month, y)) + geom_boxplot()

# ta-da! see?


########################################

# now, let's look more deeply at how functions work. 
# we just found the mean of a variable:
mean(df$urbanization)

# you can access the help file for any R function with this syntax:
?mean
  # Note! If there are NA's, you will get NA.  You must specify na.rm = TRUE

# and you can look at what kinds of objects a function can handle like this:
methods(mean)

# as you can see, there's a mean.default (which means vectors of numerics), but
# there's also a mean.Date, as we saw earlier. there is no mean.character in
# there, though! it makes sense: you can't get the mean of A and B.

# another example. you can transpose a matrix and a data.frame, and a ts
# object (ts stands for time series -- we don't have to talk about that for now)
methods(t)

# which means that this works (note how it *outputs* a matrix):
df_reloaded <- t(df)

# mean is a "primitive" R function, which is bad for us because we can't really
# read its code to figure out how it works unless we 
# 1) know where to look in the R source files; 
# 2) know C (and maybe some Fortran?). for other functions, 
# you can actually peek under the curtain to see what they do.

# for example, we know about mean.Date, but there's also a print.Date.
# we know that printing means displaying it on the console, so we can guess
# what this does:
print.Date(today)

# what if we try it with nothing in the parentheses?
print.Date()

# there's an error! this function needs an *argument* (aka an "input")
# in order for it to do its magic.

# what if we try it without the parentheses?
print.Date

# we get the code for the function. we find in there: 1) a few "if" and "else"
# statements -- that's called control flow, and we'll get to it in a second;
# 2) one line that actually does the printing, by using the print() function.

# we also get a basic idea of how writing functions works... maybe we can use it
# to write a function of our own that we can use to find out how old people are!

# we can see that print.Date starts with "function(x, max = NULL)", then
# square brackets, then a bunch of code, than the square brackets closes. let's
# try to copy that. we'll name our function "howold".

howold <- function(x) {
  # what can we put in here? well, we know that we have to convert everything
  # to a Date object before we can subtract them from today. and we know how
  # to do that! so this is the Date object corresponding to the user's birthday.
  birthday <- as.Date(x)
  
  # now we need to get today's date. we *could* use Sys.Date() inside all our
  # operations, but it's cleaner if we give it a name:
  today <- Sys.Date()
  
  # now we can subtract:
  age <- today - birthday
  
  # if you run "today - birthday" in the console, you'll see that it shows
  # some text. we can get rid of that by converting "age" into a numeric:
  age <- as.numeric(age)
}

# and that's our function! let's run it and see how old I am!
howold("1990-05-25")

# uh-oh...

# we got nothing. is our function broken? is my PRISM fellowship over???

# well... no, and maybe. the function does exactly what we asked it to do.
# the golden rule of coding is: if you got the wrong answer, it's because you
# asked the wrong question. I'd like the function to print the answer to the
# console, but I never actually wrote that into the function. so:

howold <- function(x) {
  birthday <- as.Date(x)
  today <- Sys.Date()

  age <- today - birthday
  age <- as.numeric(age)
  # here's the new line:
  print(age)
}

howold("1990-05-25")

# of course, there's a lot more we can do with that, just with the tools that
# we've seen so far:

howold <- function(x) {
  birthday <- as.Date(x)
  today <- Sys.Date()
  
  age <- today - birthday
  age <- as.numeric(age)
  age_in_years <- age/365
  
  # here's something new: we're using the paste() function to combine two pieces
  # of text that we want to print to the console.
  text <- paste("You are", age_in_years, 
                "years old. That's like, older than my mom.")
  
  print(text)
}

howold("1990-05-25")

# that's pretty dumb, for two reasons: 1) it shows too many digits after the
# decimal; 2) it will tell *every* user that they're very old.

# so we can modify it in two ways. the first is very simple: we can use round()
# to change how many digits it displays.

# the default with round() is to only show the integer part...
round(2.33333)

# but we can change that with an *optional argument* that modifies its behavior.
round(2.33333, 2)

howold <- function(x) {
  birthday <- as.Date(x)
  today <- Sys.Date()
  
  age <- today - birthday
  age <- as.numeric(age)
  age_in_years <- age/365
  age_in_years <- round(age_in_years, 2)
  
  # here's something new: we're using the paste() function to combine two pieces
  # of text that we want to print to the console.
  text <- paste("You are", age_in_years, 
                "years old. That's like, older than my mom.")
  
  print(text)
}

howold("1990-05-25")

########################################
# the second modification to the function involves *control flow*.
# control flow is simply the order in which a program runs its commands.
# the default in R is to run everything you write, from top to bottom, 
# line by line. but we can use *conditional statements* to change that.
# conditional statements will choose which chunk of code to run, based on some
# condition. for example:

x <- 3
y <- 4

if(x > y){
  print(x + y)
} else {
  print(x * y)
}

# Here, since x > y evaluates to FALSE, R skips everything in the "if" square
# brackets and runs the code inside the "else" square brackets.
# we can extend this with an "else if" condition. 

if(x > y){
  print(x + y)
} else if (x == y){
  print(x ^ y)
} else{
  print(x * y)
}

# we can read the above code as: IF x is larger than y, then print x + y.
# ELSE, IF x is equal to y, print x to the power y. ELSE, print x * y.

# how can we use that in our dumb age function? 
# how about we write different insults for people of different ages? like this:

howold <- function(x) {
  birthday <- as.Date(x)
  today <- Sys.Date()
  
  age <- today - birthday
  age <- as.numeric(age)
  age_in_years <- age/365
  age_in_years <- round(age_in_years, 2)
  
  if(birthday < as.Date("1990-05-25")){
    text <- paste("You are", age_in_years, 
                  "years old. Bet you're really, really, really into Fortnite.")
  } else if (birthday > as.Date("1990-05-25")){
    text <- paste("You are", age_in_years, 
                  "years old. That's like, older than my mom.")    
  } else{
    text <- "You either are Ricardo or his evil twin. Please be the evil twin."
  }
  
  print(text)
}

howold("1990-05-25")

# of course, any function that ends with print() or return() can be used to
# assign the object being printed or returned to a variable:

personalized_insult <- howold("1990-05-25")

########################################

# now is the time when I get to give you an example of where R falls short!
# let's go back to that dataset of Argentine legislators. here's that list of 
# urbanization rates again:
df$urbanization

# so maybe we can build a plot? let's try a histogram, with hist()
hist(df$urbanization)

# how about a plot of legislators' birth year and urbanization rates? we can
# draw a scatterplot with plot():
plot(df$year_born, df$urbanization)

# so works, but it looks really ugly. (base R plotting has other problems, but
# for now let's just go with "really ugly")

# how do you solve that problem? you *could* write your own set of functions to
# draw nicer plots. or you could download a set of functions from someone else?

# in R, third-party libraries are called packages. 
# Packages are a means of building upon what is available in base R, providing
# additional functionality! 
# Note though, often packages may conflict with base R functions, so
# be aware if you see these messages when loading packages

# Packages are installed by default from CRAN, the official R repository.
# you can install a package with:
install.packages("insert_fake_package_name_here")
install.packages("ggplot2")

# and after you install a package, you may load it with:
library(ggplot2)

# basic ggplot2 functions are qplot() (for "quick plot"; it tries to figure out 
# what you want to plot based on what variables you pass to it):
qplot(df$urbanization)
qplot(df$year_born, df$urbanization)

# and ggplot(), whose syntax is slightly more complex but which is infinitely
# customizable. the example below isn't really an example of good data viz, 
# but note how much you can modify with only a few lines of code:
ggplot(data = df, aes(x = year_born, y = urbanization)) + 
  geom_point(aes(colour = factor(female))) + 
  scale_color_discrete(labels = c("Male", "Female"),
                       guide = guide_legend(title = "Gender")) +
  theme_bw() + xlab("Legislator's birth year") + ylab("Urbanization rate") +
  ggtitle("Hello world!")


# there are about as many R packages as there are use cases. if I had to guess, 
# the packages you'll use the most in the coming couple of years are:
# ggplot2 to create plots;
# foreign to load data files created in Stata, SPSS, SAS, etc;
# texreg to create LaTeX tables (people may recommend stargazer... I hate it);
# the tidyverse packages (particularly dplyr) for data manipulation


########################################

# one last thing: loops and automating stuff. let's load a new dataset so we
# can create a problem we don't really have and thus work with making loops.

# "values" has night light data for the 1122 Colombian municipalities 
# over a 21-year period (1993-2013).
values <- read.csv("~/OSU/AU17/Quant 3/values.csv", sep="", 
                   stringsAsFactors = FALSE)

# View() shows us the data frame in the source editor pane:
View(values)

# so, what I want to do here is calculate the mean level of light of each
# municipality. we can do that for the first one like this:
mean(as.numeric(values[1,]))

# and we can continue on...
mean(as.numeric(values[2,]))
mean(as.numeric(values[3,]))
mean(as.numeric(values[4,]))
mean(as.numeric(values[5,]))
mean(as.numeric(values[6,]))
mean(as.numeric(values[7,]))
mean(as.numeric(values[8,]))
mean(as.numeric(values[9,]))
mean(as.numeric(values[10,]))
mean(as.numeric(values[11,]))
mean(as.numeric(values[12,]))
mean(as.numeric(values[13,]))
mean(as.numeric(values[14,]))
mean(as.numeric(values[15,]))
mean(as.numeric(values[16,]))
mean(as.numeric(values[17,]))
mean(as.numeric(values[18,]))
mean(as.numeric(values[19,]))
mean(as.numeric(values[20,]))
mean(as.numeric(values[21,]))
mean(as.numeric(values[22,]))
mean(as.numeric(values[23,]))
mean(as.numeric(values[24,]))
mean(as.numeric(values[25,]))
mean(as.numeric(values[26,]))
mean(as.numeric(values[27,]))
mean(as.numeric(values[28,]))
mean(as.numeric(values[29,]))
mean(as.numeric(values[30,]))
mean(as.numeric(values[31,]))
mean(as.numeric(values[32,]))
mean(as.numeric(values[33,]))
mean(as.numeric(values[34,]))
mean(as.numeric(values[35,]))
mean(as.numeric(values[36,]))
mean(as.numeric(values[37,]))
mean(as.numeric(values[38,]))
mean(as.numeric(values[39,]))
mean(as.numeric(values[40,]))

# i'm already exhausted (yes, I did write those by hand. it took forever), 
# and there are still 1,082 towns to go. there must be a different way to do it.

# that's basically the kind of problem loops are designed to solve:
# you need to run a set of instructions that follow a pattern. so we first
# create a vector in which to store our town means, and then calculate them
# with a for loop:
means <- NA

for(i in 1:nrow(values)) {
  means[i] <- mean(as.numeric(values[i,]))
}
head(means)

# pay attention to the first line of the loop: we're creating a variable
# called i (but you can call it anything) and making it equal to a *list*
# that goes from 1 to the number of rows in our data set. then, inside the
# curly brackets, we calculate the mean of the "ith" row of the data set,
# and store it in the "ith" position in a list called means.

# in R, loops are a 'last resort' measure. this is due to the fact that
# the R language's primitive functions are written in C. when you run a 
# for loop in R, R opens a C environment *each time* it goes through the loop.
# in our Colombian towns example, R created a C environment, ran the code
# inside the loop, and closed it... 1122 times. as you can imagine,
# this is quite slow.

# as a result, there are many R functions that replace loops. in our
# Colombian towns example, we could have tried:

means2 <- rowMeans(values)

# or, from the apply family:

means3 <- apply(values, 1, mean)

# we can check that the three lists are the same:

mean(means == means2 & means == means3)

# ...with the difference that rowMeans() and apply() are much faster. 
# writing a loop is usually not the solution.

# oh and by the way, I didn't write that big list of means by hand.
# I made a loop and used the paste() function to create the strings.

for(i in 1:40) {
  print(paste0("mean(as.numeric(values[",i,",]))"))
}

# R also has *while* loops, which work slightly differently. they will keep
# running, over and over, as long as the Boolean expression is TRUE.

# it can be very useful... in rare cases. say, for example, that you want to
# draw a random number with certain properties: it has to be between 1 and 100,
# and it has to be a multiple of 7.

# here, we draw one number with the sample() function, which takes 2 arguments:
# first, the vector we want to draw from, and second, how many numbers we want
random_number <- sample(c(1:100), 1)
random_number

# here, we use the modulo (or 'mod') operator: n %% m is equal to n mod m,
# which is the remainder of the division of n by m. for example, 3 mod 2 == 1;
# 4 mod 2 == 0. 
while(random_number %% 7 != 0){
  random_number <- sample(c(1:100), 1)
  print(random_number)
  Sys.sleep(0.2)
}
# note that inside the loop, we draw random_number again -- this is 
# absolutely necessary! the loop will continue running *while* random_number is
# not a multiple of 7; if you don't modify random_number inside the loop, it'll
# just keep running over and over until it fills up your RAM, crashing your
# computer.
# also note the Sys.sleep(0.2) line -- that's just for demonstration purposes.
# it tells the system to wait 0.2 seconds before continuing to run code;
# random number generators are too fast and without that line, all the random
# numbers we drew would appear all at once on the console.


# while loops can also be used to run things a certain number of times.
# here, we create a variable called expression that will control the loop below.
expression <- 5

# note that we make reference to the 'expression' variable here, BUT ALSO note
# that we decrease expression by 1 each time the loop runs. this guarantees that
# eventually, the term 'expression > 0' will be FALSE, and thus we will be able
# to leave the loop, safe and sound. without the line that decreases the value
# by 1, your R or your computer will crash.
while(expression > 0){
  print(paste("expression is still true, expression =", expression))
  expression <- expression - 1
}


########################################
########################################
##### BRINGING IT ALL TOGETHER: 
#####     REEXAMINING JOHNSON AND LEEDS (2011)
########################################
########################################

########################################
### BACKGROUND
########################################
# Jesse Johnson (UK) and Ashley Leeds (Rice) have a well known 2011 piece in 
# Foreign Policy Analysis that examines the effect of alliances on conflict, 
# esentially finding that the presence of an alliance within a dyad 
# influences conflict in varying ways between 1816-2000:
  # Deterrence: If a targeted state has a defensive alliance, they may be
  # less likely to be attacked.
  # Empowerment: If a challenging state has an offensive alliance, 
  # they may be more likely to initiate conflict.

# But there's been a lot of discussion about how the development of 
# nuclear weapons may undermine the effect of alliances in deterring conflict.  
# As such, we will briefly examine whether these effects hold on a subset 
# of the data occuring after World War 2.  
  # Disclaimer: This is an ongoing debate and this isn't the most rigorous 
  # way to address this question.  
                # See Kenwick, Vasquez, and Powers (JoP, 2015), 
                # Leeds and Johnson (JoP, 2017), Morrow (JoP, 2017), 
                # and Vasquez and Kenwick (JoP, 2017) for extended discussion.


########################################
### Exploratory Data Analysis
########################################
install.packages("rio", "Rmisc", "texreg")
##################
# Data importation
##################
# We will start with importing the data.  Leeds and Johnson conducted their 
# analysis in Stata (hiss!), so we will have to rely upon packages 
# to import this data.  
# Here, we will use Thomas Leeper's "rio" package, 
# although Hadley Wickham's "haven" package may also be useful.  
  # "rio" is nice because it only uses one function and takes care of 
  # a lot of clean up.

# Here, since I only want one function from "rio", the import function, 
# I'm not going to attach the whole package but instead point R to the "import"
# function in "rio" using the "::" operator.
  # This is good practice as it promotes clarity in where you are 
  # drawing certain functions.  
  # If you, however, are using a lot of functions from the same package, 
  # you may use "library()"

# This will take a few seconds as it is 1,000,000+ directed dyads.
dat <- rio::import('LeedsJohnsonJOPrep.dta')
  # never save something as "data", which is a function in base R

# Let's print the first 5 rows to make sure this looks like it's supposed to.  
# This is done using the "head()" function.
head(dat)
  # Looks like it imported correctly, but there's a lot of missingness here.  
# Let's print the last 5 rows too.
tail(dat)

# so, here we are working with directed dyad years! we have a 
# challenger-target-year level of analysis.
# you can also get a sense of the variables we have.

# We can see that it's directed dyads here:
table(dat$challenger < dat$target)
  # If it was undirected, it would only be false values

# Now, we want to subset down to those directed dyads after 1945 to
# see if Johnson and Leed's finding holds

# Here, we are going to subset the data based upon logicals, 
# the rows where the "year" variable is greater than 1945.
# let's get the values of it first
range(dat$year)
table(dat$year)
  # Yep! This looks about right.
  
# let's look at how many missing observations there are, the presence of 
# missingness would impact subsetting
table(is.na(dat$year))
  # no missingness

# Subset the rows where the "year" variable is greater than 1945.  
# by leaving the columns index empty, we are selecting all columns.
postWW2 <- dat[dat$year > 1945,]

  # Note: R doesn't know if NA values should take on any other value or not, 
  # look here:
NA > 1945
# This produces NA, not false, as such, it's included when subsetting this way.
# so, if variables had missing years, they would have been included

# double check we did this right
range(dat$year)
range(postWW2$year)

# Let's remove the "dat" object to free up space
rm(dat)
  # This will remove it from your workplace, this is helpful for keeping
  # things clean.  It will not, however, remove it from memory.  

# using the gc() function will "garbage collect" and permanently remove
gc()

##################
# Visual EDA
##################

# So, we have the data we are interested in examining all laid out. 
# Let's start by doing some univariate visualizations of the data. 
# Note: Usually, before you get here, you'd have to clean your data first.  
# These data are fairly clean though.

# While most of our primary variables are dichotomous, 
# a few are continuously measured and should be visualized using certain tools,
# such as histograms or density plots.

# The following variables are continuous:
# capprop: Challenger's probability of winning
# ln_distance: log distance in KM between challenger and target
# s_un_glo: similarity in alliance portolios

# since we want to do the same plots for the same variables, because of this,
# might be easier to make a function
# first, let's make a function to make histograms using ggplot2
  # Note: R has native plotting commands, but it takes more look to make it
  # look good, and it's not really an intuitive way to write code.
# load ggplot2
library(ggplot2)
# create function with 4 arguments, the variable as a vector, 
# the character string for the x axis
# the character string for the title
# the number of bins
make_histogram <- function(var = NULL, 
                           xaxis = NULL,
                           title = NULL,
                           nbins = 30){
  p <- ggplot(data = postWW2, aes(var)) + # initialize plot
    geom_histogram(bins = nbins, alpha = 0.5, color = "black", 
                   fill = "firebrick4") + # add histogram layer using nbins, 
                                          # add some transparency using alpha, 
                                          # add some color 
    xlab(xaxis) + # plug in xaxis argument
    ylab("Count") + # define y axis label
    ggtitle(title) + # plug in title
    theme_bw() +  # make the background white
    theme(plot.title = element_text(size=16, face='bold'), # change title font
      axis.title = element_text(size=16, face='bold'), # change axis title font
      axis.text=element_text(size=12)) # change axis text font
  # return p
  return(p)
}

# histogram for capprop
capprop_hist <- make_histogram(var = postWW2$capprop, 
                               xaxis = "Challenger Probability of Winning",
                               nbins = 25)
# print it
capprop_hist

# histogram for ln_distance
ln_distance_hist <- make_histogram(var = postWW2$ln_distance, 
                               xaxis = "Distance in KM (ln)",
                               nbins = 25)
# print it
ln_distance_hist

# histogram for s_un_glo
s_un_glo_hist <- make_histogram(var = postWW2$s_un_glo, 
                                   xaxis = "Alliance Porfolio Similarity",
                                   nbins = 25)
# print it
s_un_glo_hist

# What if you want these plots next to one another?
# put them in a list
hist_list <- list(capprop_hist, ln_distance_hist, s_un_glo_hist)

# use the multiplot function in Rmisc to put them in a common layout
Rmisc::multiplot(plotlist = hist_list, cols = 3)

# Or we can save them to a file
pdf("HistPlots.pdf")
Rmisc::multiplot(plotlist = hist_list, cols = 3)
dev.off()

# shucks! a little smooshed, stretch the plot width wise
pdf("HistPlots.pdf", width = 14)
Rmisc::multiplot(plotlist = hist_list, cols = 3)
dev.off()

# You could also plot these as density plots, which may be better for 
# visualization as it avoids binning bias:
make_density <- function(var = NULL,
                         xaxis = NULL,
                         title = NULL){
  p <- ggplot(data = postWW2, aes(var)) + # initialize plot
    # add density layer, add some transparency using alpha, add some color 
    geom_density(alpha = 0.5, color = "black", fill = "firebrick4") + 
    xlab(xaxis) + # plug in xaxis argument
    ylab("Density") + # define y axis label
    ggtitle(title) + # plug in title
    theme_bw() +  # make the background white
    theme(plot.title = element_text(size=16, face='bold'), # change title font
        axis.title = element_text(size=16, face='bold'),# change axis title font
        axis.text=element_text(size=12)) # change axis text
  # return p
  return(p)
}

# density for capprop
capprop_dens <- make_density(var = postWW2$capprop, 
                               xaxis = "Challenger Probability of Winning")
# print it
capprop_dens

# density for ln_distance
ln_distance_dens <- make_density(var = postWW2$ln_distance, 
                                   xaxis = "Distance in KM (ln)")
# print it
ln_distance_dens

# density for s_un_glo
s_un_glo_dens <- make_density(var = postWW2$s_un_glo, 
                                xaxis = "Alliance Porfolio Similarity")
# print it
s_un_glo_dens

# What if you want these plots next to one another?
# put them in a list
density_list <- list(capprop_dens, ln_distance_dens, s_un_glo_dens)

# use the multiplot function in Rmisc to put them in a common layout
Rmisc::multiplot(plotlist = density_list, cols = 3)

# make plot
pdf("DensityPlots.pdf", width = 14)
Rmisc::multiplot(plotlist = density_list, cols = 3)
dev.off()

# What do these tell us?

# we also have dichotomous variables we want to summarize
  # dispute
  # ptargdef
  # pchaloff
  # jdem
# How do we visualize dichotomous (or otherwise discrete) variables here? 
  # We can use bar graph which tell us about the relative frequency of values.
  # We can use time series plots which tell us about the value of 
  # variables over time.

# make bar graph function
make_bargraph <- function(var = NULL,
                         xaxis = NULL,
                         title = NULL){
  p <- ggplot(data = postWW2, aes(var)) + # initialize plot
    # add bar layer, add some transparency using alpha, add some color 
    geom_bar(alpha = 0.5, color = "black", fill = "firebrick4") + 
    xlab(xaxis) + # plug in xaxis argument
    ylab("Count") + # define y axis label
    ggtitle(title) + # plug in title
    theme_bw() +  # make the background white
    xlim("0", "1") +
    theme(plot.title = element_text(size=16, face='bold'), # change title font
        axis.title = element_text(size=16, face='bold'),# change axis title font
        axis.text=element_text(size=12)) # change axis text font
  # return p
  return(p)
}

# bar for dispute
dispute_bar <- make_bargraph(var = as.factor(postWW2$dispute),# factors help 
                                                             # with x axis ticks
                             xaxis = "Militarized Disputes")
# print it
dispute_bar

# bar for ptargdef
ptargdef_bar <- make_bargraph(var = as.factor(postWW2$ptargdef), 
                             xaxis = "Relevant Defensive Alliance")
# print it
ptargdef_bar

# bar for pchaloff
pchaloff_bar <- make_bargraph(var = as.factor(postWW2$pchaloff), 
                              xaxis = "Relevant Offensive Alliance")
# print it
pchaloff_bar

# bar for jdem
jdem_bar <- make_bargraph(var = as.factor(postWW2$jdem), 
                              xaxis = "Jointly Democratic Dyads")
# print it
jdem_bar


# What if you want these plots next to one another?
# put them in a list
bar_list <- list(dispute_bar, ptargdef_bar, pchaloff_bar, jdem_bar)

# use the multiplot function in Rmisc to put them in a common layout
Rmisc::multiplot(plotlist = bar_list, cols = 2)

# make plot
pdf("BarPlots.pdf", width = 14)
Rmisc::multiplot(plotlist = bar_list, cols =2)
dev.off()

# We might want to visualize these variables as a function of time.  
# This will require calculating counts of the variable for each year.  
# This will bring together several elements of this workshop.  

# get the years
years <- sort(unique(postWW2$year))

# initialize an empty data frame
plot_df <- data.frame()

# do a for loop! define a variable t which is iteratively drawn from years, 
# takes a few seconds
for(t in years){
  # subset broader data frame down to a particular year
  year_df <- postWW2[postWW2$year == t,]
  # get the sum of each discrete variable for this annual data frame
  n_disputes <- sum(year_df$dispute, na.rm = TRUE)
  n_dem <- sum(year_df$jdem, na.rm = TRUE)
  n_def <- sum(year_df$ptargdef, na.rm = TRUE)
  n_off <- sum(year_df$pchaloff, na.rm = TRUE)
  
  # now, let's concatenate these all into a vector so that its a row
  row <- c(t, n_disputes, n_dem, n_def, n_off)
  
  # bind this row to the data frame
  plot_df <- rbind(plot_df, row)
  
}
  # as you become more experienced, you can move to using apply functions.  
# apply functions are quicker than for loops.

# let's look at this
head(plot_df)

# ugly column names!
colnames(plot_df) <- c("year", "n_disputes", "n_dem", "n_def", "n_off")
head(plot_df)
# much better!

# let's plot each of these separately (given the differences in scales)
make_tsplot <- function(var = NULL,
                        yaxis = NULL,
                        title = NULL){
  p <- ggplot(data = plot_df, aes(x = year, y = var)) + # initialize plot
    geom_line(lwd = 1.5, color = "firebrick4") + # add thickish red line
    xlab("Year") + # plug in xaxis argument
    ylab(yaxis) + # define y axis label
    ggtitle(title) + # plug in title
    theme_bw() +  # make the background white
    theme(plot.title = element_text(size=16, face='bold'), # change title font
        axis.title = element_text(size=16, face='bold'),# change axis title font
        axis.text=element_text(size=12)) # change axis text font
  # return p
  return(p)
}

# line for dispute
# (factors help with x axis ticks)
dispute_tsp <- make_tsplot(var = plot_df$n_disputes, 
                             yaxis = "Militarized Disputes")
# print it
dispute_tsp

# line for ptargdef
ptargdef_tsp <- make_tsplot(var = plot_df$n_def, 
                            yaxis = "Relevant Defensive Alliance")
# print it
ptargdef_tsp

# line for pchaloff
pchaloff_tsp <- make_tsplot(var = plot_df$n_off, 
                            yaxis = "Relevant Offensive Alliance")
# print it
pchaloff_tsp

# line for jdem
jdem_tsp <- make_tsplot(var = plot_df$n_dem, 
                        yaxis = "Jointly Democratic Dyads")
# print it
jdem_tsp


# What if you want these plots next to one another?
# put them in a list
tsp_list <- list(dispute_tsp, ptargdef_tsp, pchaloff_tsp, jdem_tsp)

# use the multiplot function in Rmisc to put them in a common layout
Rmisc::multiplot(plotlist = tsp_list, cols = 2)

# make plot
pdf("TSPlots.pdf", width = 14)
Rmisc::multiplot(plotlist = tsp_list, cols =2)
dev.off()

# Note: once we have visualized single variables, we'd often want to move to 
# visualizing the relationships between variables using scatter plots.  
# Let's give this a shot for the two variables of interest! 

p1 <- ggplot(data = postWW2, aes(x = ptargdef, y = dispute)) + # initialize plot
    geom_point() + # add thickish red line
    xlab("Relevant Defensive Commitment") + # plug in xaxis argument
    ylab("Dispute Initiation") + # define y axis label
    theme_bw() +  # make the background white
    theme(plot.title = element_text(size=16, face='bold'), # change title font
        axis.title = element_text(size=16, face='bold'),# change axis title font
        axis.text=element_text(size=12)) # change axis text font

p2 <- ggplot(data = postWW2, aes(x = pchaloff, y = dispute)) + # initialize plot
  geom_point() + # add thickish red line
  xlab("Relevant Offensive Commitment") + # plug in xaxis argument
  ylab("Dispute Initiation") + # define y axis label
  theme_bw() +  # make the background white
  theme(plot.title = element_text(size=16, face='bold'), # change title font
        axis.title = element_text(size=16, face='bold'),# change axis title font
        axis.text=element_text(size=12)) # change axis text font

# any guesses to what these look like?
scatter_list <- list(p1, p2)

Rmisc::multiplot(plotlist = scatter_list, cols =2)

# Box plots are also an option, but not really useful for our purposes.  


##################
# Quantitative EDA
##################

# Quantitive EDA is a means of providing summary statistics for variables or 
# the relationship between variables.

# at the very basic level, we can calculate the following:
  # frequencies of variables
  # central tendencies (mean, median)
  # measures of spread (std. deviation, variance, range)
  # percentiles of variables
  # pearson correlations between variables
  # t-tests for differences in means
  # etc.  


# we will start with the univariate case and then move to bivariate 
# quantitative EDA

# let's define a list of variables that we are interested in summarizing, 
# those we've previously used
vars <- list("dispute", "ptargdef", "pchaloff", "capprop", "ln_distance", 
             "s_un_glo", "jdem")

# as I mentioned, apply family functions are faster than for loops 
# and take at min. 2 arguments, data and a function to repetitively 
# perform on that data. 
# here, the data is the list vars
# the function is a univariate eda function we will write 
# to do everything for us

univariate_eda <- function(var){
  # define var as a vector
  varname <- var
  var <- postWW2[,var]
  # get summary statistics
  table_out <- summary(var)
  
  # if dichotomous, get the frequencies 
  # (which would be odd for continuous variables)
  if(length(unique(var)) == 2){
    frequencies_out <- table(var)
  } else {
    # we need to return a variable frequencies out, so, if not calculating them,
    # give it a null
    frequencies_out <- NULL
  }
  
  # calculate measures of spread
  sd_out <- sd(var, na.rm = TRUE)
  var_out <- sd_out^2
  range_out <- range(var)
  
  # put in a named list
  out_list <- list(variable = varname,
                   summary_stats = table_out,
                   frequencies = frequencies_out,
                   stddev = sd_out,
                   variance = var_out,
                   var_range = range_out)
  return(out_list)
}

# going to use lapply or list apply, which applies a function over a list
var_desc <- lapply(vars, univariate_eda)

# we can not index these according to the order that we feed the variables in
# Ex: for dispute
var_desc[[1]]

# we can also write this to a text file
sink("VariableSummaries.txt")
var_desc
sink()

# But what about bivariate quantitative EDA? Won't this help us in understanding
# the relationship between alliances and conflict? 

# Two-way frequency tables are commonly used, especially when
# the treatment and outcome are binary
# 1st arg is row, 2nd arg is column (standard in R)
freq <- table(postWW2$ptargdef, postWW2$dispute)
rownames(freq) <- c("No Alliance", "Alliance")
colnames(freq) <- c("No Conflict", "Conflict")
freq
  # Wow, conflict is rare, regardless of if there are alliances or not.  
  # In fact, it looks like there are more conflicts in dyads where a target has an alliance

# We can also calculate the cell proportions:
prop.table(freq) # this treats the denominator as the sum of all table values, each cell is the percentage of values
  # This indicates the fast number of dyads have no conflict but the target has an alliance
prop.table(freq, 1) # this treats the denominator as the row sums
  # This indicates that conflict is relatively rare, regardless if the target has an alliance or not
prop.table(freq, 2) # This treats the denominator as the column sum
  # This indicates that in most cases of conflict, or no conflict, there are alliances

# These may not be particularly useful though as conflict is a rare event.

# we can also do a variety of operations on this table, such as:
# chi-squared tests:
chisq.test(freq)
  # fail to reject the null that there is no relationship between alliances and conflict
  # in other words: it looks as if there may not be a relationship between these variables

# We can also use T-Tests. T-tests provide a means of assessing the 
# difference in means for two samples of data, here the sample of dyads 
# for which there is no relevant alliance and those for which
# there is a relevant alliance.  
# we will call these the control and treatment samples
t_test_deterrence <- t.test(x = postWW2[postWW2$ptargdef == 0, "dispute"], 
                            y = postWW2[postWW2$ptargdef == 1, "dispute"])
t_test_deterrence
  # Interesting, we fail to reject the null
  # difference in means appears to be equal to zero.

# we can also use pearson correlations
corr_deterrence <- cor.test(x = postWW2$ptargdef, y = postWW2$dispute,
                            method = "pearson")
corr_deterrence
  # direction of the correlation is expected, negative, 
  # not significant at any conventional level


# However, we know that there are a variety of factors that confound
# this relationship.  
# We need control variables

# Logistic regression in a linear model that seeks to explain a 
# binary outcome variable.  You will cover this in Quant II

logit <- glm(dispute ~ ptargdef + pchaloff + capprop + ln_distance + s_un_glo +
               jdem,
             data = postWW2,
             family = "binomial")

texreg::screenreg(logit)
texreg::plotreg(logit)

########################################
########################################
##### Resources
########################################
########################################

# There are plenty of resources for learning R! 
   # Grolemund and Wickham: R for Data Science
      # FREE BOOK: http://r4ds.had.co.nz/
  # Comprehensive R Archive Network
      # https://cran.r-project.org/
  # swirl: Learn R in R interactively
      # https://swirlstats.com/
  # DataCamp: Many interactive classes at your own pace
      # https://www.datacamp.com/
  # StackExchange: If you have a question, it's probably been asked!
    # https://stackoverflow.com/questions/tagged/r
  # Us! PRISM is here to help you!
    # Ben's Office Hours: Tuesday, 12-2 
    # Ricardo's Office Hours: Thursday, 12-2

# We also have some workshops planned
  # Intro to Python for R Users: 9/6, 1-2:30, Spencer Room
  # Data Manipulation in the Tidyverse: 9/25, 1-2:30, Spencer Room
  # PRISM Methods Lunch: 10/22, 12:00-1:30, Information Forthcoming
  # Michael Kearney, Accessing Twitter Data in R, Tentative 11/13, 1-2:30 
  