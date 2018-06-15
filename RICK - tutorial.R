# Intro to R
# 12 June 2018
# Rick Scavetta
# IMPRS-LS & IRTG joint DA workshop

# Clear workspace
rm(list = ls())

# Load packages
library(dplyr)
library(ggplot2)
library(purrr)
library(tidyr)

# R Notation
n <- log2(8) # log 2 of 8 (2^x = 8)
n # Call an object name is short cut for print(n)

# Simple workflow with a built-in dataset
PlantGrowth

# How many groups and what are they?
# "groups" are called "levels"
levels(PlantGrowth$group)
nlevels(PlantGrowth$group)

# Descriptive statistics
mean(PlantGrowth$weight)

# group-wise statistics
# pipe operator %>% (shift + ctrl + m)
PlantGrowth %>%
  group_by(group) %>%
  summarise(avg = mean(weight),
            stdev = sd(weight))

# Make plots
# 1 - Data (PlantGrowth)
# 2 - Aesthetic mappings (the scales, axes) i.e. y, x
# 3 - Geometry (how the plot looks)

g <- ggplot(PlantGrowth, aes(group, weight))
g # the base layers

# Add geometries:
# Individual points, adjust position (between 0-1)
# Hollow circles, use shape = 1
# Transparency, alpha = (a # between 0-1)
g +
  geom_point(position = position_jitter(0.15),
             alpha = 0.6)

# Box plot
g +
  geom_boxplot()

# group differences
# build a linear model
plant.lm <- lm(weight ~ group, data = PlantGrowth)

# t-tests
plant.lm
summary(plant.lm) # get t-tests because of plant.lm structure
class(plant.lm)
summary(PlantGrowth) # because it's a data set, not an lm
class(PlantGrowth)
# ANOVA
anova(plant.lm)

# Reproduce analysis using a different data set:
chickwts

# Element 2: Functions
# Everything that happens, is because of a function

# i.e. Artihmetic operators
34 + 6
`+`(34, 6)

# Order of operations
# BEDMAS - Brackets, exp, div, mult, add, sub
2 - 3/4 # 1.25
(2 - 3)/4 # -0.25

# Make some objects:
n <- 34
p <- 6

n + p

# Exercise 1, p 27L mx+b
m <- 1.12
b <- -0.4
m * 3 + b
m * 8 + b
m * 9 + b
m * 23 + b

# Using functions:
# Generic form of functions
# fun_name(args)

# args can be named or unnamed
# called using names or position
# e.g.
log2(8) # shortcut
log(x = 8, base = 2) # full naming
log(8, 2) # only positional matching
log(8, base = 2) # mixed

log(2, x = 8) # pretty bad style - no go :/

# Common functions:
# Combine/concatenate unnamed arguments
xx <- c(3, 8, 9, 23)

myNames <- c("healthy", "tissue", "quantity")
myNames

# Sequential numbers with seq()
seq(from = 1, to = 100, by = 7)
foo1 <- seq(1, 100, 7)

foo2 <- seq(1, n, p)

# The : operator for seq(x, y, 1)
1:10 # is short for...
seq(1, 10, 1)

# Types of math functions:
# 1 - Transformation - number of output equals number of input
# Every value is treated the SAME way
# m*x+b
# log2
# sqrt

# 2 - Aggregration - typically 1 output (or small # or)
# mean
# var
# sd
# n

# Exercise 2, p30: Trans or Aggr? What do these look like?
foo2 + 100 # trans
foo2 + foo2 # trans
sum(foo2) + foo2 # aggr & trans
seq(56, 67, 3) + foo2 # trans

#########################################################
################### fundamental concept #################
################### vector recycling! ###################
#########################################################

# What if...
1:4 + foo2

# Exercise 3, p30:
m * xx + b

# Excercise 4, p30:
m2 <- c(0, 1.12)

m2 * xx + b # do this twice, once for each m2 value
# i.e. reiterate over all values of m2, NOT vector recycling!

# solutions:
# take each value in m2 1-by-1 and then do transformation
# 1 - a for loop to cycle through all values in m2.
# 2 - manually write out each equation indexing the positions in m2
# 3 - map m2 onto a function (use lapply or map in purrr package)

# First make a function:
equation <- function(x, m) {
  m * x - 0.4
}

equation(xx, 1.12)

# but this still doesn't work
equation(xx, m2)

# so use map() from purrr
map(m2, ~ equation(xx, .))

# Element 3: Objects
# Anything that exists is an object

# Vectors: 1 Dimensional, homogenous
# e.g.
foo1
foo2

# one data type
# The four most common user-defined atomic vector types
# logical - T/F, TRUE/FALSE, 1/0 (aka binary, boolean)
# integer - whole numbers
# double - real numbers (decimal places)
# character - anything

typeof(foo1)
typeof(myNames)
# numeric is for double and integer

# some more objects, p36:
foo3 <- c("Liver", "Brain", "Testes", "Muscle", "Intestine", "Heart")

foo4 <- c(T, F, F, T, T, F)
typeof(foo4)

# The values are called "elements"
length(foo1) # 15 elements

# Homogenous:
test <- c(1:10, "bob")
test
typeof(test)

# remove bob
test[-11]

###################################################
################### common problem ################
################### wrong type! ###################
###################################################

# e.g.
mean(test[-11]) # its still a character!

# solution is coercion
test <- as.integer(test)
mean(test, na.rm = T)

# List - 1 dimensional, heterogenous types
# e.g.
typeof(plant.lm)

length(plant.lm)
attributes(plant.lm)
names(plant.lm) # A character vector

# Any named element can be called with $ notation
plant.lm$coefficients
plant.lm$residuals

# Data frame - 2 dimensional, heterogenous
# A special form of a list, where
# each element is a vector of the SAME length

# row is an observations
# columns is a variable (i.e. element)

foo.df <- data.frame(foo4, foo3, foo2)
foo.df

attributes(foo.df)
typeof(foo.df)
class(foo.df)
# class determines how other functions
# deal with an object:
summary(foo.df)
summary(plant.lm)

###################################################
################### common problem # 2 ############
################### wrong structure! ##############
###################################################

# solution: coerce, or rearrange

# Access names:
names(foo.df) # This is just a character vector
names(foo.df) <- myNames

# ALWAYS explore the struture of your data!
summary(foo.df)
str(foo.df)
glimpse(foo.df)

# Element 4: Logical Expressions
# Relational operators: Ask YES/NO questions
# == equivalency
# != non-equivalency (! always means the opposite)
# <=, <, >, >=
# !x negation of x, where x is a logical vector

# Aside: Both of these work
# just use <- to avoid confusion
m <- 1.12
m = 1.12

######### ALWAYS equate to a logical vector ########
######### TRUE/FALSE, T/F, 1/0 #####################

n
p
n > p
n < p
foo4 # logical vector
!foo4 # the negation of foo4

# Logical operators: Combine YES/NO questions
# & AND - a TRUE in EVERY question
# | OR - a TRUE in at LEAST ONE questions
# %in% WITHIN - short cut for many == with |

# filter using dplyr functions and syntax
# Alternative: use subset()

# Apply to logical data
# use shift + ctrl + m for %>%
# only healthy observations
foo.df %>%
  filter(healthy == TRUE)
foo.df %>%
  filter(healthy)

# only non-healthy observations
foo.df %>%
  filter(!healthy)

# Apply to numbers:
# below quantity 10
foo.df %>%
  filter(quantity < 10)

# Aside:
filter(foo.df, quantity < 10)
subset(foo.df, quantity < 10) # old way, in this case the same

# Two questions
# Range: between 10 - 20
foo.df %>%
  filter(quantity > 10 & quantity < 20)
# in filter() the , is short for &
foo.df %>%
  filter(quantity > 10, quantity < 20)

# This is not possible:
foo.df %>%
  filter(10 < quantity < 20)

# Let's make this explicit: We asked...
foo.df$quantity > 10 # a logical vector
foo.df$quantity < 20 # a logical vector

# what if I used an OR |
# It's meaningless
foo.df %>%
  filter(quantity > 10 | quantity < 20)

# The extremes, beyond 10 and 20
foo.df %>%
  filter(quantity < 10 | quantity > 20)

# Impossible :/
foo.df %>%
  filter(quantity < 10 & quantity > 20)

# Apply to characters:
# NO pattern matching
# Heart samples:
foo.df %>%
  filter(tissue == "Heart")

# 2 or more: Liver, Heart
# cheap and easy (ok for two items)
foo.df %>%
  filter(tissue == "Heart" | tissue == "Liver")

# WRONG way - NEVER do this!
foo.df %>%
  filter(tissue == c("Liver", "Heart"))
foo.df %>%
  filter(tissue == c("Heart", "Liver"))

# generalise for many items - proper way :)
foo.df %>%
  filter(tissue %in% c("Liver", "Heart"))
foo.df %>%
  filter(tissue %in% c("Heart", "Liver"))

# What if I wanted to get everything BUT Heart and Liver
foo.df %>%
  filter(!(tissue %in% c("Liver", "Heart")))

# Element 5: Indexing
# Find information according to position using []

# Vectors:
foo1
foo1[6] # The 6th position
foo1[p] # The pth position, since p == 6
foo1[3:p] # The 3rd to pth values
foo1[p:length(foo1)] # From pth to last value

# Use all combinations of:
# Integers, objects, functions

# But... the exciting part is... logical vectors
# i.e. from logical expressions
# Get all values less than 50
foo1[foo1 < 50]

# Data frames with []:
# 2 dimensional: [ rows , columns ]
foo.df[3,] # 3rd row, ALL columns
foo.df[,3] # ALL rows, 3rd column

foo.df[3:6, 3] # 3rd to 6th row, only quantity column
foo.df[3:6, "quantity"]
# Or as a vector:
foo.df$quantity[3:6]

# 1st & 3rd column
foo.df[, c(1,3)]
foo.df[, c("healthy","quantity")]
foo.df[, -2] # By excluding column 2

# Combine in all variety of ways
foo.df[foo.df$tissue == "Heart", c("healthy","quantity")]
foo.df[foo.df$quantity < 10, "tissue"]

# Use tibbles to prevent switching between
# vector and data frames:
foo.df <- as_tibble(foo.df)
# Now... this remains a data frame!
foo.df[foo.df$quantity < 10, "tissue"]

# Basically, [] is just filter() or subset()
# so this is the same thing:
# but filter requires a logical vector
foo.df %>%
  filter(quantity < 10) %>%
  select(tissue)

# NOT possible: missing comma :/
foo.df[foo.df$tissue == "Heart"]
# But... no comma is short-hand for columns
foo.df[3] # is the same as...
foo.df[,3]

# So you don't need a comma in [] for a data frame
# But what if I DO have a comma for a vector?
foo1[,p]

# Element 8: Factor Variables (with levels)
# Categorical variables (with groups)
# AKA discrete or qualitative

# e.g.
PlantGrowth$group

# Factor variables are actually a special
# class of type integer, with labels
typeof(PlantGrowth$group)
class(PlantGrowth$group)

# see the actual integers
str(PlantGrowth)

# Also:
foo3 # character
foo.df$tissue

str(foo.df)
# The integer values are 4 1 6 5 3 2
# And the associated labels are:
levels(foo.df$tissue)
# "Brain"     "Heart"     "Intestine" "Liver"     "Muscle"    "Testes"

# Example of a common problem:
xx <- c(23:27, "bob")
xx # A chr vector
# convert to a data.frame:
test <- data.frame(xx)
test$xx # automatically converted to a factor :/

# Convert to a numeric
as.integer(xx) # from a true character
as.integer(test$xx) # from a factor, gets group ID
str(test) # we actually got the integer, not the value :(

# so... convert first from factor to character, and then to integer
as.integer(as.character(test$xx))

# Alternatively, preven factors
test <- data.frame(xx, stringsAsFactors = FALSE)
test$xx # Remains character

# Element 9 & 10: Tidy data & split-apply-combine

# Work on a new data set
source("PlayData.R")

# Make the data tidy using the tidyr package
# use gather(), four arguments
# 1 - data (input)
# 2&3 - key, value (the names of the output columns)
# 4 - either the ID or MEASURE variables
gather(PlayData, key, value, -c(type, time)) # ID vars
PlayData.t <- gather(PlayData, key, value, c(height, width)) # MEASURE vars

# Now for split-apply-combine
# See the SILAC project
# split according to some variable(s)
# apply some functions
# combine for output

# scenario 1: compare by key (height & width)
# trans: use original data
PlayData$height/PlayData$width

# Aggregrations (use tidy data)
# Scenario 1 (group by type and time)
PlayData.t %>%
  group_by(type, time) %>%
  summarise(avg = mean(value))

# Scenario 2 (group by type and key)
PlayData.t %>%
  group_by(type, key) %>%
  summarise(avg = mean(value))


# Scenario 3 (group by time and key)
PlayData.t %>%
  group_by(time, key) %>%
  summarise(avg = mean(value))




