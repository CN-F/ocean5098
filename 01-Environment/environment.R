xaringanExtra::use_clipboard()

## # R update (to run in R native GUI only)
## if(!require(installr)) {
##   install.packages("installr"); require(installr)}
## updateR()

## # install the package `abc` with its name
## install.packages("abc")

## # function are case sensitive
## # all information after `#` is ignored
## # it is call a comment. Commenting is very important

## # use/load the package `abc` with its name
## library("abc")

## # Install & load `vegan` package
## install.packages('vegan')
## library('vegan')

# simple help on the use of a function
median

## # detailed help on the use of a function
## ?median # alternative 1
## help(median) # alternative 2

## # using a keyword for search
## ??median

## # get your current working directory
## getwd()

## # set your current working directory
## setwd()

## # Use R as a calculator
## 3+2 # addition
## 3-2 # substraction
## 3*2 # multiplication
## 3/2 # division
## 3^3 # power
## log(2) # logarithm
## exp(2) # exponential
## (5 + 3) / 4 # define priority using () or {}
## pi*4 # common function

# my current list is empty 
ls()

# I create and list 3 objects 
a<-'corals' # create an object 'a' containing 'corals'
b<-'are' # create an object 'b' containing 'are'
c<-'cool'# create an object 'c' containing 'cool'
ls() # list objects 'a', 'b', 'c'

# clean objects in memory
rm(list=ls())

# 1. using the package `readxl`
# 2. reading my `reef_fish.xlsx` in my working directory
# 3. importing `reef_fish.xlsx` in a `fish` object
library(readxl) # load the package `readxl'
read_excel('data/reef_fish.xlsx') # automatically print on my screen
fish<-read_excel('data/reef_fish.xlsx') # store my table in an object called `fish`
fish # print my object `fish`   

# importing a .txt file
fish<-read.table('data/reef_fish.txt', header=T, sep='\t', dec='.') 

## # import file by path name
## fish<-read.table ("D:/.../data/reef_fish.txt",header = TRUE,sep="\t", dec=".")# long version
## fish<-read.table("D:/.../data/reef_fish.txt", TRUE, "\t",".")# short version

## fish<-read.table(file.choose (), header = TRUE,sep="\t", dec=".")

## # Too fast you need to try by yourself?
## # Maybe a look at ?write.table may help

## q()

# import data set and create an object in R studio + simple plot 
fish<-read.table('Data/reef_fish.txt', header=T, sep='\t', dec='.')
barplot(fish$richness, main="Top 10 reef fish Richness (Allen, 2000)", horiz=TRUE, names.arg=fish$country, cex.names=0.5, las=1)
