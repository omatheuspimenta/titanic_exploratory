## "Exploratory Analysis - Titanic"
## Analysis and Regression
## @ Matheus Pimenta
## github.com/omatheuspimenta/titanic_exploratory
###########################################################

#####
# Set path
setwd("/home/matheus/Dropbox/06_doutorado/2021_01/Bioestatistica/projeto/dataset/")

#####
# Libraries
library("Hmisc") #for describe
library("stringr") #for string manipulation
library("ggplot2") #for graphics
library("plyr") #for count
library("dplyr") #for summary 
library("boot") #for bootstrap
#####
# Load file
load("titanic3.RData")
