###
### Short tutorial on how to open data in R, 
### take cursory peek, and subset what you want (SELECT IF)
###

##Open R, R Studio, whatever other packages are needed

##Check to see what current working directory is

#getwd()

##3 options:
## 1. Change it to something else for easy opening
## 2. Keep it as is and move data to working dir for easy opening
## 3. Keep it as is and specify full path of non-wd data location

##Assume working dir is simply C:/R

##EG of #1: 
#setwd('DriveLetter:/level1/level2/level3/levelX')

##EG of #1: 
#setwd('C:/R/udacity_course')

##EG of #3: 
#DFname <- read.csv('DriveLetter:/level1/levelX/FileName.csv')

##EG of #3: 
#statesInfo <- read.csv('C:/R/udacity_course/stateData.csv')

## EG dataset here has data on all 50 states
## Look at the dataset in spreadsheet form

#View(statesInfo)

## Decide we only want states in region 1 (new england)
## A few diff ways to filter data

## Subset of full dataset will appear in Console
##IMO this is largely useless as you can look at it but not manipulate it

#subset(statesInfo, state.region==1)

## Create a dataset which is subset of full dataset

#region1subset <- subset(statesInfo, state.region==1)

## Create subset but specify rows and columns to keep
## Great when need a few obs w a few var from a very large dataset
## syntax is subsetDFname <- subset(DFname, row condition, column list)
## for Row condition, standard logical operators etc
## for columns there are options:
## keep all in succession - use colon (vars= a,b,c,d,e, want b-d use "select=b:d")
## keep specific - use vector (vars = a,b,c,d,e, want a,b,e use "select=c(a,b,e)")

#region1subset1 <- subset(statesInfo, state.region==1,select=state.abb:murder )
#region1subset2 <- subset(statesInfo, state.region==1,select=c(state.abb, state.region, population, murder))

