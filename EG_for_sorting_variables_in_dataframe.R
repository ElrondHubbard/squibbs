##Syntax to create a specific sort order and apply it to a dataframe

# sort1= <- order(df1$var1 , df1$varN)
# df(X) <- df1[sort1,]

# Line1 creates sort1, shows up as 'value' in DataPane, think of it as little .exe file
# Line2 applies sort1 to df of choosing
# For Line2 - df(X) could = df1 or other df
# If = df1, will apply sort1 to df1, ie change order of var in df1 per sort1
# If = df(other#), will create new df that is copy of df1, but is sorted per sort1

# name sort1 intelligently
# can sort1 by as many var as wanted, use comma to separate them
# default order is Ascending (small - large)
# to get Descending put minus (-) in front
# see example below w data if confused. I fucking was.

# Filename: Sort.R
# Assumes file is in C:/R on YK computer
load(file = "mydata.RData")
# Show all 8 var in dataset
mydata
# Show first four observations in order.
mydata[ c(1, 2, 3, 4), ]
# Show them in reverse order.
mydata[ c(4, 3, 2, 1), ]
# Create order variable which sorts mydata by workshop (A).
myW <- order( mydata$workshop )
# Show order of obs - note difference in sequence from initial b/c of sort
myW
#Show all 8 obs in mydata, sorted by workshop(A)
mydata[ myW, ]
# Create order variable to sort mydata by gender(A) then workshop(A).
myGW <- order( mydata$gender, mydata$workshop )
# SHow order of obs - note diff both from OG and from myW pattern
myGW
# Show 8 obs in mydata sorted by gender and workshop
mydata[ myGW, ]
# Create order variable for
# descending (-) workshop then gender
myWdG <- order( -mydata$workshop, mydata$gender )
myWGd
# Print data in WG order.
mydata[ myWdG, ]
# Save data in WdG order.
mydataSorted <- mydata[ myGW, ]
mydataSorted

rm(myWdG, mydata, mydataSorted, myGW, myW, myWdGd) 
