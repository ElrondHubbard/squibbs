## EG Read JSON data
## JSON is javascript language/data, frequently used for data online

## EG is taken step by step from Course 3 Week 1, Lecture 8 video/pdf

## Load json reading package
## Create var for json data, names will list all varnames

library(jsonlite)
json_eg_data <- fromJSON("https://api.github.com/users/jtleek/repos")
names(json_eg_data)

## Drill down further into list for specific names within a variable

names(json_eg_data$owner)
json_eg_data$owner$login


## Can take existing dataframe turn it into JSON data

json_iris_data <- toJSON(iris, pretty=TRUE)

## Immediately below will print out a LOT of data, but shows conversion effective
cat(json_iris_data)

## Can take JSON data and turn it into DataFrame - opposite of above

iris2 <- fromJSON(json_iris_data)
head(iris2)


