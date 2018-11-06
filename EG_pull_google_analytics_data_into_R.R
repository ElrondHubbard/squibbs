##NOTES FORMAT
## - 1# = remove # and use as code
## - 2# = notes or annotation


##original article here
##http://www.lunametrics.com/blog/2015/11/23/export-google-analytics-data-with-r/
##and here
##http://www.lunametrics.com/blog/2016/06/02/getting-started-r-google-analytics/

##
##Open R
##

#install.packages (devtools, curl, jsonlite)
#library (devtools, curl, jsonlite)

##
##devtools adds the install_github command
##

#install_github("skardhamar/rga")
#library(rga)

##
##code examples below pull diff types or reports
##


##
##Parameters list for getData command:
##
## id - create value called 'id' that = id of GA accountview wanted
## start.date / end.date - first/last day of range of dates for data collection
## metrics - list of metrics to pull, can choose up to 10
## dimensions - list of dimensions to pull, can choose up to 7
## batch - default = TRUE, gets around cap of 10K obs per pull 
## sort(optional) - variable to sort by 
## filter(optional) - filter by
## start(optional) - start pull at obs X
## max(optional) - max # of obs to pull
## segment(optional) - pulls a segment, can be predefined or can define in code

## Pulls a report that lists metrics for each page on site
## BY PAGE REPORT

rga.open(instance= "ga")
id <-138026978
ga$getData(id)
by_page_170101_170212 <-ga$getData(
id,
batch = TRUE,
start.date = as.Date("2017-01-01"),
end.date= as.Date("2017-02-12"),
metrics =
"ga:entrances,
ga:pageviews,
ga:uniquePageviews,  
ga:avgTimeOnPage,
ga:bounces,  
ga:bounceRate,
ga:exits",
dimensions = "ga:pageTitle", 
sort="-ga:pageviews",
filters="",
segment="",
start=1)

#rga.open(instance= "ga")
#id <-138026978
#ga$getData(id)
#by_page_170101-170212 <-ga$getData(id,
#start.date = as.Date('2017-01-01'),
#end.date= as.Date('2017-02-12'),
#metrics = "ga:users",
           "ga:newUsers",  
           "ga:pageviews",
           "ga:sessions",
           "ga:avgSessionDuration",  
           "ga:bounceRate",
           "ga:organicSearches",
           "ga:avgTimeOnPage",
#dimensions = "ga:date", 
#batch = TRUE,
#sort= "ga:pageviews")

#filter="ga:country=United States; ga:medium==organic",
#start = XX
#max = XX
#segment = 

##
## For each line above:
##
## id - create value called 'id' that = id of GA accountview wanted
## start.date / end.date - first/last day of range of dates for data collection
## metrics - list of metrics to pull, can choose up to 10
## dimensions - list of dimensions to pull, can choose up to 7
## batch - default = TRUE, gets around cap of 10K obs per pull 
## sort(optional) - variable to sort by 
## filter(optional) - filter by
## start(optional) - start pull at obs X
## max(optional) - max # of obs to pull
## segment(optional) - pulls a segment, can be predefined or can define in code



