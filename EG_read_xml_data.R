##EG - Read an XML File

## Load XML reading package

library(XML)

## Provide link to URL where XML data lives

fileUrlXML <- "https://www.w3schools.com/xml/simple.xml"

## Download file with XML data, list destination path

download.file(fileUrlXML, destfile="./Coursera/JHU Data Science/Course 3 Getting Cleaning Data/XML_EG.xml")

## Run above it will DL file w XML data

## cmd below does 3 things:
## create variable that IS the filepath to the DL'd xml file
## loads xml file into R so contents can be accessed
## tells R to use internal node structure within xml file

xml_eg_path <- xmlTreeParse(file="./Coursera/JHU Data Science/Course 3 Getting Cleaning Data/XML_EG.xml", useInternalNodes = TRUE)

## 'rootnode' is the wrapper/shell/umbrella of the xml file,
## Set rootnode = xml root of variable (xml_eg_path)
## all contents of XML file are contained within the root node
## Set rootnode = xml root, can access ALL contents of XML file

rootNode = xmlRoot(xml_eg_path)

## Tells R to produce the name of the xml root node
## It is 'breakfast menu' b/c XML file is a menu for restaurant

xmlName(rootNode)

## Tells R to list all other elements (parts) of XML file
## Other types of food, b/c it's a fucking menu

names(rootNode)

## Access a specific rootNode, 1-x depending, = elements
## In this case are 5 b/c prior cmd had 'food' listed 5 times
## 4each rootnode, will list everything in said node
## In this case, each menu item, description, price, etc
## Use double brackets [[X]] to view element OF a list, cannot leave blank

rootNode[[1]]

## Subsetting elements
## Lists all components (4) of 1st element, Belgian Waffle

rootNode[[1]]

## To only see specific component (3 = description here)

rootNode[[1]] [[3]]

## Extract parts of xml files, specific elements
## xmlSApply(*node*, *extract what*)
## Below is blunt, will extract ALL elements

xmlSApply(rootNode, xmlValue)

## Get more specific with xmlXPath
## See Word doc for this lecture for more detail
## Below will extract names and prices for menu items

xpathSApply(rootNode, "//name",xmlValue)
xpathSApply(rootNode, "//price", xmlValue)

## NOW READ (SCRAPE) XML DATA FROM AN ACTUAL WEBSITE
## Finally, something fucking useful
## ESPN site for MN Twins
## http://www.espn.com/mlb/team/_/name/min/minnesota-twins
## R Click 'view source' will open window display page code
## Extract specific data from this page - team name, record, ranking

fileUrl <- "http://www.espn.com/mlb/team/_/name/min/minnesota-twins"
doc <- htmlTreeParse(fileUrl, useInternalNodes = TRUE)
team_name <- xpathSApply(doc, "//li[@class='team-name']",xmlValue)
record <- xpathSApply(doc, "//li[@class='record']",xmlValue)
ranking <- xpathSApply(doc, "//li[@class='ranking']",xmlValue)
team_name
record
ranking
