library("EML")
library("rmarkdown")

title <- "LAGOS - Lake  nitrogen, phosphorus, and stoichiometry data and geospatial data for lakes in a 17-state region of the U.S."
pubDate <- "2016"
abstract <- as(set_TextType("abstract.docx"), "abstract")

#start the dataset EML
dataset <- new("dataset",
               title = title,
               pubDate = pubDate,
               abstract = abstract)

# helper function to set creators from a data frame
set_creator <- function(personinforow){
  
  individualName <- new("individualName",
                        givenName = personinforow[,"givenName"],
                        surName = personinforow[,"surName"])
  
  creator <- new("creator",
                 individualName = individualName,
                 organizationName = personinforow[,"organizationName"],
                 electronicMailAddress = personinforow[,"electronicMailAddress"])
  
  if(nchar(personinforow[,"userId"]) > 0){
  
    userId <- new("userId") 
    userId@directory <- new("xml_attribute", "http://orcid.org")
    userId@.Data <- personinforow[,"userId"]
    creator@userId <- new("ListOfuserId", c(userId))
  }
  
  creator
  
}

#read csv file with person information (givenName, surName, organization,  electronicMailAddress, userId)
personinfo <- read.csv("creators.csv", header = TRUE, sep = ",", colClasses = "character")

#run each row through the helper function to set creators
dataset@creator <- as(lapply(1:dim(personinfo)[1], function(i)
  set_creator(personinfo[i,])),
  "ListOfcreator")

#add contacts
dc <- as.person("Patricia Soranno <soranno@msu.edu>")
dataset_contact <- as(dc, "contact")
dc_userId <- new("userId") 
dc_userId@directory <- new("xml_attribute", "http://orcid.org")
dc_userId@.Data <- "http://orcid.org/0000-0003-1668-9271"
dataset_contact@userId <- new("ListOfuserId", c(dc_userId))

NTL_address <- new("address",
                   deliveryPoint = "Center for Limnology 680 North Park Str.",
                   city = "Madison",
                   administrativeArea = "WI",
                   postalCode = "53706",
                   country = "USA")

NTL_contact <- new("contact",
                   new("individualName",
                       givenName = "NTL",
                       surName = "Information-Manager"),
                   electronicMail = "infomgr@lter.limnology.wisc.edu",
                   address = NTL_address,
                   organizationName = "North Temperate Lakes LTER",
                   phone = "608-890-3446")

dataset@contact <- new("ListOfcontact", c(dataset_contact, NTL_contact))

#add keywords
keywordSet <- c(new("keywordSet",
    keyword = c("Limnology"," water quality"," lakes"," watersheds"," freshwater"," land use"," land cover",
                " bathymetry"," atmospheric properties"," climate"," geographic information systems",
                "MSB", "Macrosystems Biology", "CSI", "NSF")))

dataset@keywordSet <- new("ListOfkeywordSet", c(keywordSet))

#add methods
methods <- set_methods("methods.docx")

dataset@methods <- methods

#add coverage
begindate <- "2015-03-01"
enddate <- "2016-11-30"
geographicDescription <- "A 17-state region of the upper midwest to northeast United States. States included in the database are Minnesota, Iowa, Missouri, Wisconsin, Michigan, Illinois, Indiana, Ohio, Pennsylvania, New York, New Jersey, Connecticut, Massachusetts, Rhode Island, Vermont, New Hampshire, and Maine."
coverage <- set_coverage(begin = begindate, end = enddate,
  geographicDescription = geographicDescription,
  west = -96.330, east = -67.152,
  north = 48.165, south = 36.491)

dataset@coverage <- coverage

#add project and funding
# main project
project <- new("project",
               title = "NSF Postdoctoral Fellowship in Biology FY 2014",
               funding = "National Science Foundation DBI-1401954")
pp <- as.person("Sarah Collins <sarahmcollins@gmail.com>")
personnel <- as(pp, "personnel")
pp_userId <- new("userId") 
pp_userId@directory <- new("xml_attribute", "http://orcid.org")
pp_userId@.Data <- " http://orcid.org/0000-0001-5503-7386"
personnel@userId <- new("ListOfuserId", c(pp_userId))
role <- new("role", "Principal Investigator")
personnel@role <- new("ListOfrole", c(role))
project@personnel <- new("ListOfpersonnel", c(personnel))

#related project
rp_personnel <- as(dc, "personnel")
rp_personnel@userId <- new("ListOfuserId", c(dc_userId))
rp_personnel@role <- new("ListOfrole", c(role))

relatedProject <- new("relatedProject",
                      title = "Collaborative Research: The Effects of Cross-Scale Interactions on Freshwater Ecosystem State Across Space and Time",
                      personnel = rp_personnel,
                      funding = "National Science Foundation EF-1065786 and EF-1065818")
project@relatedProject <- new("ListOfrelatedProject", c(relatedProject))

dataset@project <- project

#data table
df <- read.csv("LAGOS_stoichiometry_forarchive.csv", header=TRUE, sep=",", quote="\"", as.is=TRUE)

#set up the attribute metadata csv file
rows <- ncol(df)

attributes <- data.frame(attributeName = character(rows),
  formatString = character(rows),
  unit = character(rows),
  numberType = character(rows),
  definition = character(rows),
  attributeDefinition = character(rows),
  columnClasses = character(rows),
  minimum = character(rows),
  maximum = character(rows),
  stringsAsFactors = FALSE)

#get some metadata from data frame
#add the column names to the template file
attributes$attributeName <- names(df)
#get the data types for each column
attributes$columnClasses <- sapply(df, class)
attributes$minimum <- sapply(df, min)
attributes$maximum <- sapply(df, max)
#set what R thinks is integer to numeric
attributes$columnClasses[attributes$columnClasses == "integer"] <- "numeric"
#write the prepared template to a csv file
write.csv(attributes, file = "collinsSarahmetadata.csv", row.names = FALSE)

#look at the standard units to get them right
#standardUnits <- get_unitList()
#View(standardUnits$units)

#read the attributes file back in with all new entries
attributes <- read.csv("collinsSarahmetadata.csv", header = TRUE, sep = ",", quote = "\"", as.is = TRUE)

# get the column classes into a vector as required by the set_attribute function
col_classes <- attributes[,"columnClasses"]

#take out that column again
attributes$columnClasses <- NULL

#with the attributes data frames in place we can create the attributeList element - no factors need to be defined for this dataset 
attributeList <- set_attributes(attributes, col_classes = col_classes)

#physical parameter are all set for standard Microsoft csv file
physical <- set_physical("LAGOS_stoichiometry_forarchive.csv", numHeaderLines = "1", recordDelimiter = "\\r\\n")

distribution <- new("distribution",
                    online = new("online",
                             url = "https://lter6.limnology.wisc.edu/sites/default/files/data/LAGOS_stoichiometry_forarchive.csv"))
physical@distribution <- new("ListOfdistribution", c(distribution))

#pull to gether information for the dataTable
dataTable <- new("dataTable",
                 entityName = "LAGOS_stoichiometry_forarchive.csv",
                 entityDescription = "This dataset includes information about total nitrogen (TN) concentrations, total phosphorus (TP) concentrations, TN:TP stoichiometry, and 12 driver variables that might predict nutrient concentrations and ratios.",
                 physical = physical,
                 attributeList = attributeList)

dataset@dataTable <- new("ListOfdataTable", c(dataTable))

#add to eml element 
eml <- new("eml",
           packageId = "knb-lter-ntl.332.2",
           system = "knb",
           dataset = dataset)

#validate the eml
eml_validate(eml)

#print out the eml xml file
write_eml(eml, "332.xml")
