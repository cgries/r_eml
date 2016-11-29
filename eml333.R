library("EML")
library("rmarkdown")

access <- new("access",
              scope="document",
              order="allowFirst",
              authSystem="knb")
allow1 <- new("allow",
              principal = "uid=NTL,o=LTER,dc=ecoinformatics,dc=org",
              permission = "all")
allow2 <- new("allow",
              principal = "public",
              permission = "read")
access@allow <- new("ListOfallow", c(allow1, allow2))

title <- "LAGOS-NE v.1.054.1 - Lake water quality time series and geophysical data from a 17-state region of the United States"
pubDate <- "2016"
abstract <- as(set_TextType("eml333/abstract.docx"), "abstract")

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
personinfo <- read.csv("eml333/creators.csv", header = TRUE, sep = ",", colClasses = "character")

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
                    keyword = c("Limnology","water quality","lakes","watersheds","freshwater","land use","land cover",
                                "nitrogen", "phosphorus", "chlorophyll", "stoichiometry", "time series"," geographic information systems",
                                "MSB", "Macrosystems Biology", "CSI", "NSF")))

dataset@keywordSet <- new("ListOfkeywordSet", c(keywordSet))

#intellectual Rights
dataset@intellectualRights <- as(set_TextType("eml333/intellectualRights.md"), "intellectualRights")

#add methods
methods <- set_methods("eml333/methods.docx")

dataset@methods <- methods

#add coverage
begindate <- "1990-06-15"
enddate <- "2013-08-15"
geographicDescription <- "A 17-state region of the upper midwest to northeast United States. States included in the database are Minnesota, Iowa, Missouri, Wisconsin, Michigan, Illinois, Indiana, Ohio, Pennsylvania, New York, New Jersey, Connecticut, Massachusetts, Rhode Island, Vermont, New Hampshire, and Maine."
coverage <- set_coverage(begin = begindate, end = enddate,
                         geographicDescription = geographicDescription,
                         west = -96.330, east = -67.152,
                         north = 48.165, south = 36.491)

dataset@coverage <- coverage

#add project and funding

rp_personnel <- as(dc, "personnel")
rp_personnel@userId <- new("ListOfuserId", c(dc_userId))
role <- new("role", "Principal Investigator")
rp_personnel@role <- new("ListOfrole", c(role))

project <- new("project",
                      title = "Collaborative Research: The Effects of Cross-Scale Interactions on Freshwater Ecosystem State Across Space and Time",
                      personnel = rp_personnel,
                      funding = "National Science Foundation EF-1065786 and EF-1065818")

dataset@project <- project

#data table LAGOS_summer_meanvals.csv
df <- read.csv("eml333/LAGOS_summer_meanvals.csv", header=TRUE, sep=",", quote="\"", as.is=TRUE)

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
                         missingValueCode = character(rows),
                         missingValueCodeExplanation = character(rows),
                         stringsAsFactors = FALSE)

#get some metadata from data frame
#add the column names to the template file
attributes$attributeName <- names(df)
#get the data types for each column
attributes$columnClasses <- sapply(df, class)
attributes$minimum <- sapply(df, min, na.rm = TRUE)
attributes$maximum <- sapply(df, max, na.rm = TRUE)
#set what R thinks is integer to numeric
attributes$columnClasses[attributes$columnClasses == "integer"] <- "numeric"
#write the prepared template to a csv file
write.csv(attributes, file = "eml333/summermeanmetadata.csv", row.names = FALSE)

#look at the standard units to get them right
standardUnits <- get_unitList()
View(standardUnits$units)

#read the attributes file back in with all new entries
attributes <- read.csv("eml333/summermeanmetadata.csv", header = TRUE, sep = ",", quote = "\"", as.is = TRUE, na.strings = "")

factors <- read.csv("eml333/summermean_factors.csv", header = TRUE, sep = ",", quote = "\"", as.is = TRUE)

# get the column classes into a vector as required by the set_attribute function
col_classes <- attributes[,"columnClasses"]

#take out that column again
attributes$columnClasses <- NULL

#with the attributes data frames in place we can create the attributeList element - no factors need to be defined for this dataset 
attributeList <- set_attributes(attributes, factors = factors, col_classes = col_classes)

#physical parameter are all set for standard Microsoft csv file
physical <- set_physical("LAGOS_summer_meanvals.csv", numHeaderLines = "1", recordDelimiter = "\\r\\n")

distribution <- new("distribution",
                    online = new("online",
                                 url = "https://lter6.limnology.wisc.edu/sites/default/files/data/LAGOS_summer_meanvals.csv"))
physical@distribution <- new("ListOfdistribution", c(distribution))

#pull to gether information for the dataTable
dataTable1 <- new("dataTable",
                 entityName = "LAGOS_summer_meanvals.csv",
                 entityDescription = "This dataset includes information about total nitrogen (TN) concentrations, total phosphorus (TP) concentrations.",
                 physical = physical,
                 attributeList = attributeList)


#data table LAGOS_supporting_geophysical.csv
df <- read.csv("eml333/LAGOS_supporting_geophysical.csv", header=TRUE, sep=",", quote="\"", as.is = TRUE, na.strings = "NA")

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
                         missingValueCode = character(rows),
                         missingValueCodeExplanation = character(rows),
                         stringsAsFactors = FALSE)

#get some metadata from data frame
#add the column names to the template file
attributes$attributeName <- names(df)
#get the data types for each column
attributes$columnClasses <- sapply(df, class)
attributes$minimum <- sapply(df, min, na.rm = TRUE)
attributes$maximum <- sapply(df, max, na.rm = TRUE)
#set what R thinks is integer to numeric
attributes$columnClasses[attributes$columnClasses == "integer"] <- "numeric"
#write the prepared template to a csv file
write.csv(attributes, file = "eml333/geophysicalmetadata.csv", row.names = FALSE)

#look at the standard units to get them right
#standardUnits <- get_unitList()
#View(standardUnits$units)

#add custom units
#define custom units
unitType <- read.csv("eml333/unit_types.csv", header = TRUE, sep = ",", quote = "\"", as.is = TRUE)
custom_units <- read.csv("eml333/custom_units.csv", header = TRUE, sep = ",", quote = "\"", as.is = TRUE)
unitsList <- set_unitList(custom_units, unitType)

#read the attributes file back in with all new entries
attributes <- read.csv("eml333/geophysicalmetadata.csv", header = TRUE, sep = ",", quote = "\"", as.is = TRUE, na.strings = "")

factors <- read.csv("eml333/geophysical_factors.csv", header = TRUE, sep = ",", quote = "\"", as.is = TRUE)

# get the column classes into a vector as required by the set_attribute function
col_classes <- attributes[,"columnClasses"]

#take out that column again
attributes$columnClasses <- NULL

#with the attributes data frames in place we can create the attributeList element - no factors need to be defined for this dataset 
attributeList <- set_attributes(attributes, factors = factors, col_classes = col_classes)

#physical parameter are all set for standard Microsoft csv file
physical <- set_physical("LAGOS_supporting_geophysical.csv", numHeaderLines = "1", recordDelimiter = "\\r\\n")

distribution <- new("distribution",
                    online = new("online",
                                 url = "https://lter6.limnology.wisc.edu/sites/default/files/data/LAGOS_supporting_geophysical.csv"))
physical@distribution <- new("ListOfdistribution", c(distribution))

#pull to gether information for the dataTable
dataTable2 <- new("dataTable",
                  entityName = "LAGOS_supporting_geophysical.csv",
                  entityDescription = "This dataset includes information geophysical conditions surrounding the lakes.",
                  physical = physical,
                  attributeList = attributeList)

dataset@dataTable <- new("ListOfdataTable", c(dataTable1, dataTable2))

#add to eml element 
eml <- new("eml",
           packageId = "knb-lter-ntl.333.3",
           system = "knb",
           access = access,
           dataset = dataset,
           additionalMetadata = as(unitsList, "additionalMetadata"))

#validate the eml
eml_validate(eml)

#print out the eml xml file
write_eml(eml, "eml333/333.xml")
