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

title <- "Long-term studies of secondary succession and community assembly in the prairie-forest ecotone of eastern Kansas, Old-field succession experiment"
pubDate <- "2016"
abstract <- as(set_TextType("emlobfs1/E1Abstract.docx"), "abstract")

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
personinfo <- read.csv("emlobfs1/creators.csv", header = TRUE, sep = ",", colClasses = "character")

#run each row through the helper function to set creators
dataset@creator <- as(lapply(1:dim(personinfo)[1], function(i)
  set_creator(personinfo[i,])),
  "ListOfcreator")

#add contacts
dc <- as.person("Bryan Foster <bfoster@ku.edu>")
dataset_contact <- as(dc, "contact")
# dc_userId <- new("userId") 
# dc_userId@directory <- new("xml_attribute", "http://orcid.org")
# dc_userId@.Data <- "http://orcid.org/0000-0002-1210-059X"
# dataset_contact@userId <- new("ListOfuserId", c(dc_userId))

# NTL_address <- new("address",
#                    deliveryPoint = "Center for Limnology 680 North Park Str.",
#                    city = "Madison",
#                    administrativeArea = "WI",
#                    postalCode = "53706",
#                    country = "USA")
# 
# NTL_contact <- new("contact",
#                    new("individualName",
#                        givenName = "NTL",
#                        surName = "Information-Manager"),
#                    electronicMail = "infomgr@lter.limnology.wisc.edu",
#                    address = NTL_address,
#                    organizationName = "North Temperate Lakes LTER",
#                    phone = "608-890-3446")

dataset@contact <- new("ListOfcontact", c(dataset_contact)) #, NTL_contact

#add keywords
keywordSet <- c(new("keywordSet",
                    keyword = c("LTREB", "NSF", "plant community", "succession", "fertilization")))

dataset@keywordSet <- new("ListOfkeywordSet", c(keywordSet))

#intellectual Rights
dataset@intellectualRights <- as(set_TextType("emlobfs1/intellectualRights.md"), "intellectualRights")

#add methods
methods <- set_methods("emlobfs1/E1Methods.docx")

dataset@methods <- methods

#add coverage
begindate <- "2001-07-01"
enddate <- "2015-07-31"
geographicDescription <- "the University of Kansas Field Station (KUFS) located in eastern Kansas in the prairie-forest ecotone region of the central USA. KUFS and is located just north of Lawrence, Kansas, USA."
coverage <- set_coverage(begin = begindate, end = enddate,
                         geographicDescription = geographicDescription,
                         west = -95.249891, east = -95.134409,
                         north = 39.085468, south = 38.995391)

dataset@coverage <- coverage

#add project and funding

rp_personnel <- as(dc, "personnel")
#rp_personnel@userId <- new("ListOfuserId", c(dc_userId))
role <- new("role", "Principal Investigator")
rp_personnel@role <- new("ListOfrole", c(role))

project <- new("project",
               title = "Long-term studies of secondary succession and community assembly in the prairie-forest ecotone of eastern Kansas",
               personnel = rp_personnel,
               funding = "National Science Foundation LTREB 0950100")

dataset@project <- project

# develop attribute metadata - uncomment if some changes need to be made, but mostly this involves changing the csv file
# df <- read.csv("emlobfs1/E1_Plant_Biomass_6_16.csv", header=TRUE, sep=",", quote="\"", as.is=TRUE, na.strings = "NA")
# 
# #set up the attribute metadata csv file
# rows <- ncol(df)
# 
# attributes <- data.frame(attributeName = character(rows),
#                          formatString = character(rows),
#                          unit = character(rows),
#                          numberType = character(rows),
#                          definition = character(rows),
#                          attributeDefinition = character(rows),
#                          columnClasses = character(rows),
#                          minimum = character(rows),
#                          maximum = character(rows),
#                          missingValueCode = character(rows),
#                          missingValueCodeExplanation = character(rows),
#                          stringsAsFactors = FALSE)
# 
# #get some metadata from data frame
# #add the column names to the template file
# attributes$attributeName <- names(df)
# #get the data types for each column
# attributes$columnClasses <- sapply(df, class)
# attributes$minimum <- sapply(df, min, na.rm = TRUE)
# attributes$maximum <- sapply(df, max, na.rm = TRUE)
# #set what R thinks is integer to numeric
# attributes$columnClasses[attributes$columnClasses == "integer"] <- "numeric"
# #write the prepared template to a csv file
# write.csv(attributes, file = "emlobfs1/E1_1metadata.csv", row.names = FALSE)

#look at the standard units to get them right
#standardUnits <- get_unitList()
#View(standardUnits$units)

#read the attributes file back in with all new entries
# !!!I accidentally overwrote this file - just edit the eml file from now on!!!
#!!!attributes <- read.csv("emlobfs1/E1_1metadata.csv", header = TRUE, sep = ",", quote = "\"", as.is = TRUE, na.strings = "")

factors <- read.csv("emlobfs1/E1_1factors.csv", header = TRUE, sep = ",", quote = "\"", as.is = TRUE)

# get the column classes into a vector as required by the set_attribute function
col_classes <- attributes[,"columnClasses"]

#take out that column again
attributes$columnClasses <- NULL

#with the attributes data frames in place we can create the attributeList element - no factors need to be defined for this dataset 
attributeList <- set_attributes(attributes, factors = factors, col_classes = col_classes)

#physical parameter for standard Microsoft csv file
physical <- set_physical("E1 Plant Biomass 6 16.csv", 
                         numHeaderLines = "1", 
                         recordDelimiter = "\\r\\n",
                         url = "https://foster.ku.edu/sites/foster.ku.edu/files/files/E1%20Plant%20Biomass%206%2016.csv")


#pull to gether information for the dataTable
dataTable1 <- new("dataTable",
                  entityName = "E1 Plant Biomass 6 16.csv",
                  entityDescription = "This file contains data on plant community biomass (live and litter,expressed in gramsPerMeterSquared) harvested from each of the 96 experimental plots (2001-2015).",
                  physical = physical,
                  attributeList = attributeList)


# # develop attribute metadata - uncomment if some changes need to be made, but mostly this involves changing the csv file
# df <- read.csv("emlobfs1/E1_Plant_Species_composition_6_16_long.csv", header=TRUE, sep=",", quote="\"", as.is = TRUE, na.strings = "NA")
# 
# #set up the attribute metadata csv file
# rows <- ncol(df)
# 
# attributes <- data.frame(attributeName = character(rows),
#                          formatString = character(rows),
#                          unit = character(rows),
#                          numberType = character(rows),
#                          definition = character(rows),
#                          attributeDefinition = character(rows),
#                          columnClasses = character(rows),
#                          minimum = character(rows),
#                          maximum = character(rows),
#                          missingValueCode = character(rows),
#                          missingValueCodeExplanation = character(rows),
#                          stringsAsFactors = FALSE)
# 
# #get some metadata from data frame
# #add the column names to the template file
# attributes$attributeName <- names(df)
# #get the data types for each column
# attributes$columnClasses <- sapply(df, class)
# attributes$minimum <- sapply(df, min, na.rm = TRUE)
# attributes$maximum <- sapply(df, max, na.rm = TRUE)
# #set what R thinks is integer to numeric
# attributes$columnClasses[attributes$columnClasses == "integer"] <- "numeric"
# #write the prepared template to a csv file
# write.csv(attributes, file = "emlobfs1/E1_2metadata.csv", row.names = FALSE)

#read the attributes file back in with all new entries
attributes <- read.csv("emlobfs1/E1_2metadata.csv", header = TRUE, sep = ",", quote = "\"", as.is = TRUE, na.strings = "")

factors <- read.csv("emlobfs1/E1_2factors.csv", header = TRUE, sep = ",", quote = "\"", as.is = TRUE)

# get the column classes into a vector as required by the set_attribute function
col_classes <- attributes[,"columnClasses"]

#take out that column again
attributes$columnClasses <- NULL

#with the attributes data frames in place we can create the attributeList element - no factors need to be defined for this dataset 
attributeList <- set_attributes(attributes, factors = factors, col_classes = col_classes)

#physical parameter are all set for standard Microsoft csv file
physical <- set_physical("E1_Plant_Species_composition_6_16_long.csv", 
                         numHeaderLines = "1", 
                         recordDelimiter = "\\r\\n",
                         url = "https://lter.limnology.wisc.edu/sites/default/files/data/E1_Plant_Species_composition_6_16_long.csv")

#pull to gether information for the dataTable
dataTable2 <- new("dataTable",
                  entityName = "E1_Plant_Species_composition_6_16_long.csv",
                  entityDescription = "This file contains data on plant species composition (abundance of each component species expressed in % cover) in each of the 96 experimental 4 x 5 m plots (2002-2015).",
                  physical = physical,
                  attributeList = attributeList)

dataset@dataTable <- new("ListOfdataTable", c(dataTable1, dataTable2))

#add to eml element 
eml <- new("eml",
           packageId = "edi.1.2",
           system = "edi",
           access = access,
           dataset = dataset)

#validate the eml
eml_validate(eml)

#print out the eml xml file
write_eml(eml, "emlobfs1/edi1.xml")
