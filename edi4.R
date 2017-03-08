library("EML")
library("rmarkdown")

#change the working directory here for everything
workingdirectory <- "J:/datamanagement/NTLcode/r_eml/edi4"
#change the data file name
datafile <- "water_elev_ice_cover_1993_2015_EDI.csv"

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

title <- "Water level and ice cover at Frank B. Cross Reservoir (Kansas, USA), 1993 - 2015"
pubDate <- "2017"
abstract <- as(set_TextType(paste(workingdirectory,"abstract.docx", sep = "/" )), "abstract")
maintenance <- new ("maintenance",
                    description = "ongoing")

#start the dataset EML
dataset <- new("dataset",
               title = title,
               pubDate = pubDate,
               abstract = abstract,
               maintenance = maintenance)

# helper function to set creators from a data frame
set_creator <- function(personinforow){
  
  individualName <- new("individualName",
                        givenName = personinforow[,"givenName"],
                        surName = personinforow[,"surName"])
  
  creator <- new("creator",
                 individualName = individualName,
                 organizationName = personinforow[,"organizationName"])

    if(nchar(personinforow[,"electronicMailAddress"]) > 0){
      
      email <- new("electronicMailAddress")
      email@.Data <- personinforow[,"electronicMailAddress"]
      creator@electronicMailAddress <- new("ListOfelectronicMailAddress", c(email))
  }
  
  if(nchar(personinforow[,"userId"]) > 0){
    
    userId <- new("userId") 
    userId@directory <- new("xml_attribute", "http://orcid.org")
    userId@.Data <- personinforow[,"userId"]
    creator@userId <- new("ListOfuserId", c(userId))
  }
  
  creator
  
}

#read csv file with person information (givenName, surName, organization,  electronicMailAddress, userId)
personinfo <- read.csv(paste(workingdirectory,"creators.csv", sep = "/" ), header = TRUE, sep = ",", colClasses = "character")

#run each row through the helper function to set creators
dataset@creator <- as(lapply(1:dim(personinfo)[1], function(i)
  set_creator(personinfo[i,])),
  "ListOfcreator")

#add contacts
dc_address <- new("address",
               deliveryPoint = "Kansas Biological Survey",
               city = "Lawrence",
               administrativeArea = "KS")
dataset_contact <- new("contact",
                       new("individualName",
                       givenName = "Dean",
                       surName = "Kettle"),
                       electronicMail = "kettle@ku.edu",
                       address = dc_address,
                       organizationName = "University of Kansas")
# dc_userId <- new("userId") 
# dc_userId@directory <- new("xml_attribute", "http://orcid.org")
# dc_userId@.Data <- ""
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
                    keyword = c("OBFS", "lakes", "hydrology", "lake level", "water level", "watersheds", "ice", "runoff", "freshwater")))

dataset@keywordSet <- new("ListOfkeywordSet", c(keywordSet))

#intellectual Rights
dataset@intellectualRights <- as(set_TextType(paste(workingdirectory,"intellectualRights.md", sep = "/" )), "intellectualRights")

#add methods
methods <- set_methods(paste(workingdirectory,"methods.docx", sep = "/" ))

dataset@methods <- methods

#add coverage
begindate <- "1993-12-01"
enddate <- "2015-12-31"
geographicDescription <- "the University of Kansas Field Station (KUFS) located in eastern Kansas in the prairie-forest ecotone region of the central USA. KUFS and is located just north of Lawrence, Kansas, USA."
coverage <- set_coverage(begin = begindate, end = enddate,
                         geographicDescription = geographicDescription,
                         west = -95.249891, east = -95.134409,
                         north = 39.085468, south = 38.995391)

dataset@coverage <- coverage

#add project and funding

rp_personnel <- new("personnel",
                    new("individualName",
                        givenName = "Dean",
                        surName = "Kettle"),
                    electronicMail = "kettle@ku.edu",
                    organizationName = "University of Kansas",
                    role = "Station Director")

#rp_personnel@userId <- new("ListOfuserId", c(dc_userId))

project <- new("project",
               title = "The University of Kansas Field Station, Kansas, USA",
               personnel = rp_personnel,
               funding = "This work received no specific funding, but rather was supported by general resources at the University of Kansas Field Station.")

dataset@project <- project

# # develop attribute metadata - uncomment if some changes need to be made, but mostly this involves changing the csv file
# this doesn't need to run again, metadata are in file
# df <- read.csv(paste(workingdirectory, datafile, sep = "/" ), header=TRUE, sep=",", quote="\"", as.is=TRUE, na.strings = "NA")
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
# write.csv(attributes, file = paste(workingdirectory,"table_metadata.csv", sep = "/" ), row.names = FALSE)

#look at the standard units to get them right
#standardUnits <- get_unitList()
#View(standardUnits$units)

#read the attributes file back in with all new entries
attributes <- read.csv(paste(workingdirectory,"table_metadata.csv", sep = "/" ), header = TRUE, sep = ",", quote = "\"", as.is = TRUE, na.strings = "")

#factors <- read.csv(paste(workingdirectory,"table_factors.csv", sep = "/" ), header = TRUE, sep = ",", quote = "\"", as.is = TRUE)

# get the column classes into a vector as required by the set_attribute function
col_classes <- attributes[,"columnClasses"]

#take out that column again
attributes$columnClasses <- NULL

#with the attributes data frames in place we can create the attributeList element - no factors need to be defined for this dataset 
attributeList <- set_attributes(attributes, col_classes = col_classes)

#physical parameter for standard Microsoft csv file
physical <- set_physical(datafile, 
                         numHeaderLines = "1", 
                         recordDelimiter = "\\r\\n",
                         url = paste("https://lter.limnology.wisc.edu/sites/default/files/ntl/data",datafile, sep = "/"))


#pull to gether information for the dataTable
dataTable1 <- new("dataTable",
                  entityName = datafile,
                  entityDescription = "This file contains data water level and surface ice cover at Frank B. Cross Reservoir, a small freshwater impoundment in northeastern Kansas (USA).",
                  physical = physical,
                  attributeList = attributeList)


dataset@dataTable <- new("ListOfdataTable", c(dataTable1))

#add to eml element 
eml <- new("eml",
           packageId = "edi.4.2",
           system = "edi",
           access = access,
           dataset = dataset)

#validate the eml
eml_validate(eml)

#print out the eml xml file
write_eml(eml, paste(workingdirectory,"edi4.2.xml", sep = "/" ))

