library("EML")
library("dplyr")

#read the existing EML file
workingDirectory <- "J:/datamanagement/LAGOS/Cheruvelil2016"
infilename <- "328.1.xml"
eml <- read_eml(paste(workingDirectory,infilename, sep = "/"))
creators <- eml_get(eml, "creator")
firstNames <- eml_get(creators, "givenName")
lastNames <- eml_get(creators, "surName")

db_conn = read.csv("db_conn.csv", header = TRUE, sep = ",", quote = "\"")
dbconn <- src_mysql(db_conn$db_name, host = db_conn$host, user = db_conn$user, password = db_conn$password)


i <- 1
for (i in 1:length(lastNames)){
  print(firstNames[[i]][[1]][1])
  firstNameValue <- firstNames[[i]][[1]][1]
  lastNameValue <- lastNames[[i]][1]
  
  #query the deims names table for entity ID of that person to get the orcid ID out of the orcidId table
  namesTbl <- tbl(dbconn, "field_data_field_name")
  orcidTbl <- tbl(dbconn, "field_data_field_orcid")
  
  entityIdrow <- as.data.frame(filter(namesTbl, field_name_given == firstNameValue && field_name_family == lastNameValue))
  
  if(nrow(entityIdrow) > 1){
    print("more than one person with this name")
  }else{
    orcidIdrow <- as.data.frame(filter(orcidTbl, entity_id == entityIdrow$entity_id))
    
    #add oricd ID to the eml file
    if(length(orcidIdrow$field_orcid_value) > 0){
      userId <- new("userId") 
      userId@directory <- new("xml_attribute", "http://orcid.org")
      userId@.Data <- orcidIdrow$field_orcid_value
      eml@dataset@creator[[i]]@userId <- new("ListOfuserId", c(userId))
    }
  }
  
  i = i+1
}

#get a new eml element, this is necessary because the R package automatically updates to eml version 2.1.1, but doesn't update the schema location 
emlupdated <- new("eml",
                  packageId = eml@packageId,
                  system = eml@system,
                  access = eml@access,
                  dataset = eml@dataset,
                  additionalMetadata = eml@additionalMetadata)

#validate the eml
eml_validate(emlupdated)

#print out the eml xml file
write_eml(emlupdated, paste(workingDirectory,"editedEML.xml", sep = "/"))
