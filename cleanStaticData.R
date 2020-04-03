#*************************
# ---- Data cleaning ----
#************************
library(openxlsx)
library(dplyr)

#Load the region data and add the full FIPS
fipsData = read.xlsx("metro_fips_codes.xlsx") %>% 
  mutate(FIPS = paste0(FIPS.State.Code, FIPS.County.Code), County = `County/County.Equivalent`)
  

#Get population data
popData = read.csv("est2019-allData.csv") %>% 
  #Make sure the fips is a character with leading 0 is needed
  mutate(fips = as.character(fips), fips = ifelse(nchar(fips) < 5, paste0(0, fips), fips))


#Merge
fipsData = fipsData %>% 
  left_join(popData, by = c("FIPS" = "fips"))

# test = fipsData %>% filter(str_detect(County, "Kendall"), State.Name == "Illinois") %>% pull(FIPS)
# paste(test, collapse = ", ")
# fipsData %>% filter(FIPS %in% c("36005", "36047", "36061", "36081", "36085"))
# fipsData %>% filter(FIPS %in% c("29037", "29047", "29095", "29165"))

test =  fipsData %>% filter(FIPS %in% c("29037", "29047", "29095", "29165"))
sum(test$POPESTIMATE2019)

#Manually add NYC
fipsData = rbind(fipsData, 
      list("35620", "35614", "408", "New York City", 
           "Metropolitan Statistical Area", NA, "New York City", 
           "Bronx-Kings-New York-Queens-Richmont", "New York", "36", 
           "000", "Central", "00000", "Bronx-Kings-New York-Queens-Richmont", 8336817))

#Manually add Kansas City
fipsData = rbind(fipsData, 
                 list("28140", NA, "312", "Kansas City", 
                      "Metropolitan Statistical Area", NA, "Kansas City", 
                      "Cass-Clay-Jackson-Platte", "Missouri", "29", 
                      "000", "Central", "00001", "Cass-Clay-Jackson-Platte", 1163157))

#Merge Chicago (check counties to delete)
fipsData[fipsData$FIPS == "17031", "County"] = "All Chicago Counties"

#Remove the merged counties
fipsData = fipsData %>% filter(!FIPS %in%  c("36005", "36047", "36061", "36081", "36085", "29037", "29047", "29095", "29165"))

#Save
write.csv(fipsData, "fipsData.csv", row.names = F)
