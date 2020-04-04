#*************************
# ---- Data cleaning ----
#************************
library(openxlsx)
library(dplyr)
library(tigris)


#Load the region data and add the full FIPS
fipsData = read.xlsx("metro_fips_codes.xlsx") %>% 
  mutate(FIPS = paste0(FIPS.State.Code, FIPS.County.Code), County = `County/County.Equivalent`) %>% 
  left_join(fips_codes %>% mutate(fips = paste0(state_code, county_code)) %>% 
              select(fips, State = state), by = c("FIPS" = "fips"))
  

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
#c("17031", "17043", "17063", "17111", "17197", "17037", "17089", "17093", "18073", "18089", "18111", "18127", "17097", "55059")

test =  fipsData %>% filter(FIPS %in% c("29037", "29047", "29095", "29165"))

#Remove the counties we'll merge
fipsData = fipsData %>% filter(!FIPS %in%  
                                 c("36005", "36047", "36061", "36081", "36085", "29037", 
                                   "29047", "29095", "29165", "17043", "17063", 
                                   "17111", "17197", "17037", "17089", "17093", "18073", 
                                   "18089", "18111", "18127", "17097", "55059"))

#Manually add NYC
fipsData = rbind(fipsData, 
      list("35620", "35614", "408", "New York City", 
           "Metropolitan Statistical Area", NA, "New York-Newark, NY-NJ-CT-PA", 
           "Bronx-Kings-New York-Queens-Richmont", "New York", "36", 
           "000", "Central", "00000", "Bronx-Kings-New York-Queens-Richmont", "NY", 8336817))

#Manually add Kansas City
fipsData = rbind(fipsData, 
                 list("28140", NA, "312", "Kansas City", 
                      "Metropolitan Statistical Area", NA, "Kansas City-Overland Park-Kansas City, MO-KS", 
                      "Cass-Clay-Jackson-Platte", "Missouri", "29", 
                      "000", "Central", "00001", "Cass-Clay-Jackson-Platte", "MO", 1163157))


#Edit Chicago
fipsData[fipsData$FIPS == "17031", "County"] = "Cook-DuPage-Grundy-McHenry-Will-DeKalb-Kane-Kendall-Jasper-Lake-Newton-Porter-Lake-Kenosha"
fipsData[fipsData$FIPS == "17031", "POPESTIMATE2019"] = 9458539



#Save
write.csv(fipsData, "fipsData.csv", row.names = F)
