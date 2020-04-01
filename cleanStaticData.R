#*************************
# ---- Data cleaning ----
#************************
library(openxlsx)
library(dplyr)

#Load the region data and add the full FIPS
fipsData = read.xlsx("metro_fips_codes.xlsx") %>% 
  mutate(FIPS = paste0(FIPS.State.Code, FIPS.County.Code))

#Get population data
popData = read.csv("est2019-allData.csv") %>% 
  #Make sure the fips is a character with leading 0 is needed
  mutate(fips = as.character(fips), fips = ifelse(nchar(fips) < 5, paste0(0, fips), fips))


#Merge
fipsData = fipsData %>% 
  left_join(popData, by = c("FIPS" = "fips"))

#Save
write.csv(fipsData, "fipsData.csv", row.names = F)

