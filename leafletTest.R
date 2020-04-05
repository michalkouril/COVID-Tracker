library(leaflet)
library(tigris)


test =combined_statistical_areas(cb = T)

leaflet(states(cb = T)) %>% 
  addPolygons(opacity = 0.5, fillColor = "white", color = "gray", weight = 1) %>% 
  addPolygons(data = test, color = "green", weight = 1)

rose_island <- blocks(state = "OH")

leaflet(rose_island) %>%
  addTiles() %>%
  addPolygons()
