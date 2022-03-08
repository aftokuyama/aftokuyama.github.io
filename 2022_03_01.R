#load packages
x1 <- c("tidytuesdayR", "tidyverse", "leaflet", "htmlwidgets")
x2 <- x1[!(x1 %in% installed.packages()[,"Package"])]
if(length(x2)) install.packages(x2, repos='http://cran.rstudio.com/')
#update.packages(ask = FALSE, oldPkgs = x1)
lapply(x1, require, character.only=TRUE)

# Get the Data
tuesdata <- tidytuesdayR::tt_load('2022-03-01')

stations <- tuesdata$stations

##FUEL_TYPE_CODE
#BD	Biodiesel (B20 and above)
#CNG	Compressed Natural Gas (CNG)
#ELEC	Electric
#E85	Ethanol (E85)
#HY	Hydrogen
#LNG	Liquefied Natural Gas (LNG)
#LPG	Propane (LPG)

#filter to only public stations geocoded in the US and edit fuel type to be full name (for legend labels)
stations <- stations %>% 
  filter(GROUPS_WITH_ACCESS_CODE == "Public" | 
           GROUPS_WITH_ACCESS_CODE == "Public - Credit card at all times" |
           GROUPS_WITH_ACCESS_CODE == "Public - Credit card after hours" |
           GROUPS_WITH_ACCESS_CODE == "Public - Call ahead" |
           GROUPS_WITH_ACCESS_CODE == "Public - Card key at all times" |
           GROUPS_WITH_ACCESS_CODE == "Public - Card key after hours") %>% 
  #not geocoding in US
  filter(STREET_ADDRESS != "1150 Keystone Park Rd") %>% 
  mutate(FUEL_TYPE_CODE = ifelse(FUEL_TYPE_CODE == "BD", paste("Biodiesel (BD)"),
                                 ifelse(FUEL_TYPE_CODE == "CNG", paste("Compressed Natural Gas (CNG)"),
                                        ifelse(FUEL_TYPE_CODE == "ELEC", paste("Electric (ELEC)"),
                                               ifelse(FUEL_TYPE_CODE == "E85", paste("Ethanol (E85)"),
                                                      ifelse(FUEL_TYPE_CODE == "HY", paste("Hydrogen (HY)"),
                                                             ifelse(FUEL_TYPE_CODE == "LNG", paste("Liquefied Natural Gas (LNG)"),
                                                                    ifelse(FUEL_TYPE_CODE == "LPG", paste("Propane (LPG)"),
                                                                           paste(FUEL_TYPE_CODE)))))))))
#create palette (cb friendly)
pal <- colorFactor(c("#750D37", "#72AC7B", "#7CBBDF",  "#E1BE6A", "#E79F00", "#000000", "#5D3A9B"), 
                   domain = c("Biodiesel (BD)",
                              "Compressed Natural Gas (CNG)",
                              "Electric (ELEC)",
                              "Ethanol (E85)",
                              "Hydrogen (HY)",
                              "Liquefied Natural Gas (LNG)",
                              "Propane (LPG)"))

#create map with points leaflet
m <- leaflet() %>%
  #base
  addTiles() %>%
  #markers with popup (clusters)
  addCircleMarkers(data=stations,
                   lng=~LONGITUDE, lat=~LATITUDE,
                   fillColor =~pal(FUEL_TYPE_CODE), fillOpacity = 0.9, 
                   stroke = FALSE, radius = 10,
                   clusterOptions = markerClusterOptions(),
                   popup=paste("Type:", stations$FUEL_TYPE_CODE, "<br>",
                               "Address:", stations$STREET_ADDRESS, "<br>",
                               "City:", stations$CITY, "<br>",
                               "Hours:", stations$ACCESS_DAYS_TIME)) %>% 
  #legend
  leaflet::addLegend("bottomright", pal=pal, values=c("Biodiesel (BD)",
                                                      "Compressed Natural Gas (CNG)",
                                                      "Electric (ELEC)",
                                                      "Ethanol (E85)",
                                                      "Hydrogen (HY)",
                                                      "Liquefied Natural Gas (LNG)",
                                                      "Propane (LPG)"), title = "Fuel Types")

#export html script
saveWidget(m, file = "m.html")