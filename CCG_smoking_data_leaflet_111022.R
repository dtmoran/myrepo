library(leaflet)
library(sf)
library(tidyverse)
library(htmltools)
library(rmapshaper)


data <- readRDS("C:/R_Projects/myrepo/data/smoking/data.RDS")   #load RDS file
data["Measure"][data["Measure"] == "Smokers "] <- "Smokers"     #clean data

output_ccg <- data %>%                  
    dplyr::filter(grepl('CCG', OrgName)) %>%
    filter(FinancialYear=="2020/2021", Quarter=="1") %>%
    mutate(Value=as.integer(Value)) %>%
    group_by(FinancialYear,Quarter, Measure, OrgName) %>%
    summarise(Value=sum(Value)) %>%
    pivot_wider(names_from = Measure, values_from = Value) %>%
    mutate(percentage=`Smokers`/`Maternities`) %>%
    mutate(YearQTR=paste(FinancialYear,Quarter)) %>%
    mutate(percent_multiplied=percentage *100) %>%
    mutate(percent_multiplied_round= round(percent_multiplied, 1))



#read in file and simplify(rmapshaper)

library(rgdal)
#library(tictoc)
CCG <- readOGR("Clinical_Commissioning_Groups_(April_2020)_EN_BFC_V2.geojson")


library(rmapshaper)
ms_simplify(CCG, keep = 0.001,keep_shapes = FALSE)
readr::write_rds(CCG,"shapefile.RDS")
#tic()
CCG <- readRDS("shapefile.RDS")
#toc()


#join on CCG_name

temp <- CCG %>% as.data.frame() %>%  
    left_join(output_ccg %>% ungroup() %>% select(Maternities, percent_multiplied_round, ccg20nm=OrgName), by="ccg20nm")

CCG@data <- temp





#add hover tooltip

pal <- colorBin("plasma", 
                bins = 5, 
                na.color = "#808080", 
                domain = CCG$percent_multiplied_round)


leaflet(CCG) %>%
    addProviderTiles(provider="CartoDB.Positron") %>%
    addPolygons(weight = 2, fill=0.3, 
                color = "#228096", 
                label = ~lapply(paste0("Name: ",CCG$ccg20nm, 
                                       #"<br>Maternities: " CCG$Maternities,
                                       "<br>Percentage: ", CCG$percent_multiplied_round),
                                htmltools::HTML),
                fillColor = ~pal(CCG$percent_multiplied_round), 
                layerId = ~CCG$ccg20cd) %>%
       addLegend("bottomright", pal = pal, values = CCG$percent_multiplied_round,
              title = "Women known to be smokers at time of delivery",
              labFormat = labelFormat(suffix = "%"),
              opacity = 1
    )



        
    










#older versions (not working)
#create map and add colour


leaflet(CCG) %>%
    addTiles() %>%
    addPolygons(weight = 2, fillOpacity=~percent_multiplied/22, color = "#228096")



#add popup
leaflet(CCG) %>%
    addTiles() %>%
    addPolygons(weight = 2, fillOpacity=~percent_multiplied/22, 
                color = "#228096") %>%
    addMarkers(
        lng = ~long, lat = ~lat,
        popup = ~ccg20nm)




#addMarkers

leaflet(CCG) %>%
    addTiles() %>%
    addPolygons(weight = 2, fillOpacity=~percent_multiplied/22, 
                color = "#228096") %>%
    addMarkers(lng = ~long, lat = ~lat, 
               label = ~c(ccg20nm, percent_multiplied))




#label list
tooltip <- c(~ccg20nm, ~percent_multiplied) %>%
    gsub(",", ",<br>", .)

leaflet(CCG) %>%
    addTiles() %>%
    addPolygons(weight = 2, fillOpacity=~percent_multiplied/22, 
                color = "#228096") %>%
    addMarkers(
        lng = ~long, lat = ~lat,
        label = tooltip)





