
#install.packages("xlsx")
#install.packages("rio")
#install.packages("tidyverse")
#install.packages("readxl")
#install.packages("dplyr")
#install.packages("leaflet")
#install.packages("htmltools")
library(xlsx)
library(rio)
library(tidyverse)
library(readxl)
library(leaflet)
library(dplyr)
tw <- read.xlsx('work.xlsx', sheetIndex = 1, encoding = 'UTF-8')
tw


#summary()則給出每個欄位的「最大值」、「最小值」、「平均值」、「中位數」、「第一四分位數」…等等。
summary(tw)
#無離缺值
any(is.na(tw))
#各欄位名稱
names(tw)
#筆欄位數
dim(tw)
#str()列出資料內每個欄位的狀態
str(tw)

nrow(tw)
tw_ok <- tw[(!is.na(tw$經度))|(!is.na(tw$緯度)),]
nrow(tw_ok)
library(leaflet)
names(tw_ok)
tw_map <- leaflet(tw_ok,options=leafletOptions(minZoom = 5,maxZoom = 15))
tw_map <- addTiles(tw_map)
tw_map <- addMarkers(tw_map,lng = ~經度, lat = ~緯度,
                     popup = ~等級, label = ~公廁類別)



#02
myicon <- icons(iconUrl = ifelse(tw_ok$公廁類別 == "女廁所", "WC02.jpg",
                                 ifelse(tw_ok$公廁類別 == "男廁所", "WC01.jpg",
                                        ifelse(tw_ok$公廁類別 == "混合廁所","WC03.jpg","WC04.png"))), iconWidth = 20, iconHeight = 20)
tw_map2 <- leaflet(tw_ok, options = leafletOptions(minZoom = 5,maxZoom = 20))

tw_map2 <- addTiles(tw_map2)
tw_map2 <- addMarkers(tw_map2, lng = ~經度, lat = ~緯度,
                      popup = ~等級, label = ~公廁類別, icon = myicon,
                      clusterOptions = markerClusterOptions()) 
tw_map2

#03
interactive_map <- leaflet(tw_map2, options = leafletOptions(minZoom = 5,
                                                             maxZoom = 20))
interactive_map <- addTiles(interactive_map, group = "OSM")
interactive_map <- addProviderTiles(interactive_map, providers$Esri.WorldPhysical, group = "Esri.WorldPhysical")
interactive_map <- addProviderTiles(interactive_map, providers$MtbMap, group =
                                      "MtbMap")
interactive_map <- addProviderTiles(interactive_map, providers$OpenTopoMap, group =
                                      "OpenTopoMap")
interactive_map <- addProviderTiles(interactive_map, providers$HikeBike, group =
                                      "HikeBike")
interactive_map <- addProviderTiles(interactive_map, providers$CartoDB, group =
                                      "CartoDB")
interactive_map <- addLayersControl(interactive_map, baseGroups = c("OSM", "Esri.WorldPhysical",
                                                                    "MtbMap", "OpenTopoMap", "HikeBike", "CartoDB"), overlayGroups = c("女廁所
", "男廁所", "混合廁所", "無障礙廁所"), options =layersControlOptions(collapsed = TRUE))
interactive_map
#_六
icon_1 <- makeIcon("WC01.jpg", iconWidth = 25, iconHeight = 25)
icon_2 <- makeIcon("WC02.jpg", iconWidth = 25, iconHeight = 25)
icon_3 <- makeIcon("WC03.jpg", iconWidth = 25, iconHeight = 25)
icon_4 <- makeIcon("WC04.jpg", iconWidth = 25, iconHeight = 25)
interactive_map <- addMarkers(interactive_map, lng =
                                tw_ok[tw_ok$公廁類別 =="男廁所",4], lat =
                                tw_ok[tw_ok$公廁類別 == "男廁所",3], popup =
                                tw_ok[tw_ok$公廁類別 == "男廁所",7], popupOptions = popupOptions(maxWidth = 300, minWidth = 50, keepInView = TRUE, closeButton =
                                                                                            TRUE), label = tw_ok[tw_ok$公廁類別 == "男廁所",
                                                                                                                 5], icon = icon_1, group = "男廁所", clusterOptions = markerClusterOptions())
interactive_map
interactive_map <- addMarkers(interactive_map, lng =
                                tw_ok[tw_ok$公廁類別 == "女廁所", 4], lat =
                                tw_ok[tw_ok$公廁類別 == "女廁所", 3], popup =
                                tw_ok[tw_ok$公廁類別 == "女廁所", 7], popupOptions =
                                popupOptions(maxWidth = 300, minWidth = 50, keepInView = TRUE, closeButton =
                                               TRUE), label = tw_ok[tw_ok$公廁類別 == "女廁所", 5], 
                              icon = icon_2, group = "女廁所", clusterOptions = markerClusterOptions())
interactive_map
interactive_map <- addMarkers(interactive_map, lng =
                                tw_ok[tw_ok$公廁類別 == "混合廁所", 4], lat =
                                tw_ok[tw_ok$公廁類別 == "混合廁所", 3], popup =
                                tw_ok[tw_ok$公廁類別 == "混合廁所", 7], popupOptions =
                                popupOptions(maxWidth = 300, minWidth = 50, keepInView = TRUE, closeButton =
                                               TRUE), label = tw_ok[tw_ok$公廁類別 == "混合廁所", 5], icon =
                                icon_3, group = "混合廁所", clusterOptions = markerClusterOptions())
interactive_map
interactive_map <- addMarkers(interactive_map, lng =
                                tw_ok[tw_ok$公廁類別 == "無障礙廁所", 4], lat =
                                tw_ok[tw_ok$公廁類別 == "無障礙廁所", 3], popup =
                                tw_ok[tw_ok$公廁類別 == "無障礙廁所", 7], popupOptions =
                                popupOptions(maxWidth = 300, minWidth = 50, keepInView = TRUE, closeButton =
                                               TRUE), label = tw_ok[tw_ok$公廁類別 == "無障礙廁所", 5], icon =
                                icon_4, group = "無障礙廁所", clusterOptions = markerClusterOptions())
interactive_map
# Layers control
interactive_map <- addLayersControl(interactive_map, baseGroups = c("OSM", "Esri.WorldPhysical",
                                                                    "MtbMap", "OpenTopoMap", "HikeBike", "CartoDB"), overlayGroups = c("女廁所","男廁所", "混合廁所", "無障礙廁所"), options =layersControlOptions(collapsed = TRUE))
interactive_map

#_________________________________用網路的圖

icon_5<- awesomeIcons(icon = "man",iconColor = "blue",library = "ion",
                      markerColor = "lightblue")
icon_6 <- awesomeIcons(icon = "woman",iconColor = "red",library = "ion",
                       markerColor = "lightblue")
icon_7 <- awesomeIcons(icon = "person",iconColor = "green",library = "ion",
                       markerColor = "lightblue")
icon_8 <- awesomeIcons(icon = "leaf", iconColor = "red", library = "ion",
                       markerColor = "lightblue")

#______________________網路
interactive_map <- leaflet(tw_ok, width = 800, height = 450, options = leafletOptions(minZoom = 5, maxZoom = 20))
interactive_map <- addTiles(interactive_map, group = "OSM", attribution = "TR ZHAN \uA9 2020")
interactive_map <- addProviderTiles(interactive_map, providers$Esri.WorldPhysical, group = "Esri.WorldPhysical")
interactive_map <- addProviderTiles(interactive_map, providers$MtbMap, group = "MtbMap")
interactive_map <- addProviderTiles(interactive_map, providers$OpenTopoMap, group = "OpenTopoMap")
interactive_map <- addProviderTiles(interactive_map, providers$HikeBike, group = "HikeBike")
interactive_map <- addProviderTiles(interactive_map, providers$CartoDB, group = "CartoDB")
interactive_map

interactive_map <- addAwesomeMarkers(interactive_map, lng = tw_ok[tw_ok$公廁類別 =="男廁所",4],
                                     lat = tw_ok[tw_ok$公廁類別 =="男廁所",3],
                                     popup = tw_ok[tw_ok$公廁類別 =="男廁所",7],
                                     popupOptions = popupOptions(maxWidth = 300, minWidth = 50, keepInView = TRUE, closeButton = TRUE),
                                     label = tw_ok[tw_ok$公廁類別 =="男廁所",5],
                                     icon = icon_5, group = "男廁所",
                                     clusterOptions = markerClusterOptions())

interactive_map <- addAwesomeMarkers(interactive_map, lng = tw_ok[tw_ok$公廁類別 == "女廁所", 4],
                                     lat = tw_ok[tw_ok$公廁類別 == "女廁所", 3],
                                     popup = tw_ok[tw_ok$公廁類別 == "女廁所", 7],
                                     popupOptions = popupOptions(maxWidth = 300, minWidth = 50, keepInView = TRUE, closeButton = TRUE),
                                     label = tw_ok[tw_ok$公廁類別 == "女廁所", 5],
                                     icon = icon_6, group = "女廁所",
                                     clusterOptions = markerClusterOptions())

interactive_map <- addAwesomeMarkers(interactive_map, lng = tw_ok[tw_ok$公廁類別 == "混合廁所", 4],
                                     lat = tw_ok[tw_ok$公廁類別 == "混合廁所", 3],
                                     popup = tw_ok[tw_ok$公廁類別 == "混合廁所", 7],
                                     popupOptions = popupOptions(maxWidth = 300, minWidth = 50, keepInView = TRUE, closeButton = TRUE),
                                     label = tw_ok[tw_ok$公廁類別 == "混合廁所", 5],
                                     icon = icon_7, group = "混合廁所",
                                     clusterOptions = markerClusterOptions())

interactive_map <- addAwesomeMarkers(interactive_map, lng = tw_ok[tw_ok$公廁類別 == "無障礙廁所", 4],
                                     lat = tw_ok[tw_ok$公廁類別 == "無障礙廁所", 3],
                                     popup = tw_ok[tw_ok$公廁類別 == "無障礙廁所", 7],
                                     popupOptions = popupOptions(maxWidth = 300, minWidth = 50, keepInView = TRUE, closeButton = TRUE),
                                     label = tw_ok[tw_ok$公廁類別 == "無障礙廁所", 5],
                                     icon = icon_8, group = "無障礙廁所",
                                     clusterOptions = markerClusterOptions())

#Layers contrlo

interactive_map <- addLayersControl(interactive_map,
                                    baseGroups = c("OSM", "Esri.WorldPhysical", "MtbMap", "OpenTopoMap", "HikeBike", "CartoDB"),
                                    overlayGroups = c("男廁所", "女廁所", "混合廁所", "無障礙廁所"),
                                    options = layersControlOptions(collapsed = FALSE))
interactive_map

library(htmltools)
save_html(interactive_map, file = "tw_WC.html")



