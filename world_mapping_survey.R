library(data.table)
library(leaflet)
library(feather)
library(rgdal)
library(maps)
library(maptools)



world <- geojsonio::geojson_read("world.geo.json", what = "sp")

world <- map("world", fill = TRUE, plot = FALSE)
world_map <- map2SpatialPolygons(world, sub(":.*$", "", world$names))
world_map <- SpatialPolygonsDataFrame(world_map,
                                      data.frame(country=names(world_map), 
                                                 stringsAsFactors=FALSE), 
                                      FALSE)

# Survey data




# Create a color palette for the map:

mypalette <- colorNumeric("BuPu", domain = world$names
                          )


# Map creation

labels <- sprintf(
  "<strong>%s</strong><br/>%g per cent women",
  world$country, world$proportion_women
) %>% lapply(htmltools::HTML)

leaflet(data = world)  %>% 
  addTiles() %>%
  setView( lat=10, lng=0 , zoom=2) %>%
  addPolygons(fillColor = ~mypalette(proportion_women), 
              weight = 2,
              opacity = 1,
              color = "white",
              dashArray = "3",
              fillOpacity = 0.7, 
              highlight = highlightOptions(
                weight = 3,
                color = "#666",
                dashArray = "",
                fillOpacity = 0.7,
                bringToFront = TRUE),
              label = labels,
              labelOptions = labelOptions(
                style = list("font-weight" = "normal", padding = "3px 8px"),
                textsize = "15px",
                direction = "auto")) %>%
  addLegend(pal = mypalette, values = ~proportion_women, opacity = 0.7, title = NULL,
                                                   position = "bottomleft")


