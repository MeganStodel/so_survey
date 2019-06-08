library(data.table)
library(leaflet)
library(geojsonio)
library(rgdal)



world <- geojsonio::geojson_read("world.geo.json", what = "sp")

# Survey data

dev_survey_19 <- fread("./SOresults/SO_survey_results_2019.csv")
num_responses <- dev_survey_19[, .(responses = .N), by = Country]
setnames(num_responses, "Country", "name")


world <- merge(world, num_responses, by = "name")


# Create a color palette for the map:

mypalette <- colorNumeric("BuPu", domain = world$names
                          )


# Map creation

labels <- sprintf(
  "<strong>%s</strong><br/>%g responses",
  world$name, world$responses
) %>% lapply(htmltools::HTML)

leaflet(data = world)  %>% 
  addTiles() %>%
  setView( lat=10, lng=0 , zoom=2) %>%
  addPolygons(fillColor = ~mypalette(responses), 
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
  addLegend(pal = mypalette, values = ~responses, opacity = 0.7, title = NULL,
                                                   position = "bottomleft")


