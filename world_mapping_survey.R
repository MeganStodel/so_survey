library(data.table)
library(leaflet)
library(feather)
library(rgdal)
library(maps)
library(maptools)

# World map file

world <- map("world", fill = TRUE, plot = FALSE)
world <- map2SpatialPolygons(world, sub(":.*$", "", world$names))
world <- SpatialPolygonsDataFrame(world, data.table(country = names(world)), FALSE)

# Survey data

dev_survey_19 <- as.data.table(read_feather("dev_survey_19.feather"))

# Create a color palette for the map:

mypalette <- colorNumeric("BuPu", domain = world$names)

# Proportion respondents who are women 

women_by_country <- dev_survey_19[Gender == "Woman", .(total_women = .N), by = country]
total_gender_by_country <- dev_survey_19[!is.na(Gender), .(total_people = .N), by = country]
women_by_country <- merge(women_by_country, total_gender_by_country, by = "country", all = TRUE)
women_by_country[is.na(total_women), total_women := 0]  
women_by_country[, proportion_women := total_women/total_people*100]
women_by_country[total_people < 100, proportion_women := NA]
women_by_country <- women_by_country[, .(country, proportion_women)]
women_by_country[, prop_women_char := as.character(round(proportion_women, 1))]
world <- merge(world, women_by_country)

# Map creation for women data

labels <- sprintf(
  "<strong>%s</strong><br/>%s per cent women",
  world$country, world$prop_women_char
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


# Proportion data scientists -------------------------------------

ds_by_country <- dev_survey_19[DevType %like% "Data scien", .(total_ds = .N), by = country]
total_dev_by_country <- dev_survey_19[!is.na(DevType), .(total_people = .N), by = country]
ds_by_country <- merge(ds_by_country, total_dev_by_country, by = "country", all = TRUE)
ds_by_country[is.na(total_ds), total_ds := 0]  
ds_by_country[, proportion_ds := total_ds/total_people*100]
ds_by_country[total_people < 100, proportion_ds := NA]
ds_by_country <- ds_by_country[, .(country, proportion_ds)]
ds_by_country[, prop_ds_char := as.character(round(proportion_ds, 1))]
world <- merge(world, ds_by_country)

# Map creation for ds data

labels_ds <- sprintf(
  "<strong>%s</strong><br/>%s per cent data scientists",
  world$country, world$prop_ds_char
) %>% lapply(htmltools::HTML)

leaflet(data = world)  %>% 
  addTiles() %>%
  setView( lat=10, lng=0 , zoom=2) %>%
  addPolygons(fillColor = ~mypalette(proportion_ds), 
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
              label = labels_ds,
              labelOptions = labelOptions(
                style = list("font-weight" = "normal", padding = "3px 8px"),
                textsize = "15px",
                direction = "auto")) %>%
  addLegend(pal = mypalette, values = ~proportion_ds, opacity = 0.7, title = NULL,
            position = "bottomleft")


# Proportion humanities / arts ----------------------------

humarts_by_country <- dev_survey_19[UndergradMajor %like% "humanities|arts", .(total_humarts = .N), by = country]
total_degree_by_country <- dev_survey_19[!is.na(UndergradMajor), .(total_people = .N), by = country]
humarts_by_country <- merge(humarts_by_country, total_degree_by_country, by = "country", all = TRUE)
humarts_by_country[is.na(total_humarts), total_humarts := 0]  
humarts_by_country[, proportion_humarts := total_humarts/total_people*100]
humarts_by_country[total_people < 100, proportion_humarts := NA]
humarts_by_country <- humarts_by_country[, .(country, proportion_humarts)]
humarts_by_country[, prop_humarts_char := as.character(round(proportion_humarts, 1))]
world <- merge(world, humarts_by_country)

# Map creation for ds data

labels_ha <- sprintf(
  "<strong>%s</strong><br/>%s per cent humanities or arts grads",
  world$country, world$prop_humarts_char
) %>% lapply(htmltools::HTML)

leaflet(data = world)  %>% 
  addTiles() %>%
  setView(lat = 10, lng = 0, zoom = 2) %>%
  addPolygons(fillColor = ~mypalette(proportion_humarts), 
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
              label = labels_ds,
              labelOptions = labelOptions(
                style = list("font-weight" = "normal", padding = "3px 8px"),
                textsize = "15px",
                direction = "auto")) %>%
  addLegend(pal = mypalette, values = ~proportion_humarts, opacity = 0.7, title = NULL,
            position = "bottomleft")


# Proportion age ----------------------------

age_by_country <- dev_survey_19[!is.na(Age), .(med_age = median(Age)), by = country]
total_age_by_country <- dev_survey_19[!is.na(Age), .(total_people = .N), by = country]
age_by_country <- merge(age_by_country, total_age_by_country, by = "country", all = TRUE)
age_by_country[is.na(med_age), med_age := 0]  
age_by_country[total_people < 100, med_age := NA]
age_by_country <- age_by_country[, .(country, med_age)]
age_by_country[, med_age_char := as.character(round(med_age, 1))]
world <- merge(world, age_by_country)

# Map creation for ds data

labels_age <- sprintf(
  "<strong>%s</strong><br/>Median age: %s years old",
  world$country, world$med_age_char
) %>% lapply(htmltools::HTML)

leaflet(data = world)  %>% 
  addTiles() %>%
  setView(lat = 10, lng = 0, zoom = 2) %>%
  addPolygons(fillColor = ~mypalette(med_age), 
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
              label = labels_age,
              labelOptions = labelOptions(
                style = list("font-weight" = "normal", padding = "3px 8px"),
                textsize = "15px",
                direction = "auto")) %>%
  addLegend(pal = mypalette, values = ~med_age, opacity = 0.7, title = NULL,
            position = "bottomleft")

