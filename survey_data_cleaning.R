library(data.table)
library(leaflet)
library(feather)
library(rgdal)



world <- geojsonio::geojson_read("world.geo.json", what = "sp")


# Survey data

dev_survey_19 <- fread("./SOresults/SO_survey_results_2019.csv")

dev_survey_19 <- dev_survey_19[, .(MainBranch,
                  Hobbyist, 
                  Employment, 
                  country = Country,
                  UndergradMajor, 
                  DevType, 
                  YearsCode,
                  Age1stCode, 
                  YearsCodePro,  
                  CareerSat,
                  JobSat, 
                  Age, 
                  Gender, 
                  Trans, 
                  Sexuality, 
                  Ethnicity
                  )]


orig_survey_countries <- c(
  "Antigua and Barbuda",
  "Brunei Darussalam", 
  "CÃ´te d'Ivoire", 
  "Lao People's Democratic Republic", 
  "Libyan Arab Jamahiriya", 
  "The former Yugoslav Republic of Macedonia", 
  "Republic of Moldova", 
  "Congo, Republic of the...", 
  "Russian Federation", 
  "Saint Vincent and the Grenadines", 
  "Saint Kitts and Nevis", 
  "Trinidad and Tobago", 
  "Syrian Arab Republic", 
  "United Republic of Tanzania", 
  "United Kingdom", 
  "United States",
  "Venezuela, Bolivarian Republic of...", 
  "Viet Nam"
  )

replace_survey_countries <- c(
  "Antigua",
  "Brunei", 
  "Ivory Coast", 
  "Laos", 
  "Libya", 
  "Macedonia", 
  "Moldova", 
  "Republic of Congo", 
  "Russia", 
  "Saint Vincent", 
  "Saint Kitts", 
  "Trinidad", 
  "Syria", 
  "Tanzania", 
  "UK", 
  "USA", 
  "Venezuela", 
  "Vietnam"
)



for (i in 1:length(replace_survey_countries)) {
  dev_survey_19[country %like% orig_survey_countries[i], country := replace_survey_countries[i]]
}



countries_not_on_map <- setdiff(sort(unique(dev_survey_19$country)), world_map$country)


women_by_country <- dev_survey_19[Gender == "Woman", .(total_women = .N), by = country]
total_gender_by_country <- dev_survey_19[!is.na(Gender), .(total_people = .N), by = country]
women_by_country <- merge(women_by_country, total_gender_by_country, by = "country", all = TRUE)
women_by_country[is.na(total_women), total_women := 0]  
women_by_country[, proportion_women := total_women/total_people*100]
women_by_country[total_people < 100, proportion_women := NA]

world <- merge(world_map, women_by_country, by = "country")

