library(data.table)
library(feather)

# Survey data

dev_survey_19 <- fread("./SOresults/SO_survey_results_2019.csv")

dev_survey_19 <- dev_survey_19[, .(
                  country = Country,
                  UndergradMajor, 
                  DevType, 
                  Age, 
                  Gender
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

write_feather(dev_survey_19, "dev_survey_19.feather")
