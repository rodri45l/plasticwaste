library(tidyverse)


temp_country <-read_csv("./Datos/GlobalLandTemperaturesByCountry.csv")

temp_2000 <- subset(temp_country,dt> "1900-01-01")
temp_2000 <-temp_2000 %>% filter(Country == paises)
paises <- c("United States","China")