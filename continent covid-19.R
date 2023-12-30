# Load packages
library(dplyr)
library(ggplot2)
library(gganimate)
library(rvest)
library(haven)

# Import data
h <- read_html("https://www.worldometers.info/coronavirus")
tab <- h %>% html_nodes("table")
corona_data <- tab[[1]] %>% html_table()
continent= corona_data %>% select(Continent, `Country,Other`)

covid= read_sav("G:/covid-19/mushi/Covid-19_mushi.sav")
country= covid %>% select(Date, Confirmed, Country)

# Spelling corrections
setdiff(country$Country, continent$`Country,Other`)

continent$`Country,Other`[grep("South Africa", continent$`Country,Other`)] = "Central African Republic"

continent$`Country,Other`[grep("Iv", continent$`Country,Other`)] = "Cote d'Ivoire"

continent$`Country,Other`[grep("Con", continent$`Country,Other`)] = "Congo (Brazzaville)"

continent$`Country,Other`[grep("Kor", continent$`Country,Other`)] = "Korea, South"

continent$`Country,Other`[grep("Tai", continent$`Country,Other`)] = "Taiwan*"

continent$`Country,Other`[grep("US", continent$`Country,Other`)] = "US"

continent$`Country,Other`[grep("UK", continent$`Country,Other`)] = "United Kingdom"

continent$`Country,Other`[grep("UA", continent$`Country,Other`)] = "United Arab Emirates"


# Data joining
covid_continent= left_join(country, continent, 
          by= c("Country"= "Country,Other"))

View(covid_continent)


# Some necessary wranglings
covid_group= covid_continent %>% group_by(Date, Continent) %>%
  summarise(Confirmed= sum(Confirmed))

# Replace the empty element by NA from the attribute Continent
covid_group$Continent[which(covid_group$Continent == "")] = NA

View(covid_group)

# Remove the rows containing NA
covid_final= na.omit(covid_group)

View(covid_final)

# Set a theme for the graph
theme_set(theme_light())

# Animation line graph of covid-19 by continentwise
p= covid_final %>% ggplot(aes(x= Date, y= Confirmed, colour= Continent))+
  geom_line(size= 1)+
  transition_reveal(Date)+
  facet_wrap(~Continent)+
  scale_y_continuous(labels = scales::comma)+
  theme(axis.text.x = element_text(angle = 90, size = 15),
        axis.text.y = element_text(size = 15),
        strip.text.x = element_text(face = "bold", size = 10),
        axis.title = element_text(face = "bold", size = 20),
        plot.title = element_text(face= "bold", size = 30),
        axis.ticks = element_line(size = 1))+
  labs(title = "Confirm Cases of Covid-19", 
       x= "Date", y= "Confirmed Cases")

animate(p, nframes = 50, renderer = gifski_renderer("continent_covid-19.gif"),
        height= 700, width= 1000)
