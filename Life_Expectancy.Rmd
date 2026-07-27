---
title: "Life Expectancy"
author: "Jessica Kotlarek"
date: "2025-10-30"
output:
  pdf_document: default
---

# Libraries

```{r setup, include = FALSE}
library(tidyverse)
library(readxl)
options(scipen = 999)
library(ggrepel)
library(car)
library(corrplot)
library(mice)
library(olsrr)
library(naniar)
```

# Data sources

<https://worldpopulationreview.com/country-rankings/life-expectancy-by-country> <https://www.worldhappiness.report/ed/2024/> <https://worldpopulationreview.com/country-rankings/gdp-per-capita-by-country> <https://data.worldbank.org/indicator/SH.XPD.CHEX.PC.CD> <https://worldpopulationreview.com/country-rankings/literacy-rate-by-country> <https://ourworldindata.org/grapher/continents-according-to-our-world-in-data> <https://ourworldindata.org/grapher/world-bank-income-groups> <https://ourworldindata.org/grapher/annual-co2-emissions-per-country?tab=table> <https://worldpopulationreview.com/country-rankings/doctors-per-capita-by-country> <https://data.worldbank.org/indicator/SI.POV.GINI> <https://data.worldbank.org/indicator/EN.POP.DNST> <https://data.worldhappiness.report/chart> <https://data.worldhappiness.report/chart> <https://worldpopulationreview.com/countries> <https://worldpopulationreview.com/country-rankings/environmental-performance-index-by-country>

# Other sources

<https://www.who.int/data/gho/data/themes/mortality-and-global-health-estimates/ghe-life-expectancy-and-healthy-life-expectancy> <https://www.worldeconomics.com>

# My downloaded csv and xmlx files

Life expectancy: <https://drive.google.com/file/d/1DjX6zu4cQSd3nrmr4t5V-E8hrsKCLylL/view?usp=share_link>

Happiness score: <https://docs.google.com/spreadsheets/d/1eYSWw7yM8kwKvO0isUmFVKfNxcTe_0SA/edit?usp=share_link&ouid=105058065616169999099&rtpof=true&sd=true>

GDP: <https://drive.google.com/file/d/1Be1V4odCA6Pr2wjFTEgTQNuPEsKlOa2h/view?usp=share_link>

Health expenditure: <https://docs.google.com/spreadsheets/d/1B-ve2Rt-_bGFy48r2dYO8-wGiElMFcSS/edit?usp=share_link&ouid=105058065616169999099&rtpof=true&sd=true>

Adult literacy rate: <https://docs.google.com/spreadsheets/d/1rCvVX2Gn7V2GDBOeb3ulWPciMktzZPYl/edit?usp=share_link&ouid=105058065616169999099&rtpof=true&sd=true>

Continent: <https://drive.google.com/file/d/1QBZlvHKmZHYcx_5L4KluPJrd_YsSXKhH/view?usp=share_link>

Income class: <https://drive.google.com/file/d/1NFZMKlxvc2USreC0FBm69tvIPdcjC2T6/view?usp=share_link>

CO2: <https://drive.google.com/file/d/1dpuglPxFWbVvwqSSlNOe9ZeTycBfCqqF/view?usp=share_link>

Healthcare professionals per 1000: <https://drive.google.com/file/d/11ognAXyT0IKTm6ce-bVopPppS8sq_PRo/view?usp=share_link>

Environmental performative index: <https://drive.google.com/file/d/1D2Ct68SNa6OHFWSzMwSAvMVcmUrLOPNt/view?usp=share_link>

Gini index: <https://drive.google.com/file/d/179iEnA9HOkWOd2NRsLPc6vNjH_DXMwJ1/view?usp=share_link>

Population density: <https://docs.google.com/spreadsheets/d/1UqWxGW-Mw8ti0itctSKJ8PkqiNAFjkUY/edit?usp=share_link&ouid=105058065616169999099&rtpof=true&sd=true>

Freedom to make life choices: <https://docs.google.com/spreadsheets/d/1_Hd5eerei3p-6AW86uH778xl3UaZaXBM/edit?usp=share_link&ouid=105058065616169999099&rtpof=true&sd=true>

Government corrupt belief: <https://docs.google.com/spreadsheets/d/1UfLHR1dwDE1TiG4qbwzdgDOfTqVzFQ3r/edit?usp=share_link&ouid=105058065616169999099&rtpof=true&sd=true>

Population: <https://drive.google.com/file/d/1AJWngHLeTK-MqXv-bSGthQkbrKByK4Xv/view?usp=share_link>

Standardized country list: <https://drive.google.com/file/d/1HEoVPHxBsi0UDnTMWKeXmFKIA7zv-kQN/view?usp=share_link>

# Reading in data

```{r reading in data}
expectancy <- read.csv("~/Documents/Villanova/Stats Seminar - Michael Posner/Course Project/Life Expectancy/life_expectancy_2024.csv")
head(expectancy) 

happiness <- read_excel("~/Documents/Villanova/Stats Seminar - Michael Posner/Course Project/Life Expectancy/happiness_2024.xls")
happiness <- happiness %>% 
    rename(country = 'Country name')
head(happiness)

gdp <- read.csv("~/Documents/Villanova/Stats Seminar - Michael Posner/Course Project/Life Expectancy/gdp_2024.csv")
head(gdp)

health_expenditure <- read_excel("~/Documents/Villanova/Stats Seminar - Michael Posner/Course Project/Life Expectancy/health_expenditure.xls")
health_expenditure <- health_expenditure %>% 
    rename(country = 'Country Name')
head(health_expenditure)

literacy <- read_excel("~/Documents/Villanova/Stats Seminar - Michael Posner/Course Project/Life Expectancy/literacy_rate_15+.xls")
literacy <- literacy %>% 
    rename(country = 'Country Name')
head(literacy)

continent <- read.csv("~/Documents/Villanova/Stats Seminar - Michael Posner/Course Project/Life Expectancy/continent.csv")
head(continent)
continent <- continent %>% 
    rename(country = Entity,continent = World.regions.according.to.OWID)

income <- read.csv("~/Documents/Villanova/Stats Seminar - Michael Posner/Course Project/Life Expectancy/income_group.csv")
income <- income %>% 
    rename(country = Entity, income_class = World.Bank.s.income.classification)
head(income)

CO2 <- read.csv("~/Documents/Villanova/Stats Seminar - Michael Posner/Course Project/Life Expectancy/CO2.csv")
CO2 <- CO2 %>% 
    rename(country = Entity, emissions = Annual.CO..emissions)
head(CO2)

nurses_doctors_midwives_percapita <- read.csv("~/Documents/Villanova/Stats Seminar - Michael Posner/Course Project/Life Expectancy/nurses_doctors_midwives_percapita.csv")
head(nurses_doctors_midwives_percapita)

epi <- read.csv("~/Documents/Villanova/Stats Seminar - Michael Posner/Course Project/Life Expectancy/EPI.csv")
head(epi)

gini <- read.csv("~/Documents/Villanova/Stats Seminar - Michael Posner/Course Project/Life Expectancy/gini_ coefficient.csv")
head(gini)

popdens <- read_excel("~/Documents/Villanova/Stats Seminar - Michael Posner/Course Project/Life Expectancy/pop_dens_per_sqkm.xls")
popdens <- popdens %>% 
    rename(country = 'Country Name')
head(popdens)

freedom <- read_excel("~/Documents/Villanova/Stats Seminar - Michael Posner/Course Project/Life Expectancy/freedom.xlsx")
freedom <- freedom %>% 
    rename(country = 'Geography', year = Time)
head(freedom)

gov_corrup <- read_excel("~/Documents/Villanova/Stats Seminar - Michael Posner/Course Project/Life Expectancy/government_corruption.xlsx")
gov_corrup <- gov_corrup %>% 
    rename(country = 'Geography', year = Time)
head(gov_corrup)

pop <- read.csv("~/Documents/Villanova/Stats Seminar - Michael Posner/Course Project/Life Expectancy/total_population_2025.csv")
head(pop)

countries <- read.csv("~/Documents/Villanova/Stats Seminar - Michael Posner/Course Project/Life Expectancy/countries.csv")
countries <- countries %>% 
    select(country = name)
```

# Cleaning data

```{r cleaning data}
# for each variable, select relevant columns, rename columns, and if necessary, choose the most recent year of available data

# life expectancy (dependent variable)
expectancy <- expectancy %>% 
    select(-flagCode) %>% 
    rename(both_genders_2024 = LifeExpectancyViaUN_2024, 
           female_2024 = LifeExpectancyFemalesUN_2024,
           male_2024 = LifeExpectancyMalesUN_2024) %>% 
    arrange(country)

# happiness level
happiness <- happiness %>% 
    select(country, 'Ladder score') %>% 
    arrange(country)

# gdp per capita
gdp <- gdp %>%  
    mutate(GDPperCapita_MostRecent = coalesce(GDPPerCapitaViaIMF_2024, GDPPerCapitaViaUN_2023)) %>%
    select(country, GDPperCapita_MostRecent) %>%  # take the most recent year per country
    arrange(country)

# health expenditure per capita
yrs <- grep("^\\d{4}$", names(health_expenditure), value = TRUE) %>% 
    as.integer() %>%  
    sort(decreasing = TRUE) %>%  
    as.character()
health_expenditure <- health_expenditure %>%
    transmute(country,
              health_expenditure_MostRecent = coalesce(!!!select(., all_of(yrs)))) %>%  # take the most recent year per country
    arrange(country)

# adult literacy rate
yrs2 <- grep("^\\d{4}$", names(literacy), value = TRUE) %>%  
    as.integer() %>%  
    sort(decreasing = TRUE) %>%  
    as.character()
literacy <- literacy %>%
    transmute(country,
    literacy_MostRecent = coalesce(!!!select(., all_of(yrs)))) %>% # take the most recent year per country
    arrange(country)

# continent (categorical)
continent <- continent %>% 
    select(country, continent) %>% 
    arrange(country)

# income group (categorical)
income <- income %>%
    group_by(country) %>%
    slice_max(Year, n = 1, with_ties = FALSE) %>%   # take the most recent year per country
    ungroup() %>%
    select(country, income_class) %>% 
    arrange(country)

# CO2 emissions
CO2 <- CO2 %>% 
    group_by(country) %>%
    slice_max(Year, n = 1, with_ties = FALSE) %>%   # take the most recent year per country
    ungroup() %>%
    select(country, emissions) %>% 
    arrange(country)

# nurses doctors and midwives per 1000 people
nurses_doctors_midwives_percapita <- nurses_doctors_midwives_percapita %>% 
    select(country, DoctorsPerCapita_DensityOfMedicalDoctors_Per10000) %>% 
    arrange(country)

# environmental performative index
epi <- epi %>% 
    select(country, EnvironmentalPerformanceIndex_2024) %>% 
    arrange(country)

# gini index
gini <- gini %>% 
    select(country, GiniCoefficientByCountry) %>% 
    arrange(country)

# population density
yrs3 <- grep("^\\d{4}$", names(popdens), value = TRUE) %>% 
    as.integer() %>% 
    sort(decreasing = TRUE) %>% 
    as.character()
popdens <- popdens %>%
    transmute(country,
              popdens_MostRecent = coalesce(!!!select(., all_of(yrs)))) %>%  # take the most recent year per country
    arrange(country)

# freedom to make life choices
freedom <- freedom %>%
    group_by(country) %>%
    filter(year == max(year)) %>%  # take the most recent year per country  
    ungroup() %>%
    select(country, Satisfied) %>% 
    arrange(country)

# is government corrupt
gov_corrup <- gov_corrup %>%
    group_by(country) %>%
    filter(year == max(year)) %>%    # take the most recent year per country
    ungroup() %>%
    select(country, Yes) %>% 
    arrange(country)

# population
pop <- pop %>% 
    select(pop2025, country) %>% 
    arrange(country)

# countries
countries
```

# Standardizing country names across datasets

```{r finding country name mismatches, 193 countries to start}
# seeing what countries don't match the standardized country list

# canonical list of country names (193)
canonical <- countries %>%
    distinct(country) %>%
    arrange(country)

# putting all the tables to check into a named list (exclude `countries`)
tables_to_check <- list(
    expectancy = expectancy,
    happiness = happiness,
    gdp = gdp,
    health_exp = health_expenditure,
    literacy = literacy,
    continent = continent,
    income = income,
    CO2 = CO2,
    med_staff = nurses_doctors_midwives_percapita,
    epi = epi,
    gini = gini,
    popdens = popdens,
    freedom = freedom,
    gov_corrup = gov_corrup,
    pop = pop)

# for each table, find country names that are NOT in the canonical 193
mismatches <- imap_dfr(tables_to_check, ~ anti_join(.x, canonical, by = "country") %>%
                           distinct(country) %>%
                           mutate(table = .y, .before = 1))

# look at all mismatched country names across all tables
mismatches %>% arrange(country, table)

# manually compare to standardized table
countries %>% 
    arrange(country)
```

```{r standardizing country names, 196 countries now (added 3 not in file)}
# bolivia
countries$country[countries$country == "Bolivia, Plurinational State of"] <- "Bolivia"
# brueni
countries$country[countries$country == "Brunei Darussalam"] <- "Brunei"
health_expenditure$country[health_expenditure$country == "Brunei Darussalam"] <- "Brunei"
literacy$country[literacy$country == "Brunei Darussalam"] <- "Brunei"
popdens$country[popdens$country == "Brunei Darussalam"] <- "Brunei"
# bahamas
health_expenditure$country[health_expenditure$country == "Bahamas, The"] <- "Bahamas"
literacy$country[literacy$country == "Bahamas, The"] <- "Bahamas"
popdens$country[popdens$country == "Bahamas, The"] <- "Bahamas"
# cape verde
countries$country[countries$country == "Cabo Verde"] <- "Cape Verde"
health_expenditure$country[health_expenditure$country == "Cabo Verde"] <- "Cape Verde"
literacy$country[literacy$country == "Cabo Verde"] <- "Cape Verde"
popdens$country[popdens$country == "Cabo Verde"] <- "Cape Verde"
# republic of the congo
countries$country[countries$country == "Congo"] <- "Republic of the Congo"
happiness$country[happiness$country == "Congo (Brazzaville)"] <- "Republic of the Congo"
freedom$country[freedom$country == "Congo Brazzaville"] <- "Republic of the Congo"
gov_corrup$country[gov_corrup$country == "Congo Brazzaville"] <- "Republic of the Congo"
health_expenditure$country[health_expenditure$country == "Congo, Rep."] <- "Republic of the Congo"
literacy$country[literacy$country == "Congo, Rep."] <- "Republic of the Congo"
popdens$country[popdens$country == "Congo, Rep."] <- "Republic of the Congo"
CO2$country[CO2$country == "Congo"] <- "Republic of the Congo"
continent$country[continent$country == "Congo"] <- "Republic of the Congo"
income$country[income$country == "Congo"] <- "Republic of the Congo"
# democratic republic of the congo
happiness$country[happiness$country == "Congo (Kinshasa)"] <- "Democratic Republic of the Congo"
freedom$country[freedom$country == "Congo (Kinshasa)"] <- "Democratic Republic of the Congo"
gov_corrup$country[gov_corrup$country == "Congo (Kinshasa)"] <- "Democratic Republic of the Congo"
countries$country[countries$country == "Congo, Democratic Republic of the"] <- "Democratic Republic of the Congo"
health_expenditure$country[health_expenditure$country == "Congo, Dem. Rep."] <- "Democratic Republic of the Congo"
literacy$country[literacy$country == "Congo, Dem. Rep."] <- "Democratic Republic of the Congo"
popdens$country[popdens$country == "Congo, Dem. Rep."] <- "Democratic Republic of the Congo"
epi$country[epi$country == "DR Congo"] <- "Democratic Republic of the Congo"
expectancy$country[expectancy$country == "DR Congo"] <- "Democratic Republic of the Congo"
gdp$country[gdp$country == "DR Congo"] <- "Democratic Republic of the Congo"
gini$country[gini$country == "DR Congo"] <- "Democratic Republic of the Congo"
nurses_doctors_midwives_percapita$country[nurses_doctors_midwives_percapita$country == "DR Congo"] <- "Democratic Republic of the Congo"
pop$country[pop$country == "DR Congo"] <- "Democratic Republic of the Congo"
CO2$country[CO2$country == "Democratic Republic of Congo"] <- "Democratic Republic of the Congo"
continent$country[continent$country == "Democratic Republic of Congo"] <- "Democratic Republic of the Congo"
income$country[income$country == "Democratic Republic of Congo"] <- "Democratic Republic of the Congo"
# ivory coast
countries$country[countries$country == "Côte d'Ivoire"] <- "Ivory Coast"
CO2$country[CO2$country == "Cote d'Ivoire"] <- "Ivory Coast"
continent$country[continent$country == "Cote d'Ivoire"] <- "Ivory Coast"
freedom$country[freedom$country == "Cote d'Ivoire"] <- "Ivory Coast"
gov_corrup$country[gov_corrup$country == "Cote d'Ivoire"] <- "Ivory Coast"
health_expenditure$country[health_expenditure$country == "Cote d'Ivoire"] <- "Ivory Coast"
income$country[income$country == "Cote d'Ivoire"] <- "Ivory Coast"
literacy$country[literacy$country == "Cote d'Ivoire"] <- "Ivory Coast"
popdens$country[popdens$country == "Cote d'Ivoire"] <- "Ivory Coast"
# czech republic
countries$country[countries$country == "Czechoslovakia"] <- "Czechia"
continent$country[continent$country == "Czechoslovakia"] <- "Czechia"
income$country[income$country == "Czechoslovakia"] <- "Czechia"
freedom$country[freedom$country == "Czech Republic"] <- "Czechia"
gov_corrup$country[gov_corrup$country == "Czech Republic"] <- "Czechia"
# vietman
countries$country[countries$country == "Viet Nam"] <- "Vietnam"
continent$country[continent$country == "Democratic Republic of Vietnam"] <- "Vietnam"
health_expenditure$country[health_expenditure$country == "Viet Nam"] <- "Vietnam"
literacy$country[literacy$country == "Viet Nam"] <- "Vietnam"
popdens$country[popdens$country == "Viet Nam"] <- "Vietnam"
# egypt
health_expenditure$country[health_expenditure$country == "Egypt, Arab Rep."] <- "Egypt"
literacy$country[literacy$country == "Egypt, Arab Rep."] <- "Egypt"
popdens$country[popdens$country == "Egypt, Arab Rep."] <- "Egypt"
# gambia
health_expenditure$country[health_expenditure$country == "Gambia, The"] <- "Gambia"
literacy$country[literacy$country == "Gambia, The"] <- "Gambia"
popdens$country[popdens$country == "Gambia, The"] <- "Gambia"
# iran
countries$country[countries$country == "Iran, Islamic Republic of"] <- "Iran"
health_expenditure$country[health_expenditure$country == "Iran, Islamic Rep."] <- "Iran"
literacy$country[literacy$country == "Iran, Islamic Rep."] <- "Iran"
popdens$country[popdens$country == "Iran, Islamic Rep."] <- "Iran"
# democratic people's republic of korea
countries$country[countries$country == "Korea, Democratic People's Republic of"] <- "Democratic People's Republic of Korea"
health_expenditure$country[health_expenditure$country == "Korea, Dem. People's Rep."] <- "Democratic People's Republic of Korea"
literacy$country[literacy$country == "Korea, Dem. People's Rep."] <- "Democratic People's Republic of Korea"
popdens$country[popdens$country == "Korea, Dem. People's Rep."] <- "Democratic People's Republic of Korea"
pop$country[pop$country == "North Korea"] <- "Democratic People's Republic of Korea"
CO2$country[CO2$country == "North Korea"] <- "Democratic People's Republic of Korea"
continent$country[continent$country == "North Korea"] <- "Democratic People's Republic of Korea"
expectancy$country[expectancy$country == "North Korea"] <- "Democratic People's Republic of Korea"
gdp$country[gdp$country == "North Korea"] <- "Democratic People's Republic of Korea"
income$country[income$country == "North Korea"] <- "Democratic People's Republic of Korea"
nurses_doctors_midwives_percapita$country[nurses_doctors_midwives_percapita$country == "North Korea"] <- "Democratic People's Republic of Korea"
# republic of korea
countries$country[countries$country == "Korea, Republic of"] <- "Republic of Korea"
health_expenditure$country[health_expenditure$country == "Korea, Rep."] <- "Republic of Korea"
literacy$country[literacy$country == "Korea, Rep."] <- "Republic of Korea"
popdens$country[popdens$country == "Korea, Rep."] <- "Republic of Korea"
CO2$country[CO2$country == "South Korea"] <- "Republic of Korea"
continent$country[continent$country == "South Korea"] <- "Republic of Korea"
expectancy$country[expectancy$country == "South Korea"] <- "Republic of Korea"
gdp$country[gdp$country == "South Korea"] <- "Republic of Korea"
nurses_doctors_midwives_percapita$country[nurses_doctors_midwives_percapita$country == "South Korea"] <- "Republic of Korea"
pop$country[pop$country == "South Korea"] <- "Republic of Korea"
income$country[income$country == "South Korea"] <- "Republic of Korea"
epi$country[epi$country == "South Korea"] <- "Republic of Korea"
freedom$country[freedom$country == "South Korea"] <- "Republic of Korea"
gini$country[gini$country == "South Korea"] <- "Republic of Korea"
gov_corrup$country[gov_corrup$country == "South Korea"] <- "Republic of Korea"
happiness$country[happiness$country == "South Korea"] <- "Republic of Korea"
# kyrgyzstan
health_expenditure$country[health_expenditure$country == "Kyrgyz Republic"] <- "Kyrgyzstan"
literacy$country[literacy$country == "Kyrgyz Republic"] <- "Kyrgyzstan"
popdens$country[popdens$country == "Kyrgyz Republic"] <- "Kyrgyzstan"
# laos
countries$country[countries$country == "Lao People's Democratic Republic"] <- "Laos"
health_expenditure$country[health_expenditure$country == "Lao PDR"] <- "Laos"
literacy$country[literacy$country == "Lao PDR"] <- "Laos"
popdens$country[popdens$country == "Lao PDR"] <- "Laos"
freedom$country[freedom$country == "Lao People's Democratic Republic"] <- "Laos"
gov_corrup$country[gov_corrup$country == "Lao People's Democratic Republic"] <- "Laos"
# federated states of micronesia
countries$country[countries$country == "Micronesia, Federated States of"] <- "Federated States of Micronesia"
nurses_doctors_midwives_percapita$country[nurses_doctors_midwives_percapita$country == "Micronesia"] <- "Federated States of Micronesia"
pop$country[pop$country == "Micronesia"] <- "Federated States of Micronesia"
CO2$country[CO2$country == "Micronesia (country)"] <- "Federated States of Micronesia"
continent$country[continent$country == "Micronesia (country)"] <- "Federated States of Micronesia"
income$country[income$country == "Micronesia (country)"] <- "Federated States of Micronesia"
health_expenditure$country[health_expenditure$country == "Micronesia, Fed. Sts."] <- "Federated States of Micronesia"
literacy$country[literacy$country == "Micronesia, Fed. Sts."] <- "Federated States of Micronesia"
popdens$country[popdens$country == "Micronesia, Fed. Sts."] <- "Federated States of Micronesia"
epi$country[epi$country == "Micronesia"] <- "Federated States of Micronesia"
expectancy$country[expectancy$country == "Micronesia"] <- "Federated States of Micronesia"
gdp$country[gdp$country == "Micronesia"] <- "Federated States of Micronesia"
gini$country[gini$country == "Micronesia"] <- "Federated States of Micronesia"
# moldova
countries$country[countries$country == "Moldova, Republic of"] <- "Moldova"
freedom$country[freedom$country == "Moldova, Republic of"] <- "Moldova"
gov_corrup$country[gov_corrup$country == "Moldova, Republic of"] <- "Moldova"
# netherlands
freedom$country[freedom$country == "Netherlands (Kingdom of the)"] <- "Netherlands"
gov_corrup$country[gov_corrup$country == "Netherlands (Kingdom of the)"] <- "Netherlands"
# russia
countries$country[countries$country == "Russian Federation"] <- "Russia"
freedom$country[freedom$country == "Russian Federation"] <- "Russia"
gov_corrup$country[gov_corrup$country == "Russian Federation"] <- "Russia"
health_expenditure$country[health_expenditure$country == "Russian Federation"] <- "Russia"
literacy$country[literacy$country == "Russian Federation"] <- "Russia"
popdens$country[popdens$country == "Russian Federation"] <- "Russia"
# slovakia
health_expenditure$country[health_expenditure$country == "Slovak Republic"] <- "Slovakia"
literacy$country[literacy$country == "Slovak Republic"] <- "Slovakia"
popdens$country[popdens$country == "Slovak Republic"] <- "Slovakia"
# somalia
health_expenditure$country[health_expenditure$country == "Somalia, Fed. Rep."] <- "Somalia"
literacy$country[literacy$country == "Somalia, Fed. Rep."] <- "Somalia"
popdens$country[popdens$country == "Somalia, Fed. Rep."] <- "Somalia"
# saint kitts and nevis
health_expenditure$country[health_expenditure$country == "St. Kitts and Nevis"] <- "Saint Kitts and Nevis"
literacy$country[literacy$country == "St. Kitts and Nevis"] <- "Saint Kitts and Nevis"
popdens$country[popdens$country == "St. Kitts and Nevis"] <- "Saint Kitts and Nevis"
# saint lucia
health_expenditure$country[health_expenditure$country == "St. Lucia"] <- "Saint Lucia"
literacy$country[literacy$country == "St. Lucia"] <- "Saint Lucia"
popdens$country[popdens$country == "St. Lucia"] <- "Saint Lucia"
# saint lucia
health_expenditure$country[health_expenditure$country == "St. Vincent and the Grenadines"] <- "Saint Vincent and the Grenadines"
literacy$country[literacy$country == "St. Vincent and the Grenadines"] <- "Saint Vincent and the Grenadines"
popdens$country[popdens$country == "St. Vincent and the Grenadines"] <- "Saint Vincent and the Grenadines"
# syria
countries$country[countries$country == "Syrian Arab Republic"] <- "Syria"
health_expenditure$country[health_expenditure$country == "Syrian Arab Republic"] <- "Syria"
literacy$country[literacy$country == "Syrian Arab Republic"] <- "Syria"
popdens$country[popdens$country == "Syrian Arab Republic"] <- "Syria"
# tanzania
countries$country[countries$country == "Tanzania, United Republic of"] <- "Tanzania"
# turkey
countries$country[countries$country == "Türkiye"] <- "Turkey"
health_expenditure$country[health_expenditure$country == "Turkiye"] <- "Turkey"
literacy$country[literacy$country == "Turkiye"] <- "Turkey"
popdens$country[popdens$country == "Turkiye"] <- "Turkey"
happiness$country[happiness$country == "Turkiye"] <- "Turkey"
freedom$country[freedom$country == "Türkiye"] <- "Turkey"
gov_corrup$country[gov_corrup$country == "Türkiye"] <- "Turkey"
# uk
countries$country[countries$country == "United Kingdom of Great Britain and Northern Ireland"] <- "United Kingdom"
freedom$country[freedom$country == "United Kingdom of Great Britain and Northern Ireland"] <- "United Kingdom"
gov_corrup$country[gov_corrup$country == "United Kingdom of Great Britain and Northern Ireland"] <- "United Kingdom"
# united states
countries$country[countries$country == "United States of America"] <- "United States"
freedom$country[freedom$country == "United States of America"] <- "United States"
gov_corrup$country[gov_corrup$country == "United States of America"] <- "United States"
# venezuela
countries$country[countries$country == "Venezuela, Bolivarian Republic of"] <- "Venezuela"
health_expenditure$country[health_expenditure$country == "Venezuela, RB"] <- "Venezuela"
literacy$country[literacy$country == "Venezuela, RB"] <- "Venezuela"
popdens$country[popdens$country == "Venezuela, RB"] <- "Venezuela"
# yemen
health_expenditure$country[health_expenditure$country == "Yemen, Rep."] <- "Yemen"
literacy$country[literacy$country == "Yemen, Rep."] <- "Yemen"
popdens$country[popdens$country == "Yemen, Rep."] <- "Yemen"

# add in countries that weren't in countries data file but are actually countries
countries <- add_row(countries, country = "Cook Islands")
countries <- add_row(countries, country = "Curacao")
countries <- add_row(countries, country = "Niue")
# countries file contains 196 countries now

# remove duplicates
# czechia and vietnam were duplicated in continent table
continent <- continent %>%
    distinct()
```

```{r remove czechia duplicate from income}
# removing czechia from upper middle income class because it was duplicated,
# it had both high income and upper middle income, after further research 
# it is classified as high income
income <- income %>% 
    filter(!(country == "Czechia" & income_class == "Upper-middle-income countries"))
```

# Merging datasets

```{r complete merge, keeping NAs}
# keeping NAs
dfs <- list(expectancy, happiness, gdp, health_expenditure, literacy, continent, 
            income, CO2, nurses_doctors_midwives_percapita, epi, gini, popdens,
            freedom, gov_corrup, pop)
# checking for duplicate countries in each dataframe
lapply(dfs, function(df) df %>% 
           count(country) %>% 
           filter(n > 1))

merged_with_nas <- reduce(dfs, left_join, by = "country", .init = countries) 
```

```{r renaming columns and factoring cat vars, reference vars}
# changing total CO2 emissions to CO2 emissions per capita/person
# removing population becuase population density is more important

merged_with_nas <- merged_with_nas %>% 
    rename(life_expectancy_both = both_genders_2024,
           female_life_expectancy = female_2024,
           male_life_expectancy = male_2024,
           happiness_score = `Ladder score`,
           gdp_per_capita = GDPperCapita_MostRecent,
           health_expenditure_per_capita = health_expenditure_MostRecent,
           adult_literacy_rate = literacy_MostRecent,
           continent = continent,
           income_class = income_class,
           co2_emissions = emissions,
           healthcare_professionals_per_1000 = DoctorsPerCapita_DensityOfMedicalDoctors_Per10000,
           epi = EnvironmentalPerformanceIndex_2024,
           gini_index = GiniCoefficientByCountry,
           popdens = popdens_MostRecent,
           freedom_to_make_life_choices = Satisfied,
           gov_is_corrupt_yes = Yes,
           population = pop2025) %>% 
    mutate(co2_per_capita = co2_emissions/population) %>% 
    select(-co2_emissions, -male_life_expectancy, -female_life_expectancy) 

# factoring categorical variables and setting reference levels
merged_with_nas <- merged_with_nas %>%
    mutate(continent = fct_relevel(continent, "Europe"), 
           income_class = fct_relevel(income_class, "High-income countries"))
```

# Note:

Merged with NAs has 196 countries, full dataset with all NAs - Kept for sensitivity analysis at the end

# NA evaluation

```{r looking into NAs, which countries are removed?}
# how many NAs per variable
colSums(is.na(merged_with_nas))
# gov is corrupt with 59 NA
# happiness score with 57 NA
# freedom to make life choices with 51 NAs
# adult literacy rate with 34 NAs

# googled externally
# manually looking up missing values for variables with only one NA
# source: World Economics

# from 2019
merged_with_nas$gini_index[merged_with_nas$country == "Afghanistan"] <- 59
# from 2021
merged_with_nas$gini_index[merged_with_nas$country == "Antigua and Barbuda"] <- 48
# from 2019
merged_with_nas$gini_index[merged_with_nas$country == "Bahrain"] <- 44.3
# from 2019
merged_with_nas$gini_index[merged_with_nas$country == "Brunei"] <- 63.4
# from 2019
merged_with_nas$gini_index[merged_with_nas$country == "Cambodia"] <- 54.6
# from 2019
merged_with_nas$gini_index[merged_with_nas$country == "Bahamas"] <- 46.7
# year unknown
merged_with_nas$gini_index[merged_with_nas$country == "Eritrea"] <- 37.63
# 2024
merged_with_nas$epi[merged_with_nas$country == "Guinea-Bissau"] <- 42
# from 2019
merged_with_nas$gini_index[merged_with_nas$country == "Kuwait"] <- 47.1
# from 2019
merged_with_nas$gini_index[merged_with_nas$country == "Oman"] <- 55.7
# 2024
merged_with_nas$epi[merged_with_nas$country == "Russia"] <- 46.7
# from 2019
merged_with_nas$gini_index[merged_with_nas$country == "Saudi Arabia"] <- 45.6
# 2024
merged_with_nas$epi[merged_with_nas$country == "Zambia"] <- 46.7

# look again
colSums(is.na(merged_with_nas))
# gini drops to 16 NAs
# epi drops to 17 NAs


# showing all countries with at least one NA
merged_with_nas %>%
    mutate(na_count = rowSums(is.na(.))) %>%
    select(country, na_count, population) %>%
    arrange(desc(na_count)) %>% 
    filter(na_count > 0) 
# with 13 predictors, 91 countries have at least one NA
# if a single country is missing >40–50% of all variables, 
# imputation becomes unreliable, and dropping the country is often the better choice

# looking at the 10 countries with 6 or more missing values
na_6 <- merged_with_nas %>%
    mutate(na_count = rowSums(is.na(.))) %>%
    select(country, na_count, life_expectancy_both, population) %>%
    arrange(desc(na_count)) %>% 
    filter(na_count > 5) %>% 
    # is it within the first quartile? aka small?
    mutate(is_small = (ifelse(population < 1843618, "yes", "no")))
# yes all but north korea but I knew that, they just don't report their data well

# looking at distribution of population to claim these are small countries and one country that doesn't report its data
summary(merged_with_nas$population) 
# they are all within the first quartile so yes small countries (north korea is the only large one but doesn't report their data well)

# dataset for imputation, all data minus 10 countries with 6 or more NAs
# 186 countries now
clean_data_for_impute <- merged_with_nas %>%
    mutate(na_count = rowSums(is.na(.))) %>%
    filter(na_count <= 5) %>%   # KEEP only rows with 5 or fewer NAs
    select(-na_count, -population)           # drop helper column

# average life expectancy for plot
avg_life <- mean(merged_with_nas$life_expectancy_both)

# plotting distribution of countries that I'm dropping to see if they are 
# concentrated in any life expectancy range, they're not, okay to drop
ggplot(na_6, aes(life_expectancy_both, country)) +
    geom_point() +
    coord_flip() +
    geom_vline(xintercept = avg_life, linetype = "dashed", color = "red") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 90, hjust = 1))

# missingness plot
vis_miss(clean_data_for_impute) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0))
# confirming that 6 variables have missing data
# 6.4% of overall data is missing
```

# Multiple imputation

Using MICE package

```{r imputation, at 186 countries now}
# final look at data for imputation
summary(clean_data_for_impute)
colSums(is.na(clean_data_for_impute))
# 6 variables need imputation:
# need to impute 41 of 186 values for freedom to make life choices
# need to impute 27 of 186  values for adult literacy rate
# need to impute 8 of 186 values for epi
# need to impute 49 of 186 values for gov is corrupt
# need to impute 47 of 186 values for happiness score
# need to impute 7 of 186 values for gini index

# setting up mice imputation
ini_untransformed  <- mice(clean_data_for_impute, maxit = 0)
meth_untransformed <- ini_untransformed$method
pred_untransformed <- ini_untransformed$predictorMatrix

# country is an ID → don't impute it
meth_untransformed["country"] <- ""

# also don't use country to predict anything
pred_untransformed[, "country"] <- 0
pred_untransformed["country", ] <- 0

meth_untransformed
# pmm = predictive mean matching
# "" = not imputing because I said not to or becuase no NAs to impute
pred_untransformed["country", ]
pred_untransformed[, "country"]

# setting the random number generator to a known starting point
set.seed(123)

# perform the imputation

# for every variable with missing values, mice:
# - uses all other selected predictors (from predictorMatrix)
# - uses the method assigned in meth ("pmm" for numeric variables)
# - fits a regression model for that variable
# - generates predictions for the missing values
# - adds appropriate uncertainty

# for example:
# missing life expectancy → predicted from GDP, happiness, literacy, etc.
# missing GDP → predicted from life expectancy, happiness, etc.
# missing literacy → predicted from GDP, happiness, region, etc.

imp_untransformed <- mice(clean_data_for_impute,
                          m = 5,                   # number of imputed datasets
                          method = meth_untransformed,
                          predictorMatrix = pred_untransformed,
                          maxit = 20)               # iterations 
# run through the variables with missing data 20 times
# each cycle updates/improves imputed values as models refine

densityplot(imp_untransformed)   # compares original vs. imputed distributions

# fit contains 5 separate regression models, one for each dataset
first_imputed_fit <- with(imp_untransformed, lm(life_expectancy_both ~ happiness_score + gdp_per_capita +
                        health_expenditure_per_capita + adult_literacy_rate +
                        healthcare_professionals_per_1000 + epi + gini_index +
                        popdens + freedom_to_make_life_choices + gov_is_corrupt_yes +
                        co2_per_capita + income_class + continent))
# pooled gives you:
# combined coefficients
# correct standard errors
# correct confidence intervals
# correct p-values
summary(pool(first_imputed_fit))
```

```{r average of 5 untransformed imputed datasets}
# averaged dataset of all 5 imputed datasets
# extracts all 5 imputed datasets, then creates one final dataset where every numeric column is 
# replaced by the row-wise average across the 5 imputations, while keeping non-numeric columns unchanged
untransformed_imputed_dfs <- lapply(1:5, function(i) complete(imp_untransformed, i))

num_cols_untransformed <- names(untransformed_imputed_dfs[[1]])[sapply(untransformed_imputed_dfs[[1]], is.numeric)]
averaged_untransformed_imputed_data <- untransformed_imputed_dfs[[1]]

averaged_untransformed_imputed_data[num_cols_untransformed] <- lapply(num_cols_untransformed, function(v) {
    rowMeans(sapply(untransformed_imputed_dfs, `[[`, v), na.rm = TRUE)})

# dataset for next steps
averaged_untransformed_imputed_data
```

# Transformation assessment

```{r plotting all data included after imputation against life expectancy}
# use average of all 5 datasets, with the untransformed imputed data
averaged_untransformed_imputed_data

# box-tidwell tests 
boxTidwell(life_expectancy_both ~ gdp_per_capita, data = averaged_untransformed_imputed_data)
boxTidwell(life_expectancy_both ~ co2_per_capita, data = averaged_untransformed_imputed_data)
boxTidwell(life_expectancy_both ~ health_expenditure_per_capita, data = averaged_untransformed_imputed_data)
boxTidwell(life_expectancy_both ~ healthcare_professionals_per_1000, data = averaged_untransformed_imputed_data)
# MLE of lambda for all is ~0, so log transformation is appropriate for all ^

boxTidwell(life_expectancy_both ~ gini_index, data = averaged_untransformed_imputed_data)
# don't transform
boxTidwell(life_expectancy_both ~ happiness_score, data = averaged_untransformed_imputed_data)
# happiness needs ^2
boxTidwell(life_expectancy_both ~ epi, data = averaged_untransformed_imputed_data)
# don't transform
boxTidwell(life_expectancy_both ~ freedom_to_make_life_choices, data = averaged_untransformed_imputed_data)
# freedom need power ^7
boxTidwell(life_expectancy_both ~ gov_is_corrupt_yes, data = averaged_untransformed_imputed_data)
# dont' transform
boxTidwell(life_expectancy_both ~ popdens, data = averaged_untransformed_imputed_data)
# don't transform

# plot before transformations
num_long <- averaged_untransformed_imputed_data %>% 
    select(country, life_expectancy_both, where(is.numeric)) %>%
    pivot_longer(-c(country, life_expectancy_both), names_to  = "variable", values_to = "value")
label_df <- num_long %>%
    filter(country %in% c("Nauru"))
ggplot(num_long, aes(x = value, y = life_expectancy_both)) +
    geom_point(alpha = .4, size = 1) +
    geom_smooth(method = "loess", se = FALSE, color = "red", linewidth = 0.7) +
    geom_smooth(method = "lm",   se = FALSE, color = "darkgreen", linewidth = 0.7) +
    geom_text_repel(data = label_df, aes(x = value, y = life_expectancy_both, label = country), size = 2, min.segment.length = 0) +
    facet_wrap(~ variable, scales = "free_x") +
    labs(title = "Life Expectancy vs Numeric Variables (Imputed Data Averaged)",
         x = "Value of Variable",
         y = "Life Expectancy") +
    theme_minimal(base_size = 8) +
    theme(panel.grid.minor = element_blank(),
        strip.text = element_text(face = "bold"),
        plot.title  = element_text(face = "bold", hjust = 0.5))
# horrible, so many violations of linearity, apply box-tidwell recommended transformations in next step then rerun imputation
```

```{r untransformed univariate plots to look at data quality}
# to see distribution better, will be included in modeling
limited_popdens_untransformed <- averaged_untransformed_imputed_data %>% 
    filter(averaged_untransformed_imputed_data$popdens < 1000)

num_long <- limited_popdens_untransformed %>%
    select(where(is.numeric)) %>%
    pivot_longer(cols = everything(), names_to = "variable", values_to = "value")

ggplot(num_long, aes(x = value)) +
    geom_histogram(bins = 20, color = "black", fill = "grey70") +
    facet_wrap(~ variable, scales = "free_x") +
    labs(title = "Distributions of Numeric Variables", x = NULL, y = "Count") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
# lots of skewness, transformations should help

cat_long <- limited_popdens_untransformed %>%
    select(where(is.factor)) %>%
    pivot_longer(cols = everything(), names_to = "variable", values_to = "value")

ggplot(cat_long, aes(x = value)) +
    geom_bar(color = "black", fill = "grey70") +
    facet_wrap(~ variable, scales = "free_x") +
    labs(title = "Distributions of Categorical Variables", x = NULL, y = "Count") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
```

# Redoing imputation with transformations

```{r redoing imputation with log transformations before imputation}
# transform variables now before imputation:
data_for_impute_with_transformations <- clean_data_for_impute %>%
    mutate(continent = factor(continent),
           income_class = factor(income_class),
           log_gdp = log(gdp_per_capita),
           log_co2 = log(co2_per_capita),
           log_hexp = log(health_expenditure_per_capita),
           log_healthcare_per1000 = log(healthcare_professionals_per_1000),
           happiness_score_square = happiness_score^2,
           freedom_to_make_life_choices_seven = freedom_to_make_life_choices^7) %>% 
    select(-gdp_per_capita, -co2_per_capita, -health_expenditure_per_capita, -healthcare_professionals_per_1000, 
           -happiness_score, -freedom_to_make_life_choices)

# same process as before but with transformed data
ini_transformed  <- mice(data_for_impute_with_transformations, maxit = 0)
meth_transformed <- ini_transformed$method
pred_transformed <- ini_transformed$predictorMatrix

meth_transformed["country"] <- ""

pred_transformed[, "country"] <- 0
pred_transformed["country", ] <- 0

meth_transformed

pred_transformed["country", ]
pred_transformed[, "country"]

imp_transformed <- mice(data_for_impute_with_transformations,
                        m = 5,                   # number of imputed datasets
                        method = meth_transformed,
                        predictorMatrix = pred_transformed,
                        maxit = 20,               # iterations 
                        seed = 123)

densityplot(imp_transformed) # compares original vs. imputed distributions

# fit contains 5 separate regression models, one for each dataset
imputed_fit_with_transformations <- with(imp_transformed, 
                                         lm(life_expectancy_both ~ happiness_score_square + log_gdp + 
                                                log_hexp + adult_literacy_rate +
                                                log_healthcare_per1000 + epi + gini_index +
                                                popdens + freedom_to_make_life_choices_seven + gov_is_corrupt_yes +
                                                log_co2 + income_class + continent))


# pooled gives you:
# combined coefficients
# correct standard errors
# correct confidence intervals
# correct p-values
summary(pool(imputed_fit_with_transformations))
```

```{r average of *transformed imputed datasets}
# average dataset of all 5 imputed datasets
# extracts all 5 imputed datasets, then creates one final dataset where every numeric column is replaced by the row-wise average across the 5 imputations, while keeping non-numeric columns unchanged.
transformed_imputed_dfs <- lapply(1:5, function(i) complete(imp_transformed, i))

num_cols_transformed <- names(transformed_imputed_dfs[[1]])[sapply(transformed_imputed_dfs[[1]], is.numeric)]
averaged_transformed_imputed_data <- transformed_imputed_dfs[[1]]

averaged_transformed_imputed_data[num_cols_transformed] <- lapply(num_cols_transformed, function(v) {
  rowMeans(sapply(transformed_imputed_dfs, `[[`, v), na.rm = TRUE)
})

averaged_transformed_imputed_data
```

# Note:

averaged_transformed_imputed_data is used as analytic dataset

```{r transformed univariate plots to look at data quality}
# get rid of large points to see distribution better, will be included in modeling
limited_popdens <- averaged_transformed_imputed_data %>% 
       filter(averaged_transformed_imputed_data$popdens < 1000)

num_long <- limited_popdens %>%
    select(where(is.numeric)) %>%
    pivot_longer(cols = everything(), names_to = "variable", values_to = "value")

ggplot(num_long, aes(x = value)) +
    geom_histogram(bins = 50, color = "black") +
    facet_wrap(~ variable, scales = "free_x") +
    labs(title = "Distributions of Numeric Variables", x = NULL, y = "Count") +
    theme_minimal()
# this is much better after transformations applied

cat_long <- limited_popdens %>%
    select(where(is.factor)) %>%
    pivot_longer(cols = everything(), names_to = "variable", values_to = "value")

# this one will be the same
ggplot(cat_long, aes(x = value)) +
    geom_bar(color = "black", fill = "grey70") +
    facet_wrap(~ variable, scales = "free_x") +
    labs(title = "Distributions of Categorical Variables", x = NULL, y = "Count") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
```

# Bivariate plots

```{r plotting again against life expectancy with log transformations and imputations}
# analytic dataset
averaged_transformed_imputed_data

# numeric variables you want grouped together at the end
transformed <- c(
    "freedom_to_make_life_choices_seven",
    "happiness_score_square",
    "log_co2",
    "log_gdp",
    "log_hexp",
    "log_healthcare_per1000")

# remove extreme popdens outliers for better visualization (keep full data for modeling)
limited_popdens <- averaged_transformed_imputed_data %>%
    filter(popdens < 1000)

# convert numeric predictors into long format (KEEP country + outcome!)
num_long_limited_popdens <- limited_popdens %>%
    select(country, life_expectancy_both, where(is.numeric)) %>%
    pivot_longer(cols = -c(country, life_expectancy_both), names_to = "variable", values_to = "value")

# reorder facets so transformed variables appear together at the end
levels_vec <- c(setdiff(sort(unique(num_long_limited_popdens$variable)), transformed),
                intersect(transformed, unique(num_long_limited_popdens$variable))) # only keep ones that exist

num_long_in_order <- num_long_limited_popdens %>%
    mutate(variable = factor(variable, levels = levels_vec))

# label Nauru (and only where it exists in the filtered/long data)
label_df <- num_long_in_order %>%
    filter(country %in% c("Nauru"))

# remove extreme popdens outliers for better visualization, add them back in for modeling
limited_popdens <- averaged_transformed_imputed_data %>% 
       filter(averaged_transformed_imputed_data$popdens < 1000)
# SAME THING AGAIN BUT REMOVING SINGAPORE TO SEE POPDENS (this is a temporary removal)
# convert all numeric predictors into long format
num_long_limited_popdens <- limited_popdens %>% 
    select(country, life_expectancy_both, where(is.numeric)) %>%
    pivot_longer(-c(country, life_expectancy_both), names_to  = "variable", values_to = "value")

num_long_in_order_2 <- num_long_limited_popdens %>%
    mutate(variable = factor(variable, levels = c(setdiff(sort(unique(variable)), transformed), transformed)))
      # ^ puts transformed ones at the end as a group

# plot all numeric variables against life expectancy
ggplot(num_long_in_order_2, aes(x = value, y = life_expectancy_both)) +
    geom_point(alpha = .4, size = 1) +
    geom_smooth(method = "lm",   se = FALSE, color = "darkgreen", linewidth = 0.7) +
    geom_smooth(method = "loess", se = FALSE, color = "red", linewidth = 0.7) +
    geom_text_repel(data = label_df, aes(x = value, y = life_expectancy_both, label = country), size = 2, min.segment.length = 0) +
    facet_wrap(~ variable, scales = "free_x") +
    labs(
        title = "Life Expectancy vs Numeric Variables", # imputed data averaged
        x = "Value of Variable (some transformations applied)",
        y = "Life Expectancy") +
    theme_minimal(base_size = 8) +
    theme(panel.grid.minor = element_blank(), strip.text = element_text(face = "bold"), plot.title = element_text(face = "bold", hjust = 0.5))
# so much better than untransformed plots
# linear enough, good for MLR
```

```{r bivariate boxplots for categorical vars}
# continent
ggplot(averaged_transformed_imputed_data, aes(x = continent, y = life_expectancy_both)) +
    geom_boxplot() +
    labs(title = "Life Expectancy by Continent", x = "Continent", y = "Life expectancy (both sexes)") +
    theme_minimal()

# income class
ggplot(averaged_transformed_imputed_data, aes(x = income_class, y = life_expectancy_both)) +
    geom_boxplot() +
    labs(title = "Life Expectancy by Income Class", x = "Income class", y = "Life expectancy (both sexes)") +
    theme_minimal()
```

# Multicollinearity

```{r multicollinearity}
# check corrplot for all numeric vars
numerical_data_before_dropping_vars <- averaged_transformed_imputed_data %>% 
    select(adult_literacy_rate, epi, gini_index,
           gov_is_corrupt_yes, log_gdp, log_co2, log_hexp, popdens,
           log_healthcare_per1000, happiness_score_square,
           freedom_to_make_life_choices_seven)
# compute correlation matrix
corr_matrix <- cor(numerical_data_before_dropping_vars, use = "complete.obs")
# plot
corrplot(corr_matrix, method = "color", tl.cex = .5, addCoef.col = "black")
# horrible

# need in order to check VIFs
fit_before_dropping_colinear_vars <- lm(life_expectancy_both ~ adult_literacy_rate + continent + 
                                            income_class + epi + gini_index + popdens + gov_is_corrupt_yes +
                                            log_gdp + log_co2 + log_hexp + log_healthcare_per1000 +
                                            happiness_score_square + freedom_to_make_life_choices_seven,
                                        data = averaged_transformed_imputed_data)

vif(fit_before_dropping_colinear_vars)
# based on these VIFs, making the decision to drop one by one:
# log_gdp (vif = 26.69), once removed, next highest is:
# log_health_expenditure (vif = 12.08), once removed, next highest is:
# log_co2 (vif = 7.55), once removed, next highest is:
# log_healthcare_per1000 (vif = 7.18), once removed, next highest is:
# happiness_score_square (vif = 6.19)
# now all VIFs are ~5 or lower, which is acceptable

# I originally included several healthcare variables, but they were so highly correlated with income and each other that I removed them to avoid multicollinearity... healthcare effects are still captured indirectly through things like income class

# doing this again to recheck VIFs
fit_after_dropping_colinear_vars <- lm(life_expectancy_both ~ adult_literacy_rate + continent + 
                                            income_class + epi + gini_index + popdens + 
                                            gov_is_corrupt_yes + freedom_to_make_life_choices_seven,
                                        data = averaged_transformed_imputed_data)

vif(fit_after_dropping_colinear_vars)
# all less than or equal to 5 now, this was the goal

# check corrplot again for new set of numerical vars
numerical_averaged_transformed_imputed_data_removed_collinear_vars <- averaged_transformed_imputed_data %>% 
    select(adult_literacy_rate, epi, gini_index, gov_is_corrupt_yes, popdens, freedom_to_make_life_choices_seven)
# compute correlation matrix
corr_matrix <- cor(numerical_averaged_transformed_imputed_data_removed_collinear_vars, use = "complete.obs")
# plot
corrplot(corr_matrix, method = "color", tl.cex = .3, addCoef.col = "black")
# much better
```

# Model selection

```{r stepwise}
# run stepwise once on fit_after_dropping_colinear_vars fit with averaged imputed data to get a stable set of predictors
step <- ols_step_both_p(
    fit_after_dropping_colinear_vars,
    penter  = 0.05,   # entry cutoff
    prem    = 0.10,   # removal cutoff
    details = TRUE)
# life_expectancy_both ~ adult_literacy_rate + epi + continent + income_class + gini_index + popdens 
# adj R^2 = 0.756

# imputed fit
stepwise_fit <- with(imp_transformed, lm(life_expectancy_both ~ adult_literacy_rate + continent + income_class + epi + gini_index + popdens))
# model with averaged imputed data for anova
stepwise_basic_lm <- lm(life_expectancy_both ~ adult_literacy_rate + continent + income_class + epi + gini_index + popdens,
                        averaged_transformed_imputed_data)

summary(pool(stepwise_fit))
Anova(stepwise_basic_lm, type = 2)

# testing if popdens should stay in the model or if it is not necessary
reduced_stepwise_model <- with(imp_transformed, lm(life_expectancy_both ~ adult_literacy_rate + 
                                                       continent + income_class + epi + gini_index))

stepwise_basic_lm_no_popdens <- lm(life_expectancy_both ~ adult_literacy_rate + continent + income_class + epi + gini_index,
                                   averaged_transformed_imputed_data)

anova(stepwise_fit, reduced_stepwise_model)
# high p-value, use the reduced model, no popdens

# to compare adj-R^2 of 2 fits
pool.r.squared(stepwise_fit, adjusted = TRUE)
pool.r.squared(reduced_stepwise_model, adjusted = TRUE)
# they are practically the same, so removing popdens is fine

summary(pool(reduced_stepwise_model))
Anova(stepwise_basic_lm_no_popdens, type = 2)

# when we remove popdens, gini becomes insignificant at the .05 level, test it with the new reduced model
reduced_again_stepwise_model_no_gini <- with(imp_transformed, lm(life_expectancy_both ~ adult_literacy_rate + 
                                                                  continent + income_class + epi))

anova(reduced_stepwise_model, reduced_again_stepwise_model_no_gini)
# high p-value, use the second reduced model, no popdens or gini
# p-val = .127
pool.r.squared(reduced_again_stepwise_model_no_gini, adjusted = TRUE)
# Adj-R^2 only drops from .752 to .748, so it's fine to remove gini too

# basic model for anova
stepwise_basic_lm_no_popdens_no_gini <- lm(life_expectancy_both ~ adult_literacy_rate + continent + income_class + epi,
                        averaged_transformed_imputed_data)

# looking at new model
summary(pool(reduced_again_stepwise_model_no_gini))
Anova(stepwise_basic_lm_no_popdens_no_gini, type = 2)
# all variables significant now
# check to see if adj-R^2 changed
pool.r.squared(reduced_again_stepwise_model_no_gini, adjusted = TRUE)
```

```{r best subset exact same process and results as above}
# for all imputed datasets
model_for_best_subset <- with(imp_transformed, lm(life_expectancy_both ~ adult_literacy_rate + epi +
                                                      freedom_to_make_life_choices_seven + gini_index +
                                                      gov_is_corrupt_yes + popdens + continent + income_class))
# list of lm objects, one per imputed dataset
models <- model_for_best_subset$analyses

# run best subset on each lm
best_list <- purrr::map(models, ols_step_best_subset)
# based on what predictors get picked most frequently in each of the datasets best subset, I refitted a model with the best subset 
# predictors based on all 5 datasets
# used best combination of AIC and adj-R^2 to pick models

best_subset_model <- with(imp_transformed, lm(life_expectancy_both ~ adult_literacy_rate + epi + gini_index + 
                                                  popdens + continent + income_class))
summary(pool(best_subset_model))

# do we need popdens since it's not significant?
best_subset_model_reduced_no_popdens <- with(imp_transformed, lm(life_expectancy_both ~ adult_literacy_rate + 
                                                                     epi + gini_index + continent + income_class))

anova(best_subset_model, best_subset_model_reduced_no_popdens)
# p-val = .1717, remove popdens

# remove gini since it becomes insignificant?
best_subset_model_reduced_no_popdens_gini <- with(imp_transformed, lm(life_expectancy_both ~ adult_literacy_rate + 
                                                                     epi + continent + income_class))

anova(best_subset_model_reduced_no_popdens, best_subset_model_reduced_no_popdens_gini)
# p-val .127, remove gini too
```

# Best model so far

```{r best model so far}
# pooled model
summary(pool(best_subset_model_reduced_no_popdens_gini))

# basic model with averaged data needed for anova
model_testing_categorical_vars_as_one <- lm(life_expectancy_both ~ adult_literacy_rate + epi + continent + income_class,
                                            data = averaged_transformed_imputed_data)

Anova(model_testing_categorical_vars_as_one, type = 2)
# full categorical vars significant 
```

# Testing interactions

```{r testing interactions with anova}
# best model from stepwise and best subset selection
summary(pool(best_subset_model_reduced_no_popdens_gini))

# testing adult literacy rate and income class interaction
literacy_income_interaction <- with(imp_transformed, lm(life_expectancy_both ~ adult_literacy_rate + epi + continent + income_class +
                                                            adult_literacy_rate:income_class))
summary(pool(literacy_income_interaction))

anova(literacy_income_interaction, best_subset_model_reduced_no_popdens_gini)
# p-val is .0526, cant reject null, don't include interaction

# testing adult literacy rate and continent interaction
literacy_continent_interaction <- with(imp_transformed, lm(life_expectancy_both ~ adult_literacy_rate + 
                                                               epi + continent + income_class + adult_literacy_rate:continent))

anova(literacy_continent_interaction, best_subset_model_reduced_no_popdens_gini)
# p-val = 0.077, reject null, cant reject null, don't include interaction

# testing epi and income class interaction
epi_income_interaction <- with(imp_transformed, lm(life_expectancy_both ~ adult_literacy_rate + epi + continent + income_class + 
                                                       epi:income_class))

anova(epi_income_interaction, best_subset_model_reduced_no_popdens_gini)
# p-val = .246, cannot reject null, don't include interaction
```

# Normality & EV

```{r residual plots}
# linear fit
# basic model with averaged data needed for anova
model_testing_categorical_vars_as_one <- lm(life_expectancy_both ~ adult_literacy_rate + epi + continent + income_class,
                                            data = averaged_transformed_imputed_data)

# check plot for normality 
qqnorm(residuals(model_testing_categorical_vars_as_one))
qqline(residuals(model_testing_categorical_vars_as_one), col = "red", lwd = 2)
# check residual plot for equal variance
plot(model_testing_categorical_vars_as_one, which = 1)
# these look pretty good


# trying to see if plots look even better with a transformation on y
# box-cox transformation to find optimal transformation for y to fix assumptions
bc <- boxCox(model_testing_categorical_vars_as_one)
# extract optimal lambda 
bc$x[which.max(bc$y)]
# 2

# trying to fit the model with the recommended squared transformation
# squared fit
squared_fit <- lm((life_expectancy_both)^2 ~ adult_literacy_rate + continent + 
                                            income_class + epi,
                                        data = averaged_transformed_imputed_data)

# checking plots again
qqnorm(residuals(squared_fit))
qqline(residuals(squared_fit), col = "red", lwd = 2)
plot(squared_fit, which = 1)
# they look very similar to the other ones
# since the transformation didn't improve the assumptions that much,
# stick with the original fit for interpretability
```

# Influential points

```{r checking for influential points}
n <- nrow(averaged_transformed_imputed_data)

cooks.distance(model_testing_categorical_vars_as_one) %>%
    enframe(name = "row", value = "cooks") %>%
    filter(cooks > 4/n)

4/n
# .0215

plot(model_testing_categorical_vars_as_one, which = 4)

averaged_transformed_imputed_data$country[118]
# Nauru

# will do a sensitivity analysis on Nauru  
```

# Sensitivity analysis 1

```{r sensitivity analysis without Nauru}
# start by doing imputation without Nauru 
data_imp_no_nauru <- clean_data_for_impute %>%
    mutate(continent = factor(continent), 
           income_class = factor(income_class),

    log_gdp = log(gdp_per_capita),
    log_co2 = log(co2_per_capita),
    log_hexp = log(health_expenditure_per_capita),
    log_popdens = log(popdens),
    log_healthcare_per1000 = log(healthcare_professionals_per_1000),
    happiness_score_square = happiness_score^2,
    freedom_to_make_life_choices_six = freedom_to_make_life_choices^6) %>% 
    select(-gdp_per_capita, 
           -co2_per_capita,
           -health_expenditure_per_capita,
           -popdens,
           -healthcare_professionals_per_1000,
           -happiness_score,
           -freedom_to_make_life_choices) %>% 
    filter(country != "Nauru")

# same imputation process as above
ini_reduced_data  <- mice(data_imp_no_nauru, maxit = 0)
meth_reduced_data <- ini_reduced_data$method
pred_reduced_data <- ini_reduced_data$predictorMatrix

meth_reduced_data["country"] <- ""

pred_reduced_data[, "country"] <- 0
pred_reduced_data["country", ] <- 0

meth_reduced_data

pred_reduced_data["country", ]
pred_reduced_data[, "country"]


imp_reduced_data <- mice(
    data_imp_no_nauru,
    m = 5,                   # number of imputed datasets
    method = meth_reduced_data,
    predictorMatrix = pred_reduced_data,
    maxit = 20,               # iterations 
    seed = 123)

densityplot(imp_reduced_data) # compares original vs. imputed distributions

fit_no_nauru <- with(imp_reduced_data, lm(life_expectancy_both ~ adult_literacy_rate + epi + income_class + continent))


# pooled fit
summary(pool(fit_no_nauru))
# look at differences compared to good model below
summary(pool(best_subset_model_reduced_no_popdens_gini))

# sensitivity analysis: to assess the influence of Nauru, I re-estimated the final pooled multiple-imputation model after removing this observation. parameter estimates, standard errors, and p-values were changed, but in very small magnitudes (all coefficient changes < 5% besides Oceania). no predictor changed sign or significance. these results indicate that our findings are robust to this influential data point

# oceania: small group size, and its standard error is ~1.28 in both models
# nauru is one of 14 countries in oceania so the sensitivity is expected
```

# Sensitivity analysis 2

```{r sensitivity analysis removing NAs instead of imputing}
# dataset with NAs, 196 countries
merged_with_nas

colSums(is.na(merged_with_nas))

# perform transformations so the process is identical
sens_anal_no_nas <- merged_with_nas %>% 
    drop_na() %>%
    select(-happiness_score, -health_expenditure_per_capita, -gdp_per_capita, -healthcare_professionals_per_1000, -co2_per_capita)
sens_anal_no_nas$freedom_to_make_life_choices_seven <- sens_anal_no_nas$freedom_to_make_life_choices^7 
sens_anal_no_nas$popdens <- as.numeric(sens_anal_no_nas$popdens)

# limits to 105 countries
dropped_nas_model <- lm(life_expectancy_both ~ adult_literacy_rate + epi + gini_index + popdens + continent + 
                            income_class + freedom_to_make_life_choices_seven + gov_is_corrupt_yes, 
                        data = sens_anal_no_nas)

ols_step_best_subset(dropped_nas_model)
# R^2 =.8120 with model: adult_literacy_rate epi popdens continent income_class 
```

# Final fit

```{r FINAL FIT}
Anova(model_testing_categorical_vars_as_one, type = 2)
# pooled
the_end_model <- best_subset_model_reduced_no_popdens_gini
summary(pool(the_end_model))
# significant predictors:
# adult literacy rate
# epi
# continent
# income class

# R^2 and adj-R^2 checks
pool.r.squared(the_end_model, adjusted = TRUE)
# adj-R^2 = 0.748
pool.r.squared(the_end_model, adjusted = FALSE)
# R^2 estimate = .762
```

# Final viz

```{r final viz}
summary(pool(the_end_model))
Anova(model_testing_categorical_vars_as_one, type = 2)

# finding average adult literacy rate so I can split it at average for visualization
averaged_transformed_imputed_data %>%
    summarize(avg = mean(adult_literacy_rate))
# 86.17

# splitting the literacy data to better show it on the graph
split_literacy_data <- averaged_transformed_imputed_data %>% 
    mutate(literacy_group = ifelse(adult_literacy_rate >= 86.17, "Above Average Literacy", "Below Average Literacy"),
           income_class = factor(income_class, levels = c("Low-income countries",
                                                          "Lower-middle-income countries",
                                                          "Upper-middle-income countries",
                                                          "High-income countries")))
# count countries per continent
continent_counts <- split_literacy_data %>%
    distinct(country, continent) %>%
    count(continent, name = "n_countries")
# custom facet labels 
facet_labels <- continent_counts %>%
    mutate(label = paste0(continent, " (n = ", n_countries, ")")) %>%
    select(continent, label)

# final viz
ggplot(split_literacy_data, aes(x = epi, y = life_expectancy_both)) +
    geom_point(aes(color = income_class, fill = income_class, shape = literacy_group), 
               alpha = .8, size = 2.5, stroke = .25,
               position = position_jitter(width = 0.4, height = 0.2, seed = 1)) +
    geom_smooth(method = "lm", se = FALSE, linewidth = 0.2, color = "grey") +
    scale_shape_manual(values = c("Below Average Literacy" = 24, "Above Average Literacy" = 21)) +
    labs(title = "Life Expectancy and Environmental Performance by Income, Literacy, and Continent", 
         x = "Environmental Performance Index (EPI)", y = "Life Expectancy (years)",
         fill = "Income Group", color = "Income Group", shape = "Adult Literacy Group (Average = 86.17%)") +
    facet_wrap(~ continent, labeller = labeller(continent = setNames(facet_labels$label, facet_labels$continent))) +
    theme_minimal(base_size = 8) +
    theme(panel.grid.minor = element_blank(), strip.text = element_text(face = "bold"), legend.position = "bottom", 
          legend.box = "vertical", plot.title = element_text(hjust = 0.5, face = "bold")) 
```

# Random calcs

```{r random calculations for presentation}
averaged_transformed_imputed_data %>%
    group_by(income_class) %>%
    summarize(avg = mean(life_expectancy_both))


averaged_transformed_imputed_data %>%
    summarize(avg = mean(life_expectancy_both))

merged_with_nas %>%
  filter(life_expectancy_both == min(life_expectancy_both, na.rm = TRUE)) %>%
  select(country, life_expectancy_both)

merged_with_nas %>%
  filter(life_expectancy_both == max(life_expectancy_both, na.rm = TRUE)) %>%
  select(country, life_expectancy_both)
```

```{r}
write.csv(averaged_untransformed_imputed_data, "shiny_app_data.csv", row.names = TRUE)

averaged_untransformed_imputed_data |> 
    filter(country %in% c("Western Sahara"  ,

"East Timor " ,

"North Korea  ",

"Taiwan",  

"Antarctica",  

"Somaliland",  

"Kosovo  "))
```
