library(data.table)
library(openxlsx)
library(countrycode)
library(rsdmx)
library(janitor)
library(haven)
library(dplyr)
library(magrittr)
library(tidyr)
library(tibble)
library(readr)

# questions -----

# methodology for calc gallup dat, slight variation between previous response
# Household from 2010 USD -> 2015 US$ (same with pop)
# safely managed sanitation -> improved (this is debatable and could be automated potentially)
# School enrollment, secdonary (NET)
# Feeling about household income: which feeling?

# WDI pull -----

data_wellness_WB <- readxl::read_excel("./data/data_wellness_WB.xlsx", sheet="ALB")

gallup_indic <- data_wellness_WB %>% filter(Source == "Gallup") %>% select(indicator, indicator_code)
code_dict <- data_wellness_WB %>% select(indicator_code, indicator_code2) %>% drop_na() %>% rbind(., data.frame(indicator_code = "POP", indicator_code2 = "SP.POP.TOTL"))

wdi_dat <- WDI::WDI(indicator = c("NE.CON.PRVT.PC.KD", "SI.POV.DDAY", "SL.EMP.TOTL.SP.ZS", "SL.UEM.TOTL.ZS", "SL.EMP.VULN.ZS",
                                  "SE.SEC.NENR", "SE.ADT.LITR.ZS", "SP.DYN.LE00.IN", "VC.IHR.PSRC.P5", "per_allsp.cov_pop_tot",
                                  "SP.POP.TOTL"))

labs_list <- lapply(wdi_dat, attr, "label")
labs_list <- labs_list %>% unlist() %>% as.data.frame() %>% rownames_to_column() %>% rename("indicator_code" = 1, "indicator" = 2)

wdi_tidy <- wdi_dat %>%
  select(-iso2c, -iso3c) %>%
  pivot_longer(!c(country, year), names_to="indicator_code") %>%
  merge(., labs_list, by="indicator_code") %>%
  mutate(country = countrycode(country, "country.name", "country.name")) %>%
  drop_na(country) %>%
  arrange(indicator, country, year) %>%
  drop_na(value) %>%
  mutate(source = "WDI") %>%
  rename("indicator_code2" = "indicator_code") %>%
  left_join(., code_dict, by="indicator_code2") %>%
  mutate(indicator =
           case_when(
             indicator == "School enrollment, secondary (% net)" ~ "School enrollment, secondary (NET)",
             TRUE ~ indicator
           ))


# OECD pull -----

url <- "https://stats.oecd.org/restsdmx/sdmx.ashx/GetData/EXP_PM2_5/AUS+AUT+BEL+CAN+CHL+COL+CRI+CZE+DNK+EST+FIN+FRA+DEU+GRC+HUN+ISL+IRL+ISR+ITA+JPN+KOR+LVA+LTU+LUX+MEX+NLD+NZL+NOR+POL+PRT+SVK+SVN+ESP+SWE+CHE+TUR+GBR+USA+OECDAO+EA19+EU28+EU27_2020+G7M+G20+OECDE+OECD+WLD+NMEC+AFG+ALB+DZA+ASM+AND+AGO+AIA+ATG+ARG+ARM+ABW+AZE+BHS+BHR+BGD+BRB+BLR+BLZ+BEN+BMU+BTN+BOL+BIH+BWA+BRA+VGB+BRN+BGR+BFA+BDI+KHM+CMR+CPV+CYM+CAF+TCD+CHN+CXR+CCK+COM+COG+COD+COK+CIV+HRV+CUB+CYP+DJI+DMA+DOM+ECU+EGY+SLV+GNQ+ERI+ETH+FRO+FLK+FJI+PYF+GAB+GMB+GEO+GHA+GIB+GRL+GRD+GUM+GTM+GIN+GNB+GUY+HTI+VAT+HND+HKG+IND+IDN+IRN+IRQ+GGY+IMN+JAM+JEY+JOR+KAZ+KEN+PRK+KIR+KWT+KGZ+LAO+LBN+LSO+LBR+LBY+LIE+MAC+MDG+MWI+MYS+MDV+MLI+MLT+MHL+MRT+MUS+FSM+MDA+MCO+MNG+MNE+MSR+MAR+MOZ+MMR+NAM+NRU+NPL+ANT+NCL+NIC+NER+MKD+NGA+NIU+NFK+MNP+PSE+OMN+PAK+PLW+PAN+PNG+PRY+PER+PHL+PCN+PRI+QAT+ROU+RUS+RWA+SHN+KNA+LCA+SPM+VCT+WSM+SMR+STP+SAU+SEN+SRB+SYC+SLE+SGP+SLB+SOM+ZAF+LKA+SDN+SUR+SSD+SJM+SWZ+SYR+TWN+TJK+TZA+THA+TLS+TGO+TKL+TON+TTO+TUN+TKM+TCA+TUV+UGA+UKR+ARE+URY+UZB+VUT+VEN+VNM+VIR+WLF+YEM+ZMB+ZWE+GRPS+LAC+MENA+ESH+ASEAN+BRIICS+OECDAM+EECCA.TOTAL.TOTAL.PWM_EX/all?startTime=1990&endTime=2022"

oecd_dat <- readSDMX(url) %>% as.data.frame()

oecd_tidy <- oecd_dat %>%
  select(country = COU, year = obsTime, value = obsValue) %>%
  mutate(country = countrycode(country, "iso3c", "country.name"),
         indicator = "Mean population exposure to PM2.5",
         indicator_code2 = NA,
         indicator_code = "ENVI2",
         source = "OECD",
         year = as.numeric(year)) %>%
  drop_na(country)

# WHO pull -----

url <- "https://ghoapi.azureedge.net/api/WSH_SANITATION_SAFELY_MANAGED"

who_dat <- jsonlite::fromJSON(url) %>% as.data.frame()

who_tidy <- who_dat %>%
  clean_names() %>%
  filter(value_dim1 == "TOTL" & value_spatial_dim_type == "COUNTRY") %>%
  select(country = value_spatial_dim, year = value_time_dim, value = value_value) %>%
  mutate(country = countrycode(country, "iso3c", "country.name"),
         indicator = "Population using safely managed sanitation services (%)",
         indicator_code = "HOUS2", indicator_code2 = NA, source = "WHO",
         year = as.numeric(year))

# FAO pull -----

# Land use data
url <-"https://fenixservices.fao.org/faostat/static/bulkdownloads/Inputs_LandUse_E_All_Data.zip"

temp <- tempfile()
download.file(url, temp)
unzip(temp, "Inputs_LandUse_E_All_Data.csv")
landuse <- read.csv("Inputs_LandUse_E_All_Data.csv")
unlink(temp)

#Agriculture area under organic agric.

land_tidy <- landuse %>%
  clean_names() %>%
  filter(item == "Forest land" & element == "Area") %>%
  select(country = area, starts_with("y"), -ends_with("f")) %>%
  pivot_longer(!c(country), names_to="year", names_transform = list(year = parse_number)) %>%
  filter(!country %in% c("China, mainland")) %>%
  mutate(country = countrycode(country, "country.name", "country.name")) %>%
  drop_na() %>%
  arrange(country, year) %>%
  group_by(country) %>%
  mutate(value = ((value/lag(value,9))^(1/10)) - 1,
         value = value * 100) %>%
  drop_na(value) %>%
  mutate(indicator = "Annual deforestation (average annual change in forest land, 10 years)",
         indicator_code = "ENVI1",
         indicator_code2 = "FAO.DEFORES.CHANGE",
         source = "FAO",
         country = countrycode(country, "country.name", "country.name")) %>%
  drop_na(country)

# Transparency -----

url <- "https://images.transparencycdn.org/images/CPI-2021-Full-Data-Set.zip"

temp <- tempfile()
download.file(url, temp)
unzip(temp, "CPI 2021 Full Data Set/CPI2021_GlobalResults&Trends.xlsx")
transparency_dat <- readxl::read_excel("CPI 2021 Full Data Set/CPI2021_GlobalResults&Trends.xlsx", skip=2)
unlink(temp)

trans_tidy <- transparency_dat %>%
  clean_names() %>%
  select(country = country_territory, value = cpi_score_2021) %>%
  mutate(year = 2021,
         indicator = "Corruption index",
         indicator_code = "EMPO1",
         indicator_code2 = NA,
         country = countrycode(country, "country.name", "country.name"),
         source = "Transparency")

# unesco load -----

url <- "http://data.uis.unesco.org/RestSDMX/sdmx.ashx/GetData/NATMON_DS/MYS_1T8_AG25T99+MYS_1T8_AG25T99_F+MYS_1T8_AG25T99_M.AFG+ALA+ALB+DZA+ASM+AND+AGO+AIA+ATG+ARG+ARM+ABW+AUS+AUT+AZE+BHS+BHR+BGD+BRB+BLR+BEL+BLZ+BEN+BMU+BTN+BOL+BIH+BWA+BRA+VGB+BRN+BGR+BFA+BDI+KHM+CMR+CAN+CPV+CYM+CAF+TCD+ZZA+CHL+CHN+HKG+MAC+COL+COM+COG+COK+CRI+CIV+HRV+CUB+CUW+CYP+CZE+PRK+COD+DNK+DJI+DMA+DOM+ECU+EGY+SLV+GNQ+ERI+EST+SWZ+ETH+FRO+FLK+FJI+FIN+FRA+GUF+PYF+GAB+GMB+GEO+DEU+GHA+GIB+GRC+GRL+GRD+GLP+GUM+GTM+GGY+GIN+GNB+GUY+HTI+VAT+HND+HUN+ISL+IND+IDN+IRN+IRQ+IRL+IMN+ISR+ITA+JAM+JPN+JEY+JOR+KAZ+KEN+KIR+KWT+KGZ+LAO+LVA+LBN+LSO+LBR+LBY+LIE+LTU+LUX+MDG+MWI+MYS+MDV+MLI+MLT+MHL+MTQ+MRT+MUS+MYT+MEX+FSM+MCO+MNG+MNE+MSR+MAR+MOZ+MMR+NAM+NRU+NPL+NLD+ANT+NCL+NZL+NIC+NER+NGA+NIU+NFK+MKD+MNP+NOR+OMN+PAK+PLW+PSE+PAN+PNG+PRY+PER+PHL+PCN+POL+PRT+PRI+QAT+KOR+MDA+REU+ROU+RUS+RWA+SHN+KNA+LCA+SPM+VCT+BLM+MAF+WSM+SMR+STP+SAU+SEN+SRB+SYC+SLE+SGP+SXM+SVK+SVN+SLB+SOM+ZAF+SSD+ESP+LKA+SDN+XDN+SUR+SJM+SWE+CHE+SYR+TJK+THA+TLS+TGO+TKL+TON+TTO+TUN+TUR+TKM+TCA+TUV+UGA+UKR+ARE+GBR+TZA+USA+VIR+URY+UZB+VUT+VEN+VNM+WLF+ESH+YEM+ZMB+ZWE+SDG+40675+40330+40334+40344+40606+40617+40603+40614+40618+40612+40616+40619+40611+40613+40630+40650+40651+40656+40642+40620+40640+UIS+40510+40525+40530+40505+40515+40520+40500+40535+40540+40550+WB+40044+40042+40030+40043+40041/all?startTime=2016&endTime=2022"

educ_dat <- readSDMX(url) %>% as.data.frame()

educ_tidy <- educ_dat %>%
  clean_names() %>%
  filter(natmon_ind == "MYS_1T8_AG25T99") %>%
  select(country = location, year = obs_time, value = obs_value) %>%
  mutate(indicator = "Mean years of schooling (ISCED 1 or higher), population 25+ years, both sexes",
         indicator_code = "EDUC4", indicator_code2 = NA, source = "UNESCO",
         country = countrycode(country, "iso3c", "country.name"))


# Should probably splice this with above data so that more historical dat is available
# urluis<-"ftp://ftp.uis.unesco.org/BDDS/NATMON.zip"
#
# temp <- tempfile()
# download.file(urluis, temp)
# unzip(temp, "NATMON_DATA_NATIONAL.csv")
# unzip(temp, "NATMON_LABEL.csv")
# uis <- read.csv("NATMON_DATA_NATIONAL.csv")
# # Code labels FOUND HERE
# uis.label <- read.csv("NATMON_LABEL.csv")
# unlink(temp)
# educ_tidy <- educ_dat %>%
#   clean_names() %>%
#   filter(indicator_id == "MYS.1T8.AG25T99") %>%
#   select(country = country_id, year, value) %>%
#   mutate(indicator = "Mean years of schooling (ISCED 1 or higher), population 25+ years, both sexes",
#          indicator_code = "EDUC4", indicator_code2 = NA, source = "UNESCO",
#          country = countrycode(country, "iso3c", "country.name"))

# gallup load -----

dat <- data.table::fread("./data/gallup_db.csv") %>% filter(source == "Gallup")

gallup_tidy <- dat %>%
  filter(source == "Gallup") %>%
  mutate(indicator =
           case_when(
             indicator == "Satisfaction with housing (city)" ~ "Satisfaction with housing",
             indicator == "Satisfaction with education system" ~ "Satisfaction with the educational system",
             indicator == "No health problems" ~ "No health problem",
             indicator == "Feeling of safety" ~ "Feeling safe walking alone at night",
             indicator == "Has someone to count on to help" ~ "Someone to count on to help",
             indicator == "Voicing opinions to officials" ~ "Voicing opinion to official",
             TRUE ~ indicator
           )) %>%
  filter(indicator %in% c(gallup_indic$indicator)) %>%
  select(indicator, country, year, value = VALUE) %>%
  mutate(indicator_code2 = NA, source = "Gallup") %>%
  merge(., gallup_indic, by = "indicator") %>%
  filter(!indicator %in% c("Voicing opinion to official", "No health problem", "Life satisfaction",
                           "Feeling about household income", "Corruption is widespread throughout the government")) %>%
  distinct()

# Additional gallup data -----

# Full gallup world poll
# Takes long time to run
# full_gallup <-  read_dta("~/Downloads/Gallup (1).dta")
#
# vals <- lapply(full_gallup, attr, "label") %>% unlist()
# vals[grepl("Index", vals)]
#
# dat_short <- full_gallup %>%
#   select(country = WP5, date = FIELD_DATE, "road" = WP92, "living" = WP30, "corruption" = WP146, income = WP2319,
#          health = WP23, opinion = WP111, life = INDEX_LE)
#
# saveRDS(dat_short, "short_df.RDS")

dat_short <- readRDS("./data/short_df.RDS")

temp <- lapply(unique(dat_short$country), attr, "labels")[[1]] %>% unlist()
code_dict <- cbind(country = names(temp), code = temp)
rownames(code_dict) <- c()

dat_tidy <- dat_short %>%
  rename("code" = country) %>%
  mutate(code = as.character(code)) %>%
  merge(., code_dict, by = "code") %>%
  select(-code) %>%
  select(country, date, everything())

colnames_df <- colnames(dat_tidy[,-c(1,2)])

gallupCleaner <- function(dat_tidy, colnames_df, val) {

  var_name <- colnames_df[val]
  temp_df <- dat_tidy %>% select(country, date, var_name)

  temp <- lapply(unique(temp_df[,3]), attr, "labels")[[1]] %>% unlist()
  temp_code <- cbind(lab = names(temp), code = temp)
  rownames(temp_code) <- c()

  temp_fin <- temp_df %>%
    rename("code" = 3) %>%
    merge(., temp_code, by="code") %>%
    select(-code) %>%
    mutate(year = format(date, "%Y")) %>%
    select(-date) %>%
    select(country, year, {{var_name}} := lab) %>%
    mutate(country = case_when(
      country == "Northern Cyprus" ~ "Cyprus",
      TRUE ~ country
    )) %>%
    group_by(country, year) %>%
    count(!!!syms(var_name)) %>%
    mutate(tot = sum(n),
           pct = (n/tot)*100) %>%
    ungroup()

  return(temp_fin)

}

road_dat <- gallupCleaner(dat_tidy, colnames_df, 1)
living_dat <- gallupCleaner(dat_tidy, colnames_df, 2)
corruption_dat <- gallupCleaner(dat_tidy, colnames_df, 3)
income_dat <- gallupCleaner(dat_tidy, colnames_df, 4)
health_dat <- gallupCleaner(dat_tidy, colnames_df, 5)
opinion_dat <- gallupCleaner(dat_tidy, colnames_df, 6)
# life_dat <- gallupCleaner(dat_tidy, colnames_df, 7)

road_tidy <- road_dat %>%
  filter(road == "Satisfied") %>%
  select(country, year, value = pct) %>%
  arrange(country, year) %>%
  mutate(indicator = "Satisfaction with roads and highways",
         indicator_code = "HOUS3", indicator_code2 = NA,
         source = "Gallup")

living_tidy <- living_dat %>%
  filter(living == "Satisfied") %>%
  select(country, year, value = pct) %>%
  arrange(country, year) %>%
  mutate(indicator = "Satisfaction with standard of living",
         indicator_code = "CONS2", indicator_code2 = NA,
         source = "Gallup")

corruption_tidy <- corruption_dat %>%
  filter(corruption == "Yes") %>%
  select(country, year, value = pct) %>%
  arrange(country, year) %>%
  mutate(indicator = "Corruption is widespread throughout the government",
         indicator_code = "EMPO3", indicator_code2 = NA,
         source = "Gallup")

income_tidy <- income_dat %>%
  filter(income %in% c("Getting by on present income", "Living comfortably on present income")) %>%
  group_by(country, year) %>%
  mutate(n = sum(n),
         pct = (n/tot)*100) %>%
  slice(1) %>%
  ungroup() %>%
  select(country, year, value = pct) %>%
  arrange(country, year) %>%
  mutate(indicator = "Feeling about household income",
         indicator_code = "CONS3", indicator_code2 = NA,
         source = "Gallup")

health_tidy <- health_dat %>%
  filter(health == "No") %>%
  select(country, year, value = pct) %>%
  arrange(country, year) %>%
  mutate(indicator = "No health problem",
         indicator_code = "HEAL4", indicator_code2 = NA,
         source = "Gallup")

opinion_tidy <- opinion_dat %>%
  filter(opinion == "Yes") %>%
  select(country, year, value = pct) %>%
  arrange(country, year) %>%
  mutate(indicator = "Voicing opinion to official",
         indicator_code = "EMPO5", indicator_code2 = NA,
         source = "Gallup")

gallup_ext <- rbind(living_tidy, road_tidy,  opinion_tidy, health_tidy, corruption_tidy, income_tidy) %>%
  mutate(country = countrycode(country, "country.name", "country.name")) %>%
  drop_na(country)

# combine dfs
wellness_dat <- rbind(wdi_tidy, oecd_tidy, who_tidy, trans_tidy, land_tidy, gallup_tidy, gallup_ext, educ_tidy)

# assign lower
lower_wanted <- c("Poverty headcount ratio at $2.15 a day (2017 PPP) (% of population)",
                  "Unemployment, total (% of total labor force) (modeled ILO estimate)",
                  "Vulnerable employment, total (% of total employment) (modeled ILO estimate)",
                  "Mean population exposure to PM2.5",
                  "Intentional homicides (per 100,000 people)",
                  "Corruption index",
                  "Corruption is widespread throughout the government",
                  "Negative experience index")

wellness_tidy <- wellness_dat %>%
  mutate(region = countrycode(country,"country.name", "un.region.name"),
         subregion = countrycode(country,"country.name", "un.regionsub.name"),
         iso3c = countrycode(country,"country.name", "iso3c"),
         lower_wanted = ifelse(indicator %in% lower_wanted, 1, 0),
         year = as.numeric(year), value = as.numeric(value)) %>%
  select(indicator_code, indicator_code2, indicator, source, region, subregion, country, iso3c, year, value) %>%
  drop_na(region) %>%
  arrange(indicator_code, country, year)

# openxlsx::write.xlsx(wellness_tidy, "wellness_dat_raw_18nov.xlsx")
# save(wellness_tidy, file = paste0("data/data_", make.names(Sys.time() %>% format(., format=("%Y-%m-%d"))), ".csv"))

write_csv(wellness_tidy, paste0("data/wellness_database", ".csv"))

# git push -f origin master

