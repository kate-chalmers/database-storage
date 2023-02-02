library(WDI)
library(countrycode)
library(tidyverse)
library(Rilostat)
library(rgho)
library(OECD)
library(rsdmx)
library(readxl)
library(haven)
library(jsonlite)
library(janitor)

start.time <- Sys.time()
wdi.vars <- WDI(indicator = c("SL.AGR.EMPL.ZS", "SL.IND.EMPL.ZS", "SL.SRV.EMPL.ZS", "SL.TLF.BASC.ZS", "SH.STA.MALN.ZS",
                              "SL.TLF.INTM.ZS", "SL.TLF.ADVN.ZS", "SH.DYN.AIDS.ZS", "SL.UEM.1524.ZS",
                              "ER.FST.DFST.ZG", "EN.ATM.NOXE.ZG", "EN.ATM.CO2E.PC", "EG.USE.ELEC.KH.PC",
                              "ER.LND.PTLD.ZS", "ER.MRN.PTMR.ZS", "EN.MAM.THRD.NO", "EN.FSH.THRD.NO",
                              "SP.POP.TOTL", "EN.CLC.MDAT.ZS", "mobileaccount.t.d", "SN.ITK.DEFC.ZS",
                              "FB.AST.NPER.ZS", "NY.GDP.TOTL.RT.ZS", "NY.GDP.PCAP.PP.KD", "NY.GDP.MKTP.PP.KD",
                              "NY.GNP.PCAP.CD", "NV.AGR.TOTL.ZS", "NV.IND.MANF.ZS", "NV.IND.TOTL.ZS",
                              "NV.SRV.TOTL.ZS", "SP.POP.GROW", "EN.POP.DNST", "SP.URB.TOTL.IN.ZS", "SL.EMP.WORK.ZS",
                              "EN.URB.LCTY.UR.ZS", "SP.RUR.TOTL.ZS", "SP.POP.0014.TO.ZS", "SP.POP.1564.TO.ZS",
                              "SP.POP.65UP.TO.ZS", "SP.POP.DPND", "SM.POP.TOTL.ZS", "BX.TRF.PWKR.DT.GD.ZS",
                              "SP.DYN.LE00.IN", "VC.IHR.PSRC.P5", "SG.GEN.PARL.ZS", "IC.FRM.FEMO.ZS",
                              "SP.M18.2024.FE.ZS", "SG.VAW.1549.ZS", "SI.POV.LMIC", "SI.POV.DDAY", "SI.POV.LMIC.GP",
                              "SI.POV.GAPS", "NE.CON.PRVT.ZS", "NY.GDP.MKTP.KD.ZG", "NV.AGR.EMPL.KD", "AG.YLD.CREL.KG",
                              "IC.FRM.OUTG.ZS", "IC.FRM.CMPU.ZS", "FD.AST.PRVT.GD.ZS", "CM.MKT.TRAD.GD.ZS",
                              "CM.MKT.LCAP.GD.ZS", "FB.BNK.CAPA.ZS", "FB.CBK.BRWR.P3", "FS.AST.PRVT.GD.ZS",
                              "FS.AST.DOMS.GD.ZS", "FB.ATM.TOTL.P5", "IC.FRM.BNKS.ZS", "IP.PAT.RESD", "SI.POV.GINI",
                              "SI.DST.05TH.20", "SI.DST.FRST.20", "EG.ELC.ACCS.ZS", "IT.CEL.SETS.P2", "LP.LPI.OVRL.XQ",
                              "IS.AIR.GOOD.MT.K1", "IS.VEH.NVEH.P3", "EG.GDP.PUSE.KO.PP", "EG.IMP.CONS.ZS",
                              "EG.EGY.PRIM.PP.KD", "EG.USE.PCAP.KG.OE", "EG.FEC.RNEW.ZS", "EG.USE.COMM.FO.ZS",
                              "EG.ELC.LOSS.ZS", "EN.ATM.CO2E.PP.GD.KD", "IC.ELC.OUTG", "ER.FSH.CAPT.MT", "EN.CLC.DRSK.XQ",
                              "GC.TAX.TOTL.GD.ZS", "GC.XPN.TRFT.ZS", "IQ.SCI.OVRL", "IC.FRM.CORR.ZS", "IC.BUS.EASE.XQ",
                              "IC.LGL.DURS", "IC.REG.COST.PC.ZS", "IC.REG.DURS", "IC.TAX.DURS", "TM.TAX.MRCH.WM.AR.ZS",
                              "TM.TAX.MRCH.IP.ZS", "FP.CPI.TOTL.ZG", "FR.INR.RINR", "PX.REX.REER", "FI.RES.TOTL.MO",
                              "GC.DOD.TOTL.GD.ZS", "DT.TDS.DECT.EX.ZS", "NE.TRD.GNFS.ZS", "NE.EXP.GNFS.ZS",
                              "TX.VAL.TECH.MF.ZS", "BN.CAB.XOKA.GD.ZS", "NE.GDI.FTOT.ZS", "BX.KLT.DINV.WD.GD.ZS",
                              "BM.KLT.DINV.WD.GD.ZS", "NY.GDS.TOTL.ZS", "IQ.CPA.IRAI.XQ", "SL.EMP.VULN.ZS", "DT.ODA.ODAT.GN.ZS",
                              "GB.XPD.RSDV.GD.ZS", "AG.LND.TOTL.K2", "SL.TLF.CACT.ZS","SL.TLF.CACT.FE.ZS"))

end.time1 <- Sys.time()
time.taken.wdi <- end.time1 - start.time

wdi.vars1 <- wdi.vars %>%
  select(-iso2c) %>%
  mutate(iso3c = countrycode(country, origin="country.name", destination = "iso3c", custom_match = c("Kosovo" = "XKX"))) %>%
  select(!country) %>%
  pivot_longer(!c(iso3c,year), names_to="indicator_code", values_to="VALUE") %>%
  mutate(source = "WDI") %>%
  drop_na() %>%
  arrange(iso3c, indicator_code, year)

benchmark <- wdi.vars1

#--------------------------------
# FAOSTAT datasets
# Notes: Sourced zip links from searching XML files
#--------------------------------

# !! Create temp folder and delete after !!

# Land use data
url.land<-"http://fenixservices.fao.org/faostat/static/bulkdownloads/Environment_LandUse_E_All_Data_(Normalized).zip"

temp <- tempfile()
download.file(url.land, temp)
unzip(temp, "Environment_LandUse_E_All_Data_(Normalized).csv")
landuse <- read.csv("Environment_LandUse_E_All_Data_(Normalized).csv")
unlink(temp)

# % Agricultural
agriland <- landuse[landuse$Item %in% "Agricultural land",]
agriland <- agriland %>% select(Area, Year, Value)
names(agriland)[1:3] <- c("iso3c","year","VALUE")
agriland$indicator_code <- "FAO.AGRI"

# % Arable
arabland <- landuse[landuse$Item %in% "Arable land",]
arabland<-arabland %>% select(Area, Year, Value)
names(arabland)[1:3] <-c("iso3c","year","VALUE")
arabland$indicator_code <- "FAO.ARABLE"

# % Forest
forestland <- landuse[landuse$Item %in% "Forest land",]
forestland<-forestland %>% select(Area, Year, Value)
names(forestland)[1:3] <-c("iso3c","year","VALUE")
forestland$indicator_code <- "FAO.FOREST"

fao <- rbind(agriland,arabland,forestland)
rm(agriland,arabland,forestland)

fao_tidy <- fao %>%
  mutate(iso3c = countrycode(iso3c, "country.name", "iso3c"),
         source = "FAO")

benchmark <- rbind(benchmark, fao_tidy)

#--------------------------------
# UIS datasets
# Source: https://apiportal.uis.unesco.org/bdds
# Notes: No longer updating API - check above site for updated zip links
#--------------------------------

# ******** UIS Education indicators ********
urluis<-"ftp://ftp.uis.unesco.org/BDDS/NATMON.zip"

temp <- tempfile()
download.file(urluis, temp)
unzip(temp, "NATMON_DATA_NATIONAL.csv")
unzip(temp, "NATMON_LABEL.csv")
uis <- read.csv("NATMON_DATA_NATIONAL.csv")
# Code labels FOUND HERE
uis.label <- read.csv("NATMON_LABEL.csv")
unlink(temp)

# Govt exp on educ
educexp <- uis[uis$INDICATOR_ID %in% "XGDP.FSGOV" ,]
educexp<-educexp %>% select(COUNTRY_ID, YEAR, VALUE)
names(educexp)[1:3] <-c("iso3c","year","VALUE")
educexp$indicator_code <- "uis.XPD.TOTL.GD.ZS"

# Primary enroll (net)
netprim <- uis[uis$INDICATOR_ID %in% "NERT.1.CP" ,]
netprim<-netprim %>% select(COUNTRY_ID, YEAR, VALUE)
names(netprim)[1:3] <-c("iso3c","year","VALUE")
netprim$indicator_code <- "uis.PRM.NENR"

# Secondary enroll (gross)
grosecond <- uis[uis$INDICATOR_ID %in% "GER.2T3" ,]
grosecond<-grosecond %>% select(COUNTRY_ID, YEAR, VALUE)
names(grosecond)[1:3] <-c("iso3c","year","VALUE")
grosecond$indicator_code <- "uis.SEC.NENR"

# Enrollment ratio (GPI)
enrolld <- uis[uis$INDICATOR_ID %in% "GER.1T3.GPI" ,]
enrolld<-enrolld %>% select(COUNTRY_ID, YEAR, VALUE)
names(enrolld)[1:3] <-c("iso3c","year","VALUE")
enrolld$indicator_code <- "UIS.ENR.PRSC.FM.ZS"

# Mean schooling
avgschool <- uis[uis$INDICATOR_ID %in% "MYS.1T8.AG25T99" ,]
avgschool<-avgschool %>% select(COUNTRY_ID, YEAR, VALUE)
names(avgschool)[1:3] <-c("iso3c","year","VALUE")
avgschool$indicator_code <- "uis.SCHOOLING"

uisnm <-rbind(educexp,netprim,grosecond,avgschool, enrolld)
rm(educexp,netprim,grosecond,avgschool, enrolld)

# ******** UIS SDG education indicators ********
urlsdg <-"ftp://ftp.uis.unesco.org/BDDS/SDG.zip"

temp<- tempfile()
download.file(urlsdg, temp)
unzip(temp, "SDG_DATA_NATIONAL.csv")
unzip(temp, "SDG_LABEL.csv")
sdg <- read.csv("SDG_DATA_NATIONAL.csv")
sdg.label <- read.csv("SDG_LABEL.csv")
unlink(temp)

# Percentage of qualified teachers in primary education, both sexes (%)
qual.teach.p <- sdg[sdg$INDICATOR_ID %in% "QUTP.1" ,]
qual.teach.p<-qual.teach.p %>% select(COUNTRY_ID, YEAR, VALUE)
names(qual.teach.p)[1:3] <-c("iso3c","year","VALUE")
qual.teach.p$indicator_code <- "uis.PRM.TCAQ.ZS"

# Percentage of qualified teachers in secondary education, both sexes (%)
qual.teach.s <- sdg[sdg$INDICATOR_ID %in% "QUTP.2T3" ,]
qual.teach.s<-qual.teach.s %>% select(COUNTRY_ID, YEAR, VALUE)
names(qual.teach.s)[1:3] <-c("iso3c","year","VALUE")
qual.teach.s$indicator_code <- "uis.SEC.TCAQ.ZS"

# Pre primary enrollment
preprim <- sdg[sdg$INDICATOR_ID %in% "GER.02" ,]
preprim<-preprim %>% select(COUNTRY_ID, YEAR, VALUE)
names(preprim)[1:3] <-c("iso3c","year","VALUE")
preprim$indicator_code <- "uis.PRE.ENRR"

# Primary completion
complet <- sdg[sdg$INDICATOR_ID %in% "CR.1" ,]
complet<-complet %>% select(COUNTRY_ID, YEAR, VALUE)
names(complet)[1:3] <-c("iso3c","year","VALUE")
complet$indicator_code <- "uis.COMPLRATE.PRIMARY"

# Pupil (qualified) teacher ratio
tratio <- sdg[sdg$INDICATOR_ID %in% "PTRHC.1.QUALIFIED" ,]
tratio<-tratio %>% select(COUNTRY_ID, YEAR, VALUE)
names(tratio)[1:3] <-c("iso3c","year","VALUE")
tratio$indicator_code <- "uis.PRM.ENRL.TC.ZS"

# Math proficiency
math <- sdg[sdg$INDICATOR_ID %in% "MATH.PRIMARY" ,]
math<-math %>% select(COUNTRY_ID, YEAR, VALUE)
names(math)[1:3] <-c("iso3c","year","VALUE")
math$indicator_code <- "uis_math"

# Reading proficiency
read <- sdg[sdg$INDICATOR_ID %in% "READ.PRIMARY" ,]
read<-read %>% select(COUNTRY_ID, YEAR, VALUE)
names(read)[1:3] <-c("iso3c","year","VALUE")
read$indicator_code <- "uis_reading"

# Literacy rate
lit <- sdg[sdg$INDICATOR_ID %in% "LR.AG15T99" ,]
lit<-lit %>% select(COUNTRY_ID, YEAR, VALUE)
names(lit)[1:3] <-c("iso3c","year","VALUE")
lit$indicator_code <- "uis.ADT.LITR.ZS"

sdg.tot <- rbind(math,read,lit,tratio,complet,preprim,qual.teach.s,qual.teach.p)
rm(math,read,lit,tratio,complet,preprim,qual.teach.s,qual.teach.p)

unesco<-rbind(uisnm,sdg.tot)

unesco$source <- "UIS"

benchmark <- rbind(benchmark, unesco)

#--------------------------------
# ILO datasets
# Notes: Using built-in R library that parses the HTML, link to CRAN
#--------------------------------

# Labour prod. per hour (prev from conference board)
lprod_hour <- readSDMX("https://www.ilo.org/sdmx/rest/data/ILO,DF_GDP_PHRW_NOC_NB/?format=genericdata&formatVersion=2.1&startPeriod=1990-01-01&endPeriod=2023-12-31")
lprod_hour <- lprod_hour %>%
  as.data.frame() %>%
  select(iso3c = REF_AREA, year = obsTime, VALUE = obsValue) %>%
  mutate(indicator_code = "PROD_HOUR")

# Labour prod. per worker
lprod <- readSDMX("https://www.ilo.org/sdmx/rest/data/ILO,DF_GDP_211P_NOC_NB/?format=genericdata&formatVersion=2.1&startPeriod=1990-01-01&endPeriod=2023-12-31")
lprod <- lprod %>%
  as.data.frame() %>%
  select(iso3c = REF_AREA, year = obsTime, VALUE = obsValue) %>%
  mutate(indicator_code = "GDP_211P_NOC_NB")

# Total unemployment % labor force (15+)
unemp <- readSDMX("https://www.ilo.org/sdmx/rest/data/ILO,DF_UNE_2UNE_SEX_AGE_NB/?format=genericdata&formatVersion=2.1&startPeriod=1990-01-01&endPeriod=2023-12-31")
unemp <- unemp %>%
  as.data.frame() %>%
  filter(SEX == "SEX_T" & AGE == "AGE_YTHADULT_YGE15") %>%
  select(iso3c = REF_AREA, year = obsTime, VALUE = obsValue) %>%
  mutate(indicator_code = "SDG_0852_SEX_AGE_RT_A")

ilo <- rbind(unemp, lprod, lprod_hour)
ilo$source <- "ILO"

benchmark <-rbind(ilo,benchmark)

#--------------------------------
# WHO indicators
# API info: https://www.who.int/data/gho/info/gho-odata-api
#--------------------------------

# Download available indicator list
gho_dims <- as.data.frame(fromJSON("https://ghoapi.azureedge.net/api/Indicator"))

gho.cleaner <- function(df) {
  df <- df %>%
    select(SpatialDim, TimeDim, NumericValue) %>%
    rename("iso3c" = "SpatialDim", "year" = "TimeDim", "VALUE" = "NumericValue")
}

# Health expenditure %GDP
health <- as.data.frame(fromJSON("https://ghoapi.azureedge.net/api/GHED_CHEGDP_SHA2011"))
colnames(health) = gsub("value.", "", colnames(health))
health <- gho.cleaner(health)
health$indicator_code <- "HEALTH.EXP.GPD"

# Out of pocket expenditure
oop <- as.data.frame(fromJSON("https://ghoapi.azureedge.net/api/GHED_OOPSCHE_SHA2011"))
colnames(oop) = gsub("value.", "", colnames(oop))
oop <- gho.cleaner(oop)
oop$indicator_code <- "HEALTH.OOPS.EXP"

# Access to sanitation
sani <- as.data.frame(fromJSON("https://ghoapi.azureedge.net/api/WSH_SANITATION_BASIC"))
colnames(sani) = gsub("value.", "", colnames(sani))
sani <- sani %>% filter(Dim1 == "TOTL")
sani <- gho.cleaner(sani)
sani$indicator_code <- "HEALTH.SANITATION.POP"

# Infant mortality rate (probability of death between birth and 1)
infan <- as.data.frame(fromJSON("https://ghoapi.azureedge.net/api/MDG_0000000001"))
colnames(infan) = gsub("value.", "", colnames(infan))
infan <- infan %>% filter(Dim1 == "BTSX")
infan <- gho.cleaner(infan)
infan$indicator_code <- "HEALTH.MORTALITY.PROB"

# Underweight
under <- as.data.frame(fromJSON("https://ghoapi.azureedge.net/api/uwgt5"))
colnames(under) = gsub("value.", "", colnames(under))
under <- under %>% filter(SpatialDimType == "COUNTRY")
under <- gho.cleaner(under)
under <- under %>% drop_na() %>% group_by(iso3c, year) %>% mutate(VALUE = mean(VALUE)) %>% slice(1)
under$indicator_code <- "HEALTH.UNDERWEIGHT"

# Overweight
over <- as.data.frame(fromJSON("https://ghoapi.azureedge.net/api/overwgt5"))
colnames(over) = gsub("value.", "", colnames(over))
over <- over %>% filter(SpatialDimType == "COUNTRY")
over <- gho.cleaner(over)
over <- over %>% drop_na() %>% group_by(iso3c, year) %>% mutate(VALUE = mean(VALUE)) %>% slice(1)
over$indicator_code <- "HEALTH.OVERWEIGHT"

# Medical doctors per 10,000
phys <- as.data.frame(fromJSON("https://ghoapi.azureedge.net/api/HWF_0001"))
colnames(phys) = gsub("value.", "", colnames(phys))
phys <- gho.cleaner(phys)
phys$indicator_code <- "HEALTH.DOC.POP"

# Hospital beds per 10,000 (DIDNT WORK)
hosp <- as.data.frame(fromJSON("https://ghoapi.azureedge.net/api/WHS6_102"))
colnames(hosp) = gsub("value.", "", colnames(hosp))
hosp <- gho.cleaner(hosp)
hosp$indicator_code <- "HEALTH.BEDS.POP"

# Health exp per capita
exppc <- as.data.frame(fromJSON("https://ghoapi.azureedge.net/api/GHED_CHEGDP_SHA2011"))
colnames(exppc) = gsub("value.", "", colnames(exppc))
exppc <- exppc %>% filter(SpatialDimType == "COUNTRY")
exppc <- gho.cleaner(exppc)
exppc$indicator_code <- "HEALTH.EXP.PCAP"

who <- rbind(exppc,hosp,phys,infan,sani,oop,health,under,over)
rm(exppc,hosp,phys,infan,sani,oop,health,under,over, gho.cleaner)

who <- na.omit(who)
who$source <- "WHO"

benchmark <- rbind(benchmark,who)

#--------------------------------
# OECD data
# Notes:
#--------------------------------

# Environmental taxes
url.envi <- "https://stats.oecd.org/restsdmx/sdmx.ashx/GetData/ERTR/AUS+AUT+BEL+CAN+CHL+COL+CZE+DNK+EST+FIN+FRA+DEU+GRC+HUN+ISL+IRL+ISR+ITA+JPN+KOR+LVA+LTU+LUX+MEX+NLD+NZL+NOR+POL+PRT+SVK+SVN+ESP+SWE+CHE+TUR+GBR+USA+OECDAO+OAVG_A+OECDE+OECD+NMEC+ALB+ARG+BHS+BRB+BLZ+BTN+BOL+BIH+BWA+BRA+BGR+BFA+CMR+CPV+TCD+CHN+COD+COK+CRI+CIV+HRV+CYP+DOM+ECU+EGY+SLV+GNQ+FJI+GHA+GTM+GUY+HND+IND+IDN+JAM+KAZ+KEN+LIE+MDG+MWI+MYS+MLI+MLT+MRT+MUS+MNG+MNE+MAR+MOZ+NAM+NRU+NIC+NER+MKD+NGA+PAN+PNG+PRY+PER+PHL+ROU+RUS+RWA+LCA+WSM+SEN+SRB+SYC+SGP+SLB+ZAF+SWZ+THA+TGO+TTO+TUN+UGA+URY+VEN+OECDAM.BASE_NC+BASE_ERTR+BASE_REV+BASE_GDP+BASE_POP.TOT+ENE+TRA+POL+RES.TOT/"
envitax <- readSDMX(url.envi)
envitax <- as.data.frame(envitax)

envitax <- envitax[envitax$VAR=="BASE_REV" & envitax$CAT=="TOT",]
envitax<-envitax[,c("COU", "obsTime", "obsValue")]
names(envitax)[1:3] <-c("iso3c","year","VALUE")
envitax$indicator_code <- "OECD.ENVTAX.REV"

# Gender equality indicators
gen <-get_dataset("SIGI2019")
gen <- gen[gen$REGION=="ALL" & gen$INCOME=="AIC",]
gen <- gen[,-c(1,3,5)]
names(gen)[1:4] <-c("iso3c","indicator_code","year","VALUE")

gen1 <- gen[gen$indicator_code %in% "RCL__2",]
gen2 <- gen[gen$indicator_code %in% "SIGI_2",]
gen3 <- gen[gen$indicator_code %in% "DF__2",]
gen4 <- gen[gen$indicator_code %in% "RPI__2",]
gen5 <- gen[gen$indicator_code %in% "RAPFR__2",]

gen.tot<-rbind(gen1,gen2,gen3,gen4,gen5)
rm(gen1,gen2,gen3,gen4,gen5)

#url.oecd <- "http://stats.oecd.org/restsdmx/sdmx.ashx/GetData/EXP_PM2_5/AUS+AUT+BEL+CAN+CHL+COL+CZE+DNK+EST+FIN+FRA+DEU+GRC+HUN+ISL+IRL+ISR+ITA+JPN+KOR+LVA+LTU+LUX+MEX+NLD+NZL+NOR+POL+PRT+SVK+SVN+ESP+SWE+CHE+GBR+USA+EU28+G7M+G20+OECD+WLD+NMEC+AFG+ALB+DZA+AND+AGO+ATG+ARG+ARM+AZE+BHS+BHR+BGD+BRB+BLR+BLZ+BEN+BTN+BOL+BIH+BWA+BRA+BRN+BGR+BFA+BDI+KHM+CMR+CPV+CAF+TCD+CHN+COM+COG+COD+CRI+CIV+HRV+CUB+DJI+DMA+DOM+ECU+EGY+SLV+GNQ+ERI+ETH+FJI+MKD+GAB+GMB+GEO+GHA+GRD+GTM+GIN+GNB+GUY+HTI+HND+IND+IDN+IRQ+JAM+JOR+KAZ+KEN+PRK+KWT+KGZ+LAO+LBN+LSO+LBR+LBY+LIE+MDG+MWI+MYS+MLI+MLT+MRT+MUS+FSM+MDA+MNG+MNE+MAR+MOZ+MMR+NAM+NPL+NIC+NER+NGA+OMN+PAK+PAN+PNG+PRY+PER+PHL+QAT+ROU+RUS+RWA+LCA+VCT+WSM+SMR+STP+SAU+SEN+SRB+SYC+SLE+SGP+SLB+SOM+ZAF+LKA+SDN+SUR+SSD+SWZ+SYR+TJK+TZA+THA+TLS+TGO+TON+TTO+TUN+TKM+UGA+UKR+ARE+URY+UZB+VUT+VEN+VNM+YEM+ZMB+ZWE.TOTAL+398+69550.TOTAL.PWM_EX+SPEX_10+SPEX_15+SPEX_25+SPEX_35+SPOP_W/all?startTime=1990&endTime=2017"
url.oecd <- "https://stats.oecd.org/restsdmx/sdmx.ashx/GetData/EXP_PM2_5/AUS+AUT+BEL+CAN+CHL+COL+CZE+DNK+EST+FIN+FRA+DEU+GRC+HUN+ISL+IRL+ISR+ITA+JPN+KOR+LVA+LTU+LUX+MEX+NLD+NZL+NOR+POL+PRT+SVK+SVN+ESP+SWE+CHE+TUR+GBR+USA+AFG+ALB+DZA+ASM+AND+AGO+AIA+ATG+ARG+ARM+ABW+AZE+BHS+BHR+BGD+BRB+BLR+BLZ+BEN+BMU+BTN+BOL+BIH+BWA+BRA+VGB+BRN+BGR+BFA+BDI+KHM+CMR+CPV+CYM+CAF+TCD+CHN+CXR+CCK+COM+COG+COD+COK+CRI+CIV+HRV+CUB+CYP+DJI+DMA+DOM+ECU+EGY+SLV+GNQ+ERI+ETH+FRO+FLK+FJI+PYF+GAB+GMB+GEO+GHA+GIB+GRL+GRD+GUM+GTM+GIN+GNB+GUY+HTI+VAT+HND+HKG+IND+IDN+IRN+IRQ+GGY+IMN+JAM+JEY+JOR+KAZ+KEN+PRK+KIR+KWT+KGZ+LAO+LBN+LSO+LBR+LBY+LIE+MAC+MDG+MWI+MYS+MDV+MLI+MLT+MHL+MRT+MUS+FSM+MDA+MCO+MNG+MNE+MSR+MAR+MOZ+MMR+NAM+NRU+NPL+ANT+NCL+NIC+NER+MKD+NGA+NIU+NFK+MNP+PSE+OMN+PAK+PLW+PAN+PNG+PRY+PER+PHL+PCN+PRI+QAT+ROU+RUS+RWA+SHN+KNA+LCA+SPM+VCT+WSM+SMR+STP+SAU+SEN+SRB+SYC+SLE+SGP+SLB+SOM+ZAF+LKA+SDN+SUR+SSD+SJM+SWZ+SYR+TWN+TJK+TZA+THA+TLS+TGO+TKL+TON+TTO+TUN+TKM+TCA+TUV+UGA+UKR+ARE+URY+UZB+VUT+VEN+VNM+VIR+WLF+YEM+ZMB+ZWE+GRPS+LAC+MENA+ESH+EECCA.TOTAL.TOTAL.PWM_EX/"
pm2 <- readSDMX(url.oecd)
pm2 <- as.data.frame(pm2)

# Average exposure to PM2
pm2.avg <- pm2[pm2$VAR=='PWM_EX',]
pm2.avg <- pm2.avg[,c(1,8,9)]
colnames(pm2.avg)[1:3]<-c("iso3c","year","VALUE")
pm2.avg <- filter(pm2.avg, !(iso3c %in% c("AGO","ZWE")))
pm2.avg$indicator_code <- "ENVI2"

oecd <- rbind(gen.tot,envitax,pm2.avg)
rm(gen.tot,envitax,pm2.avg)
oecd$source <- "OECD"

benchmark <- rbind(oecd,benchmark)

#--------------------------------
# OEC data
# Notes:
#--------------------------------

url <- "https://oec.world/olap-proxy/data.jsonrecords?cube=legacy_complexity_eci_a&drilldowns=Country,Year&measures=ECI&parents=false&sparse=false"
oec <- fromJSON(url)

eci <-oec[["data"]]
eci <- eci[,-1]

names(eci)[1:3]<-c("country","year","VALUE")
iso3c<-countrycode(eci$country, origin = 'country.name', destination = 'iso3c', custom_match = (Kosovo = "XKX"))
eci<-data.frame(iso3c,eci)
eci <- eci[,-2]

eci$source <- "Economic Complexity Index (OEC)"
eci$indicator_code <- "OEC.ECI"

benchmark <- rbind(eci, benchmark)

# -------------------------------
# BTI from Stata dta file
# https://www.bti-project.org/content/en/downloads/data/BTI%202006-2020.dta
# -------------------------------

current_year <- parse_number(format(Sys.Date(), "%Y"))
last_year <- parse_number(format(Sys.Date(), "%Y")) - 1

if(!RCurl::url.exists(paste0("https://bti-project.org/fileadmin/api/content/en/downloads/data/BTI_2006-", current_year, ".dta"))) {
  bti <- read_dta(paste0("https://bti-project.org/fileadmin/api/content/en/downloads/data/BTI_2006-", last_year, ".dta"))
} else {
  bti <- read_dta(paste0("https://bti-project.org/fileadmin/api/content/en/downloads/data/BTI_2006-", current_year, ".dta"))
}

bti <- bti %>%
  select(country_code, year, gov_perf, dem_stat) %>%
  pivot_longer(!c(country_code, year), names_to="indicator_code", values_to="VALUE") %>%
  rename("iso3c" = "country_code") %>%
  mutate(indicator_code = ifelse(indicator_code=="gov_perf", "BTI.GOVPERF", "BTI.DEMO")) %>%
  mutate(source = "BTI")

benchmark <- rbind(benchmark, bti)

# -------------------------------
# TC360 World Bank data
# Found here: https://tcdata360.worldbank.org/indicators/IQ.WEF.CUST.XQ?country=BRA&indicator=1802&viz=line_chart&years=2007,2017
# -------------------------------

world.bank <- data.table::fread("https://tcdata360-backend.worldbank.org/api/v1/datasets/56/dump.csv")

world.bank<-world.bank %>%
  filter(Indicator == "Burden of customs procedure, WEF (1=extremely inefficient to 7=extremely efficient)" |
           Indicator == "Quality of port infrastructure, WEF (1=extremely underdeveloped to 7=well developed and efficient by international standards)") %>%
  select(`Country ISO3`, Indicator, starts_with("19"), starts_with("20")) %>%
  pivot_longer(!c(`Country ISO3`, Indicator), names_to="year", values_to="VALUE") %>%
  rename("iso3c" = "Country ISO3") %>%
  mutate(source = "TCdata360 (World Bank)",
         indicator_code=ifelse(Indicator=="Burden of customs procedure, WEF (1=extremely inefficient to 7=extremely efficient)",
                               "IQ.WEF.CUST.XQ", "IQ.WEF.PORT.XQ")) %>%
  select(-Indicator) %>%
  drop_na()

benchmark<-rbind(benchmark,world.bank)

# ------------------------------- Non automated from here on -------------------------------

#--------------------------------
# Import Gallup
# Notes: Paywall
#--------------------------------

# gallup <- read_dta("./Data collection/The_Gallup_032621.dta")
# gallup_short <- gallup[,c("WP5", "FIELD_DATE", "WP138", "WP112", "WP137","WP92",
#                           "WP131", "INDEX_LE")]
#
# labs <- lapply(gallup_short[,1], attr, "labels") %>% unlist()
# names(labs) <- str_remove(names(labs), "WP5.")
#
# vals <- lapply(gallup_short, attr, "label")
#
# gallup_short$WP5 <- names(labs)[match(gallup_short$WP5, labs)]
#
# gallup_short <- gallup_short %>%
#   clean_names() %>%
#   mutate(field_date = format(field_date, "%Y"))
#
# df <- c()
# for(i in 3:ncol(gallup_short)) {
#
#   temp <- gallup_short[,c(1,2,i)]
#   labs <- lapply(temp[,3], attr, "labels") %>% unlist()
#   names(labs) <- str_remove(names(labs), paste0(colnames(temp[,3]), "."))
#   colnames(temp)[3] <- "value"
#
#   temp$value <- names(labs)[match(temp$value, labs)]
#
#   temp <- temp %>%
#     group_by(wp5, field_date, value) %>%
#     count() %>%
#     group_by(wp5, field_date) %>%
#     mutate(val = sum(n)) %>%
#     filter(!value %in% c("(DK)", "(Refused)")) %>%
#     ungroup() %>%
#     mutate(pct = (n/val) * 100,
#            indic = colnames(gallup_short[,i])) %>%
#     select(-n, -val)
#
#   df <- rbind(df, temp)
#
# }
#
# df <- df %>%
#   mutate(indic = ifelse(value == "Struggling" & indic == "index_le", "index_strug",
#                         ifelse(value == "Thriving" & indic == "index_le", "index_thrive", indic)))
#
# matches <- c("CONF2" = "wp138",
#              "CONF1" = "wp112",
#              "CONF3"= "wp137",
#              "ROAD1" = "wp92",
#              "POOR1" = "wp131",
#              "LIFE12" = "index_strug",
#              "LIFE11" = "index_thrive")
#
# df$indicator_code <- names(matches)[match(df$indic, matches)]
#
# df <- df %>%
#   filter(!is.na(indicator_code)) %>%
#   mutate(val = ifelse(indicator_code %in% names(matches[1:3]) & value == "No", 1,
#                       ifelse(indicator_code %in% c("ROAD1", "POOR1") & value == "Dissatisfied", 1,
#                              ifelse(is.na(value), 1, 0)))) %>%
#   filter(!val == 1) %>%
#   select("country" = wp5, "year" = "field_date", "VALUE" = pct, indicator_code)
#
# openxlsx::write.xlsx(df, "gallup_data.xlsx")

gallup <- read_excel("./data/gallup_data.xlsx") %>%
  mutate(source = "Gallup",
         iso3c = countrycode(country, "country.name", "iso3c", custom_match = c("Kosovo" = "XKX"))) %>%
  select(-country)

benchmark <-rbind(benchmark,gallup)

# -------------------------------
# EMDAT Download
# Source: https://public.emdat.be/data
# Note: Can be updated every year
# -------------------------------

emdat <- read_excel("./data/emdat_public_2023_01_19_query_uid-BF2lmn.xlsx")

colnames(emdat) <- emdat[6,]
emdat<-emdat[-c(1:6),]

emdat <- emdat %>%
  select(Year, ISO, `Total Deaths`, `Total Damages ('000 US$)`, `No Injured`) %>%
  rename("year" = "Year", "iso3c" = "ISO") %>%
  pivot_longer(!c(year, iso3c), names_to="indicator_name", values_to="VALUE") %>%
  mutate(VALUE = as.numeric(VALUE),
         year = as.numeric(year)) %>%
  filter(year > 1950) %>%
  group_by(year, iso3c, indicator_name) %>%
  mutate(VALUE = if_else(is.na(VALUE), 0, VALUE)) %>%
  mutate(VALUE = sum(VALUE)) %>%
  ungroup() %>%
  filter(!VALUE == 0) %>%
  mutate(source = "EMDAT") %>%
  mutate(indicator_code = ifelse(indicator_name == "Total Deaths", "EMDAT.TOT.DEATH", ""),
         indicator_code = ifelse(indicator_name == "Total Damages ('000 US$)", "EMDAT.TOT.DAMAGE", indicator_code),
         indicator_code = ifelse(indicator_name == "No Injured", "EMDAT.TOT.DEATHINJU", indicator_code)) %>%
  select(-indicator_name)

benchmark <- rbind(benchmark, emdat)

#--------------------------------
# Import OECD excel
# Notes: Unable to use API
#--------------------------------

electricity <- read_excel("~/OneDrive/HLM - Key Issues Paper/Climate/IRENA_data.xlsx")

elec.gen <- electricity %>%
  clean_names() %>%
  rename(country = country_area) %>%
  group_by(country, year) %>%
  mutate(value = sum(value, na.rm=T)) %>%
  slice(1) %>%
  ungroup() %>%
  mutate(iso3c = countrycode(country, "country.name", "iso3c", custom_match=c("Kosovo*" = "XKX"))) %>%
  select(iso3c, year, "VALUE" = value) %>%
  mutate(indicator_code = "OECD.ELE.GEN",
         source = "IRENA")

renewelect <- electricity %>%
  clean_names() %>%
  mutate_if(is.factor, as.character) %>%
  mutate(year = as.numeric(year),
         value = ifelse(is.na(value), 0, value),
         value = as.numeric(value),
         country = countrycode(country_area, "country.name", "country.name")) %>%
  select(-country_area) %>%
  arrange(country, year, technology) %>%
  mutate(renewable = ifelse(technology %in% c("Fossil fuels", "Nuclear", "Other non-renewable energy", "Coal and peat",
                                              "Natural gas", "Fossil fuels n.e.s.", "Oil"), "no", "yes")) %>%
  group_by(country, year) %>%
  mutate(tot = sum(value),
         renewable_tot = sum(value[renewable == "yes"]),
         value = (renewable_tot/tot)*100) %>%
  slice(1) %>%
  ungroup() %>%
  mutate(iso3c = countrycode(country, "country.name", "iso3c", custom_match=c("Kosovo" = "XKX"))) %>%
  select(iso3c, year, VALUE = value) %>%
  mutate(indicator_code = "OECD.REN.ELE",
         source = "IRENA")

# Pisa Math scores
pisa.math <- read_excel("./oecd data/PISA_maths.xlsx")
pisa.math <- pisa.math[,c(1,3,6,7)]
pisa.math<-aggregate(x=pisa.math$Value,
                     by=list(pisa.math$LOCATION,pisa.math$TIME),
                     FUN=mean)
colnames(pisa.math)[1:3]<-c("iso3c","year","VALUE")
pisa.math$indicator_code <- "pisa_math"

# Pisa Reading scores
pisa.read <- read_excel("./oecd data/PISA_reading.xlsx")
pisa.read <- pisa.read[,c(1,3,6,7)]
pisa.read<-aggregate(x=pisa.read$Value,
                     by=list(pisa.read$LOCATION,pisa.read$TIME),
                     FUN=mean)
colnames(pisa.read)[1:3]<-c("iso3c","year","VALUE")
pisa.read$indicator_code <- "pisa_reading"

# Pisa Science scores
pisa.sci <- read_excel("./oecd data/PISA_science.xlsx")
pisa.sci <- pisa.sci[,c(1,3,6,7)]
pisa.sci<-aggregate(x=pisa.sci$Value,
                    by=list(pisa.sci$LOCATION,pisa.sci$TIME),
                    FUN=mean)
colnames(pisa.sci)[1:3]<-c("iso3c","year","VALUE")
pisa.sci$indicator_code <- "pisa_science"

# Share renewable sources
renewpct <- read_excel("./oecd data/Renewable to total primary energy supply (TPES).xlsx")
renewpct <- renewpct[renewpct$MEASURE =="KTOE",]
renewpct <- renewpct[,c(1,6,7)]
colnames(renewpct)[1:3]<-c("iso3c","year","VALUE")
renewpct$indicator_code<-"OECD.REN.TPES"

# Material productivity
mat.prod <- read_excel("./oecd data/OECD_Material productivity_USDDMC_KG.xlsx")
mat.prod <- mat.prod[mat.prod$SUBJECT =="TOTMAT",]
mat.prod <- mat.prod[,c(1,6,7)]
colnames(mat.prod)[1:3]<-c("iso3c","year","VALUE")
mat.prod$indicator_code<-"OECD.MATPROD"

# Municipal waste
munic.waste <- read_excel("./oecd data/munic_waste.xlsx")
munic.waste <- munic.waste[munic.waste$MEASURE =="KG_CAP",]
munic.waste <- munic.waste[,c(1,6,7)]
colnames(munic.waste)[1:3]<-c("iso3c","year","VALUE")
munic.waste$indicator_code<-"OECD.WASTEPOP"

oecd.excel <- rbind(munic.waste,mat.prod,renewpct,pisa.sci, pisa.read,pisa.math)
rm(munic.waste,mat.prod,renewpct,pisa.sci, pisa.read,pisa.math)

oecd.excel$source <- "OECD"

non.auto <- rbind(gallup, oecd.excel, emdat, renewelect, elec.gen)

benchmark <- rbind(oecd.excel,benchmark)

# ------------------------------- Cleaning and organizing -------------------------------

end.time2 <- Sys.time()
time.taken.tot <- end.time2 - start.time

bench.code <- unique(benchmark$indicator_code)

bench.indicators <- data.frame()

for (var in bench.code){
  dat <- benchmark %>%
    filter(indicator_code==var) %>%
    mutate(year=as.numeric(year)) %>%
    mutate(min.year=min(year)) %>%
    mutate(max.year=max(year))%>%
    mutate(`Observation period` = paste0(min.year, "-", max.year)) %>%
    mutate(`Number of obs`=length(unique(iso3c))) %>%
    mutate(`Number of years` = length(unique(year))) %>%
    mutate(`Difference max-min` = max.year-min.year) %>%
    mutate(`Complete?` = (`Number of years` - `Difference max-min`) == 1) %>%
    slice(1) %>%
    select(indicator_code, max.year, `Observation period`,`Number of years`, `Number of obs`, `Difference max-min`, `Complete?`)

  bench.indicators<-rbind(bench.indicators, dat)
}

indic.list <- read_excel("./data/Indicator list.xlsx", sheet = "Information sheet")

indic.list<-merge(indic.list, bench.indicators, by.x="Indicator code", by.y = "indicator_code", all=T)

indic.list$`Automated?` <- ifelse(indic.list$`Indicator code` %in% unique(non.auto$indicator_code), "no", "yes")

missings <- indic.list %>%
  filter(is.na(`Observation period`))

paste("There are", format(nrow(benchmark), big.mark = ","), "observations in the database.")
paste("WDI download took", round(time.taken.wdi, digits=2), "minutes to complete and the full script took",
      round(time.taken.tot, digits=2), "minutes to complete.")

# Review countries listed/check for missing ones

benchmark <- benchmark %>%
  mutate(country = countrycode(iso3c, "iso3c", "country.name")) %>%
  drop_na() %>%
  select(-country)

# write.xlsx(benchmark, "0_benchmark_db_v2.xlsx")
write.csv(benchmark, "0_benchmark_db_v2.csv")

meta.data <- data.frame("Time" = c(time.taken.tot, rep(0, 17)),
                        "Non-automated" = unique(non.auto$indicator_code))

write.csv(meta.data, "0_benchmark_meta_v2.csv")

# --------------- STOP RUN ---------------------
# Extra steps, checks, etc.

# ------- Duplicate checker --------
# library(data.table)
#
# setDT(benchmark)
# benchmark <-as.data.table(benchmark)
#
# setkey(benchmark, indicator_code, VALUE)
# benchmark[duplicated(benchmark)]



