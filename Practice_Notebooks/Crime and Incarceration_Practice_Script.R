#setting up

library(tidyverse)
library(curl)
library(sf)
library(ggplot2)
library(tigris)

#Loading Data
curl::curl_download("https://chicagojustice.org/wp-content/uploads/2016/08/cook_county_conviction_records__2005_2009.zip", destfile = "data/convictions.zip")
unzip("data/convictions.zip", exdir = "data")
incarceration <- read.csv("data//cook_county_conviction_records__2005_2009.csv")
incarceration_full<-readRDS("data/inc_add_full.rds")
library(tidyverse)
nrow(incarceration_full)
incarceration_full <- incarceration_full %>% filter(!is.na (lon) & !is.na(lat), na.rm=TRUE)
nrow(incarceration_full)
inc_add_chi<-left_join(incarceration, incarceration_full, by = c("st_address", "city", "state"))
nrow(inc_add_chi)
inc_add_chi <- inc_add_chi %>% filter(city=="CHICAGO") %>% filter(!is.na(address1))
inc_add_chi %>% select(chrgdisp) %>% distinct()
inc_add_chi<-inc_add_chi %>% mutate(in_prison = case_when(
  chrgdisp %in% c(
    "DEF SENTENCED ILLINOIS DOC",
    "DEF SENTENCED TO COOK CNTY DOC",
    "DEF SENT TO LIFE IMPRISONMENT",
    "SENTENCED CCDOC BOOT CAMP",			
    "DEF SENTENCED TO DEATH",
    "DEF SENT CCDOC, PERIODIC IMP") ~ "In Prison",
  chrgdisp %in% c(
    "CREDIT DEFENDANT FOR TIME SERV",				
    "PROB AND OTHER DISC CONDS",				
    "DEF SENT TO PAY RESTITUTION",			
    "ELIGIBLE IMPACT PROG-BOOT CAMP",				
    "DEF SENTENCED TO PROBATION",			
    "DEF SENT CONDITIONAL DISCHARGE",				
    "SENTENCED TO COMMUNITY SERVICE",				
    "SENT/CRT SUPV-SOC SERV-SPC CND",	
    "SUPERVISION - SOCIAL SERV DEPT",			
    "SENT/CRT SUPV/SPEC CONDS",			
    "DEF SENT TO HOME CONFINEMENT",			
    "COND DISCH AND OTHER DISC COND",				
    "DEF SENT TO INDETERMINATE TERM",			
    "SENT PER IMP + OTH DISC COND") ~ "Not in Prison"))
nrow(inc_add_chi %>% filter(in_prison == "In Prison"))/nrow(inc_add_chi)
inc_add_chi <- inc_add_chi %>% filter(in_prison == "In Prison")
inc_add_chi <- inc_add_chi %>% mutate(min_sent_total = round(minsent_years*365 + minsent_months*30.4167+minsent_days, digits = 0))
inc_add_chi %>% summarise(avg_min_sent_total = mean(min_sent_total, na.rm = TRUE))
inc_add_chi %>% group_by(chrgdesc)%>% summarise(avg_min_sent_total = mean(min_sent_total, na.rm = TRUE))
inc_add_chi <- inc_add_chi %>% mutate(sentence_cost = min_sent_total* (22000/365))
charges <- read_csv("data/ilcs.coded.csv") %>% select(Statute, Code, Desc)
inc_add_chi <- left_join(inc_add_chi, charges, by=c("statute" = "Statute"))
inc_add_chi <- inc_add_chi %>% filter(!is.na(Code))

#Analyzing
inc_add_chicago<-readRDS("data/data_chicago_Thursday.rds")
inc_add_chicago <- inc_add_chicago %>% mutate(sentence_cost = case_when(minsent_life == TRUE ~ 1298000,
                                                                        minsent_life == FALSE ~ sentence_cost))
inc_add_chicago %>% filter(minsent_life == TRUE) %>% summarise(N=n(), Cost = sum(sentence_cost))
inc_add_chicago %>% filter(minsent_life == TRUE) %>% group_by(Desc) %>% 
  summarise(Sentences = n(), Cost = sum(sentence_cost)) %>% arrange(desc(Sentences))
inc_add_chicago %>% group_by(Desc) %>% summarise(Sentence = n(), Cost = sum(sentence_cost)) %>% arrange(desc(Sentence))
inc_add_chicago %>% group_by(Desc) %>% summarise(Sentence = n(), Cost = sum(sentence_cost)) %>% arrange(desc(Cost))

#Making it spatial
trt <- tracts(state = "Illinois", class = "sf")
trt <- trt %>% filter(COUNTYFP == "031", TRACTCE != "990000")
ggplot()+geom_sf(data = trt)
chi <- places(state = "Illinois", class = "sf") %>% filter(NAME == "Chicago")
ggplot()+geom_sf(data = chi)
ggplot()+geom_sf(data = trt, colour = "light gray", fill = NA)+
  geom_sf(data = chi, colour = "dark gray", fill = NA)
chi_trt <- trt %>% filter(st_contains(chi, trt, sparse = FALSE))
ggplot()+geom_sf(data = chi_trt, colour = "light gray", fill = NA)+
  geom_sf(data = chi, colour = "dark gray", fill = NA)+theme_minimal()
inc_add_chicago_sf <- inc_add_chicago %>% st_as_sf(coords = c ("lon", "lat"), crs = 4269)
ggplot()+geom_sf(data = inc_add_chi_sf)
inc_add_chicago_sf <- st_join(inc_add_chicago_sf, chi_trt, join=st_within)
inc_add_chicago_sf <- inc_add_chicago_sf %>% filter(!is.na(GEOID))
ggplot()+
  geom_sf(data=inc_add_chicago_sf, cex=.18, alpha=.2)+
  geom_sf(data=chi, colour = "dark gray", fill = NA)+theme_minimal()
ggplot()+
  geom_sf(data=inc_add_chicago_sf %>% filter(Code == "Drug"), cex=.18, alpha=.2)+
  geom_sf(data=chi, colour = "dark gray", fill = NA)+theme_minimal()
ggplot()+
  geom_sf(data=inc_add_chicago_sf, cex=.18, alpha=.2)+
  geom_sf(data=chi, colour = "dark gray", fill = NA)+theme_minimal()+facet_wrap(~Code)
inc_add_chicago_sf %>% group_by(GEOID) %>% summarise(Sentence = n(), 
                                                     Cost = sum(sentence_cost, na.rm = TRUE))
inc_add_chi_trt <- inc_add_chicago_sf %>% 
  group_by(GEOID) %>% 
  summarise(count = n(),cost = sum(sentence_cost, na.rm = TRUE)) %>% 
  st_set_geometry(NULL)
tracts_to_map <- left_join(chi_trt, inc_add_chi_trt, by="GEOID")
ggplot()+geom_sf(data=tracts_to_map, aes(fill=cost))+theme_minimal()
ggplot()+geom_sf(data=tracts_to_map %>% filter(cost>=1000000), aes(fill=cost))+
  geom_sf(data = chi, colour = "dark gray", fill = NA)+theme_minimal()
ggplot()+geom_sf(data=tracts_to_map, aes(fill=count, colour=count))+
  geom_sf(data = chi, colour = "dark gray", fill = NA)+theme_minimal()
