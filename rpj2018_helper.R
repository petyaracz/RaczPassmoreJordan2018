########################################################################################################
# pull in EA data from dplace github repo, tidy it up, add region and language family information, the latter from the dplace # html, the former from wherever. write results into ea_tidy.csv
#
########################################################################################################
try(setwd('~/Work/Bristol/RaczPassmoreJordan2018/'))

library("RCurl") # to connect to the informational superhighway
library("dplyr") # to massage data
library("stringr") # to edit strings
library("reshape2") # to massage data more
library("jsonlite") # to get the json

pullInEA <- function(){
# Ethno Atlas from dplace github. accessed 12/7/17
variables <- read.csv(text=getURL("https://raw.githubusercontent.com/D-PLACE/dplace-data/master/datasets/EA/variables.csv"), header=T)
codes <- read.csv(text=getURL("https://raw.githubusercontent.com/D-PLACE/dplace-data/master/datasets/EA/codes.csv"), header=T)
dat <- read.csv(text=getURL("https://raw.githubusercontent.com/D-PLACE/dplace-data/master/datasets/EA/data.csv"), header=T) # data is a reserved word
societies <- read.csv(text=getURL("https://raw.githubusercontent.com/D-PLACE/dplace-data/master/datasets/EA/societies.csv"), header=T) %>% select(id, glottocode, pref_name_for_society, Lat, Long) %>% rename(lat = Lat, lon = Long, soc_id = id, society = pref_name_for_society)
locations <- getURL("https://raw.githubusercontent.com/D-PLACE/dplace-data/master/geo/societies_tdwg.json") %>% fromJSON
locations2 <- do.call(rbind.data.frame, locations)
locations2$soc_id <- rownames(locations2)  
locations2 <- subset(locations2, soc_id %in% dat$soc_id) %>% select(-code) %>% rename(region = name)

languages <- read.csv(text=getURL("https://raw.githubusercontent.com/D-PLACE/dplace-data/master/csv/glottolog.csv"), header=T)
languages <- languages %>% rename(family = family_name, glottocode = id) %>% select(family, glottocode)

societies <- merge(societies,languages)

# get society list from sccs

sccs <- read.csv(text=getURL("https://raw.githubusercontent.com/D-PLACE/dplace-data/master/datasets/SCCS/societies.csv"), header=T) %>% select(pref_name_for_society,glottocode,HRAF_name_ID)
societies$in_sccs <- ifelse(societies$glottocode %in% sccs$glottocode, T, F)

variables2 <- variables[,c('id','category','title','definition','type')]
variables2 <- rename(variables2, var_id = id)
variables3 <- merge(variables2,codes)
# adds up
dat2 <- dat[,c('soc_id','var_id','code')]
dat3 <- merge(dat2,variables3)
# doesn't add up: for some reason pop size (EA202) drops out. submitted an Issue on github. in the meantime
dat3b <- dat2[dat2$var_id == 'EA202',]
merge_with_dat3b <- variables3[variables3$var_id == 'EA202',] %>% select(-code)
dat3b2 <- merge(dat3b, merge_with_dat3b)
dat4 <- rbind(dat3,dat3b2)
# still doesn't add up, now there are a couple hundred extra rows. 
# > nrow(dat2[dat2$var_id == 'EA202',])
# [1] 1291
# > nrow(dat4[dat4$var_id == 'EA202',])
# [1] 1629
dat5 <- unique(dat4)
# right.

# add regions and long / lat
dat6 <- merge(dat5,locations2)

# add lang fam
dat7 <- merge(societies,dat6)
}

dat7 <- pullInEA()

write.csv(dat7, file = 'ea_tidy.csv', row.names = F)

########################################################################################################
#
# restrict dataset to variables I actually need. tidy them some more. set factor levels by brute force.
#
########################################################################################################

kell <- paste('EA', c("015","023","043","027","031","030","032","033","202","042"), sep='')
d8 <- subset(dat7, var_id %in% kell)

# need to make it from long to wide. easiest if I slice off soc-spec data and var-spec data first.

d8_soc <- d8 %>% select(soc_id,region,society,family,lon,lat,in_sccs) %>% unique
d8_var <- d8 %>% select(soc_id,var_id,code)

d9 <- dcast(d8_var, soc_id ~ var_id, value.var = "code") # why do you need to put the value var in quotes but not the rest? i'm going back to fortran

d10 <- merge(d9,d8_soc)

d10 = mutate(d10, marriage =
    ifelse(EA015 %in% c(1,2), "endogamous",
      ifelse(EA015 %in% c(3), "agamous",
        ifelse(EA015  %in% c(4,5,6), "exogamous",NA
      ))))

# d10 %>% filter(var_id=='EA015') %>% select(description,name,marriage) %>% View # these Views only really worked when I had the data in a long format. but the codes do align.

d10 = mutate(d10, cousin_marriage =
    ifelse(EA023 %in% c(7,8), 1,
      ifelse(EA023 %in% c(11,12), 2,
        ifelse(EA023 %in% c(1,2,3,4,5,6,9,13), 3,
          ifelse(EA023 %in% c(10), 4,NA
      ))))) # 1 = no marriage; 2 = some second cousins; 3 = some first cousins; 4 = any first cousin

# d10 %>% filter(var_id=='EA023') %>% select(description,name,cousin_marriage) %>% View

d10 = mutate(d10, descent =
    ifelse(EA043 %in% c(1), "patrilineal",
      ifelse(EA043 %in% c(6,4), "bilateral_quasi",
        ifelse(EA043 %in% c(3), "matrilineal",
          ifelse(EA043 %in% c(2,5), "duo_ambi",
            ifelse(EA043 %in% c(7), "mixed", NA
      ))))))

# d10 %>% filter(var_id=='EA043') %>% select(description,name,descent) %>% View

d10 = mutate(d10, cousin_terms =
    ifelse(EA027 %in% c(4), 'hawaiian',
      ifelse(EA027 %in% c(3), 'eskimo',
        ifelse(EA027 %in% c(5), 'iroquois',
          ifelse(EA027 %in% c(1,6), 'crow/omaha',
            ifelse(EA027 %in% c(7,2), 'sudanese/desc', NA
            ))))))

# d10 %>% filter(var_id=='EA027') %>% select(description,name,code,cousin) %>% View
d10$cousin_terms <- factor(d10$cousin_terms, levels = c('hawaiian','eskimo','iroquois','crow/omaha','sudanese/desc'))
d10$cousin_rank = as.numeric(d10$cousin_terms)

d10$comm_size = d10$EA031
d10$settlement = d10$EA030
d10$jurisdiction_local = d10$EA032 # indep fam; ext fam; clan-barrios
d10$jurisdiction = d10$EA033

d10$log_pop_size = ifelse(d10$EA202==0,0,log(d10$EA202))

d10 = mutate(d10, subsistence =
    ifelse(EA042 %in% c(7), "int_agr",
      ifelse(EA042 %in% c(5,6,9), "ext_agr",
        ifelse(EA042 %in% c(4), "pastoralism",
          ifelse(EA042 %in% c(1,2,3), "foraging",
            NA
          )))))

# I'm keeping the original labels to make this somewhat traceable.




write.csv(d10, file = 'ea_tidy_cousin_only.csv', row.names = F)



