
# Data and figures for result2 (country level influential factors of border conservation extent)

setwd("D:/chapter1")

library(sf)
library(dplyr)

# 1 Border level protection ####

load("D:/chapter1/input/boundaries_cwn2024.RData") # shapefile of national borders
load("./Revision 1/middle/all_country_1km2024.RData") # all_country_1km: shapefile of border protected areas

#for each border, calculate the protection on each side of it.

border_length<-data.frame(borderID=numeric(),
                          ISO3_pair=character(),
                          borderlength=numeric(),
                          country=character(),
                          protectedLength=numeric()
)


for (i in 1:nrow(boundaries)) {
  
  border <- boundaries[i,] %>% st_transform(crs = "+proj=moll") #one border
  borderid <- boundaries[i,"borderID"]
  pair_name <- border$SOCpair
  country_name <- pair_name %>%
    strsplit(split = "_") %>%
    as.data.frame()
  
  for (j in 1:2) {
    
    country <- country_name[j,1]
    country_1km <- all_country_1km %>%
      filter(ISO3==country)
    
    if (nrow(country_1km)>0) {
      
      country_union_buffer <- country_1km %>% #protected land within 1 km to border
        st_make_valid() %>% #avoid error: self-intersection
        st_union() %>% #returns a single geometry with resolved boundaries
        st_buffer(dist=1000) #1km buffer
      int <- st_intersection(border,country_union_buffer)
      border_length[nrow(border_length)+1,"borderID"]=borderid
      border_length[nrow(border_length),"ISO3_pair"]=pair_name
      border_length[nrow(border_length),"borderlength"]=st_length(border)
      border_length[nrow(border_length),"country"]=country
      
      if (length(st_length(int))>0) {
        border_length[nrow(border_length),"protectedLength"]=st_length(int)#border length overlapping with pa
      } else {
        border_length[nrow(border_length),"protectedLength"]=0
      }
      cat(i,country,"done\n")
    } else {
      border_length[nrow(border_length)+1,"borderID"]=borderid
      border_length[nrow(border_length),"ISO3_pair"]=pair_name
      border_length[nrow(border_length),"country"]=country
      border_length[nrow(border_length),"borderlength"]=st_length(border)
      border_length[nrow(border_length),"protectedLength"]=0
      cat(i,country,"done\n")
    }
  }
}

country_length<-border_length %>%
  group_by(country) %>%
  summarise(blength=sum(borderlength),
            plength=sum(protectedLength))
country_length$pPro<-country_length$plength/country_length$blength

write.csv(country_length,"./Revision 1/result/country_protected_length.csv")
save(border_length,country_length,file = "./Revision 1/result/country_protected_length.RData")


# 2 Variables ####

## 2.1 Species richness ####

load("./Revision 1/middle/all_country_1km2024.RData") #all_country_1km

boundaries_sr <- boundaries %>%
  st_transform(crs = "+proj=moll") #each line presents a pair of neighboring countries

boundaries1<-boundaries
boundaries2<-boundaries
soc_pair<-strsplit(boundaries$SOCpair,"_")
boundaries1$country<-sapply(soc_pair,function(x){x[1]})
boundaries2$country<-sapply(soc_pair,"[",2)

boundaries_cp<-rbind(boundaries1[,c("borderID","country")],boundaries2[,c("borderID","country")])%>%
  st_transform(crs = "+proj=moll") %>%
  group_by(country) %>%
  summarise() #one line for one country

### Mammal----

mammal <- read_sf("D:/data/IUCN/MAMMALS/MAMMALS.shp") %>%
  filter(presence %in% c(1,2,3),origin %in% c(1,2,6),terrestria=="true") %>%#presence:1 extant, 2 probably extant, 3 possibly extant
  st_transform(crs = "+proj=moll")

#transboundary species richness + threatened species richness of each country

trans_mammal<-data.frame(country=character(),
                         mammal_sr=numeric(),
                         thr_mammal_sr=numeric()
)


for (i in 1:nrow(boundaries_cp)) { # each line present one country
  
  boundaries_cp.i<-boundaries_cp[i,]
  inter_mammal<-st_intersects(boundaries_cp.i,mammal, sparse = T)
  
  if(length(inter_mammal[[1]]>0)) {
    
    inter_thr.mammal<-mammal[inter_mammal[[1]],]%>%
      filter(category %in% c("CR","EN","VU"))
    
    trans_mammal[nrow(trans_mammal)+1,"country"]=boundaries_cp$country[i]
    trans_mammal[nrow(trans_mammal),"mammal_sr"]=length(unique(mammal$sci_name[inter_mammal[[1]]]))
    trans_mammal[nrow(trans_mammal),"thr_mammal_sr"]=length(unique(inter_thr.mammal$sci_name))
    
  } else {
    
    trans_mammal[nrow(trans_mammal)+1,"country"]<-boundaries_cp$country[i]
    trans_mammal[nrow(trans_mammal),"mammal_sr"]=0
    trans_mammal[nrow(trans_mammal),"thr_mammal_sr"]=0
    
  }

  cat(i,"\t")
  
}

save(trans_mammal,file = "./Revision 1/middle/sr_country_mammal.RData")
rm(mammal)

### Amphibians----

amphibians1<-read_sf("D:/data/IUCN/AMPHIBIANS/AMPHIBIANS_PART1.shp") %>%
  filter(presence%in% c(1,2,3),origin %in% c(1,2,6),terrestria=="true") %>%#presence:1 extant, 2 probably extant, 3 possibly extant
  st_transform(crs = "+proj=moll")

amphibians<-read_sf("D:/data/IUCN/AMPHIBIANS/AMPHIBIANS_PART2.shp") %>%
  filter(presence%in% c(1,2,3),origin %in% c(1,2,6),terrestria=="true")%>%#presence:1 extant, 2 probably extant, 3 possibly extant
  st_transform(crs = "+proj=moll")%>%
  rbind(amphibians1)
rm(amphibians1)

#

trans_amphibians<-data.frame(country=character(),
                             amphi_sr=numeric(),
                             thr_amphi_sr=numeric()
)

for (i in 1:nrow(boundaries_cp)) {
  
  boundaries_cp.i<-boundaries_cp[i,]
  inter_amphibians<-st_intersects(boundaries_cp.i,amphibians, sparse = T)
  
  if(length(inter_amphibians[[1]])>0) {
    
    inter_thr.amphibians<-amphibians[inter_amphibians[[1]],]%>%
      filter(category %in% c("CR","EN","VU"))
    
    trans_amphibians[nrow(trans_amphibians)+1,"country"]=boundaries_cp$country[i]
    trans_amphibians[nrow(trans_amphibians),"amphi_sr"]=length(unique(amphibians$sci_name[inter_amphibians[[1]]]))
    trans_amphibians[nrow(trans_amphibians),"thr_amphi_sr"]=length(unique(inter_thr.amphibians$sci_name))
    
  } else {
    
    trans_amphibians[nrow(trans_amphibians)+1,"country"]<-boundaries_cp$country[i]
    trans_amphibians[nrow(trans_amphibians),"amphi_sr"]=0
    trans_amphibians[nrow(trans_amphibians),"thr_amphi_sr"]=0
    
  }
  
  
  cat(i,"\t")
  
}

save(trans_amphibians,file = "./Revision 1/middle/sr_country_amphibians.RData")
rm(amphibians)
gc()
### Reptile-----

reptiles1<-read_sf("D:/data/IUCN/REPTILES/REPTILES_PART1.shp") %>%
  filter(presence%in% c(1,2,3),origin %in% c(1,2,6),terrestria=="true")%>%#presence:1 extant, 2 probably extant, 3 possibly extant
  st_transform(crs = "+proj=moll") 

reptiles<-read_sf("D:/data/IUCN/REPTILES/REPTILES_PART2.shp") %>%
  filter(presence%in% c(1,2,3),origin %in% c(1,2,6),terrestria=="true")%>%#presence:1 extant, 2 probably extant, 3 possibly extant
  st_transform(crs = "+proj=moll")%>%
  rbind(reptiles1)
rm(reptiles1)
gc()

trans_reptiles<-data.frame(country=character(),
                           reptiles_sr=numeric(),
                           thr_reptiles_sr=numeric()
)

for (i in 1:nrow(boundaries_cp)) {
  
  boundaries_cp.i<-boundaries_cp[i,]
  inter_reptiles<-st_intersects(boundaries_cp.i,reptiles, sparse = T)
  
  if(length(inter_reptiles[[1]])>0) {
    
    inter_thr.reptiles<-reptiles[inter_reptiles[[1]],]%>%
      filter(category %in% c("CR","EN","VU"))
    
    trans_reptiles[nrow(trans_reptiles)+1,"country"]=boundaries_cp$country[i]
    trans_reptiles[nrow(trans_reptiles),"reptiles_sr"]=length(unique(reptiles$sci_name[inter_reptiles[[1]]]))
    trans_reptiles[nrow(trans_reptiles),"thr_reptiles_sr"]=length(unique(inter_thr.reptiles$sci_name))
    
  } else {
    
    trans_reptiles[nrow(trans_reptiles)+1,"country"]<-boundaries_cp$country[i]
    trans_reptiles[nrow(trans_reptiles),"reptiles_sr"]=0
    trans_reptiles[nrow(trans_reptiles),"thr_reptiles_sr"]=0
    
  }
  
  
  cat(i,"\t")
  
}

save(trans_reptiles,file = "./Revision 1/middle/sr_country_reptiles.RData")
rm(reptiles)
gc()

### bird ----

library(xlsx)
library(rlist)

bird.infor <- xlsx::read.xlsx("D:/data/IUCN/Handbook of the Birds of the World and BirdLife International Digital Checklist of the Birds of the World_Version_81.xlsx", 1)

bird.infor <- bird.infor[c(-1),]
colnames(bird.infor) <- bird.infor[1,]
bird.infor <- bird.infor[-1,]
bird.infor<-bird.infor[!is.na(bird.infor$SISRecID),]

head(bird.infor)
bird.infor.thr <- bird.infor %>%
  filter( `2023 IUCN Red List category`%in% c("CR","EN","VU"))

save(bird.infor,bird.infor.thr,file="./middle/conservation_priorities/bird_infor2024.RData")
load("./middle/conservation_priorities/bird_infor2024.RData")


#bird1

bird1 <- read_sf("D:/data/IUCN/Birds/Birds_1.shp") %>%
  filter(presence%in% c(1,2,3),origin %in% c(1,2,6)) %>% #presence:1 extant, 2 probably extant, 3 possibly extant
  st_transform(crs = "+proj=moll") %>%
  dplyr::select(SISID,binomial) # bird.infor$SISRecID==SISID

head(bird1)

#bird.infor[which(bird.infor$SISRecID==22695544),]

trans_bird1<-data.frame(country=character(),
                        bird_list=character()
)

for (i in 1:nrow(boundaries_cp)) {
  
  boundaries_cp.i<-boundaries_cp[i,]
  inter_bird<-st_intersects(boundaries_cp.i,bird1, sparse = T)
  
  bird.list<-unique(bird1$binomial[inter_bird[[1]]])
  
  if(length(inter_bird[[1]])>0) {
    
    trans_bird1[nrow(trans_bird1)+1,"country"]=boundaries_cp$country[i]
    trans_bird1[nrow(trans_bird1),"bird_list"]<-toString(paste(bird.list,collapse = ","))
    
  } else {
    
    trans_bird1[nrow(trans_bird1)+1,"country"]<-boundaries_cp$country[i]
    
  }
  
  
  cat(i,"\t")
  
}

save(trans_bird1,file="./Revision 1/middle/trans_bird1.RData")
save(bird1,file="./Revision 1/middle/bird_selected/bird1.RData")

rm(bird1)
gc()

#bird1a

bird1a<-read_sf("D:/data/IUCN/Birds/Birds_1a.shp") %>%
  filter(presence%in% c(1,2,3),origin %in% c(1,2,6))%>%#presence:1 extant, 2 probably extant, 3 possibly extant
  st_transform(crs = "+proj=moll") %>%
  dplyr::select(SISID,binomial)

trans_bird1a<-data.frame(country=character(),
                         bird_list=character()
)

for (i in 1:nrow(boundaries_cp)) {
  
  boundaries_cp.i<-boundaries_cp[i,]
  inter_bird<-st_intersects(boundaries_cp.i,bird1a, sparse = T)
  
  bird.list<-unique(bird1a$binomial[inter_bird[[1]]])
  
  if(length(inter_bird[[1]])>0) {
    
    trans_bird1a[nrow(trans_bird1a)+1,"country"]=boundaries_cp$country[i]
    trans_bird1a[nrow(trans_bird1a),"bird_list"]<-toString(paste(bird.list,collapse = ","))#把
    
  } else {
    
    trans_bird1a[nrow(trans_bird1a)+1,"country"]<-boundaries_cp$country[i]
    
  }
  
  
  cat(i,"\t")
  
}

save(trans_bird1a,file="./Revision 1/middle/trans_bird1a.RData")
save(bird1a,file="./Revision 1/middle/bird_selected/bird1a.RData")
rm(bird1a)
gc()

#bird2

bird2<-read_sf("D:/data/IUCN/Birds/Birds_2.shp") %>%
  filter(presence%in% c(1,2,3),origin %in% c(1,2,6))%>%#presence:1 extant, 2 probably extant, 3 possibly extant
  st_transform(crs = "+proj=moll") %>%
  dplyr::select(SISID,binomial)

trans_bird2<-data.frame(country=character(),
                        bird_list=character()
)

for (i in 1:nrow(boundaries_cp)) {
  
  boundaries_cp.i<-boundaries_cp[i,]
  inter_bird<-st_intersects(boundaries_cp.i,bird2, sparse = T)
  
  bird.list<-unique(bird2$binomial[inter_bird[[1]]])
  
  if(length(inter_bird[[1]])>0) {
    
    trans_bird2[nrow(trans_bird2)+1,"country"]=boundaries_cp$country[i]
    trans_bird2[nrow(trans_bird2),"bird_list"]<-toString(paste(bird.list,collapse = ","))#把
    
  } else {
    
    trans_bird2[nrow(trans_bird2)+1,"country"]<-boundaries_cp$country[i]
    
  }
  
  
  cat(i,"\t")
  
}

save(trans_bird2,file="./Revision 1/middle/trans_bird2.RData")
save(bird2,file="./Revision 1/middle/bird_selected/bird2.RData")
rm(bird2)
gc()

#bird2a

bird2a<-read_sf("D:/data/IUCN/Birds/Birds_2a.shp") %>%
  filter(presence%in% c(1,2,3),origin %in% c(1,2,6)) %>% #presence:1 extant, 2 probably extant, 3 possibly extant
  st_transform(crs = "+proj=moll") %>%
  dplyr::select(SISID,binomial)

trans_bird2a<-data.frame(country=character(),
                         bird_list=character()
)

for (i in 1:nrow(boundaries_cp)) {
  
  boundaries_cp.i<-boundaries_cp[i,]
  inter_bird<-st_intersects(boundaries_cp.i,bird2a, sparse = T)
  
  bird.list<-unique(bird2a$binomial[inter_bird[[1]]])
  
  if(length(inter_bird[[1]])>0) {
    
    trans_bird2a[nrow(trans_bird2a)+1,"country"]=boundaries_cp$country[i]
    trans_bird2a[nrow(trans_bird2a),"bird_list"]<-toString(paste(bird.list,collapse = ","))#把
    
  } else {
    
    trans_bird2a[nrow(trans_bird2a)+1,"country"]<-boundaries_cp$country[i]
    
  }
  
  
  cat(i,"\t")
  
}

save(trans_bird2a,file="./Revision 1/middle/trans_bird2a.RData")
save(bird2a,file="./Revision 1/middle/bird_selected/bird2a.RData")
rm(bird2a)
gc()

#bird3

bird3<-read_sf("D:/data/IUCN/Birds/Birds_3.shp") %>%
  filter(presence%in% c(1,2,3),origin %in% c(1,2,6))%>%#presence:1 extant, 2 probably extant, 3 possibly extant
  st_transform(crs = "+proj=moll") %>%
  dplyr::select(SISID,binomial)

trans_bird3<-data.frame(country=character(),
                        bird_list=character()
)

for (i in 1:nrow(boundaries_cp)) {
  
  boundaries_cp.i<-boundaries_cp[i,]
  inter_bird<-st_intersects(boundaries_cp.i,bird3, sparse = T)
  
  bird.list<-unique(bird3$binomial[inter_bird[[1]]])
  
  if(length(inter_bird[[1]])>0) {
    
    trans_bird3[nrow(trans_bird3)+1,"country"]=boundaries_cp$country[i]
    trans_bird3[nrow(trans_bird3),"bird_list"]<-toString(paste(bird.list,collapse = ","))#把
    
  } else {
    
    trans_bird3[nrow(trans_bird3)+1,"country"]<-boundaries_cp$country[i]
    
  }
  
  
  cat(i,"\t")
  
}

save(trans_bird3,file="./Revision 1/middle/trans_bird3.RData")
save(bird3,file="./Revision 1/middle/bird_selected/bird3.RData")
rm(bird3)
gc()

#bird3a

bird3a<-read_sf("D:/data/IUCN/Birds/Birds_3a.shp") %>%
  filter(presence%in% c(1,2,3),origin %in% c(1,2,6))%>%#presence:1 extant, 2 probably extant, 3 possibly extant
  st_transform(crs = "+proj=moll") %>%
  dplyr::select(SISID,binomial)

trans_bird3a<-data.frame(country=character(),
                         bird_list=character()
)

for (i in 1:nrow(boundaries_cp)) {
  
  boundaries_cp.i<-boundaries_cp[i,]
  inter_bird<-st_intersects(boundaries_cp.i,bird3a, sparse = T)
  
  bird.list<-unique(bird3a$binomial[inter_bird[[1]]])
  
  if(length(inter_bird[[1]])>0) {
    
    trans_bird3a[nrow(trans_bird3a)+1,"country"]=boundaries_cp$country[i]
    trans_bird3a[nrow(trans_bird3a),"bird_list"]<-toString(paste(bird.list,collapse = ","))#把
    
  } else {
    
    trans_bird3a[nrow(trans_bird3a)+1,"country"]<-boundaries_cp$country[i]
    
  }
  
  
  cat(i,"\t")
  
}

save(trans_bird3a,file="./Revision 1/middle/trans_bird3a.RData")
save(bird3a,file="./Revision 1/middle/bird_selected/bird3a.RData")
rm(bird3a)
gc()

####bird species richness, merge data####

load("./Revision 1/middle/trans_bird1.RData")
load("./Revision 1/middle/trans_bird1a.RData")
load("./Revision 1/middle/trans_bird2.RData")
load("./Revision 1/middle/trans_bird2a.RData")
load("./Revision 1/middle/trans_bird3.RData")
load("./Revision 1/middle/trans_bird3a.RData")

trans_birds<-data.frame(country=character(),
                        birds_sr=numeric(),
                        thr_birds_sr=numeric()
)

load("./middle/conservation_priorities/bird_infor2024.RData")

for (i in 1:170) {
  
  country<-trans_bird1$country[i]
  birds1<-as.list (strsplit (trans_bird1$bird_list[which(trans_bird1$country==country)], ",") )%>%as.data.frame()
  birds2<-as.list (strsplit (trans_bird1a$bird_list[which(trans_bird1a$country==country)], ",") )%>%as.data.frame()
  birds3<-as.list (strsplit (trans_bird2$bird_list[which(trans_bird2$country==country)], ",") )%>%as.data.frame()
  birds4<-as.list (strsplit (trans_bird2a$bird_list[which(trans_bird2a$country==country)], ",") )%>%as.data.frame()
  birds5<-as.list (strsplit (trans_bird3$bird_list[which(trans_bird3$country==country)], ",") )%>%as.data.frame()
  birds6<-as.list (strsplit (trans_bird3a$bird_list[which(trans_bird3a$country==country)], ",") )%>%as.data.frame()
  
  colnames(birds1)<-"birds_i_list"
  colnames(birds2)<-"birds_i_list"
  colnames(birds3)<-"birds_i_list"
  colnames(birds4)<-"birds_i_list"
  colnames(birds5)<-"birds_i_list"
  colnames(birds6)<-"birds_i_list"
  
  birds.i<-birds1%>%
    rbind(birds2)%>%
    rbind(birds3)%>%
    rbind(birds4)%>%
    rbind(birds5)%>%
    rbind(birds6)%>%
    unique()
  
  trans_birds[nrow(trans_birds)+1,"country"]=country
  trans_birds[nrow(trans_birds),"birds_sr"]=length(birds.i$birds_i_list)
  trans_birds[nrow(trans_birds),"thr_birds_sr"]=length(birds.i[which(birds.i$birds_i_list %in% bird.infor.thr$`Scientific name`),"birds_i_list"])
  
}


save(trans_birds,file = "./Revision 1/middle/sr_country_birds.RData")


### [sum]: Species richness ####

load("./Revision 1/middle/mammal_sr.RData")
load("./Revision 1/middle/amphibians_sr.RData")
load("./Revision 1/middle/reptiles_sr.RData")

#transboundary species richness and transboundary threatened species richness for each country

head(trans_amphibians)

country_species_richness<-merge(trans_mammal,trans_amphibians,by.x="country",by.y="country") %>%
  merge(trans_reptiles,by.x="country",by.y="country") %>%
  merge(trans_birds,by.x="country",by.y="country")

country_species_richness$species_richness<-country_species_richness$mammal_sr+country_species_richness$amphi_sr+country_species_richness$reptiles_sr+country_species_richness$birds_sr
country_species_richness$thr_sr<-country_species_richness$thr_mammal_sr+country_species_richness$thr_amphi_sr+country_species_richness$thr_reptiles_sr+country_species_richness$thr_birds_sr

save(country_species_richness,file = "./Revision 1/middle/country_species_richness.RData")
load("./Revision 1/middle/country_species_richness.RData")

## 2.2 Conservation priorities ####

load("./Revision 1/result/result3_moll.RData")

head(border4level_pro_shp)

boundaries1<-border4level_pro_shp%>%st_drop_geometry()
boundaries2<-border4level_pro_shp%>%st_drop_geometry()
soc_pair<-strsplit(boundaries1$SOCpair,"_")
boundaries1$country<-sapply(soc_pair,function(x){x[1]})
boundaries2$country<-sapply(soc_pair,"[",2)
head(boundaries1)

country_4level.length<-rbind(boundaries1[,c("border_4level_ID","country","borderlength","layers", "pri_level")],
                             boundaries2[,c("border_4level_ID","country","borderlength","layers", "pri_level")])

head(country_4level.length)
table(country_4level.length$pri_level)

country_cp<-country_4level.length %>%
  dplyr::filter(pri_level>0) %>%
  group_by(country) %>%
  summarise(cp123_length=sum(borderlength)) # km

## 2.3 Other variables ####

### 2.3.1 government####

gov<-read.csv("./input/influential factors/wgidataset_mean.csv",header = T,row.names = 1)

### 2.3.2 area of countries####

library(sf)
load("D:/chapter1/input/boundaries_cwn2024.RData")

#length of border/area
library(units)

load("./Revision 1/result/country_protected_length.RData")#border_length, country_length

cwn_moll <- read_sf("./input/cwn_moll.shp")

cwn_moll$area_km <- drop_units(st_area(cwn_moll))/1000000 #km2
cwn_moll <- cwn_moll %>% st_drop_geometry()

country_length_area <- country_length %>%
  filter(!grepl("Z0",country),!grepl("XKO",country),!grepl("XAD",country)) %>%
  merge(cwn_moll,by.x = "country",by.y = "GID_0")

### 2.3.3 population####
#population

pop0<-read.csv("./input/influential factors/API_SP.POP.TOTL_DS2_en_csv_v2_3628828/population.csv",header = T)

pop0<-pop0[,c("Country.Code","X2000","X2001","X2002","X2003","X2004","X2005","X2006","X2007","X2008",
              "X2009","X2010","X2011","X2012","X2013","X2014","X2015","X2016","X2017","X2018","X2019","X2020")]
pop0$pop_mean<-rowMeans(pop0[,2:21],na.rm=T)

#population growth
popgro<-read.csv("./input/influential factors/API_SP.POP.GROW_DS2_en_csv_v2_3643237/population growth.csv",header = T)
head(popgro)
popgro<-popgro[,c("Country.Code","X2000","X2001","X2002","X2003","X2004","X2005","X2006","X2007","X2008",
                  "X2009","X2010","X2011","X2012","X2013","X2014","X2015","X2016","X2017","X2018","X2019","X2020")]
popgro$popgro_mean<-rowMeans(popgro[,2:21],na.rm=T)

### 2.3.4  PA coverage####
###source: wdpa website
load("D:/chapter1/input/boundaries_cwn2024.RData")

cwn %>%
  filter(GID_0=="CHN")%>%
  st_make_valid() %>% #avoid error: self-intersection
  st_transform(crs = "+proj=moll") %>%
  st_union() %>%
  st_area()

wdpa_cover<-read.csv("./input/influential factors/wdpa_coverage.csv",header = T,row.names = 1)

wdpa_cover%>%
  filter(country=="CHN")

### 2.3.5  gdp####
#2000-2020 mean gdp per capita

gdp<-read.csv("./input/influential factors/API_NY.GDP.PCAP.CD_DS2_en_csv_v2_3632113/gdp_pcap.csv",header = T)

gdp<-gdp[,c("Country.Code","X2000","X2001","X2002","X2003","X2004","X2005","X2006","X2007","X2008",
            "X2009","X2010","X2011","X2012","X2013","X2014","X2015","X2016","X2017","X2018","X2019","X2020")]
gdp$gdp_mean<-rowMeans(gdp[,2:21],na.rm=T)

#2000-2020 mean gdp growth rate

gdpgro<-read.csv("./input/influential factors/API_NY.GDP.MKTP.KD.ZG_DS2_en_csv_v2_3628907/gdp_growth.csv",header = T)
gdpgro<-gdpgro[,c("Country.Code","X2000","X2001","X2002","X2003","X2004","X2005","X2006","X2007","X2008",
                  "X2009","X2010","X2011","X2012","X2013","X2014","X2015","X2016","X2017","X2018","X2019","X2020")]
gdpgro$gdpgro_mean<-rowMeans(gdpgro[,2:21],na.rm=T)

save(gov,pop0,popgro,country_length1,gdp,gdpgro,wdpa_cover,
     country_species_richness,country_cp,country_length_area, 
     file = "./Revision 1/middle/variables.RData")

#3 Regression ####

## 3.1 prepare data ####

load("./2 Revision 1/middle/variables.RData")
load("./2 Revision 1/result/country_protected_length.RData")#border_length, country_length

head(country_length_area)

lm.data<-country_length_area %>% 
  merge(gov[,c("country","mean")],by.x = "country",by.y = "country",all.x = T) %>%
  merge(pop0[,c("Country.Code","pop_mean")],by.x = "country",by.y = "Country.Code",all.x = T)%>%
  merge(y=popgro[,c("Country.Code","popgro_mean")],by.x = "country",by.y = "Country.Code",all.x = T) %>%
  merge(y=gdpgro[,c("Country.Code","gdpgro_mean")],by.x = "country",by.y = "Country.Code",all.x = T) %>%
  merge(y=gdp[,c("Country.Code","gdp_mean")],by.x = "country",by.y = "Country.Code",all.x = T) %>%
  merge(wdpa_cover[,c("country","coverage")],by.x = "country",by.y = "country",all.x = T) %>%
  merge(country_species_richness[,c("country","species_richness","thr_sr")],by.x = "country",by.y = "country",all.x = T) %>%
  merge(country_cp[,c("country","cp123_length")],by.x = "country",by.y = "country",all.x = T)

summary(lm.data$cp123_length)

lm.data$cp123_length[is.na(lm.data$cp123_length)]<-0
lm.data.complete <- lm.data[complete.cases(lm.data),] 

head(lm.data.complete)
class(lm.data.complete$area_km)

lm.data.complete$pop_den<-as.numeric(lm.data.complete$pop_mean/lm.data.complete$area_km) # people/km2
lm.data.complete$border_area<-as.numeric(lm.data.complete$blength/lm.data.complete$area_km) # border-area ratio
lm.data.complete$cp_pro<-as.numeric(lm.data.complete$cp123_length/lm.data.complete$blength) # proportion of conservation priorities
lm.data.complete$log_area_km<-log(lm.data.complete$area_km)
lm.data.complete$log_border_area<-log(lm.data.complete$border_area)
lm.data.complete$log_pd<-log(lm.data.complete$pop_den)
lm.data.complete$log_gdp_mean<-log(lm.data.complete$gdp_mean)

data_st<-scale(lm.data.complete[,c("pPro","mean","log_border_area","coverage",
                                   "log_gdp_mean","gdpgro_mean","log_pd","popgro_mean",
                                   "species_richness","thr_sr","cp_pro","log_area_km")]) %>% 
  as.data.frame()

row.names(data_st)<-lm.data.complete$country
head(data_st)
colnames(data_st)<-c("pPro","Governance","border_area","PACoverage",
                     "GDP","GDPGrowth","pd","PopGrowth","richness","thr_richness","cp_pro","country_area")

save(lm.data,data_st,file="./Revision 1/middle/lm_data0923.RData")
load("./2 Revision 1/middle/lm_data.RData")

#### Spearman’s rank correlation ####

cor1<-as.dist(round(cor(data_st),4))
cor1

#### linear regression ####

head(data_st)

fit0<-lm(pPro ~ Governance +
          border_area+ 
          country_area +
          PACoverage + 
          PopGrowth+
          GDPGrowth+
          GDP+
          pd+
          richness+
          thr_richness+
          cp_pro+
          pd:PACoverage+
          GDPGrowth:PACoverage+
          Governance:PACoverage+
          PopGrowth:PACoverage+
          Governance:pd+
          Governance:PopGrowth+
          Governance:GDPGrowth+
          Governance:GDP+
          GDPGrowth:PopGrowth,
        data=data_st)
par(mfrow=c(2,2))
plot(fit0)

# regression diagnostics
library(car)

qqPlot(fit,id.method="identify")# QQ plot

durbinWatsonTest(fit0)# Durbin-Watson statistics

#c.线性(需要去掉交互项)

fit1<-lm(pPro ~ Governance +
           border_area + 
           country_area+
           PACoverage + 
           PopGrowth+
           GDPGrowth+
           GDP+
           pd+
           richness+
           thr_richness+
           cp_pro,
         data=data_st)

crPlots(fit1)

spreadLevelPlot(fit0)

vif(fit0)
sqrt(vif(fit0))>2

fit<-lm(pPro ~ Governance +
          country_area +
          PACoverage + 
          PopGrowth+
          GDPGrowth+
          pd+
          richness+
          cp_pro+
          pd:PACoverage+
          GDPGrowth:PACoverage+
          Governance:PACoverage+
          PopGrowth:PACoverage+
          Governance:pd+
          Governance:PopGrowth+
          Governance:GDPGrowth+
          GDPGrowth:PopGrowth+
          Governance:country_area
        ,
        data=data_st)
vif(fit)

#model selection

library(MASS)

stepAIC(fit,direction = "both")

#best:

fit.best<-lm(formula = pPro ~ Governance + country_area + PACoverage + 
               PopGrowth + GDPGrowth + pd + cp_pro + PACoverage:PopGrowth + 
               Governance:PopGrowth + PopGrowth:GDPGrowth, data = data_st)

summary(fit.best)


#contribution

library(relaimpo)

calc.relimp(pPro ~ Governance + country_area + PACoverage + 
              PopGrowth + GDPGrowth + pd + cp_pro + PACoverage:PopGrowth + 
              Governance:PopGrowth + PopGrowth:GDPGrowth
            ,
            data_st,
            type = c("lmg"),
            rela = TRUE
            
)

fit.re <- read.table ("clipboard",header=T) # read from clipboard
fit.re

# 4 plot: fig2 ####

load("D:/chapter1/input/boundaries_cwn2024.RData")

library(ggplot2)

###fig2.1 ####

countries_plength<-merge(cwn,country_length,all.x=T,by.x="GID_0",by.y="country")
glo <- read_sf("D:/chapter1/input/country-boundaries/countries_new1_noATA.shp") %>% filter(GID_0!="ATA")
countries_plength$pPro_100<-countries_plength$pPro*100

head(countries_plength)

fig2.1<-ggplot()+
  geom_sf(data=glo,color=NA,fill="#E1E1E1")+
  geom_sf(data=countries_plength,color="#333333",aes(fill=pPro_100),size=0.1)+
  scale_fill_gradient2(low = "#4B83A6", mid="#F2F2F2",high = "#BF7878",
                       name="Proportion of \nprotection",
                       midpoint = 45,
                       breaks=c(0,45,90),
                       labels=c("0","45","90")
  )+
  theme_bw()+
  coord_sf(xlim = c(-180,180),
           ylim = c(-60,90))+
  theme(legend.position = c(0.5,0.1),#'bottom',
        legend.direction = "horizontal",
        legend.background = element_blank(),
        panel.grid = element_blank(),
        legend.title = element_text(size=8),#size=12
        legend.key.width = unit(0.18, 'in'),
        legend.key.height = unit(0.1, 'in'),
        axis.text.x = element_text(size=7)) 
fig2.1

###fig2.2 barplot####

fit.re

fit.re$vatiables<-c("Governance",
                    "Country size",
                    "PA coverage",
                    "PGR",
                    "GDP growth rate",
                    "PD",
                    "Conservation Priorities",
                    "PA coverage × PGR",
                    "Governance × PGR",
                    "GDP growth rate × PGR"
)

fig2.2 <- ggplot(fit.re,aes(x=reorder(vatiables,lmg),y=lmg*100))+
  geom_bar(stat = 'identity',fill='lightgray',width = 0.6)+
  coord_flip()+
  theme_bw()+
  theme(panel.grid = element_blank(),
        axis.title.y = element_blank(),
        axis.title.x = element_text(size = 8), 
        axis.ticks.length.y = unit(-0.15,'cm'),
        axis.ticks.length.x = unit(-0.15,'cm'),
        axis.text.y = element_text(size=7), 
        axis.text.x = element_text(size=7) 
  )+
  ylab("Relative importance (%)")


### fig2.3 ####

conf<-confint(fit.best) %>% as.data.frame()
conf$variable<-row.names(conf)
conf$estimate<-coefficients(fit.best)
colnames(conf)<-c("conf.low","conf.high","term","estimate")

conf$term<-c("(Intercept)",
             "Governance",
             "Country size",
             "PA coverage",
             "PGR",
             "GDP growth rate",
             "PD",
             "Conservation Priorities",
             "PA coverage × PGR",
             "Governance × PGR",
             "GDP growth rate × PGR"
)

conf$term <- factor(conf$term,levels = c(
  "Conservation Priorities",
  "Country size",
  "GDP growth rate × PGR",
  "PD",
  "Governance × PGR",  
  "GDP growth rate",
  "PA coverage × PGR",
  "PGR",
  "Governance",
  "PA coverage"
))


library(GGally)
library(ggpubr)

fig2.3<-GGally::ggcoef(conf[c(2:11),],
                       mapping = aes(x = estimate, y = term),
                       errorbar_color = c("red","red","red","red","black","red","black","red","red","red"),
                       color = c("red","red","red","red","black","red","black","red","red","red"),
                       size=1,
                       exclude_intercept = TRUE,
                       sort="none")+
  theme_bw()+
  theme(panel.grid = element_blank(),
        axis.ticks.length.y = unit(-0.15,'cm'),
        axis.ticks.length.x = unit(-0.15,'cm'),
        axis.title.x = element_text(size = 8),
        axis.text.x = element_text(size=7),
        axis.text.y = element_blank())+
  xlab("Coefficients")+
  ylab(" ")

fig2 <- ggpubr::ggarrange(fig2.1,
                        ggarrange(fig2.2, fig2.3, ncol = 2, labels = c("B", "C"),widths=c(1,0.7)), 
                        nrow = 2, 
                        labels = "A",
                        widths=c(1,1),
                        align="v"
) 

ggsave(fig2, file='./3 revision2/result/fig2.tiff', width=6.5, height=6.8,units = "in", dpi = 300)
ggsave(fig2, file='./3 revision2/Figures/Figure 2.pdf', width=6.5, height=6.8,units = "in", dpi = 300)

#5. sfig5 ####

library(ggsci)
library(ggrepel)

head(lm.data.complete)
lm.data.complete[which(lm.data.complete$pPro==max(lm.data.complete$pPro)),]

sfig5.plot.data<-lm.data.complete[,c("country","pPro","mean","log_border_area","coverage",
                               "popgro_mean","gdpgro_mean","log_pd")]

continent<-read.csv("./input/continent_country.csv",header = T)
names(continent)<-c("continent","sub.regions","region","intermediary.regions","Country.or.Area","M49.code","ISO.alpha3.code","Other.groupings")

plot.data.con<-merge(sfig5.plot.data,continent[,c("region","ISO.alpha3.code","Country.or.Area")],all.x = T,by.x = "country",by.y = "ISO.alpha3.code")

plot.data.con$pPro <- 100*plot.data.con$pPro
pointstolabel1<-plot.data.con[complete.cases(plot.data.con),] %>%
  top_n(n=3,wt=pPro)
pointstolabel2<-plot.data.con[complete.cases(plot.data.con),] %>%
  top_n(n=-2,wt=pPro)

###sfig5.1 pa coverage####

pointstolabel_pa1<-plot.data.con[complete.cases(plot.data.con),] %>%
  top_n(n=2,wt=coverage)
pointstolabel_pa2<-plot.data.con[complete.cases(plot.data.con),] %>%
  top_n(n=-2,wt=coverage)
pointstolabel_pa<-rbind(pointstolabel1,pointstolabel_pa1,pointstolabel_pa2) %>% unique()#,pointstolabel2

sfig5.1 <- ggplot(data=plot.data.con[complete.cases(plot.data.con),],aes(x=coverage,y=pPro,color=region))+
  geom_point(shape=20,size=1.5,stroke = 1,alpha=0.7)+
  scale_color_npg() +
  xlab("PA Coverage (%)") +
  ylab("Proportion of protected border (%)")+
  geom_smooth(method = "lm",color="black", formula = y ~ x,se=F,linewidth=1)+
  geom_ribbon(aes(x=coverage,y=pPro),formula = y ~ x,
              data=plot.data.con[complete.cases(plot.data.con),],
              stat = "smooth",
              method = "lm",
              se = TRUE,
              alpha = 0, # or, use fill = NA
              colour = "black",
              linetype = "dotted",
              size = 0.7)+
  theme_bw()+
  theme(legend.position = "none",
        panel.grid = element_blank(),
        axis.ticks.length.y = unit(-0.15,'cm'),
        axis.ticks.length.x = unit(-0.15,'cm'),
        axis.title.x = element_text(size = 8),
        axis.title.y = element_text(size = 8),
        axis.text.y = element_text(size=7),
        axis.text.x = element_text(size=7))+
  scale_y_continuous(limits = c(0, 100))+
  geom_text_repel(aes(label=Country.or.Area),color="gray20",
                  data = subset(plot.data.con,Country.or.Area %in% pointstolabel_pa$Country.or.Area),
                  force = 10,size=2)

###sfig5.2 governance####

pointstolabel_gov1<-plot.data.con[complete.cases(plot.data.con),] %>%
  top_n(n=2,wt=mean)
pointstolabel_gov2<-plot.data.con[complete.cases(plot.data.con),] %>%
  top_n(n=-2,wt=mean)
pointstolabel_gov<-rbind(pointstolabel1,pointstolabel_gov1,pointstolabel_gov2) %>% unique()

sfig5.2 <- ggplot(data=plot.data.con[complete.cases(plot.data.con),],aes(x=mean,y=pPro,color=region))+
  geom_point(shape=20,size=1.5,stroke = 1,alpha=0.7)+
  scale_color_npg()+
  xlab("Governance")+
  ylab("Proportion of protected border (%)")+
  geom_smooth(method = "lm",color="black", formula = y ~ x,se=F,linewidth=1)+
  geom_ribbon(aes(x=mean,y=pPro),formula = y ~ x,
              data=plot.data.con[complete.cases(plot.data.con),],
              stat = "smooth",
              method = "lm",
              se = TRUE,
              alpha = 0, # or, use fill = NA
              colour = "black",
              linetype = "dotted",
              size = 0.7)+
  theme_bw()+
  theme(legend.position = "none",
        panel.grid = element_blank(),
        axis.ticks.length.y = unit(-0.15,'cm'),
        axis.ticks.length.x = unit(-0.15,'cm'),
        axis.title.x = element_text(size = 8),
        axis.title.y = element_text(size = 8),
        axis.text.y = element_text(size=7),
        axis.text.x = element_text(size=7))+
  scale_y_continuous(limits = c(0, 100))+
  geom_text_repel(aes(label=Country.or.Area),color="gray20",
                  data = subset(plot.data.con,Country.or.Area %in% pointstolabel_gov$Country.or.Area),force = 10,size=2)

sfig5.2

###sfig5.3 pgr####

plot.data.con$region[which(plot.data.con$region=="Latin America and the Caribbean")] <- "LAC"
plot.data.con$region[which(plot.data.con$region=="Northern America")] <- "NA"
pointstolabel_pog1 <- plot.data.con[complete.cases(plot.data.con),] %>%
  top_n(n=1,wt=popgro_mean)
pointstolabel_pog2 <- plot.data.con[complete.cases(plot.data.con),] %>%
  top_n(n=-1,wt=popgro_mean)
pointstolabel_pog<-rbind(pointstolabel1,pointstolabel_pog1,pointstolabel_pog2) %>% unique() #pointstolabel2,

sfig5.3 <- ggplot(data=plot.data.con[complete.cases(plot.data.con),],aes(x=popgro_mean,y=pPro,color=region))+
  geom_point(shape=20,size=1.5,stroke = 1,alpha=0.7)+
  scale_color_npg()+
  xlab("Population growth rate (%)")+
  ylab("Proportion of protected border (%)")+
  geom_smooth(method = "lm",color="black", formula = y ~ x,se=F,linewidth=1)+
  geom_ribbon(aes(x=popgro_mean,y=pPro),formula = y ~ x,
              data=plot.data.con[complete.cases(plot.data.con),],
              stat = "smooth",
              method = "lm",
              se = TRUE,
              alpha = 0, # or, use fill = NA
              colour = "black",
              linetype = "dotted",
              size = 0.7)+
  theme_bw()+
  theme(legend.title=element_blank(),
        legend.text=element_text(size=7),
        legend.position=c(0.83,0.7),
        legend.background = element_rect(color = "black", linetype = "solid", size = 0.1),
        panel.grid = element_blank(),
        axis.ticks.length.y = unit(-0.15,'cm'),
        axis.ticks.length.x = unit(-0.15,'cm'),
        axis.title.x = element_text(size = 8),
        axis.title.y = element_text(size = 8),
        axis.text.y = element_text(size=7),
        axis.text.x = element_text(size=7))+
  scale_y_continuous(limits = c(0, 100))+
  geom_text_repel(aes(label=Country.or.Area),color="gray20",data = subset(plot.data.con,Country.or.Area %in% pointstolabel_pog$Country.or.Area),force = 10,size=2)


#
###sfig5.4 pgr:PA coverage####

data5 <- plot.data.con[complete.cases(plot.data.con),]

p<-median(data5$popgro_mean)
p

for (i in 1:nrow(data5)) {
  
  if(data5$popgro_mean[i]>p) {
    data5$pgr_class[i]<-"higher population growth rate"
  } else {
    data5$pgr_class[i]<-"lower population growth rate"
  }
  
}

data5$pgr_class<-factor(data5$pgr_class,levels = c("higher population growth rate","lower population growth rate"))

head(data5)
sfig5.4 <- ggplot(data=data5,aes(x=coverage,y=pPro))+
  #geom_point(size=3,aes(color=pgr_class),alpha=0.7)+
  geom_point(size=1.5,aes(color=pgr_class),alpha=0.7)+
  scale_color_manual(values = c("#4DBBD5","#E64B35"),
                     breaks=c("higher population growth rate","lower population growth rate"),
                     labels=c("Higher PGR","Lower PGR"))+
  xlab("PA coverage (%)")+
  ylab("Proportion of protected border (%)")+
  geom_smooth(method = "lm", color="#4DBBD5",
              aes(x=coverage,y=pPro),
              formula = y ~ x,
              data=data5[which(data5$pgr_class=="higher population growth rate"),],
              se=F,size=1)+
  geom_smooth(method = "lm",color="#E64B35", 
              aes(x=coverage,y=pPro),
              formula = y ~ x,
              data=data5[which(data5$pgr_class=="lower population growth rate"),],
              se=F,size=1)+#,fill="black"
  geom_ribbon(aes(x=coverage,y=pPro),formula = y ~ x,
              data=data5[which(data5$pgr_class=="higher population growth rate"),],
              stat = "smooth",
              method = "lm",
              se = TRUE,
              alpha = 0, # or, use fill = NA
              colour = "#4DBBD5",
              linetype = "dotted",
              size=0.7)+
  geom_ribbon(aes(x=coverage,y=pPro),formula = y ~ x,
              data=data5[which(data5$pgr_class=="lower population growth rate"),],
              stat = "smooth",
              method = "lm",
              se = TRUE,
              alpha = 0, # or, use fill = NA
              colour = "#E64B35",
              linetype = "dotted",
              size=0.7)+
  theme_bw()+
  theme(legend.title=element_blank(),
        legend.text=element_text(size=7),
        legend.position=c(0.35,0.92),
        legend.direction="horizontal",
        legend.background = element_rect(color = "black", linetype = "solid", size = 0.1,fill = NA),
        panel.grid = element_blank(),
        axis.ticks.length.y = unit(-0.15,'cm'),
        axis.ticks.length.x = unit(-0.15,'cm'),
        axis.title.x = element_text(size = 8),
        axis.title.y = element_text(size = 8),
        axis.text.y = element_text(size=7),
        axis.text.x = element_text(size=7))+
  scale_y_continuous(limits = c(0, 110), n.breaks = 5) +
  geom_text_repel(aes(label=Country.or.Area),color="gray20",data = subset(data5,Country.or.Area %in% pointstolabel_pa$Country.or.Area),force = 10,size=2)


library(patchwork)

# layout<-'
# AABBCCDD
# #EEFFGG#
# '

layout<-'
AB
CD
'
sfig5 <- sfig5.1+sfig5.2+sfig5.3+sfig5.4 +
  plot_annotation(tag_levels = "A")+
  plot_layout(design = layout)

ggsave(sfig5,filename = "./3 revision2/result/sfig5.tiff",width = 7,height = 7,dpi = 300)
ggsave(sfig5,filename = "./3 revision2/Figures/Supplementary Figure 5.pdf",width = 7,height = 7,dpi = 300)
