
setwd("D:/chapter1")

library(sf)
library(dplyr)

# ----- 1. Select BPAs from WDPAs --------------- ####

## ----- Input data:
# PAs of each country: "./input/splitByISO3_all"
load("D:/chapter1/input/boundaries_cwn2024.RData") # 430 international boundaries; 170 countries which have at least 1 neighboring country.

# For each side of each international border, input all PAs of that country
# Multipart to singlepart polygon
# For each polygon, calculate the distance to international borders
# Select polygons with a distance <= 1000 m as BPAs.
# Write selected polygons to [all_country_1km]

border<-boundaries[1,] %>% st_transform(crs = "+proj=moll") #one border
pair_name<-border$SOCpair
country_name<-pair_name %>%
  strsplit(split = "_") %>%
  as.data.frame()

country<-country_name[1,1]
filenamec<-paste("./input/splitByISO3_all/",country,".shp",sep = "")
file.exists(filenamec)
country_shp<-filenamec %>%
  read_sf() %>%
  st_transform(crs = "+proj=moll") %>% #Mollweide projection
  st_cast("POLYGON") #Cast geometry to single polygons
country_shp$distancetoBorder<-as.numeric(st_distance(country_shp,border,by_element=T))
all_country_1km<-subset(country_shp,country_shp$distancetoBorder<=1000)

country<-country_name[2,1]
filenamec<-paste("./input/splitByISO3_all/",country,".shp",sep = "")
file.exists(filenamec)
country_shp<-filenamec %>%
  read_sf() %>%
  st_transform(crs = "+proj=moll") %>%#Mollweide projection
  st_cast("POLYGON") #Cast geometry to single polygons
country_shp$distancetoBorder<-as.numeric(st_distance(country_shp,border,by_element=T))
country_1km<-subset(country_shp,country_shp$distancetoBorder<=1000)
all_country_1km<-rbind(all_country_1km,country_1km)

for (i in 2:nrow(boundaries)) {
  border<-boundaries[i,] %>% st_transform(crs = "+proj=moll")#one border
  pair_name<-border$SOCpair
  country_name<-pair_name %>%
    strsplit(split = "_") %>%
    as.data.frame()
  for (j in 1:2) {
    country<-country_name[j,1]
    filenamec<-paste("./input/splitByISO3_all/",country,".shp",sep = "")
    if (file.exists(filenamec)) {
      country_shp<-filenamec %>%
        read_sf() %>%
        st_transform(crs = "+proj=moll") %>%#Mollweide projection
        st_cast("POLYGON") #Cast geometry to single polygons
      country_shp$distancetoBorder<-as.numeric(st_distance(country_shp,border,by_element=T))
      country_1km<-subset(country_shp,country_shp$distancetoBorder<=1000)
      if (nrow(country_1km)>0) {
        all_country_1km<-rbind(all_country_1km,country_1km)
        cat(i,"\n")
      } 
    }
  }
}

write_sf(all_country_1km,"./result",layer="fig1_world_pl_in1km", driver="ESRI Shapefile",delete_layer=T)
save(all_country_1km,file = "./result/fig1_world_pl_in1km.RData")

# ----- 2. Adjacent cross-boundary BPAs -------- ####

country_neighbor<-data.frame(country1=character(),
                             country2=character()
)

for (i in 1:nrow(boundaries)) {
  soc_pair<-strsplit(boundaries$SOCpair[i],"_") %>% as.data.frame()
  country_neighbor[i,"country1"]<-soc_pair[1,1]
  country_neighbor[i,"country2"]<-soc_pair[2,1]
}
country_neighbor2<-country_neighbor
colnames(country_neighbor2)[1] = 'country2'
colnames(country_neighbor2)[2] = 'country1'

country_neighbor<-rbind(country_neighbor,country_neighbor2) %>%
  unique() # all combinations of countries

# countries that have BPAs
country.list<-unique(all_country_1km$ISO3[!grepl(";",all_country_1km$ISO3)])

cwn_moll<-read_sf("./input/cwn_moll.shp") # Polygons of countries

# For country i, get all BPAs
# To avoid double counting of overlapped polygons, union all BPAs, then turn multipart polygons into singlepart polygons.
i=1
country0<-country.list[i]
polygon.i<-cwn_moll[which(cwn_moll$GID_0==country0),]
country0.pa<-
  all_country_1km[grepl(country0,all_country_1km$ISO3),] %>%
  st_make_valid() %>% #avoid error: self-intersection
  st_union() %>% #returns a single geometry with resolved boundaries
  st_cast("POLYGON") %>%
  st_as_sf() %>%
  st_intersection(polygon.i) # avoid include BPAs of neighboring countries because of TBPAs.

country0.pa <- country0.pa %>%
  mutate(FID = row_number())

# Get all BPAs from all neighboring countries:
neighbors<-country_neighbor[which(country_neighbor$country1==country0),"country2"]

neighbors.pa <- all_country_1km[grepl(paste(neighbors,collapse = "|"),all_country_1km$ISO3),] %>%
  st_make_valid()  %>% # include TBPAs
  st_difference(polygon.i) # to avoid the influence of TBPAs.

j=1 # BPA polygon j
country0_pa<-country0.pa[j,]
distonei<-as.numeric(st_distance(country0_pa,neighbors.pa,by_element=F)) # meter
country0.pa.dis<-data.frame(FID=numeric(),
                            min_dist_nei=numeric(),
                            ISO3=character(),
                            has_neighbor=character()
)
country0.pa.dis[1,"min_dist_nei"]<-min(distonei)
country0.pa.dis[1,"ISO3"]<-country0
country0.pa.dis[1,"FID"]<-country0.pa$FID[j]
if(country0.pa.dis[1,"min_dist_nei"]==0) {
  country0.pa.dis[1,"has_neighbor"]<-"t"
} else {
  country0.pa.dis[1,"has_neighbor"]<-"f"
}
bpa_union<-merge(country0_pa,country0.pa.dis,by.x = "FID",by.y = "FID")

for (j in 2:nrow(country0.pa)) {
  country0_pa<-country0.pa[j,]
  distonei<-as.numeric(st_distance(country0_pa,neighbors.pa,by_element=F))#meter
  country0.pa.dis<-data.frame(FID=numeric(),
                              min_dist_nei=numeric(),
                              ISO3=character(),
                              has_neighbor=character()
  )
  country0.pa.dis[1,"min_dist_nei"]<-min(distonei)
  country0.pa.dis[1,"ISO3"]<-country0
  country0.pa.dis[1,"FID"]<-country0.pa$FID[j]
  if(country0.pa.dis[1,"min_dist_nei"]==0) {
    country0.pa.dis[1,"has_neighbor"]<-"t"
  } else {
    country0.pa.dis[1,"has_neighbor"]<-"f"
  }
  country0.data<-merge(country0_pa,country0.pa.dis,by.x = "FID",by.y = "FID")
  bpa_union<-rbind(bpa_union,country0.data)
}

#
for (i in 2:length(country.list)) {#length(country.list)
  country0 <- country.list[i]
  polygon.i <- cwn_moll[which(cwn_moll$GID_0==country0),]
  country0.pa0<-all_country_1km %>%
    filter(grepl(country0,ISO3))
  if(nrow(country0.pa0) > 0){
    country0.pa<-country0.pa0 %>%
      st_make_valid() %>% #avoid error: self-intersection
      st_union() %>% #returns a single geometry with resolved boundaries
      st_cast("POLYGON")%>%
      st_as_sf() %>%
      st_intersection(polygon.i) # avoid include BPAs of neighboring countries because of TBPAs.
    
    country0.pa <- country0.pa %>%
      mutate(FID = row_number())
    
    neighbors<-country_neighbor[which(country_neighbor$country1==country0),"country2"]

    if (length(neighbors)>1) {
      
      neighbors.pa <- all_country_1km[grepl(paste(neighbors,collapse = "|"),all_country_1km$ISO3),] %>%
        st_make_valid()  %>% # include TBPAs
        st_difference(polygon.i) # to avoid the influence of TBPAs.
      
    } else {
      
      neighbors.pa <- all_country_1km[grepl(neighbors,all_country_1km$ISO3),] %>%
        st_make_valid()  %>% # include TBPAs
        st_difference(polygon.i) # to avoid the influence of TBPAs.
      
    }

    for (j in 1:nrow(country0.pa)) {
      country0_pa<-country0.pa[j,]
      distonei<-as.numeric(st_distance(country0_pa,neighbors.pa,by_element=F))#meter
      country0.pa.dis<-data.frame(FID=numeric(),
                                  min_dist_nei=numeric(),
                                  ISO3=character(),
                                  has_neighbor=character()
      )
      country0.pa.dis[1,"min_dist_nei"]<-min(distonei)
      country0.pa.dis[1,"ISO3"]<-country0
      country0.pa.dis[1,"FID"]<-country0.pa$FID[j]
      if(country0.pa.dis[1,"min_dist_nei"]<=2000) {
        country0.pa.dis[1,"has_neighbor"]<-"t"
      } else {
        country0.pa.dis[1,"has_neighbor"]<-"f"
      }
      country0.data<-merge(country0_pa,country0.pa.dis,by.x = "FID",by.y = "FID")
      bpa_union<-rbind(bpa_union,country0.data)
    }
    
  }
  
  cat(i,country0,"\n")
}


save(bpa_union,file="D:/chapter1/Revision 1/bpa_union.RData")
save(bpa_union_id,file="D:/chapter1/Revision 1/bpa_union_id.RData")

write_sf(bpa_union,"D:/chapter1/Revision 1",layer="bpa_union2", driver="ESRI Shapefile",delete_layer=T)

bpa_union$has_neighbor<-ifelse(bpa_union$min_dist_nei<=2000,"t","f")

