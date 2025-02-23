setwd("D:/chapter1")

library(sf)
library(dplyr)
library(ggplot2)
library(ggpubr)
library(tidyr)
library(raster)
library(terra)
library(ggsci)
library(units)#drop_units
library(colorspace) 

# 1. refine 3 layers####

##----------------- 1.1 hfi data in 5km buffer borderlands ------------
# mask
# vecterise

load("D:/chapter1/input/boundaries_cwn2024.RData") # 430 international boundaries; 170 countries which have at least 1 neighboring country.

boundaries5km <- boundaries %>% st_transform(crs = "+proj=moll") %>%
  st_union() %>%
  st_make_valid() %>%
  st_buffer(dist = 5000) %>%
  st_as_sf()

hfi2018 <- terra::rast("D:/data/hfp2018/hfp2018.tif") %>% terra::project("+proj=moll")
hfi2018_5km <- mask(hfi2018,boundaries5km)
hfi2018_5km_2class <- hfi2018_5km
hfi2018_5km_2class[hfi2018_5km_2class<4] <- 1
hfi2018_5km_2class[hfi2018_5km_2class>4|hfi2018_5km_2class==4] <- NA
terra::writeRaster(hfi2018_5km_2class,filename = "./middle/conservation_priorities/hfi2018_5km_2class.tif",filetype="GTiff",overwrite=T)

# convert raster to vector in arcgis
# first convert cell value to integer: math---int

hfi2018_less4 <- read_sf("./middle/conservation_priorities/hfi_less4_5km.shp") %>%
  st_union() %>%
  st_make_valid()

# mask out in three layers------------------------------------------
## 1.2 G200--------------------------------------------------####

# select G200 located within a 1km buffer of borders, interaction with 1km buffer
# mask HFI with border G200, convert to vector
# clip border G200 with vectorised HFI

boundaries2km <- boundaries %>% st_transform(crs = "+proj=moll") %>%
  st_union() %>%
  st_make_valid() %>%
  st_buffer(dist = 2000) %>%
  st_as_sf()

g200 <- read_sf("./input/conservation_priorities/global200.shp") %>% st_transform(crs = "+proj=moll") 

g200_refine <- st_intersection(g200,boundaries2km) %>% st_intersection(hfi2018_less4)

write_sf(g200_refine,"./input/conservation_priorities",layer="g200_refine", driver="ESRI Shapefile",delete_layer=T)


## 1.3 KBA--------------------------------------------------####

kba<-read_sf("./input/conservation_priorities/kba202309version_polygon.shp")%>% st_transform(crs = "+proj=moll") %>% st_make_valid()

kba_refine<-st_intersection(kba,boundaries2km) %>% st_make_valid()%>%st_intersection(hfi2018_less4)

write_sf(kba_refine,"./input/conservation_priorities",layer="kba_refine", driver="ESRI Shapefile",delete_layer=T)


## 1.4 BH-------------

bh<-read_sf("./input/conservation_priorities/biodiversity_hotspots2016.shp") %>%
  filter(Type=="hotspot area") %>%
  st_transform(crs = "+proj=moll") %>% st_make_valid()

bh_refine<-st_intersection(bh,boundaries2km) %>% st_make_valid()%>% st_intersection(hfi2018_less4)

write_sf(bh_refine,"./input/conservation_priorities",layer="bh_refine", driver="ESRI Shapefile",delete_layer=T)

save(g200_refine,kba_refine,bh_refine,file = "./input/conservation_priorities/refine_3layers.RData")

# 2. interaction between 3layers and borders ####

## 2.1 single layers####

#bh

conser_pri <- bh_refine %>% #BH within 1 km to border
  st_transform(crs = "+proj=moll") %>%
  st_make_valid() %>% #avoid error: self-intersection
  st_union() %>% #returns a single geometry with resolved boundaries
  st_buffer(dist=1000) #1km buffer

for (i in 1:nrow(boundaries)) {
  
  border<-boundaries[i,] %>%st_transform(crs = "+proj=moll")#one border
  
  borderid<-border[1,"borderID"]
  pair_name<-border$SOCpair
  
  int<-st_intersection(border,conser_pri)
  
  if(nrow(int)>0) {
    
    int[1,"BHlength"]=st_length(int)/1000
    
    if (exists("border_bh")==F) {
      
      border_bh<-int
      
    }else {
      
      border_bh<-rbind(border_bh,int)
      
    }
  }
  
  
  cat(i,"done\n")
  
}

write_sf(border_bh,"./middle/conservation_priorities/0909",layer="border_bh", driver="ESRI Shapefile",delete_layer=T)


#G200

conser_pri<- g200_refine %>% #BH within 1 km to border
  st_transform(crs = "+proj=moll") %>%
  st_make_valid() %>% #avoid error: self-intersection
  st_union() %>% #returns a single geometry with resolved boundaries
  st_buffer(dist=1000) #1km buffer

for (i in 1:nrow(boundaries)) {
  
  border <- boundaries[i,] %>% st_transform(crs = "+proj=moll")#one border
  
  int <- st_intersection(border,conser_pri)
  
  if(nrow(int)>0) {
    
    int[1,"G200length"]=st_length(int)/1000
    
    if (exists("border_g200")==F) {
      
      border_g200<-int
      
    }else {
      
      border_g200<-rbind(border_g200,int)
      
    }
  }
  
  cat(i,"done\n")
  
}

write_sf(border_g200,"./middle/conservation_priorities/0909",layer="border_g200", driver="ESRI Shapefile",delete_layer=T)

#kba

conser_pri<- kba_refine %>% #BH within 1 km to border
  st_transform(crs = "+proj=moll") %>%
  st_make_valid() %>% #avoid error: self-intersection
  st_union() %>% #returns a single geometry with resolved boundaries
  st_buffer(dist=1000) #1km buffer

rm(border_kba)

for (i in 1:nrow(boundaries)) {
  
  border<-boundaries[i,] %>%st_transform(crs = "+proj=moll")#one border
  
  int<-st_intersection(border,conser_pri)
  
  if(nrow(int)>0) {
    
    int[1,"KBAlength"]=st_length(int)/1000
    
    if (exists("border_kba")==F) {
      
      border_kba<-int
      
    }else {
      
      border_kba<-rbind(border_kba,int)
      
    }
  }
  
  cat(i,"done\n")
  
}

write_sf(border_kba,"./middle/conservation_priorities/0909",layer="border_kba", driver="ESRI Shapefile",delete_layer=T)


save(border_kba,border_g200,border_bh,file = "./middle/conservation_priorities/0909/border_cp_3single.RData")

##2.2 7 priority levels####

conser_pri_bh<-bh_refine %>%#BH within 2 km to border
  st_transform(crs = "+proj=moll") %>%
  st_make_valid() %>% #avoid error: self-intersection
  st_union() %>% #returns a single geometry with resolved boundaries
  st_buffer(dist=1000) #1km buffer

conser_pri_g200<- g200_refine %>% #g200 within 2 km to border
  st_transform(crs = "+proj=moll") %>%
  st_make_valid() %>% #avoid error: self-intersection
  st_union() %>% #returns a single geometry with resolved boundaries
  st_buffer(dist=1000) #1km buffer

#kba_1km<-read_sf("./input/conservation_priorities/kba202309version_polygon_1km.shp")
conser_pri_kba<-kba_refine %>%
  st_transform(crs = "+proj=moll") %>%
  st_make_valid() %>%
  st_union() %>% #returns a single geometry with resolved boundaries
  st_buffer(dist=1000) #1km buffer

boundaries_moll<-boundaries %>%
  st_transform(crs = "+proj=moll")

write_sf(boundaries_moll,"./middle/conservation_priorities/0909",layer="boundaries_moll", driver="ESRI Shapefile",delete_layer=T)

# based on kba, get level i,iv,i,vii
border_kba<- st_intersection(boundaries_moll,conser_pri_kba)

kba_bh<-st_intersection(border_kba,conser_pri_bh)

border_vii<-st_intersection(kba_bh,conser_pri_g200)
border_vii$layers<-"kba,bh,g200"

border_v<-st_difference(kba_bh,conser_pri_g200)
border_v$layers<-"bh,kba"

kba_notbh<-st_difference(border_kba,conser_pri_bh)

border_i<-kba_notbh %>%
  st_difference(conser_pri_g200)
border_i$layers<-"kba"

border_iv<-kba_notbh%>%
  st_intersection(conser_pri_g200)
border_iv$layers<-"kba,g200"

#
border_iii<-st_intersection(boundaries_moll,conser_pri_bh)%>%
  st_difference(conser_pri_kba)%>%
  st_difference(conser_pri_g200)
border_iii$layers<-"bh"

#
border_g200<-st_intersection(boundaries_moll,conser_pri_g200)

g200_notkba<-border_g200%>%
  st_difference(conser_pri_kba)

border_ii<-g200_notkba%>%
  st_difference(conser_pri_bh)
border_ii$layers<-"g200"

border_vi<-g200_notkba %>%
  st_intersection(conser_pri_bh)
border_vi$layers<-"g200,bh"

border_0<-st_difference(boundaries_moll,conser_pri_g200) %>%
  st_difference(conser_pri_bh) %>%
  st_difference(conser_pri_kba)

border_0$pri_level<-"0"
border_0$layers<-""

border_level1<-rbind(border_i,border_ii) %>%
  rbind(border_iii)
border_level1$pri_level<-"1"

border_level2<-rbind(border_iv,border_vi) %>%
  rbind(border_v)
border_level2$pri_level<-"2"

border_vii$pri_level<-"3"

border_4level0<-border_level1 %>%
  rbind(border_level2) %>%
  rbind(border_vii) %>%
  rbind(border_0) %>%
  st_make_valid()
head(border_4level)

# check: total length before and after clip

border_4level0 %>%
  mutate(border_length=st_length(border_4level0)) %>%
  st_drop_geometry() %>%
  #group_by(layers) %>%
  summarise(sum_length=sum(border_length)/1000)

boundaries_moll %>%
  mutate(border_length=st_length(boundaries_moll)) %>%
  st_drop_geometry() %>%
  summarise(sum_length=sum(border_length)/1000)

# equal(263868 km)

#remove points
border_4level <- st_collection_extract(border_4level0, type = "LINESTRING") # extract LINESTRING from GEOMETRYCOLLECTION

write_sf(border_4level,"./middle/conservation_priorities/0909",layer="border_4level", driver="ESRI Shapefile",delete_layer=T)

save(border_4level,file="./middle/conservation_priorities/0909/border_4level.RData")

# convert multipart to singlepart in arcgis
# save the result as "./middle/conservation_priorities/0909/border_4level_single.shp"

border_4level_single <- st_read("./middle/conservation_priorities/0909/border_4level_single.shp") %>%
  st_set_crs("+proj=moll")

##2.3 conservation extent ####

border_4level_single$border_4level_ID<-rownames(border_4level_single)

load("./Revision 1/middle/bpa_union_id.RData") #bpa_union_id

### 2.3.1 conservation####

border4level_length0 <- data.frame(border_4level_ID=character(),
                                 borderID=numeric(),# link to boundaries_moll
                                 ISO3_pair=character(),
                                 borderlength=numeric(),
                                 protectedLength=numeric()
)

country_pairs <- unique(border_4level_single$SOCpair)

## for each country-pair, union all BPAs first

border4level_length0 <- data.frame(border_4level_ID=character(),
                                   borderID=numeric(),# link to boundaries_moll
                                   #fid=numeric(),
                                   ISO3_pair=character(),
                                   borderlength=numeric(),
                                   protectedLength=numeric()
)

for (i in 1:length(country_pairs)){ #length(country_pairs)
  
  pair_name<-country_pairs[i]
  
  country_name<-pair_name %>%
    strsplit(split = "_") %>%
    as.data.frame()
  country_name<-country_name[,1]
  
  bpa.iso3s <- bpa_union_id %>%
    filter(ISO3 %in% country_name)
  
  if (nrow(bpa.iso3s)>0) {
    
    country_union_buffer <- bpa.iso3s %>% 
      st_make_valid() %>% #avoid error: self-intersection
      st_union() %>% #returns a single geometry with resolved boundaries
      st_buffer(dist=1000) #1km buffer
    
    borders <- border_4level_single[which(border_4level_single$SOCpair==pair_name),]
    
    for (j in 1:nrow(borders)) {
      
      border<-borders[j,]
      
      border4id<-border[1,"border_4level_ID"]
      borderid<-border[1,"borderID"]

      border4level_length0[nrow(border4level_length0)+1,"borderID"]=borderid
      border4level_length0[nrow(border4level_length0),"border_4level_ID"]=border4id
      border4level_length0[nrow(border4level_length0),"ISO3_pair"]=pair_name
      border4level_length0[nrow(border4level_length0),"borderlength"]=st_length(border)/1000
      
      int<-st_intersection(border,country_union_buffer)
      
      if (length(st_length(int))>0) {
        
        border4level_length0[nrow(border4level_length0),"protectedLength"] = st_length(int)/1000 #border length overlapping with pa
        
      }else {
        
        border4level_length0[nrow(border4level_length0),"protectedLength"] = 0
        
      }

    }

    
  } else { # if there is no BPAs in both countries
    
    borders <- border_4level_single[which(border_4level_single$SOCpair==pair_name),]
    
    for (j in 1:nrow(borders)) {
      
      border<-borders[j,]
      
      border4id<-border[1,"border_4level_ID"]
      borderid<-border[1,"borderID"]
      border4level_length0[nrow(border4level_length0)+1,"borderID"]=borderid
      border4level_length0[nrow(border4level_length0),"border_4level_ID"]=border4id
      border4level_length0[nrow(border4level_length0),"ISO3_pair"]=pair_name
      border4level_length0[nrow(border4level_length0),"borderlength"]=st_length(border)/1000
      border4level_length0[nrow(border4level_length0),"protectedLength"]=0
    }

  }
  
  cat(i,pair_name,"done\n")
}

border4level_length0$pPro<-border4level_length0$protectedLength/border4level_length0$borderlength

border4level_pro_shp<-merge(border_4level_single,border4level_length0[,c("border_4level_ID","borderlength","protectedLength","pPro")],by.x="border_4level_ID",by.y="border_4level_ID",all.x=T)
write_sf(border4level_pro_shp,"./Revision 1/result",layer="border4level_pro", driver="ESRI Shapefile",delete_layer=T)

### 2.3.2 two-sided conservation---------------------------------------------------------------####

border4level_both_length0<-data.frame(borderID=character(),
                                      border_4level_ID=numeric(),
                                      ISO3_pair=character(),
                                      borderlength=numeric(),
                                      two_ProLen=numeric()
)


# country-pair ---> border segments

for (i in 1:length(country_pairs)) {
  
  pair_name<-country_pairs[i]
  
  country_name <- pair_name %>%
    strsplit(split = "_") %>%
    as.data.frame()
  country_name <- country_name[,1]
  
  bpa.iso31 <- bpa_union_id %>%
    filter(ISO3 == country_name[1])
  bpa.iso32 <- bpa_union_id %>%
    filter(ISO3 == country_name[2])
  
  borders <- border_4level_single[which(border_4level_single$SOCpair==pair_name),]
  
  if (nrow(bpa.iso31)>0 & nrow(bpa.iso32)>0) { # if the two countries both have BPAs
    
    country1_union_buffer <- bpa.iso31 %>%
      st_make_valid() %>% #avoid error: self-intersection
      st_union() %>% #returns a single geometry with resolved boundaries
      st_buffer(dist=1000) #1km buffer
    country2_union_buffer<- bpa.iso32 %>%
      st_make_valid() %>% #avoid error: self-intersection
      st_union() %>% #returns a single geometry with resolved boundaries
      st_buffer(dist=1000) #1km buffer
    
    for  (j in 1:nrow(borders)) {
      
      border <- borders[j,]
      
      border4id <- border[1,"border_4level_ID"]
      borderid <- border[1,"borderID"]
      border4level_both_length0[nrow(border4level_both_length0)+1,"borderID"]=borderid
      border4level_both_length0[nrow(border4level_both_length0),"border_4level_ID"]=border4id
      border4level_both_length0[nrow(border4level_both_length0),"ISO3_pair"]=pair_name
      border4level_both_length0[nrow(border4level_both_length0),"borderlength"]=st_length(border)/1000
      border4level_both_length0[nrow(border4level_both_length0),"two_ProLen"]=0
      
      int <- st_intersection(border,country1_union_buffer) %>%
        st_intersection(country2_union_buffer)
      
      if(length(st_length(int))>0) {
        
        border4level_both_length0[nrow(border4level_both_length0),"two_ProLen"] = st_length(int)/1000
        
      } else {
        
        border4level_both_length0[nrow(border4level_both_length0),"two_ProLen"] = 0
      }
      
    }

  } else { #at least one side do not have BPA，all border segments of this country-pair
    
    for (j in 1:nrow(borders)) {
      
      border <- borders[j,]
      
      border4id <- border[1,"border_4level_ID"]
      borderid <- border[1,"borderID"]
      border4level_both_length0[nrow(border4level_both_length0)+1,"borderID"]=borderid
      border4level_both_length0[nrow(border4level_both_length0),"border_4level_ID"]=border4id
      border4level_both_length0[nrow(border4level_both_length0),"ISO3_pair"]=pair_name
      border4level_both_length0[nrow(border4level_both_length0),"borderlength"]=st_length(border)/1000
      border4level_both_length0[nrow(border4level_both_length0),"two_ProLen"]=0
    }
    
  }
  
  cat(i,pair_name,"done\n")
}

border4level_Tpro_shp<-merge(border4level_pro_shp,border4level_both_length0[,c("border_4level_ID","two_ProLen")],by.x="border_4level_ID",by.y="border_4level_ID",all.x=T)
border4level_Tpro_shp$tPro<-border4level_Tpro_shp$two_ProLen/border4level_Tpro_shp$protectedLength

write_sf(border4level_Tpro_shp,"./Revision 1/result",layer="border4level_Tpro", driver="ESRI Shapefile",delete_layer=T)

save(border4level_length0,border4level_both_length0,border4level_pro_shp,border4level_Tpro_shp,file = "./Revision 1/result/result3_moll.RData")

load("./2 Revision 1/result/result3_moll.RData")

#border scale

for (i in 1:nrow(border_4level_single)) {
  
  border<-border_4level_single[i,]#one border
  #fid<-border_4level_all[i,"fid1"]
  border4id<-border_4level_single[i,"border_4level_ID"]
  borderid<-border_4level_single[i,"borderID"]
  pair_name<-border$SOCpair
  country_name<-pair_name %>%
    strsplit(split = "_") %>%
    as.data.frame()
  country_name<-country_name[,1]
  
  border4level_both_length0[nrow(border4level_both_length0)+1,"borderID"]=borderid
  border4level_both_length0[nrow(border4level_both_length0),"border_4level_ID"]=border4id
  border4level_both_length0[nrow(border4level_both_length0),"ISO3_pair"]=pair_name
  border4level_both_length0[nrow(border4level_both_length0),"borderlength"]=st_length(border)/1000
  
  bpa.iso31<-bpa_union_id %>%
    filter(ISO3 == country_name[1])
  bpa.iso32<-bpa_union_id %>%
    filter(ISO3 == country_name[2])
  
  if (nrow(bpa.iso31)>0 & nrow(bpa.iso32)>0) {
    
    country1_union_buffer <- bpa.iso31 %>%
      st_make_valid() %>% #avoid error: self-intersection
      st_union() %>% #returns a single geometry with resolved boundaries
      st_buffer(dist=1000) #1km buffer
    country2_union_buffer<- bpa.iso32 %>%
      st_make_valid() %>% #avoid error: self-intersection
      st_union() %>% #returns a single geometry with resolved boundaries
      st_buffer(dist=1000) #1km buffer
    
    int<-st_intersection(border,country1_union_buffer) %>%
      st_intersection(country2_union_buffer)

    if (nrow(int)>0) {
      
      border4level_both_length0[nrow(border4level_both_length0),"two_ProLen"]=st_length(int)/1000#border length overlapping with pa
      
    } else {
      
      border4level_both_length0[nrow(border4level_both_length0),"two_ProLen"]=0
      
    }
    
  } else {
    
    border4level_both_length0[nrow(border4level_both_length0),"two_ProLen"]=0
    
  }
  
  
}




