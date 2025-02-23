setwd("D:/chapter1")

library(sf)
library(dplyr)
library(ggsci)
library(ggplot2)
library(ggpubr)
library(tidyr)
library(units)
library(colorspace) 
library(scatterpie)
library(cowplot)

#======================= Result 1 ======================####

## 1. Figure 1 ####

##Fig1.A Global map ####

load("./2 Revision 1/bpa_union.RData")
load("./2 Revision 1/middle/bpa_union_id.RData") #bpa_union_id
load("./input/boundaries_cwn2024.RData")
load("./2 Revision 1/middle/BPAlist.RData")
glo <- read_sf("./input/country-boundaries/countries_new1_noATA.shp") %>% filter(GID_0!="ATA")
continent <- read.csv("./input/continent_country.csv",header = T)

fig1.a <- ggplot() +
  geom_sf(data=glo,color=NA,fill="#E1E1E1")+
  geom_sf(data=bpa_union_id,aes(geometry=geometry,fill=has_neighbor2),color=NA)+
  scale_fill_npg(breaks=c("t","f"),
                 labels=c("With adjacent BPAs",
                          "Without adjacent BPAs"))+
  geom_sf(data=boundaries,color="#808080",size=0.3)+
  coord_sf(xlim = c(-180,180),
           ylim = c(-60,90))+
  theme_bw()+
  theme(
    legend.position.inside = c(0.15,0.3),
    legend.title = element_blank(),
    legend.text = element_text(size = 12),
    panel.grid = element_blank(),
    axis.ticks = element_blank(),
    axis.text = element_blank())


#### Add pie plot: [area] of 0/1 having adjacent BPAs ####


bpa_union_id$BPAarea<-drop_units(st_area(bpa_union_id %>% st_transform(crs = "+proj=moll"))/1000000)

fig1.a.piedata <- bpa_union_id %>%
  st_drop_geometry() %>%
  dplyr::select(GID_0,PIDs,has_neighbor,BPAarea) %>%
  unique() %>%
  merge(continent[,c("ISO.alpha3.code","region")],by.x="GID_0",by.y="ISO.alpha3.code",all.x=T) %>%
  group_by(region,has_neighbor) %>%
  summarise(area=sum(BPAarea))

fig1.a.piedata <- spread(fig1.a.piedata,key="has_neighbor",value="area")
fig1.a.piedata$f[is.na(fig1.a.piedata$f)]<-0

# Coordinates of pie plots
fig1.a.piedata$x <- c(65,140,-25,-30,-150,146)
fig1.a.piedata$y <- c(-10,20,50,-35,35,-12)
fig1.a.piedata$label_x <- c(65,158,-25,-30,-150,163)
fig1.a.piedata$label_y <- c(-25,20,40,-50,25,-12)

fig1.a.piedata$sum_area <- fig1.a.piedata$f+fig1.a.piedata$t
fig1.a.piedata$r <- fig1.a.piedata$sum_area^(1/3)/10

fig1.a <- ggplot()+
  geom_sf(data=glo,color=NA,fill="#E1E1E1")+
  geom_sf(data=bpa_union_id,aes(geometry=geometry,fill=has_neighbor2),color=NA)+
  scale_fill_npg(breaks=c("t","f"),
                 labels=c("With adjacent BPAs",
                          "Without adjacent BPAs")
  )+
  geom_sf(data=boundaries,color="#808080",size=0.3)+
  geom_scatterpie(
    aes(x,y,r=r),
    data = fig1.a.piedata,
    cols = c("f","t"),
    linewidth = 0
  )+
  coord_sf(xlim = c(-180,180),
           ylim = c(-60,90))+
  geom_scatterpie_legend(
    fig1.a.piedata$r,
    n=3,
    x=-153,
    y=-12,
    breaks = c(min(fig1.a.piedata$r),median(fig1.a.piedata$r),max(fig1.a.piedata$r)),
    labeller = function(x){c("6,081","525,701","1,433,886")}
  )+
  annotate(
    "text",
    x=fig1.a.piedata$label_x,
    y=fig1.a.piedata$label_y,
    label= c("Africa","Asia","Europe","LAC","Northern America","Oceania")
  )+
  theme_bw()+
  theme(#legend.position = "bottom",
    legend.position = c(0.15,0.15),
    legend.title = element_blank(),
    legend.text = element_text(size = 12),
    panel.grid = element_blank(),
    axis.ticks = element_blank(),
    axis.text = element_blank(),
    axis.title = element_blank())

round(median(fig1.a.piedata$sum_area),digits = 0)
round(max(fig1.a.piedata$sum_area),digits = 0)
round(min(fig1.a.piedata$sum_area),digits = 0)
#6081
#525701
#1433886

ggsave(fig1.a, file='./Revision 1/result/test_fig1.tiff', width=9, height=4,dpi = 300)

## Fig1.b,c####
library(sqldf)

load("./Revision 1/middle/bpa_union_id.RData")#bpa_union_id

pl <- bpa_union_id %>%
  merge(continent,by.x = "ISO3",by.y = "ISO.alpha3.code",all.x = T)

pl$pl_area <- st_area(pl)

# area in each year:

pl2_1 <- pl %>%
  st_drop_geometry() %>%
  group_by(region,year_no0_2) %>%
  summarise(area=sum(pl_area)/1000000)
pl2_1$area<-as.numeric(pl2_1$area)

# number in each year:
# BPA number here refers to the earliest_PID / strictest

head(pl)
t<-table(pl$earliest_PID)%>%
  as.data.frame()
t<-pl %>%
  st_drop_geometry() %>%
  distinct(region,year_no0_2,earliest_PID) # PIDs:3627; earliest_PID:3232

pl2_2 <- pl %>%
  st_drop_geometry() %>%
  distinct(region,year_no0_2,PIDs) %>%
  group_by(region,year_no0_2) %>%
  summarise(count=n())

pl2<-sqldf("select s1.*, s2.count from pl2_1 as s1,pl2_2 as s2 
           where s1.year_no0_2=s2.year_no0_2 and s1.region=s2.region")

pl2$count_add[1]<-pl2$count[1]
pl2$area_add[1]<-pl2$area[1]

for (i in 2:nrow(pl2)) {
  if(pl2$region[i]==pl2$region[i-1]) {
    pl2$count_add[i]<-pl2$count[i]+pl2$count_add[i-1]
    pl2$area_add[i]<-pl2$area[i]+pl2$area_add[i-1]
  } else {
    pl2$count_add[i]<-pl2$count[i]
    pl2$area_add[i]<-pl2$area[i]
  }
}
pl2$logArea <- log10(pl2$area_add)


fig1.b <- pl2 %>%
  filter(region!="Oceania",year_no0_2>0) %>%
  ggplot(mapping = aes(x=as.numeric(year_no0_2),y=count_add,colour=region,shape=region))+ 
  geom_line(linewidth=1.5) + 
  scale_color_npg()+
  xlab('Year')+
  scale_shape_manual(values = c(0,5,1,2,8,3,15,16,17,18))+
  theme_bw()+
  ylab("Number")+
  theme(legend.title = element_blank(),
        panel.grid = element_blank(),
        axis.ticks.length.y = unit(-0.15,'cm'),
        axis.ticks.length.x = unit(-0.15,'cm'),
        axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15),
        legend.text = element_text(size = 12),
        legend.position = c(0.4,0.8))+
  scale_x_continuous(breaks=seq(1850,2021,10),
                     labels=function(x) ifelse(x %% 50 == 0, x, ""))

fig1.c <- pl2 %>%
  filter(region != "Oceania", year_no0_2 > 0) %>%
  ggplot(mapping = aes(x=as.numeric(year_no0_2), y=logArea, colour=region, shape=region))+ 
  geom_line(size=1.5) +
  scale_color_npg()+
  xlab('Year')+
  theme_bw()+
  theme(legend.position = "none",
        panel.grid = element_blank(),
        axis.ticks.length.y = unit(-0.15,'cm'),
        axis.ticks.length.x = unit(-0.15,'cm'),
        axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15))+
  ylab(bquote('Area('~km^2~', '~log[10]~')'))+
  scale_x_continuous(breaks=seq(1850,2021,10),labels=function(x) ifelse(x %% 50 == 0, x, ""))

fig1<-ggarrange(fig1.a,                                                 
                ggarrange(fig1.b, fig1.c, ncol = 2, labels = c("B", "C")), 
                nrow = 2, 
                labels = "A",
                widths=c(1,1),
                align="v"
) 

ggsave(fig1, file='./Revision 1/result/fig1_0923.tiff', width=9, height=9,dpi = 300)

## 2. SFig.1 BPA area ####

pl <- bpa_union_id %>%
  merge(continent,by.x = "ISO3",by.y = "ISO.alpha3.code",all.x = T)

pl$pl_area <- st_area(pl)

pl$year_no0_2<-as.numeric(pl$year_no0_2)
pl$region[which(pl$region=="Latin America and the Caribbean")]<-"LAC"

pl$period[which(pl$year_no0_2>=2000)]<-"After 2000"
pl[which(pl$year_no0_2<2000),"period"]<-"Before 2000"
pl$period<-factor(pl$period,levels = c("Before 2000","After 2000"))

pl_global<-pl
pl_global$region<-"Global"
pl_global<-rbind(pl,pl_global)
pl_global$region<-factor(pl_global$region,levels = c("Global","Europe","Northern America","Asia","Africa","LAC","Oceania"))
pl_global$group<-paste(pl_global$region,pl_global$period,sep = ",")
pl_global$logarea<-log10(pl_global$pl_area/1000000) %>% as.numeric()

head(pl_global) # continent + global

# compare each group pair
pl_global1<-pl_global %>% 
  filter(!is.na(region) & region!="Oceania")

## test for equal variances
nom<-bartlett.test(logarea~group,data =pl_global1)
nom #p<0.05

## Shapiro-Wilk test for normality
library(multcomp)
t<-table(pl_global1$group) %>% as.data.frame()

for (i in 1:12) {
  group<-t[i,1]
  zt<-shapiro.test(pl_global1$logarea[which(pl_global1$group==group)])
  
  if (zt$p.value>0.05) { # if p > 0.05, the data follows a normal distribution.
    cat(group,zt$p.value)
  }
}
###only i=9 follows a normal distribution
###should use a non-parametric test

##compare group pairs

library(FSA)
dunn<-dunnTest(data=pl_global1,logarea~group,method="bh")

## significance letters
library(rcompanion)
library(tidyr)

sig<-cldList(P.adj ~ Comparison,
             data=dunn$res,
             threshold = 0.05)

sig<-sig %>%
  separate(Group,c("region","period"),"[,]")

sig$period[which(sig$period=="After2")]<-"After 2000"
sig$period[which(sig$period=="Before2")]<-"Before 2000"

sig$region<-as.factor(sig$region)
sig$period<-as.factor(sig$period)

sig$region<-factor(sig$region,levels = c("Global","Europe","NorthernAmerica","Asia","Africa","LAC"))
sig$period<-factor(sig$period,levels = c("Before 2000","After 2000"))

sfig1<-pl_global %>% 
  filter(!is.na(region) & region!="Oceania") %>%
  ggplot()+
  geom_boxplot(aes(x=period,y=logarea,color=region))+#,group=factor(region)
  theme_bw()+
  scale_color_npg()+
  theme(legend.title=element_blank(),
        legend.text=element_text(size=12),
        panel.grid = element_blank(),
        axis.ticks.length.y = unit(-0.15,'cm'),
        axis.ticks.length.x = unit(-0.15,'cm'),
        axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15),
        axis.text.y = element_text(size=14),
        axis.text.x = element_text(size=14))+
  xlab("Year")+
  ylab("Area(log10)")+
  geom_text(data=sig,
            aes(x=period,label=Letter,y=rep(6, 12),group=region),
            position=position_dodge(0.75))

ggsave(sfig1, file='./Revision 1/result/sfig1.tif', width=8.5, height=7)

# ================================== Results 3 ======================================####

## 4. Fig 3 length of important borders####

load("./2 Revision 1/result/result3_moll.RData")

library(RColorBrewer)
library(viridis)

head(border4level_Tpro_shp)

#border4level_Tpro_shp$tPro[is.na(border4level_Tpro_shp$tPro)]<-0

###fig3.1 venn ####
library(eulerr)

fig3.data<-border4level_Tpro_shp%>%
  st_drop_geometry() %>%
  dplyr::select(layers,borderlength,protectedLength,two_ProLen)%>%
  group_by(layers)%>%
  summarise(length=sum(borderlength),single_length=sum(protectedLength-two_ProLen),both_length=sum(two_ProLen),not_length=sum(borderlength-protectedLength)) %>%
  filter(!is.na(layers)) %>%
  mutate(pro_length=100*length/sum(length))

fig3.data$length

#2168.381   1805.779  22383.218   9510.108   7218.202  10037.184  10555.571 200189.799

dat<-c(
  "Biodiversity Hotspots"=2168.381,"KBA"=7218.202, "Global 200"=22383.218,
  "Biodiversity Hotspots&KBA"=1805.779,
  "Global 200&Biodiversity Hotspots"=9510.108,
  "KBA&Global 200"=10555.571,
  "KBA&Biodiversity Hotspots&Global 200"=10037.184
)

fig3.venn<-plot(euler(dat),
                 fills = list(fill= brewer.pal(3,"Paired"),alpha=0.7),
                 #fills = scale_fill_npg(alpha=0.7),
                 #quantities = c("9223.9","25818.3","44099.2","6802.7","35105.7","17633.8","21434.4"),#
                 quantities = c("3.4%","11.3%","35.2%","2.8%","14.9%","16.6%","15.8%"),#
                 edges = list(col="white",lwd=2),
                 labels = list(col="black",cex=1.2),
                 bty="o"
)

### fig3.2 priority barplot ####

library(ggplot2)
library(ggsci)

fig3.data<-as.data.frame(fig3.data)

fig3.barplot.data <- border4level_Tpro_shp %>%
  st_drop_geometry() %>%
  dplyr::select(pri_level,borderlength,protectedLength,two_ProLen) %>%
  group_by(pri_level) %>%
  summarise(single_length=sum(protectedLength-two_ProLen),both_length=sum(two_ProLen),not_length=sum(borderlength-protectedLength))

fig3.barplot.data<-as.data.frame(fig3.barplot.data)

fig3.barplot.data1<-fig3.barplot.data[,c("pri_level","single_length")]
fig3.barplot.data1$pro_type<-"One-sided protection"
colnames(fig3.barplot.data1)<-c("pri_level","length","pro_type")

fig3.barplot.data2<-fig3.barplot.data[,c("pri_level","both_length")]
fig3.barplot.data2$pro_type<-"Two-sided protection"
colnames(fig3.barplot.data2)<-c("pri_level","length","pro_type")

fig3.barplot.data3<-fig3.barplot.data[,c("pri_level","not_length")]
fig3.barplot.data3$pro_type<-"No protection"
colnames(fig3.barplot.data3)<-c("pri_level","length","pro_type")

fig3.barplot.data.0<-rbind(fig3.barplot.data1,fig3.barplot.data2)%>%
  rbind(fig3.barplot.data3)

fig3.bar<-ggplot(data=fig3.barplot.data.0, mapping=aes(x = pri_level, y = length/100,fill=pro_type))+
  geom_bar(stat="identity",position=position_dodge(0.7),width=0.6)+
  scale_fill_manual(values = viridis(3, alpha = 1, begin = 0.1, end = 1, direction = 1, option = "viridis"))+#brewer.pal(12,"Paired")[c(10,11,12)]
  theme_bw()+
  scale_y_continuous(limits=c(0,1500),expand = c(0, 0))+
  theme(legend.position=c(0.7, 0.8),
        legend.title=element_blank(),
        legend.text=element_text(size=12),
        panel.grid = element_blank(),
        axis.ticks.length.y = unit(-0.15,'cm'),
        axis.ticks.x = element_blank())+
  xlab("Conservation priority levels")+
  ylab("Border length (100 km)")+
  geom_text(aes(label = paste(round(100*length/sum(length),1),"%",sep = "")), 
            position = position_dodge(0.7), vjust = -0.8)

fig3<-ggpubr::ggarrange(fig3.venn, fig3.bar,
                         nrow = 1,
                         ncol = 2,
                         labels = c("A","B"),
                         heights = 1,
                         widths = c(1,1)
) 

ggsave(fig3, file='./Revision 1/result/fig3.jpeg', width=12, height=6)


##5.  results number ####

sum(border4level_Tpro_shp$borderlength[which(border4level_Tpro_shp$pri_level!=0)])
sum(border4level_Tpro_shp$borderlength)

#borderlands covered by at least one schemes:
63678.44/sum(border4level_Tpro_shp$borderlength)#24.1

#borderlands covered by at least one schemes and have no protection:
sum(2210.505,16821.149,9732.248)/sum(border4level_Tpro_shp$borderlength[which(border4level_Tpro_shp$pri_level!=0)])

#highest conservation priorities:
(5748.481+2078.198)/sum(border4level_Tpro_shp$borderlength[which(border4level_Tpro_shp$pri_level==3)])
2078.198/sum(border4level_Tpro_shp$borderlength[which(border4level_Tpro_shp$pri_level==3)])

## 6. Fig4 map ####

load("./2 Revision 1/result/result3_moll.RData")

library(cowplot)
library(RColorBrewer)

# set color

c1<-c("#E3BFBF","#C77F7F","#AB3F3F","#8F0000")
c2<-c("#FFDFBF","#FFBF7F","#FF9F3F","#FF8000")#orange
c3<-c("#C0D0EB","#80A2D8","#4074C5","#0046B2")#blue

color.table<-data.frame(c1,c2,c3)
d<-expand.grid(x=seq(1,4,1),y=seq(1,3,1))
for (i in 1:nrow(d)) {
  x<-d$x[i]
  y<-d$y[i]
  d$fillc[i]<-color.table[x,y]
}


#grey global map
glo <- read_sf("D:/chapter1/input/country-boundaries/countries_new1_noATA.shp") %>% filter(GID_0!="ATA")

mapb <- ggplot()+
  geom_sf(data=glo,color=NA,fill="#EEEDEE")+
  theme_void()+
  theme(legend.position = "none")+
  coord_sf(xlim = c(-180,180),
           ylim = c(-60,90))
head(brdata100_two_one)

#border line
head(border4level_pro_shp)

border.plot <- border4level_pro_shp %>%
  dplyr::select(border_4level_ID,SOCpair,pPro,pri_level) %>%
  st_transform(crs = "WGS84")

#threshold value
p.sd<-sd(border.plot$pPro)
p.mean<-mean(border.plot$pPro)

p.break1<-p.mean-p.sd/2
p.break2<-p.mean+p.sd/2

b.r<-border.plot[i,"pri_level"]%>%st_drop_geometry()

for (i in 1:nrow(border.plot)) {
  b.r<-border.plot[i,"pri_level"]%>%st_drop_geometry()
  b.r<-b.r[1,1]
  b.p<-border.plot[i,"pPro"]%>% as.numeric()
  b.p<-b.p[1]
  if (b.p<p.break1) {
    if (b.r=="0") {
      border.plot$mix1[i]<-color.table[1,1]
    } else if (b.r=="1"){
      border.plot$mix1[i]<-color.table[2,1]
    } else if (b.r=="2") {
      border.plot$mix1[i]<-color.table[3,1]
    } else {
      border.plot$mix1[i]<-color.table[4,1]
    }
  } else if (b.p<p.break2) {
    if (b.r=="0") {
      border.plot$mix1[i]<-color.table[1,2]
    } else if (b.r=="1"){
      border.plot$mix1[i]<-color.table[2,2]
    } else if (b.r=="2") {
      border.plot$mix1[i]<-color.table[3,2]
    } else {
      border.plot$mix1[i]<-color.table[4,2]
    }
    
  } else {
    if (b.r=="0") {
      border.plot$mix1[i]<-color.table[1,3]
    } else if (b.r=="1"){
      border.plot$mix1[i]<-color.table[2,3]
    } else if (b.r=="2") {
      border.plot$mix1[i]<-color.table[3,3]
    } else {
      border.plot$mix1[i]<-color.table[4,3]
    }
  }
}

#legend

le1<-ggplot(d,aes(y,x,fill=fillc))+
  geom_tile()+
  scale_fill_identity()+
  theme_classic()+
  theme(legend.position = "none",
        axis.ticks = element_blank(),
        panel.border = element_blank(),
        axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15),
        axis.line = element_line(arrow = arrow(length = unit(0.3, 'cm'))),
        axis.text = element_text(size = 12),
        rect = element_rect(fill = "transparent"))+
  scale_x_continuous(breaks=c(1.5,2.5,3.5),labels = c(round(p.break1*100,1),round(p.break2*100,1),100))+ 
  scale_y_continuous(breaks=c(1,2,3,4),labels =c(0,1,2,3))+ 
  labs(x="Proportion of protection (%)",
       y="Conservation\nPriority Levels")
?labs
border0.plot<-border.plot[which(border.plot$pri_level==0),]
border1.plot<-border.plot[which(border.plot$pri_level==1),]
border2.plot<-border.plot[which(border.plot$pri_level==2),]
border3.plot<-border.plot[which(border.plot$pri_level==3),]
?geom_sf

map1<-ggplot()+
  geom_sf(data=border0.plot,
          color=border0.plot$mix1,
          linewidth=1.1)+
  geom_sf(data=border1.plot,
          color=border1.plot$mix1,
          linewidth=1.1)+
  geom_sf(data=border2.plot,
          color=border2.plot$mix1,
          linewidth=1.1)+
  geom_sf(data=border3.plot,
          color=border3.plot$mix1,
          linewidth=1.1)+
  coord_sf(xlim = c(-180,180),
           ylim = c(-60,90))+
  theme_void()+
  theme(legend.position = "none")+
  geom_rect(aes(xmin=-69,xmax=-66, #LAC
                ymin=-25,ymax=-22.25),
            fill=NA,color="black")+
  geom_rect(aes(xmin=8,xmax=10, #Africa
                ymin=4.5,ymax=6.5),
            fill=NA,color="black")+
  geom_rect(aes(xmin=104.4,xmax=108.3, #Asia
                ymin=12.2,ymax=16),
            fill=NA,color="black")+
  geom_rect(aes(xmin=-9.5,xmax=-5, #Europe
                ymin=37,ymax=40.5),
            fill=NA,color="black")+
  geom_rect(aes(xmin=-66,xmax=-59, #North america
                ymin=0.5,ymax=7.5),
            fill=NA,color="black")

fig4.a<-cowplot::ggdraw()+
  draw_plot(mapb,0,0.05,width = 1,height = 1)+
  draw_plot(map1,0,0.05,width = 1,height = 1)+
  draw_plot(le1,x=0.4,y=0,width=0.17,height=0.29)

ggsave(fig4.a, file='./3 revision2/result/fig4.1-grey-2.png', width=7.5,height=4,units = "in", dpi = 300)

### boxes ####

##### a America####

mapbasic.na<-ggplot()+
  geom_sf(data=glo,color=NA,fill="#EEEDEE")+
  theme_void()+
  theme(legend.position = "none")+
  coord_sf(xlim = c(-66,-59),
           ylim = c(0.5,7.5))

#PAs
filenamec.VEN<-read_sf("D:/chapter1/input/splitByISO3_all/VEN.shp")
filenamec.BRA<-read_sf("D:/chapter1/input/splitByISO3_all/BRA.shp")
filenamec.GUY<-read_sf("D:/chapter1/input/splitByISO3_all/GUY.shp")

fig4.na.bpa.data<-filenamec.VEN %>%
  rbind(filenamec.BRA) %>%
  rbind(filenamec.GUY)

fig4.na.bpa<-ggplot()+
  geom_sf(data=fig4.na.bpa.data,
          color=NA,
          fill="#94C34A")+
  coord_sf(xlim = c(-66,-59),
           ylim = c(0.5,7.5))+
  theme_void()+
  theme(legend.position = "none")

#borders
fig4.na.border<-ggplot()+
  geom_sf(data=border0.plot,
          color=border0.plot$mix1,
          linewidth=3)+
  geom_sf(data=border1.plot,
          color=border1.plot$mix1,
          linewidth=3)+
  geom_sf(data=border2.plot,
          color=border2.plot$mix1,
          linewidth=3)+
  geom_sf(data=border3.plot,
          color=border3.plot$mix1,
          linewidth=3)+
  coord_sf(xlim = c(-66,-59),
           ylim = c(0.5,7.5))+
  theme_void()+
  theme(legend.position = "none",
        panel.border = element_rect(fill = NA,
                                    size = 1))

fig4.na<-ggdraw()+
  draw_plot(mapbasic.na,0,0,width=1,height=1)+
  draw_plot(fig4.na.bpa,x=0,y=0,width=1,height=1)+
  draw_plot(fig4.na.border,0,0,width=1,height=1)

ggsave(fig4.na, file='./Revision 1/result/fig4-america1.png', width=5,height=5,dpi = 300)

##### b Europe####

#grey global map
mapbasic.b<-ggplot()+
  geom_sf(data=glo,color=NA,fill="#EEEDEE")+
  theme_void()+
  theme(legend.position = "none")+
  coord_sf(xlim = c(-9.5,-5),
           ylim = c(37,40.5))

#PAs
filenamec.ESP<-read_sf("./input/splitByISO3_all/ESP.shp")
filenamec.PRT<-read_sf("./input/splitByISO3_all/PRT.shp")

fig4.b.bpa.data<-filenamec.ESP %>%
  rbind(filenamec.PRT)

fig4.b.bpa<-ggplot()+
  geom_sf(data=fig4.b.bpa.data,
          color=NA,
          fill="#94C34A")+
  coord_sf(xlim = c(-9.5,-5),
           ylim = c(37,40.5))+
  theme_void()+
  theme(legend.position = "none")

#borders
fig4.b.border<-ggplot()+
  geom_sf(data=border0.plot,
          color=border0.plot$mix1,
          linewidth=3)+
  geom_sf(data=border1.plot,
          color=border1.plot$mix1,
          linewidth=3)+
  geom_sf(data=border2.plot,
          color=border2.plot$mix1,
          linewidth=3)+
  geom_sf(data=border3.plot,
          color=border3.plot$mix1,
          linewidth=3)+
  coord_sf(xlim = c(-9.5,-5),
           ylim = c(37,40.5))+
  theme_void()+
  theme(legend.position = "none",
        panel.border = element_rect(fill = NA,
                                    size = 1))

fig4.b<-ggdraw()+
  draw_plot(mapbasic.b,0,0,width=1,height=1)+
  draw_plot(fig4.b.bpa,x=0,y=0,width=1,height=1)+
  draw_plot(fig4.b.border,0,0,width=1,height=1)

ggsave(fig4.b, file='./Revision 1/result/fig4-europe.png', width=5,height=5,dpi = 300)

##### c Asia ####

#灰色底图
mapbasic.z<-ggplot()+
  geom_sf(data=glo,color=NA,fill="#EEEDEE")+
  theme_void()+
  theme(legend.position = "none")+
  coord_sf(xlim = c(104.4,108.3),
           ylim = c(12.2,16))
mapbasic.z

#保护区
filenamec.KHM<-read_sf("./input/splitByISO3_all/KHM.shp")
filenamec.LAO<-read_sf("./input/splitByISO3_all/LAO.shp")
filenamec.THA<-read_sf("./input/splitByISO3_all/THA.shp")
filenamec.VNM<-read_sf("./input/splitByISO3_all/VNM.shp")

fig4.z.bpa.data<-filenamec.KHM %>%
  rbind(filenamec.LAO) %>%
  rbind(filenamec.THA) %>%
  rbind(filenamec.VNM)

fig4.z.bpa<-ggplot()+
  geom_sf(data=fig4.z.bpa.data,
          color=NA,
          fill="#94C34A")+ #339900
  coord_sf(xlim = c(104.4,108.3),#97.5,106.5
           ylim = c(12.2,16))+#11,19
  theme_void()+
  theme(legend.position = "none")

fig4.z.bpa

#边界线
fig4.z.border<-ggplot()+
  geom_sf(data=border0.plot,
          color=border0.plot$mix1,
          linewidth=3)+
  geom_sf(data=border1.plot,
          color=border1.plot$mix1,
          linewidth=3)+
  geom_sf(data=border2.plot,
          color=border2.plot$mix1,
          linewidth=3)+
  geom_sf(data=border3.plot,
          color=border3.plot$mix1,
          linewidth=3)+
  coord_sf(xlim = c(104.4,108.3),
           ylim = c(12.2,16))+
  theme_void()+
  theme(legend.position = "none",
        panel.border = element_rect(fill = NA,size = 1))

#fig4.z

fig4.z <- ggdraw()+
  draw_plot(mapbasic.z,0,0,width=1,height=1)+
  draw_plot(fig4.z.bpa,x=0,y=0,width=1,height=1)+
  draw_plot(fig4.z.border,0,0,width=1,height=1)

ggsave(fig4.z, file='./Revision 1/result/fig4-asia.png', width=5,height=5,dpi = 300)


#####d LAC####

#PAs
filenamec.CHL<-read_sf("./input/splitByISO3_all/CHL.shp")
filenamec.BOL<-read_sf("./input/splitByISO3_all/BOL.shp")
filenamec.ARG<-read_sf("./input/splitByISO3_all/ARG.shp")


fig4.sa.bpa.data<-filenamec.CHL %>%
  rbind(filenamec.BOL) %>%
  rbind(filenamec.ARG)

fig4.sa.bpa<-ggplot()+
  geom_sf(data=fig4.sa.bpa.data,
          color=NA,
          fill="#94C34A")+
  coord_sf(xlim = c(-69,-66),
           ylim = c(-25,-22.25),
           lims_method="box")+
  theme_void()+
  theme(legend.position = "none")

#grey global map
mapbasic.sa<-ggplot()+
  geom_sf(data=glo,color=NA,fill="#EEEDEE")+
  coord_sf(xlim = c(-69,-66),
           ylim = c(-25,-22.25))+
  theme_void()+
  theme(legend.position = "none")

#borders
fig4.sa.border<-ggplot()+
  geom_sf(data=border0.plot,
          color=border0.plot$mix1,
          linewidth=3)+
  geom_sf(data=border1.plot,
          color=border1.plot$mix1,
          linewidth=3)+
  geom_sf(data=border2.plot,
          color=border2.plot$mix1,
          linewidth=3)+
  geom_sf(data=border3.plot,
          color=border3.plot$mix1,
          linewidth=3)+
  coord_sf(xlim = c(-69,-66),#-65,-59.3
           ylim = c(-25,-22.25))+#3.3,8.7
  theme_void()+
  theme(legend.position = "none",
        panel.border = element_rect(fill = NA,
                                    size = 1))

fig4.sa<-ggdraw()+
  draw_plot(mapbasic.sa,0,0,width=1,height=1)+
  draw_plot(fig4.sa.bpa,x=0,y=0,width=1,height=1)+
  draw_plot(fig4.sa.border,0,0,width=1,height=1)

ggsave(fig4.sa, file='./Revision 1/result/fig4-lac.png', width=5,height=5,dpi = 300)

#####e Africa####

#global map
mapbasic.f<-ggplot()+
  geom_sf(data=glo,color=NA,fill="#EEEDEE")+
  theme_void()+
  theme(legend.position = "none")+
  coord_sf(xlim = c(8,10),
           ylim = c(4.5,6.5))
mapbasic.f

#PAs
filenamec.CMR<-read_sf("./input/splitByISO3_all/CMR.shp")
filenamec.NGA<-read_sf("./input/splitByISO3_all/NGA.shp")

fig4.f.bpa.data<-filenamec.CMR %>%
  rbind(filenamec.NGA)

fig4.f.bpa<-ggplot()+
  geom_sf(data=fig4.f.bpa.data,
          color=NA,
          fill="#94C34A")+ 
  coord_sf(xlim = c(8,10),
           ylim = c(4.5,6.5))+
  theme_void()+
  theme(legend.position = "none")

fig4.f.border <- ggplot()+
  geom_sf(data=border0.plot,
          color=border0.plot$mix1,
          linewidth=3)+
  geom_sf(data=border1.plot,
          color=border1.plot$mix1,
          linewidth=3)+
  geom_sf(data=border2.plot,
          color=border2.plot$mix1,
          linewidth=3)+
  geom_sf(data=border3.plot,
          color=border3.plot$mix1,
          linewidth=3)+
  coord_sf(xlim = c(8,10),
           ylim = c(4.5,6.5))+
  theme_void()+
  theme(legend.position = "none",
        panel.border = element_rect(fill = NA,size = 1))

fig4.f<-ggdraw()+
  draw_plot(mapbasic.f,0,0,width=1,height=1)+
  draw_plot(fig4.f.bpa,x=0,y=0,width=1,height=1)+
  draw_plot(fig4.f.border,0,0,width=1,height=1)

ggsave(fig4.f, file='./Revision 1/result/fig4-africa.png', width=5,height=5,dpi = 300)

## 7. Sfig 6 two-sided ####

glo <- read_sf("D:/chapter1/input/country-boundaries/countries_new1_noATA.shp") %>% filter(GID_0!="ATA")

mapb <- ggplot()+
  geom_sf(data=glo,color=NA,fill="#EEEDEE")+
  theme_void()+
  theme(legend.position = "none")+
  coord_sf(xlim = c(-180,180),
           ylim = c(-60,90))

border.plot.t<-border4level_Tpro_shp %>%
  dplyr::select(border_4level_ID,SOCpair,tPro,pri_level) %>%
  st_transform(crs = "WGS84")

border.plot.t$tPro[is.infinite(border.plot.t$tPro)]<-NA

border.plot.t.nona<-border.plot.t[which(!is.na(border.plot.t$tPro)),]

summary(border.plot.t.nona$tPro)
tp.mean <- mean(border.plot.t.nona$tPro)
tp.3 <- quantile(border.plot.t.nona$tPro,probs = 0.75)

tb.r<-border.plot.t[i,"pri_level"]%>%st_drop_geometry()


for (i in 1:nrow(border.plot.t)) {
  tb.r<-border.plot.t[i,"pri_level"]%>%st_drop_geometry()
  tb.r<-tb.r[1,1]
  tb.p<-border.plot.t[i,"tPro"]%>% as.numeric()
  tb.p<-tb.p[1]
  if (is.na(tb.p)) {
    border.plot.t$mix1[i]<-"#A9A9A9"
  } else if (tb.p<tp.mean) {
    if (tb.r=="0") {
      border.plot.t$mix1[i]<-color.table[1,1]
    } else if (tb.r=="1"){
      border.plot.t$mix1[i]<-color.table[2,1]
    } else if (tb.r=="2") {
      border.plot.t$mix1[i]<-color.table[3,1]
    } else {
      border.plot.t$mix1[i]<-color.table[4,1]
    }
  } else if (tb.p<tp.3) {
    if (tb.r=="0") {
      border.plot.t$mix1[i]<-color.table[1,2]
    } else if (tb.r=="1"){
      border.plot.t$mix1[i]<-color.table[2,2]
    } else if (tb.r=="2") {
      border.plot.t$mix1[i]<-color.table[3,2]
    } else {
      border.plot.t$mix1[i]<-color.table[4,2]
    }
    
  } else {
    if (tb.r=="0") {
      border.plot.t$mix1[i]<-color.table[1,3]
    } else if (tb.r=="1"){
      border.plot.t$mix1[i]<-color.table[2,3]
    } else if (tb.r=="2") {
      border.plot.t$mix1[i]<-color.table[3,3]
    } else {
      border.plot.t$mix1[i]<-color.table[4,3]
    }
  }
}

border.plot.t0<-border.plot.t[which(border.plot.t$pri_level==0),]
border.plot.t1<-border.plot.t[which(border.plot.t$pri_level==1),]
border.plot.t2<-border.plot.t[which(border.plot.t$pri_level==2),]
border.plot.t3<-border.plot.t[which(border.plot.t$pri_level==3),]

sfig6.map<-ggplot()+
  geom_sf(data=border.plot.t0,
          color=border.plot.t0$mix1,
          linewidth=1.1)+
  geom_sf(data=border.plot.t1,
          color=border.plot.t1$mix1,
          linewidth=1.1)+
  geom_sf(data=border.plot.t2,
          color=border.plot.t2$mix1,
          linewidth=1.1)+
  geom_sf(data=border.plot.t3,
          color=border.plot.t3$mix1,
          linewidth=1.1)+
  coord_sf(xlim = c(-180,180),
           ylim = c(-60,90))+
  theme_void()+
  theme(legend.position = "none")

#legend

le2<-ggplot(d,aes(y,x,fill=fillc))+
  geom_tile()+
  scale_fill_identity()+
  theme_classic()+
  theme(legend.position = "none",
        axis.ticks = element_blank(),
        panel.border = element_blank(),
        axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15),
        axis.line = element_line(arrow = arrow(length = unit(0.3, 'cm'))),
        rect = element_rect(fill = "transparent"))+
  scale_x_continuous(breaks=c(1.5,2.5,3.5),labels = c(round(tp.mean*100,1),round(tp.3*100,1),100))+ 
  scale_y_continuous(breaks=c(1,2,3,4),labels =c(0,1,2,3))+ 
  labs(x="Proportion of\ntwo-sided protection (%)",
       y="Conservation\nPriority Levels")

sfig6<-ggdraw()+
  draw_plot(mapb,0,0,1,1)+
  draw_plot(sfig6.map,0,0,1,1)+
  draw_plot(le2,x=0.08,y=0.2,width=0.15,height=0.28)

ggsave(sfig6, file='./Revision 1/result/sfig6.tiff', width=15, height=7)

## 8. Supplementary table 4 ####

stable4.data0<-border4level_Tpro_shp %>%
  st_drop_geometry()  %>%
  filter(pri_level%in%c("3","2","1")) %>%
  dplyr::select(border_4level_ID,SOCpair,borderID,protectedLength,two_ProLen,borderlength,pri_level) 
stable4.data0$no_length<-stable4.data0$borderlength-stable4.data0$protectedLength
stable4.data0$one_length<-stable4.data0$protectedLength-stable4.data0$two_ProLen

stable4<-stable4.data0 %>%
  group_by(SOCpair,pri_level) %>%
  summarise(one_sided=sum(one_length),two_sided=sum(two_ProLen),no_pro=sum(no_length))%>%
  as.data.frame()

write.csv(stable4,file = "./Revision 1/result/stable4.csv")

## 9. Sfig7 country-pair border length ####

# conservation extent of borders with highest conservation priorities

sfig7.data0 <- border4level_Tpro_shp %>%
  st_drop_geometry()  %>%
  filter(pri_level==3) %>%
  dplyr::select(border_4level_ID,SOCpair,borderID,protectedLength,two_ProLen,borderlength) 
sfig7.data0$no_length<-sfig7.data0$borderlength-sfig7.data0$protectedLength
sfig7.data0$one_length<-sfig7.data0$protectedLength-sfig7.data0$two_ProLen

sfig7.plotdata0<-sfig7.data0 %>%
  group_by(SOCpair) %>%
  summarise(one_sided=sum(one_length),two_sided=sum(two_ProLen),no_pro=sum(no_length)) %>%
  as.data.frame()

sfig7.plotdata.1<-sfig7.plotdata0[,c("SOCpair","one_sided")]
colnames(sfig7.plotdata.1)<-c("SOCpair","length")
sfig7.plotdata.1$Pro_type<-"One_sided protection"

sfig7.plotdata.2<-sfig7.plotdata0[,c("SOCpair","two_sided")]
colnames(sfig7.plotdata.2)<-c("SOCpair","length")
sfig7.plotdata.2$Pro_type<-"Two_sided protection"

sfig7.plotdata.3<-sfig7.plotdata0[,c("SOCpair","no_pro")]
colnames(sfig7.plotdata.3)<-c("SOCpair","length")
sfig7.plotdata.3$Pro_type<-"No protection"

sfig7.plotdata<-rbind(sfig7.plotdata.1,sfig7.plotdata.2)%>%
  rbind(sfig7.plotdata.3)

#continent：

continent <- read.csv("./input/continent_country.csv",header = T)
names(continent)<-c("continent","sub.regions","region","intermediary.regions","Country.or.Area","M49.code","ISO.alpha3.code","Other.groupings")

soc_pair <- strsplit(sfig7.plotdata$SOCpair,"_")
sfig7.plotdata$country1 <- sapply(soc_pair,function(x){x[1]})
sfig7.plotdata$country2 <- sapply(soc_pair,"[",2)

sfig7.plotdata2<-merge(sfig7.plotdata,continent[,c("region","ISO.alpha3.code")],by.x = "country1",by.y = "ISO.alpha3.code",all.x = T) %>%
  merge(continent[,c("region","ISO.alpha3.code")],by.x = "country2",by.y = "ISO.alpha3.code",all.x = T)%>%
  filter(!grepl("Z0",SOCpair),!grepl("XKO",SOCpair),!grepl("XAD",SOCpair))

# check: there are 3 borders cross continents
sfig7.plotdata2[which(sfig7.plotdata2$region.x!=sfig7.plotdata2$region.y),]


for (i in 1:nrow(sfig7.plotdata2)) {
  c1<-sfig7.plotdata2[i,"region.x"]
  c2<-sfig7.plotdata2[i,"region.y"]
  if (is.na(c1) & is.na(c2)){
    sfig7.plotdata2$region[i]="other"
  }else if (is.na(c1)==F & is.na(c2)){
    sfig7.plotdata2$region[i]=c1
  }else if (is.na(c1) & is.na(c2)==F){
    sfig7.plotdata2$region[i]=c2
  }else if (c1==c2) {
    sfig7.plotdata2$region[i]=c1
  } else {
    sfig7.plotdata2$region[i]=c2 #if cross continents, write one continent.
  }
}


sfig7.plotdata2$SOCpair<-gsub("_","-",sfig7.plotdata2$SOCpair)

sfig7.plotdata2$region[which(sfig7.plotdata2$region=="Latin America and the Caribbean")]<-"LAC"
sfig7.plotdata2$region[which(sfig7.plotdata2$region=="Northern America")]<-"NA"
sfig7.plotdata2$region[which(sfig7.plotdata2$region=="Europe")]<-"EU"
sfig7.plotdata2$region[which(sfig7.plotdata2$region=="Africa")]<-"AF"
sfig7.plotdata2$region[which(sfig7.plotdata2$region=="Oceania")]<-"OC"
sfig7.plotdata2$region[which(sfig7.plotdata2$region=="Asia")]<-"AS"

sfig7.plotdata2[which(is.na(sfig7.plotdata2$region.x)|is.na(sfig7.plotdata2$region.y)),]
sfig7.plotdata2[which(sfig7.plotdata2$region=="Oceania"),]

#order
table(sfig7.plotdata2$region)

sfig7.plotdata2[which(sfig7.plotdata2$region=="NA"),]

sfig7.plotdata2$region<-factor(sfig7.plotdata2$region,levels = c(
  "AS","AF","LAC","EU","NA","OC"))

order.data<-sfig7.plotdata2 %>%
  dplyr::select(SOCpair,length) %>%
  group_by(SOCpair) %>%
  summarise(total_length=sum(length))

sfig7.plotdata.order<-sfig7.plotdata2 %>%
  merge(order.data,all.x = T,by.x = "SOCpair",by.y = "SOCpair") %>%
  filter(total_length>20|total_length==20)%>%
  arrange(region,-total_length) %>%
  dplyr::select(SOCpair,length,region,total_length,Pro_type)

#add a blank row after each continent

datagroup<-sfig7.plotdata.order$region %>% unique()
datagroup
head(sfig5.plotdata.order)

sfig7.alldata<-tibble('region' = datagroup,
                      'SOCpair' = paste0('empty_', seq_along(datagroup)),
                      'length' = 0,
                      'Pro_type'="No protection",
                      'total_length'=0) %>% 
  bind_rows(sfig7.plotdata.order) %>%
  arrange(region,-total_length) %>%
  mutate(id = row_number())

#add a blank row at the first row

sfig7.alldata <- tibble('region' = "AS",
                      'SOCpair' = "empty_0",
                      'length' = 0,
                      'Pro_type'="No protection",
                      'total_length'=0) %>% 
  bind_rows(sfig7.alldata)%>%
  mutate(id = row_number())
head(sfig7.alldata)

#countries

y.data <- sfig7.alldata %>%
  dplyr::select(SOCpair,region,total_length) %>%
  unique()
y.data <- y.data%>%
  mutate(id = row_number())%>%
  mutate(angle = 90 - 360 * (id - 0.5) / n()) %>%
  mutate(hjust = ifelse(angle < -90, 1, 0)) %>%
  mutate(angle = ifelse(angle < -90, angle+180, angle))

head(y.data)

#region
library(stringr)

firstxid <- which(str_detect(y.data$SOCpair, pattern = "empty_"))

segment_data <- data.frame('from' = c(2,firstxid[c(2,3,4)] + 1),#length(firstxid)==5,就写2,3,4
                           'to' = firstxid[c(-1)] - 1,
                           'label' = datagroup) %>% 
  mutate(labelx = as.integer((from + to)/2))


# 2 layers:
## 1.barplot

color1 <- viridis(3, alpha = 1, begin = 0.1, end = 1, direction = 1, option = "viridis")

### define coordinate

coordy <- tibble('coordylocation' = seq(from = min(sfig7.alldata$total_length),
                                        to = max(sfig7.alldata$total_length), 500),
                 'coordytext' = as.character(round(coordylocation, 2)),
                 'x' = 0.5)

max(sfig7.alldata$total_length)

sfig7.a<-ggplot()+
  geom_bar(data=sfig7.alldata,
           aes(x=reorder(SOCpair,id),y=length,fill=Pro_type),stat = 'identity')+
  scale_fill_manual(values=color1)+
  scale_y_continuous(# Scale y axis so bars don't start in the center
    limits = c(-350, 1500),
    expand = c(0, 0),
    breaks = c(0,500,1000,1200)
  )+
  geom_segment(data = segment_data, aes(x = from, xend = to), y = -10, yend=-10) +
  geom_text(data = segment_data, aes(x = labelx, label = label), y = -100, size=4) + #region
  geom_text(data = coordy, aes(x = x, y = coordylocation, label = coordytext),
            color="darkgrey", size=4 , angle=0, fontface="bold")+#刻度标记
  coord_polar()+
  ylab("")+xlab("")+
  theme_bw()+
  theme(
    axis.text.y = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.y = element_blank(),
    #panel.grid = element_blank(),
    panel.grid.major.x = element_blank(),
    rect = element_rect(fill = "transparent"),
    panel.background = element_blank(),
    panel.border = element_blank(),
    legend.position = "none"

  )


##2. mark countries

max(sfig7.alldata$total_length)#4225.022

#placeholder：

sfig7.all0 <- sfig7.alldata[,c("region", "SOCpair","total_length","id")] %>% unique()

sfig7.all0[which(sfig7.all0$total_length>0),"total_length"] <- max(sfig7.alldata$total_length)

sfig7.b <- ggplot()+
  geom_bar(data=sfig7.all0,
           aes(x=reorder(SOCpair,id),y=total_length),stat = 'identity',fill="grey")+
  geom_text( data = y.data %>% filter(!str_detect(SOCpair, pattern = "empty_")),
             aes(x = id, label = SOCpair, y = 120+max(sfig7.alldata$total_length), angle = angle, hjust = hjust),
             color="black", size=4)+
  coord_polar()+
  scale_y_continuous( # Scale y axis so bars don't start in the center
    limits = c(-350, 1500),
    expand = c(0, 0),
    breaks = c(0,500,1000,1200,1500)
  )+
  ylab("")+
  xlab("")+
  theme_bw()+
  theme(
    axis.text.y = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.y = element_blank(),
    panel.grid = element_blank(),
    panel.background = element_blank(),
    panel.border = element_blank(),
    rect = element_rect(fill = "transparent")
  )

sfig7 <- ggdraw()+
  draw_plot(sfig7.b,0,0,width = 1,height = 1)+
  draw_plot(sfig7.a,0,0,width = 1,height = 1)

ggsave(sfig7, file='./Revision 1/result/sfig7.tiff', width=10, height=10)

