  
  #READ ME
  # 1. script requires OpenArch certificates to run
  # 2. script requires HDNG_v3 to run: https://datasets.iisg.amsterdam/file.xhtml?fileId=3988&version=2.0
  # 3. script requires the 1918 NLGIS shape files to run: https://doi.org/10.17026%2Fdans-xb9-t677
  # 4. working directories (setwd) need to be set before the script can be run
  
  
  #packages
  #visualization
  library(sp)
  library(spdep)
  library(maptools)
  library(rgdal)
  library(RColorBrewer)
  library(classInt)
  library(ggplot2)
  #data manipulation
  library(dplyr)
  library(data.table)
  #time manipulation
  library(lubridate)
  
  #Set the working directory
  rm(list = ls())
  
  #Read the data files
  HDNG <- fread("C:/Surfdrive/CLARIAH/HDNG/HDNG v3/HDNG+prov+missing.txt")
  OpenArch <- fread("C:/Surfdrive/Shared/openarch/flu paper/openarch_deaths_nov2020.csv.gz")
  
  #shape files
  setwd("C:/Surfdrive/Data/NLGIS/OpslagShapefiles1812_1997_Georef/1918Shapefiles")
  shape.shp <- readOGR(".", "nl_1918") #naam shapefile
  
  #output folder
  setwd("C:/Surfdrive/Maps")
  
  
  ################
  ####  HDNG  ####
  ################
  
  #filter death statistics
  Overleden <- HDNG[which(HDNG$description=="Overledenen" & !is.na(HDNG$ACODE)),]
  #filter 1910-1925
  Overleden <- Overleden[which(Overleden$year>=1910 & Overleden$year<=1925),]
  Overleden$value <- as.numeric(Overleden$value)
  
  #
  Overleden$ACODE[Overleden$NAAM=="VALKENBURG HOUTHEM L"] <- 11077 #Valkenburg, verkeerd gecodeerd (11111 Houthem ipv 11077 Valkenburg, Li)
  #filter double entries
  Overleden <- Overleden %>% arrange(ACODE, year, -value) %>% group_by(ACODE, year) %>% filter(row_number()==1) %>% ungroup()
  
  #n differences
  length(which(!is.na(shape.shp$ACODE))) #1,118
  length(which(!duplicated(Overleden$ACODE))) #1,225
  length(which(Overleden$ACODE %in% shape.shp$ACODE & !duplicated(Overleden$ACODE))) #1,117 + Valkenburg verkeerd gecodeerd (11111 Houthem ipv 11077 Valkenburg, Li)
  length(which(!(Overleden$ACODE %in% shape.shp$ACODE) & !duplicated(Overleden$ACODE))) #108
  length(which(!(Overleden$ACODE %in% shape.shp$ACODE) & Overleden$value==0 & !duplicated(Overleden$ACODE))) #104
  Overleden[which(!(Overleden$ACODE %in% shape.shp$ACODE) & !duplicated(Overleden$ACODE) & Overleden$value!=0),] #4
  
  #
  Overleden$ACODE[Overleden$ACODE==11011] <- 10517 #Oudshoorn -> Alphen aan den Rijn (1918)
  Overleden$ACODE[Overleden$ACODE==11065] <- 11053 #Ambt-Almelo -> Almelo (1914)
  Overleden$ACODE[Overleden$ACODE==11242] <- 10517 #Aarlanderveen -> Alphen aan den Rijn (1918)
  Overleden$ACODE[Overleden$ACODE==11276] <- 10010 #Genderen: Wrong ACODE
  #drop unused codes
  Overleden <- Overleden[which(Overleden$ACODE %in% shape.shp$ACODE),]
  #combine with larger municipality 
  Overleden <- Overleden %>% group_by(ACODE, year) %>% summarise(value=sum(value,  na.rm=T)) %>% ungroup()
  
  as.data.frame(table(as.data.frame(table(Overleden$ACODE))[,"Freq"]))
  
  
  ##################
  #### OpenArch ####
  ##################
  
  #compute death year
  OpenArch$death_year <- as.numeric(substr(OpenArch$death_date,1,4))
  OpenArch$death_year <- ifelse(is.na(OpenArch$death_year), OpenArch$event_year, OpenArch$death_year)
  OpenArch$death_year <- ifelse(is.na(OpenArch$death_year), OpenArch$source_date_year, OpenArch$death_year)
  
  #filter 1910-1925 with known amco
  OpenArch2 <- OpenArch[which(OpenArch$death_year>=1910 & OpenArch$death_year<=1925 & !is.na(OpenArch$amco)),]
  OpenArch2 <- OpenArch2 %>% group_by(amco, death_year) %>% summarise(n_death_OpenArch=n()) %>% ungroup()
  OpenArch2 <- OpenArch2[which(OpenArch2$amco!=0),]
  
  #find miscodings
  x <- OpenArch2[which(!duplicated(OpenArch2$amco)),]
  x <- merge(Overleden, x, by.x="ACODE", by.y="amco", all=T) #dropt nu valkenburg
  x[which(is.na(x$value) & !is.na(x$n_death_OpenArch)),] %>% arrange(ACODE, death_year)
  rm(x)
  
  #fix miscodings for municipalities with >1 miscoding
  OpenArch2$amco[OpenArch2$amco==10110] <- 11415 #Oploo -> Oploo, Sint Anthonis en Ledeacker
  OpenArch2$amco[OpenArch2$amco==10228] <- 10135 #Aalst -> Poederoijen
  OpenArch2$amco[OpenArch2$amco==10414] <- 10536 #Schardam -> Beets
  OpenArch2$amco[OpenArch2$amco==10512] <- 10732 #Nunspeet -> Ermelo
  OpenArch2$amco[OpenArch2$amco==10653] <- 10743 #Lunteren -> Ede
  OpenArch2$amco[OpenArch2$amco==10846] <- 10912 #Twello -> Voorst
  OpenArch2$amco[OpenArch2$amco==10908] <- 10345 #Delfshaven -> Rotterdam
  OpenArch2$amco[OpenArch2$amco==10995] <- 11296 #Alphen -> Appeltern (KLOPT DAT WEL????)
  OpenArch2$amco[OpenArch2$amco==11015] <- 10068 #Afferden -> Druten
  OpenArch2$amco[OpenArch2$amco==11065] <- 11053 #Ambt Almelo -> Almelo
  OpenArch2$amco[OpenArch2$amco==11096] <- 10650 #Ringsumageest -> Dantumadeel
  OpenArch2$amco[OpenArch2$amco==11179] <- 10743 #Bennekom -> Ede
  OpenArch2$amco[OpenArch2$amco==11242] <- 10517 #Aarlanderveen -> Alphen aan den Rijn
  OpenArch2$amco[OpenArch2$amco==11385] <- 10471 #Winssen -> Ewijk (1818)
  #drop 1
  OpenArch2 <- OpenArch2[which(OpenArch2$amco %in% Overleden$ACODE),]
  #combine with larger municipality 
  OpenArch2 <- OpenArch2 %>% group_by(amco, death_year) %>% summarise(n_death_OpenArch=sum(n_death_OpenArch, na.rm=T)) %>% ungroup()
  
  as.data.frame(table(as.data.frame(table(OpenArch2$amco))[,"Freq"]))
  
  
  
  
  ##############################
  #### Filter OpenArch data ####
  ##############################
  
  #filter baseline years
  OpenArch3 <- OpenArch2[which(OpenArch2$death_year<1918 | OpenArch2$death_year>1919),]
  #select if 1918 and at least one baseline year is available
  OpenArch1918 <- OpenArch2[which(OpenArch2$death_year==1918),]
  OpenArch3 <- OpenArch3[which(OpenArch3$amco %in% OpenArch1918$amco),]
  #select if also 1919 is present
  OpenArch1919 <- OpenArch2[which(OpenArch2$death_year==1919),]
  OpenArch3 <- OpenArch3[which(OpenArch3$amco %in% OpenArch1919$amco),]
  #bind
  OpenArch3 <- rbind(OpenArch3, OpenArch1918, OpenArch1919)
  #show number of available entries per year for each municipality 
  as.data.frame(table(as.data.frame(table(OpenArch3$amco))[,"Freq"]))
  
  
  
  
  ####################################
  #### Compare HDNG with OpenArch #### 
  ####################################
  
  Mortality <- merge(Overleden, OpenArch3, by.x=c("ACODE", "year"), by.y=c("amco", "death_year"), all.x=T)
  #drop years before 1920 (1921-1925 == collapsing municipalities)
  Mortality <- Mortality[which(Mortality$year<=1919),]
  #drop municipalities with practically no entries
  Mortality$n_death_OpenArch <- ifelse(Mortality$n_death_OpenArch<5 & Mortality$value>10, NA, Mortality$n_death_OpenArch)
  as.data.frame(table(as.data.frame(table(Mortality[!(Mortality$n_death_OpenArch<5 & Mortality$value>10),"ACODE"]))[,"Freq"]))
  #codeer lower, similar, higher
  Mortality$diff <- Mortality$value-Mortality$n_death_OpenArch 
  Mortality$diffperc <- round(Mortality$diff/Mortality$value*100)
  #build similarity
  Mortality$similarity <- NA
    #0-10
    Mortality$similarity[Mortality$value<=10 & Mortality$diff<  -2] <- "+"
    Mortality$similarity[Mortality$value<=10 & Mortality$diff>= -2 & Mortality$diff<= 2] <- "0"
    Mortality$similarity[Mortality$value<=10 & Mortality$diff>   2] <- "-"
    #11-30
    Mortality$similarity[Mortality$value>10 & Mortality$value<=30 & Mortality$diff<  -3] <- "+"
    Mortality$similarity[Mortality$value>10 & Mortality$value<=30 & Mortality$diff>= -3 & Mortality$diff<= 3] <- "0"
    Mortality$similarity[Mortality$value>10 & Mortality$value<=30 & Mortality$diff>   3] <- "-"
    #31-50
    Mortality$similarity[Mortality$value>30 & Mortality$value<=50 & Mortality$diff<  -5] <- "+"
    Mortality$similarity[Mortality$value>30 & Mortality$value<=50 & Mortality$diff>= -5 & Mortality$diff<= 7] <- "0"
    Mortality$similarity[Mortality$value>30 & Mortality$value<=50 & Mortality$diff>   5] <- "-"
    #51-100
    Mortality$similarity[Mortality$value>50 & Mortality$value<=100 & Mortality$diff<  -7] <- "+"
    Mortality$similarity[Mortality$value>50 & Mortality$value<=100 & Mortality$diff>= -7 & Mortality$diff<= 7] <- "0"
    Mortality$similarity[Mortality$value>50 & Mortality$value<=100 & Mortality$diff>   7] <- "-"
    #101-...
    Mortality$similarity[Mortality$value>100 & Mortality$diff<  -10] <- "+"
    Mortality$similarity[Mortality$value>100 & Mortality$diff>= -10 & Mortality$diff<= 10] <- "0"
    Mortality$similarity[Mortality$value>100 & Mortality$diff>   10] <- "-"
  #overview  
  as.data.frame(table(Mortality$similarity))
  
  #drop entries with less than 7 entries
  as.data.frame(table(as.data.frame(table(Mortality$ACODE))[,"Freq"]))
  #almost no data for <7 years
  Mortality %>% group_by(ACODE) %>% filter(n()==1)
  Mortality %>% group_by(ACODE) %>% filter(n()==2)
  Mortality %>% group_by(ACODE) %>% filter(n()==3)
  #drop entries with less than 7 entries
  Mortality <- Mortality %>% group_by(ACODE) %>% filter(n()>=7)
  as.data.frame(table(as.data.frame(table(Mortality$ACODE))[,"Freq"]))
  
  #count times under- and overestimated
  Mortality$min <- ifelse(Mortality$similarity=="-", 1, 0)
  Mortality$plus <- ifelse(Mortality$similarity=="+", 1, 0)
  Mortality$similar <- ifelse(Mortality$similarity=="0", 1, 0)
  Mortality <- Mortality %>% group_by(ACODE) %>% mutate(min=sum(min), plus=sum(plus), similar=sum(similar)) %>% ungroup()
  #
  as.data.frame(table(Mortality[which(!duplicated(Mortality$ACODE)),"min"]))
  as.data.frame(table(Mortality[which(!duplicated(Mortality$ACODE)),"plus"]))
  as.data.frame(table(Mortality[which(!duplicated(Mortality$ACODE)),"similar"]))
  
  
  
  #1. 
  Figure <- Mortality[which(!duplicated(Mortality$ACODE)),c("similar", "diff")]
  Figure <- Figure %>% group_by(similar) %>% summarise(diff=mean(diff)) %>% ungroup()
  Figure$diff <- round(Figure$diff*-1,2)
  
  ggplot(Figure, aes(x=similar, y=diff)) + 
    geom_bar(stat="identity") +
    geom_line(stat="smooth",method = "loess", lwd=1, lty=2, alpha=.3) +
    theme(panel.background = element_blank(),
          axis.text.y = element_text(colour="grey20",size=9,angle=0,hjust=.5,vjust=0,face="plain"),
          axis.text.x = element_text(colour="grey20",size=9,angle=0,hjust=.5,vjust=0,face="plain"),
          axis.title = element_text(size=14),
          strip.background = element_blank(),
          strip.text.x = element_text(size = 16, face="bold"),
          legend.position="bottom", legend.title = element_blank(),
          legend.key=element_blank()) +
    scale_x_continuous(expand = c(0.05, 0.05),
                       breaks=seq(0,10,by=1), 
                       limit=c(-.5,10.5)) +
    scale_y_continuous(expand = c(0.05, 0.05),
                       breaks=seq(0,15,by=1), 
                       limit=c(0,15)) +
    labs(x="Jaren met gelijk aantal cases als OpenArch",
         y="Gemiddelde verschil met OpenArch")
  ggsave("meandif.png", plot = last_plot(), unit="in", width=9, height=6)
  
  
  
  #map missings
  #summary
  png(file="summary.png",width = 1600, height = 2400, res=300)
  NL <- merge(shape.shp, Mortality[which(Mortality$year==1910),], by="ACODE", all.x=T)
  plot(NL, border="grey25", col=ifelse(is.na(NL$min), "white", 
                                       ifelse(NL$plus>3, "firebrick", 
                                              ifelse(NL$min>3, "forest green", "light steel blue"))))
  title("Entries in HDNG and WieWasWie", cex=1)
  legend("bottomleft",fill=c("white", "forest green", "light steel blue", "firebrick"),
         legend=c("no data", "less in OpenArch", "similar", "more in OpenArch"),cex=1)
  dev.off()
  #1910
  png(file="1910.png",width = 1600, height = 2400, res=300)
  NL <- merge(shape.shp, Mortality[which(Mortality$year==1910),], by="ACODE", all.x=T)
  plot(NL, border="grey25", col=ifelse(is.na(NL$min), "white", 
                                       ifelse(NL$similarity=="-", "forest green", 
                                              ifelse(NL$similarity=="+", "firebrick", 
                                                     ifelse(NL$similarity=="0", "light steel blue", "black")))))
  title("Entries in 1910", cex=1)
  legend("bottomleft",fill=c("white", "forest green", "light steel blue", "firebrick"),
         legend=c("no data", "less in OpenArch", "similar", "more in OpenArch"),cex=1)
  dev.off()
  #1911
  png(file="1911.png",width = 1600, height = 2400, res=300)
  NL <- merge(shape.shp, Mortality[which(Mortality$year==1911),], by="ACODE", all.x=T)
  plot(NL, border="grey25", col=ifelse(is.na(NL$min), "white", 
                                       ifelse(NL$similarity=="-", "forest green", 
                                              ifelse(NL$similarity=="+", "firebrick", 
                                                     ifelse(NL$similarity=="0", "light steel blue", "black")))))
  title("Entries in 1911", cex=1)
  legend("bottomleft",fill=c("white", "forest green", "light steel blue", "firebrick"),
         legend=c("no data", "less in OpenArch", "similar", "more in OpenArch"),cex=1)
  dev.off()
  #1912
  png(file="1912.png",width = 1600, height = 2400, res=300)
  NL <- merge(shape.shp, Mortality[which(Mortality$year==1912),], by="ACODE", all.x=T)
  plot(NL, border="grey25", col=ifelse(is.na(NL$min), "white", 
                                       ifelse(NL$similarity=="-", "forest green", 
                                              ifelse(NL$similarity=="+", "firebrick", 
                                                     ifelse(NL$similarity=="0", "light steel blue", "black")))))
  title("Entries in 1912", cex=1)
  legend("bottomleft",fill=c("white", "forest green", "light steel blue", "firebrick"),
         legend=c("no data", "less in OpenArch", "similar", "more in OpenArch"),cex=1)
  dev.off()
  #1913
  png(file="1913.png",width = 1600, height = 2400, res=300)
  NL <- merge(shape.shp, Mortality[which(Mortality$year==1913),], by="ACODE", all.x=T)
  plot(NL, border="grey25", col=ifelse(is.na(NL$min), "white", 
                                       ifelse(NL$similarity=="-", "forest green", 
                                              ifelse(NL$similarity=="+", "firebrick", 
                                                     ifelse(NL$similarity=="0", "light steel blue", "black")))))
  title("Entries in 1913", cex=1)
  legend("bottomleft",fill=c("white", "forest green", "light steel blue", "firebrick"),
         legend=c("no data", "less in OpenArch", "similar", "more in OpenArch"),cex=1)
  dev.off()
  #1914
  png(file="1914.png",width = 1600, height = 2400, res=300)
  NL <- merge(shape.shp, Mortality[which(Mortality$year==1914),], by="ACODE", all.x=T)
  plot(NL, border="grey25", col=ifelse(is.na(NL$min), "white", 
                                       ifelse(NL$similarity=="-", "forest green", 
                                              ifelse(NL$similarity=="+", "firebrick", 
                                                     ifelse(NL$similarity=="0", "light steel blue", "black")))))
  title("Entries in 1914", cex=1)
  legend("bottomleft",fill=c("white", "forest green", "light steel blue", "firebrick"),
         legend=c("no data", "less in OpenArch", "similar", "more in OpenArch"),cex=1)
  dev.off()
  #1915
  png(file="1915.png",width = 1600, height = 2400, res=300)
  NL <- merge(shape.shp, Mortality[which(Mortality$year==1915),], by="ACODE", all.x=T)
  plot(NL, border="grey25", col=ifelse(is.na(NL$min), "white", 
                                       ifelse(NL$similarity=="-", "forest green", 
                                              ifelse(NL$similarity=="+", "firebrick", 
                                                     ifelse(NL$similarity=="0", "light steel blue", "black")))))
  title("Entries in 1915", cex=1)
  legend("bottomleft",fill=c("white", "forest green", "light steel blue", "firebrick"),
         legend=c("no data", "less in OpenArch", "similar", "more in OpenArch"),cex=1)
  dev.off()
  #1916
  png(file="1916.png",width = 1600, height = 2400, res=300)
  NL <- merge(shape.shp, Mortality[which(Mortality$year==1916),], by="ACODE", all.x=T)
  plot(NL, border="grey25", col=ifelse(is.na(NL$min), "white", 
                                       ifelse(NL$similarity=="-", "forest green", 
                                              ifelse(NL$similarity=="+", "firebrick", 
                                                     ifelse(NL$similarity=="0", "light steel blue", "black")))))
  title("Entries in 1916", cex=1)
  legend("bottomleft",fill=c("white", "forest green", "light steel blue", "firebrick"),
         legend=c("no data", "less in OpenArch", "similar", "more in OpenArch"),cex=1)
  dev.off()
  #1917
  png(file="1917.png",width = 1600, height = 2400, res=300)
  NL <- merge(shape.shp, Mortality[which(Mortality$year==1917),], by="ACODE", all.x=T)
  plot(NL, border="grey25", col=ifelse(is.na(NL$min), "white", 
                                       ifelse(NL$similarity=="-", "forest green", 
                                              ifelse(NL$similarity=="+", "firebrick", 
                                                     ifelse(NL$similarity=="0", "light steel blue", "black")))))
  title("Entries in 1917", cex=1)
  legend("bottomleft",fill=c("white", "forest green", "light steel blue", "firebrick"),
         legend=c("no data", "less in OpenArch", "similar", "more in OpenArch"),cex=1)
  dev.off()
  #1918
  png(file="1918.png",width = 1600, height = 2400, res=300)
  NL <- merge(shape.shp, Mortality[which(Mortality$year==1918),], by="ACODE", all.x=T)
  plot(NL, border="grey25", col=ifelse(is.na(NL$min), "white", 
                                       ifelse(NL$similarity=="-", "forest green", 
                                              ifelse(NL$similarity=="+", "firebrick", 
                                                     ifelse(NL$similarity=="0", "light steel blue", "black")))))
  title("Entries in 1918", cex=1)
  legend("bottomleft",fill=c("white", "forest green", "light steel blue", "firebrick"),
         legend=c("no data", "less in OpenArch", "similar", "more in OpenArch"),cex=1)
  dev.off()
  #1919
  png(file="1919.png",width = 1600, height = 2400, res=300)
  NL <- merge(shape.shp, Mortality[which(Mortality$year==1919),], by="ACODE", all.x=T)
  plot(NL, border="grey25", col=ifelse(is.na(NL$min), "white", 
                                       ifelse(NL$similarity=="-", "forest green", 
                                              ifelse(NL$similarity=="+", "firebrick", 
                                                     ifelse(NL$similarity=="0", "light steel blue", "black")))))
  title("Entries in 1919", cex=1)
  legend("bottomleft",fill=c("white", "forest green", "light steel blue", "firebrick"),
         legend=c("no data", "less in OpenArch", "similar", "more in OpenArch"),cex=1)
  dev.off()
  
  
  
  
  
  as.data.frame(table(Mortality[which(Mortality$year==1918 & Mortality$similarity=="-"), "plus"]))
  
  
  
  Mortality2 <- Mortality[which(Mortality$year==1918 & Mortality$similarity=="+" |
                                  Mortality$year==1918 & Mortality$similarity=="0" |
                                  Mortality$year==1918 & Mortality$similarity=="-" & Mortality$diffperc<30),]
  Mortality2 <- Mortality[which(Mortality$ACODE %in% Mortality2$ACODE),]
  
  
  write.table(Mortality[,1:3], file="HDNG.txt", quote=F, sep ="\t", col.names=T, row.names=F)
  write.table(Mortality2, file="HDNG + OpenArch.txt", quote=F, sep ="\t", col.names=T, row.names=F)
  
  
  png(file="WieWasWie available.png",width = 1600, height = 2400, res=300)
  NL <- merge(shape.shp, Mortality[which(Mortality$year==1910),], by="ACODE", all.x=T)
  plot(NL, border="grey25", col=ifelse(is.na(NL$min), "white", "black"))
  title("Available entries in WieWasWie", cex=1)
  legend("bottomleft",fill=c("white", "black"),
         legend=c("no data", "available on WieWasWie"),cex=1)
  dev.off()
  