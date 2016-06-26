
### script: colour geolocations (Sweden)
#  by Jonas Bacelis. 26 June, 2016 

### description:
# color-codes 290 geographical areas in Sweden based on the
# provided variable. Plots the whole Sweden, and separately -
# Stockholm, Gothenburg, Malmo areas.

### example use:
# find it at the end of the code

library(rgdal)  # geolocation library
library(RColorBrewer)  # color pallete library

# load the geolocation data (coordinates, names, codes, areas)
fun_loadGeo = function(geo_dir) {
        geo_dir = normalizePath(geo_dir, winslash = "\\", mustWork = NA)
        file_name = unlist(strsplit(list.files(geo_dir)[4],"\\."))[1]
        dsn <<- readOGR(dsn=geo_dir,layer=file_name)
        rm(file_name,geo_dir)
}


fun_sweden = function(dsn,geo_data,variable_name) {
        
        ### NOTE: before you start - load the "rgdal" and "RColorBrewer" libraries        
        ### "geo_data" is a data frame. it MUST contain:
        # 1) a column named "lan_kom" (4-digit municipality codes, encoded as character)
        # 2) a column XXX that contains a mean value of something for each municipality
        # 3) should contain ~300 rows (as many as there are municipalities)
        ### "variable_name" is a name of a column XXX in "geo_data"
        
        if (missing(dsn)) {
                cat("missing geoData object. run \"fun_loadGeo()\" function first!")
        } else {
                if (missing(geo_data)|missing(variable_name)) {
                        if (missing(geo_data)) {
                                cat("missing geo_data object! ")
                        }
                        if (missing(variable_name)) {
                                cat("missing variable_name! ")
                        }
                        cat("random data will be generated and used.")
                        geo_data=data.frame(lan_kom=dsn$KnKod,dummy_pheno=rnorm(nrow(dsn)),stringsAsFactors=F)
                        variable_name = "dummy_pheno"
                }
                
                # assign a color value to each geo-area
                tmp1 = data.frame(lan_kom=dsn$KnKod,seq=seq(length(dsn$KnKod)),stringsAsFactors = F)
                tmp2 = merge(tmp1,geo_data,by="lan_kom",all.x=T)
                tmp2 = tmp2[order(tmp2$seq),]
                colr = as.character(cut(tmp2[,variable_name],10,labels=brewer.pal(10,"Spectral")))
                rm(tmp1,tmp2)
                
                # assign areas with no value some specific color
                colr[which(is.na(colr))]="lightgrey"
                
                # settings for plotting
                par(mai=c(0,0,0,0))
                par(oma=c(0,0,0,0))
                
                # plot geography with values for each area
                plot(dsn,col=colr,lwd=0.3) #main=variable_name,
                
                # some geo-areas will be plotted separately. mask them from the current plot
                no_polt_ix = which(substr(dsn$KnKod,1,2) %in% c("01","12","14"))
                geo <- dsn[no_polt_ix,]
                plot(geo,xlim=geo@bbox[1,],ylim=geo@bbox[2,],
                     add=T,lwd=1e-5,lty=0,col="mistyrose4")
                
                #########  ADD MAP NAMES/INDEXES AND LEGEND WITH NAMES OF THE AREAS
                
                ### extract all centers of all municipalities
                ccord = NULL # center coordinates and area
                for (i in 1:length(dsn$KnKod)) {
                        t = dsn@polygons[[i]]@Polygons[[1]]@labpt # get center coords
                        s = dsn@polygons[[i]]@Polygons[[1]]@area
                        u = data.frame(seq=i,code=dsn$KnKod[i],name=dsn$KnNamn[i],
                                       x=t[1],y=t[2],area=s,stringsAsFactors=F)
                        ccord = rbind(ccord,u)
                        rm(t,u,s)
                }
                
                # types of municipalities
                cond1 = substr(ccord$code,1,2) %in% c("01","12","14") # MAIN (Gbg,Stock,Malm)
                cond2 = ccord$area > 5e9 # LARGE
                
                
                # CORRECTING centers for problematic regions
                ccord$y[which(ccord$code=="2506")] = 7380000 # Arjeplog
                ccord$y[which(ccord$code=="2421")] = 7277000 # Storuman
                ccord$x[which(ccord$code=="2421")] = 1480000 # Storuman
                ccord$y[which(ccord$code=="2482")] = 7230000 # Skellefteå
                ccord$y[which(ccord$code=="2313")] = 7100000 # Strömsund
                ccord$x[which(ccord$code=="2313")] = 1500000 # Strömsund
                ccord$y[which(ccord$code=="2161")] = 6850000 # Ljusdal
                ccord$y[which(ccord$code=="2326")] = 6970000 # Berg
                ccord$x[which(ccord$code=="0885")] = 1580000 # Borgholm
                ccord$y[which(ccord$code=="2580")] = 7300000 # Luleå
                ccord$y[which(ccord$code=="2380")] = 7030000 # Östersund
                ccord$x[which(ccord$code=="2380")] = 1460000 # Östersund
                ccord$x[which(ccord$code=="0481")] = 1580000 # Oxelösund
                ccord$y[which(ccord$code=="0481")] = 6500000 # Oxelösund
                ccord$y[which(ccord$code=="0880")] = 6275000 # Kalmar
                ccord$x[which(ccord$code=="0880")] = 1515000 # Kalmar
                ccord$y[which(ccord$code=="0319")] = 6735000 # Älvkarleby
                ccord$x[which(ccord$code=="0319")] = 1598000 # Älvkarleby
                ccord$x[which(ccord$code=="1083")] = 1440000 # Sölvesborg
                ccord$y[which(ccord$code=="1083")] = 6205000 # Sölvesborg
                ccord$x[which(ccord$code=="0488")] = 1605000 # Trosa
                ccord$y[which(ccord$code=="0488")] = 6519000 # Trosa
                ccord$x[which(ccord$code=="0582")] = 1525000 # Söderköping
                ccord$y[which(ccord$code=="0582")] = 6480000 # Söderköping
                ccord$y[which(ccord$code=="1761")] = 6567000 # Hammarö
                #text(ccord$x,ccord$y,ccord$code,cex=0.3)
                
                
                ### assign the areas with abbrev.number based on their size
                indx = which(!cond1 & !cond2)
                sub1 = ccord[indx,] # will have indexes on the map
                sub2 = ccord[-indx,] # will be plotted separately or will have names on the map
                
                sub1 = sub1[order(sub1$area),]
                sub1$abbrev = seq(nrow(sub1))
                sub2 = sub2[order(sub2$area),] # not necessary
                sub2$abbrev = seq(nrow(sub2)) + nrow(sub1)  # not necessary
                
                # rewrite original object
                ccord = rbind(sub1,sub2)
                ccord = ccord[order(ccord$seq),]
                
                # which non-major municipalities are large enough to get their names ON the map
                indx = which(!cond1 & cond2)
                text(ccord[indx,"x"],ccord[indx,"y"],ccord[indx,"name"],cex=0.3)
                
                # which non-major municipalities are small enough to get their names in the legend
                indx = which(!cond1 & !cond2)
                text(ccord[indx,"x"],ccord[indx,"y"],ccord[indx,"abbrev"],cex=0.27)
                
                sub = ccord[which(!cond1 & !cond2),]
                sub = sub[order(sub$abbrev),]
                clmn_fract = 0.6 # fraction of areas in the 1st column of the legend
                sub1 = sub[1:floor(nrow(sub)*clmn_fract),]
                sub2 = sub[(floor(nrow(sub)*clmn_fract)+1):nrow(sub),]
                
                x_min = dsn@bbox[1,1] - (dsn@bbox[1,2]-dsn@bbox[1,1])*0.1 # text position left
                x_max = dsn@bbox[1,1] + (dsn@bbox[1,2]-dsn@bbox[1,1])*0.1 # text position right
                y_max = dsn@bbox[2,2] + (dsn@bbox[2,2]-dsn@bbox[2,1])*0.0  # top text position
                y_min = dsn@bbox[2,1] - (dsn@bbox[2,2]-dsn@bbox[2,1])*0.0  # bottom text position
                
                
                # first column of the legend
                sub1$y_new = seq(from=y_max,to=y_min,length.out=nrow(sub1))
                text(rep(x_min,nrow(sub1)),sub1$y_new,cex=0.4,pos=2,
                     labels = paste(sub1$name,sub1$abbrev,sep=" "),col="white",font=2)
                text(rep(x_min,nrow(sub1)),sub1$y_new,cex=0.4,pos=2,
                     labels = paste(sub1$name,sub1$abbrev,sep=" "))
                
                # second column of the legend
                tmp = seq(from=y_max,to=y_min,by=sub1$y_new[2]-sub1$y_new[1])
                sub2$y_new = tmp[1:nrow(sub2)]
                text(rep(x_max,nrow(sub2)),sub2$y_new,cex=0.4,pos=2,
                     labels = paste(sub2$name,sub2$abbrev,sep=" "))
                
                ## draw the legend (values for each color)
                x = dsn@bbox[1,2] - (dsn@bbox[1,2]-dsn@bbox[1,1])*0.35
                y = dsn@bbox[2,2] - (dsn@bbox[2,2]-dsn@bbox[2,1])*0.43
                l = c(names(table(cut(geo_data[,variable_name],10))),"nonsignificant")
                r = c(col=brewer.pal(10, "Spectral"),"lightgrey")
                legend(x=x,y=y,legend=l,lwd=10,col=r,box.col = 0,cex=0.6)
                
                # restore the default plotting parameters
                par(mar=c(5.1, 4.1, 4.1, 2.1))
                
        }
} # end of function




#######################
#######################  STOCKHOLM AREA

fun_stockholm = function(dsn,geo_data,variable_name) {
        
        ### NOTE: before you start - load the "rgdal" and "RColorBrewer" libraries        
        ### "geo_data" is a data frame. it MUST contain:
        # 1) a column named "lan_kom" (4-digit municipality codes, encoded as character)
        # 2) a column XXX that contains a mean value of something for each municipality
        # 3) should contain ~300 rows (as many as there are municipalities)
        ### "variable_name" is a name of a column XXX in "geo_data"
        
        if (missing(dsn)) {
                cat("missing geoData object. run \"fun_loadGeo()\" function first!")
        } else {
                if (missing(geo_data)|missing(variable_name)) {
                        if (missing(geo_data)) {
                                cat("missing geo_data object! ")
                        }
                        if (missing(variable_name)) {
                                cat("missing variable_name! ")
                        }
                        cat("random data will be generated and used.")
                        geo_data=data.frame(lan_kom=dsn$KnKod,dummy_pheno=rnorm(nrow(dsn)),stringsAsFactors=F)
                        variable_name = "dummy_pheno"
                }
                
                # assign a color value to each geo-area
                tmp1 = data.frame(lan_kom=dsn$KnKod,seq=seq(length(dsn$KnKod)),stringsAsFactors = F)
                tmp2 = merge(tmp1,geo_data,by="lan_kom",all.x=T)
                tmp2 = tmp2[order(tmp2$seq),]
                colr = as.character(cut(tmp2[,variable_name],10,labels=brewer.pal(10,"Spectral")))
                rm(tmp1,tmp2)
                
                # assign areas with no value some specific color
                colr[which(is.na(colr))]="lightgrey"
                
                # settings for plotting
                par(mai=c(0,0,0,0))
                #par(oma=c(0,0,0,0))
                
                # plot geography with values for each area
                geo <- dsn[which(substr(dsn$KnKod,1,2) == "01"),]
                plot(geo,xlim=geo@bbox[1,],ylim=geo@bbox[2,],lwd=0.3,col=colr)
                
                ### extract all centers of all municipalities
                ccord = NULL # center coordinates and area
                for (i in 1:length(geo$KnKod)) {
                        t = geo@polygons[[i]]@Polygons[[1]]@labpt # get center coords
                        s = geo@polygons[[i]]@Polygons[[1]]@area
                        u = data.frame(seq=i,code=geo$KnKod[i],name=geo$KnNamn[i],
                                       x=t[1],y=t[2],area=s,stringsAsFactors=F)
                        ccord = rbind(ccord,u)
                        rm(t,u,s)
                }
                #text(ccord$x,ccord$y,ccord$code,cex=0.7)
                
                # areas that will be plotted next to the map but not in the legend
                codes_right = c("0188","0117","0187","0186","0120","0138","0136","0192")
                codes_left = c("0125","0128","0140","0181") # "0191","0139"
                
                left = ccord[which(ccord$code %in% codes_left),]
                righ = ccord[which(ccord$code %in% codes_right),]
                cntr = ccord[which( ! ccord$code %in% c(codes_left,codes_right)),]
                cntr = cntr[order(cntr$area),]
                cntr$abbrev = seq(nrow(cntr))
                text(cntr$x,cntr$y,cntr$abbrev,cex=0.3)
                
                x_min = geo@bbox[1,1] - (geo@bbox[1,2]-geo@bbox[1,1])*0.02 # text position left
                x_max = geo@bbox[1,2] - (geo@bbox[1,2]-geo@bbox[1,1])*0 # text position right
                text(rep(x_min,nrow(left)),left$y,left$name,cex=0.4,pos=2)
                text(rep(x_max,nrow(righ)),righ$y,righ$name,cex=0.4,pos=4)
                
                for (i in 1:nrow(left)) {
                        t = left[i,]
                        #segments(x0=x_min,x1=t$x,y0=t$y_new,y1=t$y,lwd=0.2)
                        segments(x0=x_min-2000,x1=t$x,y0=t$y,y1=t$y,lwd=0.2)
                        points(t$x,t$y,pch=19,cex=0.2,col="red")
                        rm(t)
                }
                
                for (i in 1:nrow(righ)) {
                        t = righ[i,]
                        #segments(x0=x_max,x1=t$x,y0=t$y_new,y1=t$y,lwd=0.2)
                        segments(x0=x_max+2000,x1=t$x,y0=t$y,y1=t$y,lwd=0.2)
                        points(t$x,t$y,pch=19,cex=0.2,col="red")
                        rm(t)
                }
                
                
                # legend-type municipality names
                x_min = geo@bbox[1,1] + (geo@bbox[1,2]-geo@bbox[1,1])*0.1 # text position left
                y_max = geo@bbox[2,2] - (geo@bbox[2,2]-geo@bbox[2,1])*0.05  # top text position
                y_min = geo@bbox[2,2] - (geo@bbox[2,2]-geo@bbox[2,1])*0.35  # bottom text position
                
                cntr$y_new = seq(from=y_max,to=y_min,length.out=nrow(cntr))
                text(rep(x_min,nrow(cntr)),cntr$y_new,cex=0.4,pos=2,
                     labels=paste(cntr$name,cntr$abbrev,sep="  "))
                
                # restore the default plotting parameters
                par(mar=c(5.1, 4.1, 4.1, 2.1))
        }
} # end of function


#######################
#######################  GOTHENBURG AREA

fun_gothenburg = function(dsn,geo_data,variable_name) {
        
        ### NOTE: before you start - load the "rgdal" and "RColorBrewer" libraries        
        ### "geo_data" is a data frame. it MUST contain:
        # 1) a column named "lan_kom" (4-digit municipality codes, encoded as character)
        # 2) a column XXX that contains a mean value of something for each municipality
        # 3) should contain ~300 rows (as many as there are municipalities)
        ### "variable_name" is a name of a column XXX in "geo_data"
        
        if (missing(dsn)) {
                cat("missing geoData object. run \"fun_loadGeo()\" function first!")
        } else {
                if (missing(geo_data)|missing(variable_name)) {
                        if (missing(geo_data)) {
                                cat("missing geo_data object! ")
                        }
                        if (missing(variable_name)) {
                                cat("missing variable_name! ")
                        }
                        cat("random data will be generated and used.")
                        geo_data=data.frame(lan_kom=dsn$KnKod,dummy_pheno=rnorm(nrow(dsn)),stringsAsFactors=F)
                        variable_name = "dummy_pheno"
                }
                
                # assign a color value to each geo-area
                tmp1 = data.frame(lan_kom=dsn$KnKod,seq=seq(length(dsn$KnKod)),stringsAsFactors = F)
                tmp2 = merge(tmp1,geo_data,by="lan_kom",all.x=T)
                tmp2 = tmp2[order(tmp2$seq),]
                colr = as.character(cut(tmp2[,variable_name],10,labels=brewer.pal(10,"Spectral")))
                rm(tmp1,tmp2)
                
                # assign areas with no value some specific color
                colr[which(is.na(colr))]="lightgrey"
                
                # settings for plotting
                par(mai=c(0,0,0,0))
                par(oma=c(0,0,0,0))
                
                geo <- dsn[which(substr(dsn$KnKod,1,2) == "14"),]
                plot(geo,xlim=geo@bbox[1,],ylim=geo@bbox[2,],col=colr,lwd=0.3)
                
                ### extract all centers of all municipalities
                ccord = NULL # center coordinates and area
                for (i in 1:length(geo$KnKod)) {
                        t = geo@polygons[[i]]@Polygons[[1]]@labpt # get center coords
                        s = geo@polygons[[i]]@Polygons[[1]]@area
                        u = data.frame(seq=i,code=geo$KnKod[i],name=geo$KnNamn[i],
                                       x=t[1],y=t[2],area=s,stringsAsFactors=F)
                        ccord = rbind(ccord,u)
                        rm(t,u,s)
                }
                
                
                # CORRECTING centers for problematic regions
                ccord$x[which(ccord$code=="1487")] = 1290000 # Vänersborg
                ccord$x[which(ccord$code=="1485")] = 1275000 # Uddevalla
                ccord$y[which(ccord$code=="1485")] = 6468000 # Uddevalla
                ccord$y[which(ccord$code=="1493")] = 6505000 # Mariestad
                ccord$x[which(ccord$code=="1493")] = 1380000 # Mariestad
                ccord$y[which(ccord$code=="1489")] = 6425000 # Alingsås
                ccord$y[which(ccord$code=="1481")] = 6392000 # Mölndal
                ccord$y[which(ccord$code=="1415")] = 6448000 # Stenungsund
                ccord$y[which(ccord$code=="1438")] = 6560000 # Dals-Ed
                ccord$y[which(ccord$code=="1401")] = 6398000 # Härryda
                ccord$x[which(ccord$code=="1401")] = 1299000 # Härryda
                ccord$y[which(ccord$code=="1465")] = 6363000 # Svenljunga
                ccord$y[which(ccord$code=="1463")] = 6375000 # Mark
                ccord$y[which(ccord$code=="1484")] = 6477000 # Lysekil
                #text(ccord$x,ccord$y,ccord$code,cex=0.6)
                
                # areas that will be plotted next to the map but not in the legend
                codes_left = c("1438","1486","1435","1430","1427","1484","1485","1421","1415","1419","1482","1440","1407","1441")
                codes_diag = c("1480","1402","1481","1401","1463","1465")
                left = ccord[which(ccord$code %in% codes_left),]
                diag = ccord[which(ccord$code %in% codes_diag),]
                
                cntr = ccord[which( ! ccord$code %in% c(codes_left,codes_diag)),]
                cntr = cntr[order(cntr$area),]
                cntr$abbrev = seq(nrow(cntr))
                text(cntr$x,cntr$y,cntr$abbrev,cex=0.3)
                
                #### next-to-map from the left with horizontal lines
                x_min = geo@bbox[1,1] - (geo@bbox[1,2]-geo@bbox[1,1])*0.02 # text position left
                text(rep(x_min,nrow(left)),left$y,left$name,cex=0.4,pos=2)  # 0.4
                for (i in 1:nrow(left)) {
                        t = left[i,]
                        segments(x0=x_min-1000,x1=t$x,y0=t$y,y1=t$y,lwd=0.2)
                        points(t$x,t$y,pch=19,cex=0.2,col="red")
                        rm(t)
                }
                
                #### next-to-map from the left with diagonal lines
                angle_rad = 0.7
                for (i in 1:nrow(diag)) {
                        if (diag$name[i]=="Mark") {
                                x_min = geo@bbox[1,1] + (geo@bbox[1,2]-geo@bbox[1,1])*0.25
                        }
                        
                        if(diag$name[i]=="Svenljunga") {
                                x_min = geo@bbox[1,1] + (geo@bbox[1,2]-geo@bbox[1,1])*0.40
                        }
                        
                        if(! diag$name[i] %in% c("Mark","Svenljunga")) {
                                x_min = geo@bbox[1,1] + (geo@bbox[1,2]-geo@bbox[1,1])*0.05
                        }
                        
                        
                        t = diag[i,]
                        xdif = abs(t$x-(x_min))
                        ydif = tan(angle_rad)*xdif
                        segments(x0=x_min,x1=t$x,y0=t$y-ydif,y1=t$y,lwd=0.2)
                        text(x_min+2000,t$y-ydif,t$name,cex=0.4,pos=2)  # 0.4
                        points(t$x,t$y,pch=19,cex=0.2,col="red")
                        rm(t)
                }
                
                # county names to be plotted on the right side as a legend
                righ = cntr
                
                # legend-type municipality names
                x_min = geo@bbox[1,2] + (geo@bbox[1,2]-geo@bbox[1,1])*0.0
                y_max = geo@bbox[2,2] - (geo@bbox[2,2]-geo@bbox[2,1])*0 
                y_min = geo@bbox[2,1] - (geo@bbox[2,2]-geo@bbox[2,1])*0 
                
                righ$y_new = seq(from=y_max,to=y_min,length.out=nrow(righ))
                text(rep(x_min,nrow(righ)),righ$y_new,cex=0.4,pos=4,
                     labels=paste(righ$abbrev,righ$name,sep=" "))
                
                # restore the default plotting parameters
                par(mar=c(5.1, 4.1, 4.1, 2.1))
        }        
} # end of function


#######################
#######################  MALMO AREA

fun_malmo = function(dsn,geo_data,variable_name) {
        
        ### NOTE: before you start - load the "rgdal" and "RColorBrewer" libraries        
        ### "geo_data" is a data frame. it MUST contain:
        # 1) a column named "lan_kom" (4-digit municipality codes, encoded as character)
        # 2) a column XXX that contains a mean value of something for each municipality
        # 3) should contain ~300 rows (as many as there are municipalities)
        ### "variable_name" is a name of a column XXX in "geo_data"
        
        if (missing(dsn)) {
                cat("missing geoData object. run \"fun_loadGeo()\" function first!")
        } else {
                if (missing(geo_data)|missing(variable_name)) {
                        if (missing(geo_data)) {
                                cat("missing geo_data object! ")
                        }
                        if (missing(variable_name)) {
                                cat("missing variable_name! ")
                        }
                        cat("random data will be generated and used.")
                        geo_data=data.frame(lan_kom=dsn$KnKod,dummy_pheno=rnorm(nrow(dsn)),stringsAsFactors=F)
                        variable_name = "dummy_pheno"
                }
                
                # assign a color value to each geo-area
                tmp1 = data.frame(lan_kom=dsn$KnKod,seq=seq(length(dsn$KnKod)),stringsAsFactors = F)
                tmp2 = merge(tmp1,geo_data,by="lan_kom",all.x=T)
                tmp2 = tmp2[order(tmp2$seq),]
                colr = as.character(cut(tmp2[,variable_name],10,labels=brewer.pal(10,"Spectral")))
                rm(tmp1,tmp2)
                
                # assign areas with no value some specific color
                colr[which(is.na(colr))]="lightgrey"
                
                # settings for plotting
                par(mai=c(0,0,0,0))
                #par(oma=c(0,0,0,0))
                
                geo <- dsn[which(substr(dsn$KnKod,1,2) == "12"),]
                plot(geo,xlim=geo@bbox[1,],ylim=geo@bbox[2,],col=colr,lwd=0.3)
                
                ### extract all centers of all municipalities
                ccord = NULL # center coordinates and area
                for (i in 1:length(geo$KnKod)) {
                        t = geo@polygons[[i]]@Polygons[[1]]@labpt # get center coords
                        s = geo@polygons[[i]]@Polygons[[1]]@area
                        u = data.frame(seq=i,code=geo$KnKod[i],name=geo$KnNamn[i],
                                       x=t[1],y=t[2],area=s,stringsAsFactors=F)
                        ccord = rbind(ccord,u)
                        rm(t,u,s)
                }
                
                # CORRECTING centers for problematic regions
                ccord$x[which(ccord$code=="1273")] = 1390000 # Osby
                ccord$x[which(ccord$code=="1231")] = 1328500 # Burlöv
                ccord$x[which(ccord$code=="1291")] = 1401500 # Simrishamn
                ccord$y[which(ccord$code=="1260")] = 6215000 # Bjuv
                #ccord$y[which(ccord$code=="1230")] = 6175000 # Staffanstorp
                #ccord$x[which(ccord$code=="1230")] = 1333500 # Staffanstorp
                #text(ccord$x,ccord$y,ccord$code,cex=0.5)
                
                # areas that will be plotted next to the map but not in the legend
                codes_left = c("1292","1214","1278","1277","1284","1283","1282","1261","1260","1262","1231","1280","1233")
                left = ccord[which(ccord$code %in% codes_left),]
                
                cntr = ccord[which( ! ccord$code %in% codes_left),]
                cntr = cntr[order(cntr$area),]
                cntr$abbrev = seq(nrow(cntr))
                text(cntr$x,cntr$y,cntr$abbrev,cex=0.3)
                
                #### next-to-map from the left with horizontal lines
                x_min = geo@bbox[1,1] - (geo@bbox[1,2]-geo@bbox[1,1])*0.02 # text position left
                text(rep(x_min,nrow(left)),left$y,left$name,cex=0.4,pos=2)  # 0.4
                for (i in 1:nrow(left)) {
                        t = left[i,]
                        segments(x0=x_min-1000,x1=t$x,y0=t$y,y1=t$y,lwd=0.2)
                        points(t$x,t$y,pch=19,cex=0.2,col="red")
                        rm(t)
                }
                
                # county names to be plotted on the right side as a legend
                righ = cntr
                
                # legend-type municipality names
                x_min = geo@bbox[1,2] + (geo@bbox[1,2]-geo@bbox[1,1])*0.0
                y_max = geo@bbox[2,2] - (geo@bbox[2,2]-geo@bbox[2,1])*0.05 
                y_min = geo@bbox[2,1] + (geo@bbox[2,2]-geo@bbox[2,1])*0.05 
                
                righ$y_new = seq(from=y_max,to=y_min,length.out=nrow(righ))
                text(rep(x_min,nrow(righ)),righ$y_new,cex=0.4,pos=4,
                     labels=paste(righ$abbrev,righ$name,sep=" "))
                
                # restore the default plotting parameters
                par(mar=c(5.1, 4.1, 4.1, 2.1))
        }   
} # end of function



############    plot all 4 windows 

fun_plot_final = function(geo_dir,geo_data,variable_name) {
        # load the geo coordinates, create an object "dsn"
        fun_loadGeo(geo_dir)
        # split screen into 4 windows
        m = matrix(c(.1,.6, 0,  1,    .5,1,.55,  1, 
                     .5, 1,.25,.55,   .5,1,  0,.25),
                   nr=4,nc=4,byrow = T)
        # fill the windows with geographical/meta/medical data
        split.screen(m)
        screen(1); fun_sweden(dsn,geo_data,variable_name)
        screen(2); fun_stockholm(dsn,geo_data,variable_name)
        screen(3); fun_gothenburg(dsn,geo_data,variable_name)
        screen(4); fun_malmo(dsn,geo_data,variable_name)
        close.screen(all = TRUE)
}

##############
##############  AND ACTION  (example)
##############

#geo_dir = "~/Biostuff/SEMFR_DATA/postalCodes_2015/GeoData_Sweden/KommunRT90/" 
#geo_data = as.data.frame(df)
#variable_name = "m"
#pdf("~/Downloads/deleteme.pdf",width=10, height=7)
#fun_plot_final(geo_dir,geo_data,variable_name)
#dev.off()
