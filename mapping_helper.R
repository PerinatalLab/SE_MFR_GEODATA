library(rgdal)  # geolocation library
library(RColorBrewer)  # color pallete library
library(scales) # for rescaling alphas
options(stringsAsFactors = F)

# load the geolocation data (coordinates, names, codes, areas)
fun_loadGeo = function(geo_dir, geo_data, variable_name, ownPalette) {
    ### "geo_data" is a data frame. it MUST contain:
    # 1) a column named "lan_kom" (4-digit municipality codes, encoded as character)
    # 2) a column variable_name that contains a mean value of something for each municipality
    # 3) should contain ~300 rows (as many as there are municipalities)
    ### ownPalette is a brewer.pal object
    
    geo_dir = normalizePath(geo_dir, winslash = "\\", mustWork = NA)
    file_name = unlist(strsplit(list.files(geo_dir)[4],"\\."))[1]
    dsn = readOGR(dsn=geo_dir, layer=file_name)
    
    if (missing(dsn)) {
        cat("missing geoData object!")
        return()
    }
    if (missing(geo_data)|missing(variable_name)) {
        if (missing(geo_data)) {
            cat("missing geo_data object! ")
        }
        if (missing(variable_name)) {
            cat("missing variable_name! ")
        }
        cat("random data will be generated and used.")
        geo_data=data.frame(lan_kom=dsn$KnKod, dummy_pheno=rnorm(nrow(dsn)))
        variable_name = "dummy_pheno"
    }
    
    # assign a color value to each geo-area
    tmp1 = data.frame(lan_kom=dsn$KnKod, seq=seq(length(dsn$KnKod)))
    tmp2 = merge(tmp1,geo_data,by="lan_kom",all.x=T)
    tmp2 = tmp2[order(tmp2$seq),]
    colr = as.character(cut(tmp2[,variable_name],10,labels=ownPalette))

    # assign areas with no value some specific color
    colr[which(is.na(colr))] = "lightgrey"
    
    ## get the legend labels, reformat as % if it's PTD rates
    rangeLegend = levels(cut(tmp2[,variable_name],10))
    if(max(geo_data[,variable_name]) < 1){
        rangeLegend = sapply(regmatches(rangeLegend, gregexpr("0.[0-9]+", rangeLegend)),
                             function(x) sprintf("%.2f-%.2f %%", as.numeric(x[1])*100, as.numeric(x[2])*100))
    }
    
    return(list(dsn=dsn, colr=colr, rangeLegend=rangeLegend))
}

correct_coords = function(dsn){
	### extract all centers of all municipalities
	ccord = data.frame(seq=seq_along(dsn$KnKod), code=dsn$KnKod, name=dsn$KnNamn,
					   x=0, y=0, area=0)
	
	Encoding(ccord$name) = "UTF-8"
	
	for (i in 1:length(dsn$KnKod)) {
		t = dsn@polygons[[i]]@Polygons[[1]]@labpt # get center coords
		ccord$x[i] = t[1]
		ccord$y[i] = t[2]
		ccord$area[i] = dsn@polygons[[i]]@Polygons[[1]]@area
		rm(t)
	}

	### correct center positions for some troublesome areas
	# SWEDEN
	ccord$y[which(ccord$code=="2506")] = 7380000 # Arjeplog
	ccord$y[which(ccord$code=="2421")] = 7277000 # Storuman
	ccord$x[which(ccord$code=="2421")] = 1480000 # Storuman
	ccord$y[which(ccord$code=="2482")] = 7230000 # SkellefteÃ¥
	ccord$y[which(ccord$code=="2313")] = 7100000 # StrÃ¶msund
	ccord$x[which(ccord$code=="2313")] = 1500000 # StrÃ¶msund
	ccord$y[which(ccord$code=="2161")] = 6850000 # Ljusdal
	ccord$y[which(ccord$code=="2326")] = 6970000 # Berg
	ccord$x[which(ccord$code=="0885")] = 1580000 # Borgholm
	ccord$y[which(ccord$code=="2580")] = 7300000 # LuleÃ¥
	ccord$y[which(ccord$code=="2380")] = 7030000 # Ãstersund
	ccord$x[which(ccord$code=="2380")] = 1460000 # Ãstersund
	ccord$x[which(ccord$code=="0481")] = 1580000 # OxelÃ¶sund
	
	ccord$y[which(ccord$code=="0481")] = 6500000 # OxelÃ¶sund
	ccord$y[which(ccord$code=="0880")] = 6275000 # Kalmar
	ccord$x[which(ccord$code=="0880")] = 1515000 # Kalmar
	ccord$y[which(ccord$code=="0319")] = 6735000 # Ãlvkarleby
	ccord$x[which(ccord$code=="0319")] = 1598000 # Ãlvkarleby
	
	# GOTHENBURG
	ccord$x[which(ccord$code=="1083")] = 1440000 # SÃ¶lvesborg
	ccord$y[which(ccord$code=="1083")] = 6205000 # SÃ¶lvesborg
	ccord$x[which(ccord$code=="0488")] = 1605000 # Trosa
	ccord$y[which(ccord$code=="0488")] = 6519000 # Trosa
	ccord$x[which(ccord$code=="0582")] = 1525000 # SÃ¶derkÃ¶ping
	ccord$y[which(ccord$code=="0582")] = 6480000 # SÃ¶derkÃ¶ping
	ccord$y[which(ccord$code=="1761")] = 6567000 # HammarÃ¶
	
	# GOTHENBURG
	ccord$x[which(ccord$code=="1487")] = 1290000 # VÃ¤nersborg
	ccord$x[which(ccord$code=="1485")] = 1275000 # Uddevalla
	ccord$y[which(ccord$code=="1485")] = 6468000 # Uddevalla
	ccord$y[which(ccord$code=="1493")] = 6505000 # Mariestad
	ccord$x[which(ccord$code=="1493")] = 1380000 # Mariestad
	ccord$y[which(ccord$code=="1489")] = 6425000 # AlingsÃ¥s
	ccord$y[which(ccord$code=="1481")] = 6392000 # MÃ¶lndal
	ccord$y[which(ccord$code=="1415")] = 6448000 # Stenungsund
	ccord$y[which(ccord$code=="1438")] = 6560000 # Dals-Ed
	ccord$y[which(ccord$code=="1401")] = 6398000 # HÃ¤rryda
	ccord$x[which(ccord$code=="1401")] = 1299000 # HÃ¤rryda
	ccord$y[which(ccord$code=="1465")] = 6363000 # Svenljunga
	ccord$y[which(ccord$code=="1463")] = 6375000 # Mark
	ccord$y[which(ccord$code=="1484")] = 6477000 # Lysekil
	
	# MALMO
	ccord$x[which(ccord$code=="1273")] = 1390000 # Osby
	ccord$x[which(ccord$code=="1231")] = 1328500 # BurlÃ¶v
	ccord$x[which(ccord$code=="1291")] = 1401500 # Simrishamn
	ccord$y[which(ccord$code=="1260")] = 6215000 # Bjuv
	
	return(ccord)
}

fun_sweden = function(globals, na_legend, ownPalette) {
    dsn = globals$dsn
    colr = globals$colr
    rangeLegend = globals$rangeLegend
    ccord = globals$ccord
   	minimal = globals$minimal
    
    if(minimal){
    	border = NA
    	textSize = 0.7
    } else {
    	border = "black"
    }
    
    # settings for plotting
    par(mai=c(0,0,0,0))
    par(oma=c(0,2,0,0))
    
    # plot geography with values for each area
    plot(dsn,col=colr,lwd=0.3, border=border)
    
    # some geo-areas will be plotted separately. mask them from the current plot
    geo <- dsn[which(substr(dsn$KnKod,1,2) %in% c("01","12","14")),]
    plot(geo,xlim=geo@bbox[1,],ylim=geo@bbox[2,],
         add=T,lwd=1e-5,lty=0,col="mistyrose4")
    
    #########  ADD MAP NAMES/INDEXES AND LEGEND WITH NAMES OF THE AREAS
    # define main regions plotted separately (Gbg,Stock,Malm)
    ccord$main = substr(ccord$code,1,2) %in% c("01","12","14")

    # which non-major municipalities are large enough to get their names ON the map
    if(!minimal){
    	ccord$large = ccord$area > 5e9
    	indx = which(!ccord$main & ccord$large)
    	text(ccord[indx,"x"],ccord[indx,"y"],ccord[indx,"name"], cex=0.3)
    } else {
    	ccord$large = FALSE
    }
    
    if(!minimal){
    	# which non-major municipalities are small enough to get their names in the legend
    	# (assign the areas with abbrev.number based on their size)
    	sub = ccord[which(!ccord$main & !ccord$large),]
    	sub$abbrev = rank(sub$area, ties.method = "first")
    	
    	sub = sub[order(sub$abbrev),]
    	clmn_fract = 0.6 # fraction of areas in the 1st column of the legend
    	sub1 = sub[1:floor(nrow(sub)*clmn_fract),]
    	sub2 = sub[(floor(nrow(sub)*clmn_fract)+1):nrow(sub),]
    	
    	x_min = dsn@bbox[1,1] - (dsn@bbox[1,2]-dsn@bbox[1,1])*0.15 # text position left
    	x_max = dsn@bbox[1,1] + (dsn@bbox[1,2]-dsn@bbox[1,1])*0.08 # text position right
    	y_max = dsn@bbox[2,2] + (dsn@bbox[2,2]-dsn@bbox[2,1])*0.0  # top text position
    	y_min = dsn@bbox[2,1] - (dsn@bbox[2,2]-dsn@bbox[2,1])*0.0  # bottom text position
    	
    	text(sub$x, sub$y, sub$abbrev, cex=0.27)
    	
    	# first column of the legend
    	sub1$y_new = seq(from=y_max, to=y_min, length.out=nrow(sub1))
    	text(rep(x_min,nrow(sub1)), sub1$y_new, cex=0.4, pos=2,
    		 labels = paste(sub1$name, sub1$abbrev))
    	
    	# second column of the legend
    	sub2$y_new = sub1$y_new[1:nrow(sub2)]
    	text(rep(x_max,nrow(sub2)), sub2$y_new, cex=0.4, pos=2,
    		 labels = paste(sub2$name, sub2$abbrev))
    } else {
    	# non-legend municipality names
    	codes_right = c("0380", "0580", "2480", "1880", "0880")
    	righ = ccord[which(ccord$code %in% codes_right),]
    	x_max = geo@bbox[1,2] + (geo@bbox[1,2]-geo@bbox[1,1])*0.15 # text position right
    	text(rep(x_max,nrow(righ)),righ$y,righ$name,cex=textSize,pos=4)
    	
    	# red points + lines to names
    	points(righ$x, righ$y, pch=19,cex=0.2,col="red")
    	segments(x0=x_max+2000,x1=righ$x, y0=righ$y, y1=righ$y, lwd=0.2)
    }
    
    ## draw the legend (values for each color)
    if(minimal){
    	x = dsn@bbox[1,2] - (dsn@bbox[1,2]-dsn@bbox[1,1])*0.25
    } else {
    	x = dsn@bbox[1,2] - (dsn@bbox[1,2]-dsn@bbox[1,1])*0.15
    }
    y = dsn@bbox[2,2] - (dsn@bbox[2,2]-dsn@bbox[2,1])*0.43
    
    ## add legend label for NAs, if any
    if(!is.na(na_legend)){
        l = c(rangeLegend, na_legend)
        p = c(ownPalette, "lightgrey")
    } else {
        l = rangeLegend
        p = ownPalette
    }
    legend(x=x,y=y,legend=l,lwd=10,col=p,box.col = 0,cex=0.6)
    
    # restore the default plotting parameters
    par(mar=c(5.1, 4.1, 4.1, 2.1))
}


#######################  STOCKHOLM AREA

fun_stockholm = function(globals) {
    dsn = globals$dsn
    colr = globals$colr
    rangeLegend = globals$rangeLegend
    ccord = globals$ccord
    minimal = globals$minimal

    # settings for plotting
    par(mai=c(0,0,0,0))

    # plot geography with values for each area
    geo <- dsn[which(substr(dsn$KnKod,1,2) == "01"),]
    colr <- colr[which(substr(dsn$KnKod,1,2) == "01")]
    ccord <- ccord[which(substr(ccord$code,1,2) == "01"),]

    # areas that will be plotted next to the map but not in the legend
    if(minimal){
    	codes_right = c()
    	codes_left = c("0180")
    	textSize = 0.65
    	border = NA
    } else {
    	codes_right = c("0188","0117","0187","0186","0120","0138","0136","0192")
    	codes_left = c("0125","0128","0140","0181")
    	textSize = 0.4
    	border = "black"
    }
    plot(geo,xlim=geo@bbox[1,],ylim=geo@bbox[2,],lwd=0.3, col=colr, border=border)
    
    # left-side municipality-names
    left = ccord[which(ccord$code %in% codes_left),]    	
    x_min = geo@bbox[1,1] - (geo@bbox[1,2]-geo@bbox[1,1])*0.02 # text position left
    text(rep(x_min,nrow(left)),left$y,left$name,cex=textSize,pos=2)
    
    points(left$x, left$y, pch=19,cex=0.2,col="red")
	segments(x0=x_min,x1=left$x, y0=left$y, y1=left$y, lwd=0.2)
    
    if(!minimal){
    	# right-side municipality-names
    	righ = ccord[which(ccord$code %in% codes_right),]
    	x_max = geo@bbox[1,2] - (geo@bbox[1,2]-geo@bbox[1,1])*0 # text position right
    	text(rep(x_max,nrow(righ)),righ$y,righ$name,cex=textSize,pos=4)
    	
    	# red points + lines to names
    	points(righ$x, righ$y, pch=19,cex=0.2,col="red")
    	segments(x0=x_max+2000,x1=righ$x, y0=righ$y, y1=righ$y, lwd=0.2)
    	
    	# legend-type municipality names
    	cntr = ccord[which( ! ccord$code %in% c(codes_left,codes_right)),]
    	cntr$abbrev = rank(cntr$area, ties.method = "first")
    	text(cntr$x,cntr$y,cntr$abbrev,cex=0.3)
    	
    	x_min = geo@bbox[1,1] + (geo@bbox[1,2]-geo@bbox[1,1])*0.1 # text position left
    	y_max = geo@bbox[2,2] - (geo@bbox[2,2]-geo@bbox[2,1])*0.05  # top text position
    	y_min = geo@bbox[2,2] - (geo@bbox[2,2]-geo@bbox[2,1])*0.35  # bottom text position
    	
    	cntr$y_new = seq(from=y_max,to=y_min,length.out=nrow(cntr))
    	text(rep(x_min,nrow(cntr)),cntr$y_new,cex=0.4,pos=2,
    		 labels=paste(cntr$name,cntr$abbrev,sep="  "))
    }
    
    # restore the default plotting parameters
    par(mar=c(5.1, 4.1, 4.1, 2.1))
}

#######################  GOTHENBURG AREA

fun_gothenburg = function(globals) {
    dsn = globals$dsn
    colr = globals$colr
    rangeLegend = globals$rangeLegend
    ccord = globals$ccord
    minimal = globals$minimal
    
    # settings for plotting
    par(mai=c(0,0,0,0))

    geo <- dsn[which(substr(dsn$KnKod,1,2) == "14"),]
    colr <- colr[which(substr(dsn$KnKod,1,2) == "14")]
    ccord <- ccord[which(substr(ccord$code,1,2) == "14"),]
    
    if(minimal){
    	codes_left = c()
    	codes_diag = c("1480")
    	textSize = 0.65
    	border = NA
    } else {
    	codes_diag = c("1480","1402","1481","1401","1463","1465")
    	codes_left = c("1438","1486","1435","1430","1427","1484","1485","1421",
    				   "1415","1419","1482","1440","1407","1441")
    	textSize = 0.4
    	border = "black"
    }
    plot(geo,xlim=geo@bbox[1,],ylim=geo@bbox[2,],col=colr, lwd=0.3, border=border)
    
    # areas that will be plotted next to the map but not in the legend
    
    #### next-to-map from the left with horizontal lines
    if(!minimal){
    	left = ccord[which(ccord$code %in% codes_left),]
    	x_min = geo@bbox[1,1] - (geo@bbox[1,2]-geo@bbox[1,1])*0.02 # text position left
    	
    	text(rep(x_min,nrow(left)),left$y,left$name, cex=textSize, pos=2)
    	points(left$x, left$y, pch=19, cex=0.2, col="red")
    	segments(x0=x_min-1000,x1=left$x, y0=left$y,y1=left$y, lwd=0.2)
    	
    	# county names to be plotted on the right side as a legend
    	righ = ccord[which( ! ccord$code %in% c(codes_left,codes_diag)),]
    	righ$abbrev = rank(righ$area, ties.method = "first")
    	text(righ$x,righ$y,righ$abbrev,cex=0.3)
    	
    	# legend-type municipality names
    	x_min = geo@bbox[1,2] + (geo@bbox[1,2]-geo@bbox[1,1])*0.0
    	y_max = geo@bbox[2,2] - (geo@bbox[2,2]-geo@bbox[2,1])*0 
    	y_min = geo@bbox[2,1] - (geo@bbox[2,2]-geo@bbox[2,1])*0 
    	
    	righ$y_new = seq(from=y_max,to=y_min,length.out=nrow(righ))
    	text(rep(x_min,nrow(righ)),righ$y_new, cex=textSize,pos=4,
    		 labels=paste(righ$abbrev,righ$name,sep=" "))
    }
    
    #### next-to-map from the left with diagonal lines
    diag = ccord[which(ccord$code %in% codes_diag),]
    angle_rad = 0.7
    points(diag$x,diag$y,pch=19,cex=0.2,col="red")
    
    for (i in 1:nrow(diag)) {
      if (diag$name[i]=="Mark") {
        x_min = geo@bbox[1,1] + (geo@bbox[1,2]-geo@bbox[1,1])*0.25
      } else if(diag$name[i]=="Svenljunga") {
        x_min = geo@bbox[1,1] + (geo@bbox[1,2]-geo@bbox[1,1])*0.40
      } else {
      	x_min = geo@bbox[1,1] + (geo@bbox[1,2]-geo@bbox[1,1])*0.05
      }
      
      t = diag[i,]
      xdif = abs(t$x - x_min)
      ydif = tan(angle_rad) * xdif
      segments(x0=x_min,x1=t$x,y0=t$y-ydif,y1=t$y,lwd=0.2)
      text(x_min+2000,t$y-ydif,t$name, cex=textSize,pos=2)

      rm(t)
    }
    
    # restore the default plotting parameters
    par(mar=c(5.1, 4.1, 4.1, 2.1))
}

#######################  MALMO AREA

fun_malmo = function(globals) {
    dsn = globals$dsn
    colr = globals$colr
    rangeLegend = globals$rangeLegend
    ccord = globals$ccord
    minimal = globals$minimal

    # settings for plotting
    par(mai=c(0,0,0,0))
    
    geo <- dsn[which(substr(dsn$KnKod,1,2) == "12"),]
    colr <- colr[which(substr(dsn$KnKod,1,2) == "12")]
    ccord <- ccord[which(substr(ccord$code,1,2) == "12"),]
    
    if(minimal){
    	codes_left = c("1280")
    	textSize = 0.65
    	border = NA
    } else {
    	codes_left = c("1292","1214","1278","1277","1284","1283","1282","1261",
    				   "1260","1262","1231","1280","1233")
    	textSize = 0.4
    	border = "black"
    }
    plot(geo,xlim=geo@bbox[1,],ylim=geo@bbox[2,],col=colr, lwd=0.3, border=border)
    
    # areas that will be plotted next to the map but not in the legend
    left = ccord[which(ccord$code %in% codes_left),]
    
    ## next-to-map from the left with horizontal lines
    x_min = geo@bbox[1,1] - (geo@bbox[1,2]-geo@bbox[1,1])*0.02 # text position left
    text(rep(x_min,nrow(left)),left$y,left$name, cex=textSize,pos=2)
    points(left$x,left$y, pch=19,cex=0.2,col="red")
    segments(x0=x_min-1000,x1=left$x, y0=left$y,y1=left$y, lwd=0.2)
    
    if(!minimal){
    	## county names to be plotted on the right side as a legend
    	righ = ccord[which( ! ccord$code %in% codes_left),]
    	righ$abbrev = rank(righ$area, ties.method = "first")
    	text(righ$x,righ$y,righ$abbrev,cex=0.3)
    	
    	# legend-type municipality names
    	x_min = geo@bbox[1,2] + (geo@bbox[1,2]-geo@bbox[1,1])*0.0
    	y_max = geo@bbox[2,2] - (geo@bbox[2,2]-geo@bbox[2,1])*0.05 
    	y_min = geo@bbox[2,1] + (geo@bbox[2,2]-geo@bbox[2,1])*0.05 
    	
    	righ$y_new = seq(from=y_max,to=y_min,length.out=nrow(righ))
    	text(rep(x_min,nrow(righ)),righ$y_new,cex=0.4,pos=4,
    		 labels=paste(righ$abbrev,righ$name,sep=" "))
    }
    
    # restore the default plotting parameters
    par(mar=c(5.1, 4.1, 4.1, 2.1))
}


############    plot all 4 windows 

fun_plot_final = function(geo_dir, geo_data, variable_name, ownPalette, na_legend, minimal=F) {
    # load the geo coordinates, assign the globals
    globals = fun_loadGeo(geo_dir, geo_data, variable_name, ownPalette)
    globals$ccord = correct_coords(globals$dsn)
    globals$minimal = minimal
    
    # split screen into 4 windows
    if(!minimal){
    	# with space for legends
    	m = matrix(c(0,.7, 0,  1,    .58,1,.55,  1, 
    				 .58, 1,.25,.55,   .58,1,  0,.25),
    				 nr=4,nc=4,byrow = T)
    } else {
    	# tight
    	m = matrix(c(0,.55, 0,  1,    .45,1,.55,  1, 
    				 .45, 1,.25,.55,   .45,1,  0,.25),
    				 nr=4,nc=4,byrow = T)
    }
    
    # fill the windows with geographical/meta/medical data
    split.screen(m)
    screen(1); fun_sweden(globals, na_legend, ownPalette)
    screen(2); fun_stockholm(globals)
    screen(3); fun_gothenburg(globals)
    screen(4); fun_malmo(globals)
    close.screen(all = TRUE)
}
