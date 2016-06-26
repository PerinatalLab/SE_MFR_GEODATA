# by Jonas B.
# June 2016

library(rgdal)
library(RColorBrewer)

## please provide first:
geo_dir = ".../KommunRT90/" 
# extract geographical coordinates and names
file_name = unlist(strsplit(list.files(geo_dir)[4],"\\."))[1]
dsn = readOGR(dsn=".",layer=file_name)

fun_se = function(your_data,variable_name,legend) {
        # NOTE: before you start - load the "rgdal" and "RColorBrewer" libraries        
        
        ### "your_data" is  a data frame. MUST contain:
        # 1) a column "lan_kom" 
        # 2) should contain ~300 rows (as many as there are municipalities)
        # 3) a variable that contains a mean value of something for each municipality
        ### "variable_name" is a name of a column that contains values/means/betas for each municipality
        ### "legen" is a TRUE or FALSE statement denoting whether to plot a legend (easy choice, huh?)
        
        # extract municipality codes and their plotting order
        your_data = as.data.frame(your_data)
        areas = data.frame(lan_kom = as.character(dsn$KnKod),
                           com_names = as.character(dsn$KnNamn),
                           sq = seq(length(dsn$KnNamn)), stringsAsFactors = F)
        
        # merge it with your data
        geo_dat = merge(areas,your_data,by="lan_kom",all.x=T) #all.x: preserve orig. nmb. of municipal.
        geo_dat = geo_dat[order(geo_dat$sq),] # restore original plotting order!
        
        # assign colors to the region based on some variable, plot
        cols = as.character(cut(geo_dat[,variable_name],11,labels = brewer.pal(11, "Spectral")))
        cols[is.na(cols)]="lightgrey"
        plot(dsn,col=cols,main=variable_name)
        
        if(legend==TRUE) {
                x = dsn@bbox[1,2] - (dsn@bbox[1,2]-dsn@bbox[1,1])*0.1
                y = dsn@bbox[2,2] - (dsn@bbox[2,2]-dsn@bbox[2,1])*0.4
                legend(x,y,names(table(cut(geo_dat[,variable_name],11))),
                       lwd=10,col=brewer.pal(11, "Spectral"),box.col = 0,cex=0.8)
        }
}

