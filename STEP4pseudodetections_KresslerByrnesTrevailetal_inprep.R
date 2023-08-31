## Define available habitat, sample pseudos, join pseudo locations to habitat data 
#### Manuscript: 'Habitat or safety? Drivers and management implications of juvenile lemon shark space use in mangrove nursery', Kressler, Trevail, et al. (in prep)

# unless otherwise noted, code written by Molly M Kressler

	
##################
## Prepare Workspace
##################

## Load packages 

	pacman::p_load(tidyverse,flextable,lubridate,sf,ggplot2)

## Load Data 

	land<-st_as_sf(st_read('testing/bim_onlyland_noDots.kml'),crs='WGS84')	
	
	center.mg<-st_as_sf(st_read('testing/centroid_mangroves_north.shp'),crs='WGS84')

	vars<-c('day','date','lat','lon','sunrise','sunset','ghost') # superfluous in the data frame. don't need so we'll select to remove them. 
	data<-read.csv('testing/detections20192020__NOghosts_withUPDATEDhabitat_sept22.csv')%>%
		rename(tidephase=Tidepeak_type)%>%
		filter(tidephase!='M')%>%
		dplyr::select(-all_of(vars))

	final.hab<-st_as_sf(st_read('testing/hexagon_grid_wthUPDATEDhabitat_FORMODELLING_sept22.shp'),crs='WGS84')


##################
## Define available habitat 
##################

	hab.noland<-st_difference(st_make_valid(final.hab),st_union(land))
	
	available<-st_as_sf(st_union(hab.noland))
	
	ggplot()+geom_sf(data=available)+theme_bw() # check. should look like a grey rectable with some jagged edges, and the land shape of bimini but out of the middle. The Lat and long should run roughly between 25.8N-25.65N and 79.32W-79.2W.

	st_write(available,'testing/availablehabitat_updatedSept2022.kml')


##################
## Sample Pseudos and Assign Habitat 
##################

## Make an empty simple features df 

	detts.sf<-st_as_sf(data,coords=c('GPS_W','GPS_N'),crs='WGS84')

	empty<-as.data.frame(detts.sf)%>%
		dplyr::select(time,PIT,pcl,sex,tidephase,dorn)%>%
		add_column(use=0,.after='PIT')%>% # use is how we identify real detections from pseudo detectons. a use=1 is a real, and a use=0 is a pseudo-detection. 
		slice(rep(1:n(),each=7)) # based on evaluating the computing effort required for different levels of pseudo-real ratios, we decided 7 was the optimal number for the ratio of pseudo to real detections, for the method of random background sampling. This line creates 7 lines for every one original detection, creatng a large df empty of spatial informaiton with the detetion times and biometrics of the real detection. 

## Sample available space

	p7<-st_as_sf(st_geometry(st_sample(available,size=(nrow(detts.sf)*7),type='random'))) 

## Bind empty dataframe with corresponding informaton from detections data to new random locations 

	p7b<-bind_cols(p7,empty)%>%
		rename(geometry=x)

## Join pseudos to habitata data 

	p7wh<-st_join(p7b,final.hab)

	p7wh<-p7wh%>%
		add_column(GPS_W=st_coordinates(.)[,1],.before='PIT')%>%
		add_column(GPS_N=st_coordinates(.)[,2],.after='GPS_W')


## save the pseudo-detections shapefile as a dataframe
	st_write(p7wh,'testing/pseudos_GHOSTSremoved_n7_20192020_withUPDATEDhabitat_habmodel_dec22.csv',append=FALSE)
	st_write(p7wh,'testing/pseudos_GHOSTSremoved_n7_20192020_withUPDATEDhabitat_habmodel_dec22.shp',append=FALSE)









