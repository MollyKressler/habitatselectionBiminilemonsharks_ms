## Buffers (Detection Ranges) and the associated habitat, and join habitat to detections (via buffers)
	# Habitat for a detection is calculated for the entire detection range of the receiver, since we cannot assume to know the exact location within the detection range. Since habtat values are proportions of available water area, habitat informaiton at the scale of the detection range (or buffer) is comparable to that of the hexagon grid (a smaller radii).  
	# Habitat information is assgned to detectons by joining buffers dataframes with detection dataframes.
#### Manuscript: 'Habitat or safety? Drivers and management implications of juvenile lemon shark space use in mangrove nursery', Kressler, Trevail, et al. (in prep)

# unless otherwise noted, code written by Molly M Kressler

	
##################
## Prepare Workspace
##################

## Load packages 

pacman::p_load(tidyverse,sf,ggplot2)

## Load Data 

data<-read.csv('testing/detections_2019to2020_pcl60to99_withMtide_cleaned_GHOSTSREMOVED_dec22.csv')%>%dplyr::select(-X,-difftime,-segment_start,-segment_num,-SegmentID)

hab.new<-st_as_sf(st_read('testing/clean_habitat_map_bimini_aug22.shp'),crs='WGS84')

cmg<-st_as_sf(st_read('testing/centroid_mangroves_north.shp'))

##################
## Define Buffers, in a shapefile 
##################

# extract the unique receiver locations 
	sfdata<-st_as_sf(data,coords=c('GPS_W','GPS_N'),crs='WGS84')

	receivers<-sfdata%>%distinct(geometry) # takes a while, go brew some coffee or watch a tiktok

# make buffer around them, 211m radius

	buffs<-st_buffer(receivers,dist=211)%>%mutate(buffID=paste0('r',seq(1:35)),.before='geometry')
	ggplot()+geom_sf(data=buffs,alpha=0.5,fill='cadetblue2')+theme_bw() # check

# save the buffer shapes 

	st_write(buffs,'testing/buffers_2019_2020.shp')


##################
## Join Habitat to Buffers
##################

## calculate area for each buffer 

	buffs<-buffs%>%add_column(buff_ar=st_area(buffs$geometry))

## crop buffer to extent of the habitat shapefile. then retorspectively add the buffers that exist outside the known habitat later, with zeros for all proportion columns, and an addition column of unknown habitat = 1
	box<-c(xmin= -79.321, ymin= 25.675, xmax= -79.22 ,ymax= 25.795)
	buffs2.crop<-st_crop(buffs,box)
	
	# Identify which buffers/receivers got cropped out, visually:

		ggplot()+geom_sf(data=buffs[c(10,12,13,17),],alpha=0.5,fill='cadetblue2')+geom_sf(data=buffs2.crop,alpha=0.5,fill='goldenrod2')+theme_bw() # check

	cropped.out.buffs<-st_as_sf(buffs[c(10,12,13,17),])%>%mutate(prop_brs=0,prop_ldsg=0,prop_medsg=0,prop_hdsg=0,prop_man=0,prop_sarg=0,prop_marsh=0,prop_urb_r=0,prop_deep=0,prop_inlvg=0,.before='geometry')%>%add_column(prop_unkwn=1,.before='geometry')
	
	sf_use_s2(FALSE) # turn speherical geometry off

	buffs2.join<-st_join(st_make_valid(buffs2.crop),st_make_valid(hab.new))%>%mutate(hab_geo=st_geometry(st_intersection(st_make_valid(st_geometry(hab.new)),st_make_valid(st_geometry(buffs2.crop)))))%>%mutate(hab_area=st_area(hab_geo))

	buffs2.spread<-buffs2.join%>%spread(key= 'habitat',value = 'hab_area',fill=0)%>%group_by(buffID)%>%summarise(across(c('buff_ar','baresand','highdensg','lowdensg','mangrove','meddensg','sargassum','marsh','rocks.urb','deep.water','inl.vegg'),max))%>%mutate(prop_brs=baresand/buff_ar,prop_ldsg=lowdensg/buff_ar,prop_medsg=meddensg/buff_ar,prop_hdsg=highdensg/buff_ar,prop_man=mangrove/buff_ar,prop_sarg=sargassum/buff_ar,prop_marsh=marsh/buff_ar,prop_urb_r=rocks.urb/buff_ar,prop_deep=deep.water/buff_ar,prop_inlvg=inl.vegg/buff_ar)%>%dplyr::select(buffID,buff_ar,prop_brs,prop_ldsg,prop_medsg,prop_hdsg,prop_man,prop_sarg,prop_marsh,prop_marsh,prop_urb_r,prop_deep,prop_inlvg,geometry)%>%add_column(prop_unkwn=0,.before='geometry')
	
	buffs.new<-buffs2.spread
		{
		buffs.new$prop_brs<-as.numeric(buffs.new$prop_brs)
		buffs.new$prop_ldsg<-as.numeric(buffs.new$prop_ldsg)
		buffs.new$prop_medsg<-as.numeric(buffs.new$prop_medsg)
		buffs.new$prop_hdsg<-as.numeric(buffs.new$prop_hdsg)
		buffs.new$prop_man<-as.numeric(buffs.new$prop_man)
		buffs.new$prop_sarg<-as.numeric(buffs.new$prop_sarg)
		buffs.new$prop_marsh<-as.numeric(buffs.new$prop_marsh)
		buffs.new$prop_urb_r<-as.numeric(buffs.new$prop_urb_r)
		buffs.new$prop_deep<-as.numeric(buffs.new$prop_deep)
		buffs.new$prop_inlvg<-as.numeric(buffs.new$prop_inlvg)
		buffs.new$prop_unkwn<-as.numeric(buffs.new$prop_unkwn)
		}

	buffs.new2<-bind_rows(buffs.new,cropped.out.buffs) ## add the buffs back that were cropped (because outside the extent of known habitat)

## calculate habitat metrics: distance from mangrove edge, and distance from central mangrove  
	# dist.cmg - distance to central mangrove
	buffs.new2<-buffs.new2%>%add_column(dist.cmg=st_distance(st_centroid(buffs.new2$geometry),cmg),.before='geometry')

## save 

	st_write(buffs.new2,'testing/buffers20192020_withUPDATEDhabitat_sept22.shp',driver='ESRI Shapefile')
	st_write(buffs.new2,'testing/buffers20192020_withUPDATEDhabitat_sept22.csv',driver='CSV')


##################
## Join Habitat to Detections, using buffers
##################

	dett.hab<-st_join(sfdata,buffs.new2)

	dett.hab<-dett.hab%>%add_column(GPS_W=st_coordinates(.)[,1],.before='PIT')%>%add_column(GPS_N=st_coordinates(.)[,2],.after='GPS_W')

## save
	st_write(dett.hab,'testing/detections20192020_NOghosts_withUPDATEDhabitat_sept22.shp',driver='ESRI Shapefile')
	st_write(dett.hab,'testing/detections20192020__NOghosts_withUPDATEDhabitat_sept22.csv',driver='CSV')


