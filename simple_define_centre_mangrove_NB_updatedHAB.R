## This code defines the centre of the mangroves 

pacman::p_load(tidyverse,sf,ggplot2)
hab.new<-st_as_sf(st_read('testing/clean_habitat_map_bimini_aug22.shp'),crs='WGS84')
mangrove<-hab.new[9,]
mangrove

	ggplot()+geom_sf(data=mangrove,alpha=0.3,fill='cadetblue3')+theme_bw()
	mg<-st_combine(mangrove)

	# find the centroid of the mg2 geometry 
	center.mg<-st_make_valid(st_as_sf(st_centroid(mg, of_largest_polygon=FALSE)))%>%rename(geometry=x)
	
	map<-ggplot()+geom_sf(data=mangrove,alpha=0.3,fill='grey72',col='grey30')+geom_sf(data=center.mg,col='violetred',size=5)+theme_bw()
	ggsave(map,file='map_of_centroid_mangrove_bimini_forHABSAFETYmanuscript.png',device='png',units='mm',height=240,width=140,dpi=800)




