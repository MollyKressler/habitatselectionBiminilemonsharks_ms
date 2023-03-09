## Define study site hexagon grid and join with habitat data 
#### Manuscript: 'Habitat or safety? Drivers and management implications of juvenile lemon shark space use in mangrove nursery', Kressler, Trevail, et al. (in prep)

# unless otherwise noted, code written by Molly M Kressler

	
##################
## Prepare Workspace
##################

## Load packages 

pacman::p_load(sf,tidyverse,ggplot2)

## Load data
 hab.new<-st_as_sf(st_read('testing/clean_habitat_map_bimini_aug22.shp'),crs='WGS84')
	 ggplot()+geom_sf(data=hab.new)+theme_minimal()



##################
## Make a hexagon grid across study site  
##################

## make hexagon grid with habitat, and join with unknown habitat shape that captures the excluded buffers 

	new.hex<-st_make_grid(hab.new,cellsize=0.0012,square=FALSE,flat_topped=TRUE) ## average area of a cell is 117.8 m2

	new.hex<-st_as_sf(new.hex)%>%add_column(jcode=seq(1,12597,1),.before='x')%>%rename(grid_geo=x)

	new.hex$grid_ar<-as.numeric(st_area(st_geometry(new.hex)))
		## need to crop the hexagons to within the bounding box of the hab.new (bc its creating NAs and rejectign the join)
		union.hab<-st_as_sf(st_union(st_make_valid(hab.new))) # union the habitat bc it doesnt matter what the inside is, what matters is the peripheral extent. 
		box<-c(xmin= -79.321, ymin= 25.673, xmax= -79.217 ,ymax= 25.795)
		new.hex.cut<-st_crop(new.hex,box) ## good
		ggplot()+geom_sf(data=new.hex.cut,alpha=0.3,fill='goldenrod2')+geom_sf(data=union.hab,col='cadetblue3',alpha=0.2) # we want the h abitat (blue) to be bigger than the hexagon grid

##################
## Join habitat to the grid
##################

	hab.hex.join[50,]<-st_join(st_make_valid(new.hex.cut),st_make_valid(hab.new))%>%mutate(hab_geo=st_geometry(st_intersection(st_make_valid(st_geometry(hab.new)),st_make_valid(st_geometry(new.hex.cut)))))%>%mutate(hab_area=st_area(hab_geo)) # this takes a long time, coffee break!
	
	hab.hex.join<-hab.hex.join%>%mutate(grid_ar=st_area(st_geometry(hab.hex.join$grid_geo)))

	hab.hex.spread.prop<-hab.hex.join%>%spread(key= 'habitat',value = 'hab_area',fill=0)%>%group_by(jcode)%>%summarise(across(c('grid_ar','baresand','highdensg','lowdensg','mangrove','meddensg','sargassum','marsh','rocks.urb','deep.water','lg.sponge','inl.vegg'),max))%>%mutate(prop_brs=baresand/grid_ar,prop_ldsg=lowdensg/grid_ar,prop_medsg=meddensg/grid_ar,prop_hdsg=highdensg/grid_ar,prop_man=mangrove/grid_ar,prop_sarg=sargassum/grid_ar,prop_marsh=marsh/grid_ar,prop_urb_r=rocks.urb/grid_ar,prop_deep=deep.water/grid_ar,prop_spong=lg.sponge/grid_ar,prop_inlvg=inl.vegg/grid_ar)%>%dplyr::select(jcode,grid_ar,prop_brs,prop_ldsg,prop_medsg,prop_hdsg,prop_man,prop_sarg,prop_marsh,prop_marsh,prop_urb_r,prop_deep,prop_spong,prop_inlvg,prop_unkwn=0,grid_geo)
	
	hab.hex.spread.prop<-st_as_sf(hab.hex.spread.prop)
		{
		hab.hex.spread.prop$prop_brs<-as.numeric(hab.hex.spread.prop$prop_brs)
		hab.hex.spread.prop$prop_ldsg<-as.numeric(hab.hex.spread.prop$prop_ldsg)
		hab.hex.spread.prop$prop_medsg<-as.numeric(hab.hex.spread.prop$prop_medsg)
		hab.hex.spread.prop$prop_hdsg<-as.numeric(hab.hex.spread.prop$prop_hdsg)
		hab.hex.spread.prop$prop_man<-as.numeric(hab.hex.spread.prop$prop_man)
		hab.hex.spread.prop$prop_sarg<-as.numeric(hab.hex.spread.prop$prop_sarg)
		hab.hex.spread.prop$prop_marsh<-as.numeric(hab.hex.spread.prop$prop_marsh)
		hab.hex.spread.prop$prop_urb_r<-as.numeric(hab.hex.spread.prop$prop_urb_r)
		hab.hex.spread.prop$prop_deep<-as.numeric(hab.hex.spread.prop$prop_deep)
		hab.hex.spread.prop$prop_spong<-as.numeric(hab.hex.spread.prop$prop_spong)
		hab.hex.spread.prop$prop_inlvg<-as.numeric(hab.hex.spread.prop$prop_inlvg)
		}
		ggplot()+geom_sf(data=hab.hex.spread.prop,fill='cadetblue2',alpha=0.3)+theme_minimal()


##################
## calculate habitat metric: distance from central mangrove  
##################


	# dist.cmg - distance to central mangrove
	cmg<-st_as_sf(st_read('testing/centroid_mangroves_north.shp'),crs='WSG84')
	hab.hex.spread.prop<-hab.hex.spread.prop%>%add_column(dist_cmg=st_distance(st_centroid(hab.hex.spread.prop$grid_geo),cmg),.before='grid_geo')
	hist(hab.hex.spread.prop$dist.cmg)
	

##################
## Make bigger the bounding box 
##################

## Some receivers/buffers are outside of the area for whch we have habitat data. Need to expand the bounding box of the hexagon grid, to include these outlying receivers/bffers. Used Google Earth to draw a bounding box that captures all the buffers. Import, cut the area known out, and set all hab to 0 except for unknown=1. Then bind the rows between the cut bigger grid and the smaller grid.

	bigger<-st_as_sf(st_read('testing/biggerbox_hexgrid_ANDbuffers.kml'),crs='WGS84')

	u<-st_union(st_make_valid(hab.hex.spread.prop))

	bigger.grid<-st_make_grid(bigger,cellsize=0.0012,square=FALSE,flat_topped=TRUE)
	bigger.grid<-st_as_sf(st_difference(bigger.grid,u))

	ggplot()+geom_sf(data=bigger.grid,alpha=0.1,fill='goldenrod2',col='goldenrod2')+geom_sf(data=hab.hex.spread.prop,fill='darkcyan',col='darkcyan',alpha=.6)+theme_bw() # check

	# need these for computation of dist.cmg and nearest.cmg 

		b<-max(hab.hex.spread.prop$jcode)
		c<-b+nrow(bigger.grid)	

	bigger.grid<-bigger.grid%>%dplyr::rename(geometry=x)%>%add_column(jcode=seq(b+1,c,1),.before='geometry')

	bigger.grid<-bigger.grid%>%add_column(grid_ar=st_area(bigger.grid$geometry),prop_brs=0,prop_ldsg=0,prop_medsg=0,prop_hdsg=0,prop_man=0,prop_sarg=0,prop_marsh=0,prop_urb_r=0,prop_deep=0,prop_spong=0,prop_inlvg=0,prop_unkwn=1,dist_cmg=st_distance(st_centroid(bigger.grid$geometry),cmg),.before='geometry')


	## bind_rows(hex.grid,bigger)
		 # hex.grid<-hex.grid%>%add_column(prop_unkwn=0,.before='dist_cmg')
		bigger.grid$grid_ar<-as.numeric(bigger.grid$grid_ar)
		bigger.grid$dist_cmg<-as.numeric(bigger.grid$dist_cmg)
		bigger.grid$jcode<-as.factor(bigger.grid$jcode)
		hab.hex.spread.prop$grid_ar<-as.numeric(hab.hex.spread.prop$grid_ar)
		hab.hex.spread.prop$dist_cmg<-as.numeric(hab.hex.spread.prop$dist_cmg)
		hab.hex.spread.prop$jcode<-as.factor(hab.hex.spread.prop$jcode)
		hab.hex.spread.prop<-hab.hex.spread.prop%>%dplyr::select(jcode,grid_ar,prop_brs,prop_ldsg,prop_medsg,prop_hdsg,prop_man,prop_sarg,prop_marsh,prop_urb_r,prop_deep,prop_spong,prop_inlvg,prop_unkwn,dist_cmg)%>%rename(geometry=grid_geo)
		hex.grid2<-bind_rows(bigger.grid,hab.hex.spread.prop)
		summary(hex.grid2)

##################
## Save 
##################	

	st_write(hex.grid2,'testing/hexagon_grid_wthUPDATEDhabitat_FORMODELLING_sept22.shp',driver='ESRI Shapefile')
	st_write(hex.grid2,'testing/hexagon_grid_wthUPDATEDhabitat_FORMODELLING_sept22.csv',driver='CSV')




