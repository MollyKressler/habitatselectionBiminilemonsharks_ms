###########################
## - Chapter 2 Figures - ##
## final figures from chapter 2 for manuscript, supplementary materials and presentations 
## created 19 October 2022 by Molly Kressler

pacman::p_load(sf,tidyverse,ggplot2,gridExtra,flextable,arm,lubridate,cowplot,patchwork)

### Regular ggplot2 lines, for consistency
	+geom_sf(data=land,col='grey72',fill='grey82')
	labels<-as_labeller(c('H'='High Tide','L'='Low Tide'))
	+facet_grid(cols=vars(tidephs),labeller=(labels))
	device='pdf',units='mm',width=250,height=150


########
## - Meta Data Table with durations 
	meta<-read.csv('biometric_and_metadata_for_habsel_glmers_dec22.csv')%>%dplyr::select(-X)
	meta_flex<-flextable(meta)%>%set_header_labels(sex = 'Sex',average_duration_hours = 'Average Duration\nbetween Detection (hours)',min_duration_hours = 'Minimum Duration\nbetween Detection (hours)',max_duration_days = 'Maximum Duration\nbetween Detection (days)',days = 'Days at\nLiberty',pcl = 'Pre-Caudal\nLength (cm)')%>%theme_alafoli()%>%align(align = 'center', part = 'all')%>%font(fontname = 'Times', part = 'all')%>%fontsize(size = 10, part = 'all')%>%autofit()
	meta_flex

########
## - Habitat and acoustic grids

	## habitat data map with CMG
		cols<-c('baresand'='khaki2','lowdensg'='steelblue1','meddensg'='steelblue3','highdensg'='steelblue4','sargassum'='magenta4','lg.sponge'='tan2','inl.vegg'='darkseagreen4','mangrove'='darkgreen','marsh'='darkolivegreen','rocks.urb'='grey70','deep.water'='navyblue') # colours for each habitat type
		types<-c('baresand'='Bare sand', 'lowdensg'='Low density seagrass', 'meddensg'='Medium density seagrass', 'highdensg'='High density seagrass', 'sargassum'='Sargassum', 'lg.sponge'='Large sponge','inl.vegg'='Inland Vegetation','mangrove'='Mangrove', 'marsh'='Marsh & Inlets', 'rocks.urb'='Rocky or Urban outcrops', 'deep.water'='Deep Water') # formal names for habitat type legend

		setwd('/Users/mollykressler/Documents/data_phd')
		hab.new<-st_as_sf(st_read('clean_habitat_map_bimini_aug22.shp'),crs='WGS84')
		setwd('/Users/mollykressler/Documents/data_phd/habitat_model')
		cmg<-st_as_sf(st_read('centroid_mangroves_north.shp'),crs='WSG84')


		figure.hab<-ggplot()+geom_sf(data=hab.new,aes(fill=factor(habitat),col=factor(habitat)))+
			scale_fill_manual(values=cols,name=NULL,labels=types,aesthetics=c('colour','fill'),position='right')+
			theme(legend.position=c(.5,.5),legend.text=element_text(size=20),axis.text=element_text(size=20))+
			theme_bw()+geom_sf(data=cmg,pch=23,size=6,col='#FFFFFF',fill='#F01D7F')+
			theme(axis.text.x = element_blank(),axis.text.y = element_blank(),axis.ticks=element_blank(),legend.position=c(0.01,0.99),legend.justification=c(0,1))

		ggsave(figure.hab,file='habitattypes_bimini_map.pdf',device='pdf',units='mm',dpi=1200,height=250,width=200)	
 

	## receiver station map
		setwd('/Users/mollykressler/Documents/data_phd')
		land<-st_as_sf(st_read('bim_onlyland_noDots.kml'),crs='WGS84')	
		setwd('/Users/mollykressler/Documents/data_phd/habitat_model')
		buffs<-st_as_sf(st_read('buffers_2019_2020.shp'),crs='ESRI Shapefile')
		detts<-read.csv('detections_2019_2020_pcl62to99_noMtide_august22.csv')%>%dplyr::select(time,GPS_N,GPS_W,PIT,pcl,sex,tidephase,month,dorn)
		detts.sf<-st_as_sf(detts,coords=c('GPS_W','GPS_N'),crs='WGS84')
			{
			detts.sf$time<-as_datetime(detts.sf$time,tz='EST')
			detts.sf$PIT<-as_factor(detts.sf$PIT)
			detts.sf$dorn<-as_factor(detts.sf$dorn)
			detts.sf$month<-as_factor(detts.sf$month)
			detts.sf$tidephase<-as_factor(detts.sf$tidephase)
			detts.sf$sex<-as_factor(detts.sf$sex)			
			}

		base.map<-ggplot()+geom_sf(data=land,fill='grey72',col='grey72')+
			geom_sf(data=buffs,col=NA,fill='cadetblue3',alpha=0.85)+
			geom_sf(data=detts.sf,col='black',pch=20)+theme_bw()+
			north(data=buffs,location='bottomright',scale=0.11,symbol=12)+
			theme(axis.text=element_text(size=10))
		ggsave(base.map,file='receiver20192020_bimini_map.pdf',device='pdf',units='mm',dpi=800,height=250,width=200)



########
## - map combining habitat and receiver map, different color marker for CMG and inset of Florida and bimini - Vital Heim suggestion	
	# background = habitat map, foreground = receivers 
	cols<-c('baresand'='khaki2','lowdensg'='steelblue1','meddensg'='steelblue3','highdensg'='steelblue4','sargassum'='magenta4','lg.sponge'='tan2','inl.vegg'='darkseagreen4','mangrove'='darkgreen','marsh'='darkolivegreen','rocks.urb'='grey70','deep.water'='navyblue') # colours for each habitat type
	types<-c('baresand'='Bare sand', 'lowdensg'='Low density seagrass', 'meddensg'='Medium density seagrass', 'highdensg'='High density seagrass', 'sargassum'='Sargassum', 'lg.sponge'='Large sponge','inl.vegg'='Inland Vegetation','mangrove'='Mangrove', 'marsh'='Marsh & Inlets', 'rocks.urb'='Rocky or Urban outcrops', 'deep.water'='Deep Water') # formal names for habitat type legend

	allr <- st_as_sf(st_read('biminireceivers_withLocations_20190414to20201213.shp'),crs='WGS84') # all receivers during study interval, with location
	allr2 <- allr %>% filter(geometry==unique(geometry)) # gets an mapply warning but it's alright.
	study_area<-st_union(st_make_valid(hab.new))
	allr3 <- st_intersection(study_area,allr2) # there are some very very far away receivers that are outside the study area by tens of miles. This function will select only the recievers that fall within the study area. 
	yes <- detts.sf %>% distinct(geometry)%>%
		dplyr::select('geometry')%>%
		add_column(sharks = 'yes')
	no <- st_sf(allr3) %>% add_column(sharks='no') %>% rename(geometry=allr3)

	all <- bind_rows(yes,no) %>% distinct(geometry,.keep_all=TRUE)

	hab_with_receivers<-ggplot()+
		geom_sf(data=hab.new,aes(fill=factor(habitat),col=factor(habitat)))+
		scale_fill_manual(values=cols,name=NULL,labels=types,aesthetics=c('colour','fill'),position='right')+
		theme(legend.position=c(.5,.5),legend.text=element_text(size=20),axis.text=element_text(size=20))+
		geom_sf(data=land,alpha=0,col='grey30')+
		theme_bw()+
		geom_sf(data=cmg,pch=23,size=6,col='#FFFFFF',fill='#F01D7F')+
		theme(axis.text.x = element_blank(),axis.text.y = element_blank(),axis.ticks=element_blank(),legend.position=c(0.01,0.99),axis.text=element_text(size=10),legend.justification=c(0,1))+
		geom_sf(data=all%>%filter(sharks=='no'),fill='grey82', col='white',pch=22,size=3)+
		geom_sf(data=all%>%filter(sharks=='yes'),fill='white', col='grey50',pch=21,size=3)

	#inset.map
	pacman::p_load(rnaturalearth,rnaturalearthdata)
	usa<-ne_states(country='united states of america')
	names(usa)
	usa$abbrev
	wewant<-c('Fla.','Ga.','S.C.','Ala.','Miss.')
	south<-st_as_sf(usa)%>%
		filter(region=='South')%>%
		dplyr::select(abbrev,geometry)%>%
		rename(Name=abbrev)%>%
		filter(Name %in% wewant) # I know this is janky
	ggplot()+geom_sf(data=land2)+theme_bw()
	
	land<-land%>%dplyr::select('Name',geometry)
	land2<- st_zm(land,drop=TRUE)
	flbm<-bind_rows(land2,south)
	inset <-ggplot()+
		geom_sf(data=flbm,col='grey72')+
		theme_bw()+
		theme(axis.text.y = element_blank(),axis.ticks.y = element_blank(),axis.text.x = element_blank(),axis.ticks.x = element_blank())+
		geom_rect(aes(xmin=-79.20,xmax=-79.4,ymin=25.5,ymax=25.9),col='goldenrod2',fill='goldenrod2',alpha=0.4,lwd=.8)


	# put it together
	combohabmap<-ggdraw()+
		draw_plot(hab_with_receivers)+
		draw_plot(inset,height=0.2, x=-.32,y=0.03)
	
	ggsave(combohabmap,file='habitattypesANDreceiversANDinsetmap_inONE_bimini_map.png',device='png',units='in',dpi=1200,height=9,width=6)	




######## 
## - surfaces from individual habitat type GLMMs

	pacman::p_load(tidyverse,ggplot2,gridExtra,lubridate,beepr,sf)

	# habitat hexagon grid 
	setwd('/Users/mollykressler/Documents/data_phd')
	hex.grid<-st_as_sf(st_read('hexagon_grid_withUPDATEDhabitat_sept22.shp'),crs='WGS84')
		
	# land shape 
	setwd('/Users/mollykressler/Documents/data_phd')
	land<-st_as_sf(st_union(st_read('bim_onlyland_noDots.kml')),crs='WGS84')


	setwd('/Users/mollykressler/Documents/data_phd/habitat_model/no ghosts of 6hr threshold')
		cmg<-readRDS('distance2centralmangrove_glmer_habmodel_use_w7_GHOSTSremoved_dec22.RDS')
		bs<-readRDS('baresand_glmer_habmodel_use_w7_GHOSTSremoved_dec22.RDS')
		mds<-readRDS('mediumdensityseagrass_glmer_habmodel_use_w7_GHOSTSremoved_dec22.RDS')
		sar<-readRDS('sargassum_glmer_habmodel_use_w7_GHOSTSremoved_dec22.RDS')
		urb<-readRDS('urbanandrockyoutcrops_glmer_habmodel_use_w7_GHOSTSremoved_dec22.RDS')
		deep<-readRDS('deepwater_glmer_habmodel_use_w7_GHOSTSremoved_dec22.RDS')


	## (1) get estimates across the hexagon-grid for each GLMM, plot a heatmap with theme_void

		# add tdephase to hex.grid, add column tidephase then set equal to L, do the same for H on a duplicate grid set and bind rows. 
		hex.grid.L<-hex.grid%>%add_column(tidephase='L')
		hex.grid.H<-hex.grid%>%add_column(tidephase='H')
		hex.grid<-bind_rows(hex.grid.L,hex.grid.H)
		hex.grid$tidephase<-as_factor(hex.grid$tidephase)

		# Predict un-adjusted probability of use estimates into the hex.grid, making pred.hex.
		pred.hex<-hex.grid%>%add_column(bs.use=invlogit(predict(bs,.,re.form=NA)),.before='geometry')%>%add_column(lds.use=invlogit(predict(lds,hex.grid,re.form=NA)),.before='geometry')%>%add_column(mds.use=invlogit(predict(mds,hex.grid,re.form=NA)),.before='geometry')%>%add_column(hds.use=invlogit(predict(hds,hex.grid,re.form=NA)),.before='geometry')%>%add_column(sar.use=invlogit(predict(sar,hex.grid,re.form=NA)),.before='geometry')%>%add_column(urb.use=invlogit(predict(urb,hex.grid,re.form=NA)),.before='geometry')%>%add_column(deep.use=invlogit(predict(deep,hex.grid,re.form=NA)),.before='geometry')%>%add_column(cmg.use=invlogit(predict(cmg,hex.grid,re.form=NA)),.before='geometry')

		# plots per glmm
			bs.plot<-ggplot()+geom_sf(data=pred.hex,aes(fill=bs.use),col=NA)+scale_fill_distiller(palette='RdPu',direction=1,limits=c(0,1),guide=guide_colourbar(title='Probability of Use'))+geom_sf(data=land,col='grey72',fill='grey82')+facet_grid(cols=vars(tidephase))+theme_void()
			mds.plot<-ggplot()+geom_sf(data=pred.hex,aes(fill=mds.use),col=NA)+scale_fill_distiller(palette='RdPu',direction=1,limits=c(0,1),guide=guide_colourbar(title='Probability of Use'))+geom_sf(data=land,col='grey72',fill='grey82')+facet_grid(cols=vars(tidephase))+theme_void()
			sar.plot<-ggplot()+geom_sf(data=pred.hex,aes(fill=sar.use),col=NA)+scale_fill_distiller(palette='RdPu',direction=1,limits=c(0,1),guide=guide_colourbar(title='Probability of Use'))+geom_sf(data=land,col='grey72',fill='grey82')+facet_grid(cols=vars(tidephase))+theme_void()
			urb.plot<-ggplot()+geom_sf(data=pred.hex,aes(fill=urb.use),col=NA)+scale_fill_distiller(palette='RdPu',direction=1,limits=c(0,1),guide=guide_colourbar(title='Probability of Use'))+geom_sf(data=land,col='grey72',fill='grey82')+facet_grid(cols=vars(tidephase))+theme_void()
			deep.plot<-ggplot()+geom_sf(data=pred.hex,aes(fill=deep.use),col=NA)+scale_fill_distiller(palette='RdPu',direction=1,limits=c(0,1),guide=guide_colourbar(title='Probability of Use'))+geom_sf(data=land,col='grey72',fill='grey82')+facet_grid(cols=vars(tidephase))+theme_void()
			cmg.plot<-ggplot()+geom_sf(data=pred.hex,aes(fill=cmg.use),col=NA)+scale_fill_distiller(palette='RdPu',direction=1,limits=c(0,1),guide=guide_colourbar(title='Probability of Use'))+geom_sf(data=land,col='grey72',fill='grey82')+facet_grid(cols=vars(tidephase))+theme_void()

########
## - Habitat Selection GLMMS, model summaries table 
	setwd('/Users/mollykressler/Documents/data_phd/habitat_model/no ghosts of 6hr threshold')
	cmg<-readRDS('distance2centralmangrove_glmer_habmodel_use_w7_GHOSTSremoved_dec22.RDS')
	bs<-readRDS('baresand_glmer_habmodel_use_w7_GHOSTSremoved_dec22.RDS')
	mds<-readRDS('mediumdensityseagrass_glmer_habmodel_use_w7_GHOSTSremoved_dec22.RDS')
	sar<-readRDS('sargassum_glmer_habmodel_use_w7_GHOSTSremoved_dec22.RDS')
	urb<-readRDS('urbanandrockyoutcrops_glmer_habmodel_use_w7_GHOSTSremoved_dec22.RDS')
	deep<-readRDS('deepwater_glmer_habmodel_use_w7_GHOSTSremoved_dec22.RDS')
	models<-c(bs,lds,mds,hds,sar,urb,deep,dist.cmg)
	modelsummary(models,coef_rename=c('prop_brs'='Habitat Type','prop_ldsg'='Habitat Type','prop_medsg'='Habitat Type','prop_hdsg'='Habitat Type','prop_sarg'='Habitat Type','prop_urb_r'='Habitat Type','prop_deep'='Habitat Type','dist_cmg'='Habitat Type','prop_brstidephaseL'='Habitat-Tide Interaction','prop_ldsgtidephaseL'='Habitat-Tide Interaction','prop_medsgtidephaseL'='Habitat-Tide Interaction','prop_hdsgtidephaseL'='Habitat-Tide Interaction','prop_sargtidephaseL'='Habitat-Tide Interaction','prop_urb_rtidephaseL'='Habitat-Tide Interaction','prop_deeptidephaseL'='Habitat-Tide Interaction','dist_cmgtidephaseL'='Habitat-Tide Interaction','SD (prop_brs PIT)'='SD (Habitat Individual)','Cor (Intercept~prop_brs PIT)'='Cor (Intercept~Habitat Individual)','SD (prop_ldsg PIT)'='SD (Habitat Individual)','Cor (Intercept~prop_ldsg PIT)'='Cor (Intercept~Habitat Individual)','SD (prop_medsg PIT)'='SD (Habitat Individual)','Cor (Intercept~prop_medsg PIT)'='Cor (Intercept~Habitat Individual)','SD (prop_hdsg PIT)'='SD (Habitat Individual)','Cor (Intercept~prop_hdsg PIT)'='Cor (Intercept~Habitat Individual)','SD (prop_sarg PIT)'='SD (Habitat Individual)','Cor (Intercept~prop_sarg PIT)'='Cor (Intercept~Habitat Individual)','SD (prop_urb_r PIT)'='SD (Habitat Individual)','Cor (Intercept~prop_urb_r PIT)'='Cor (Intercept~Habitat Individual)','SD (prop_deep PIT)'='SD (Habitat Individual)','Cor (Intercept~prop_deep PIT)'='Cor (Intercept~Habitat Individual)','SD (dist_cmg PIT)'='SD (Habitat Individual)','Cor (Intercept~dist_cmg PIT)'='Cor (Intercept~Habitat Individual)','SD (Intercept PIT)' = 'SD (Group Intercept)'), fmt=3,estimate='estimate', statistic='conf.int',stars=TRUE,conf_level=0.95,output='modelsummaries_glmers_ghostsfiltered_habselmodels_dec22.html')

########
## - Habitat selection GLMMS, marginal effects plots

	setwd('/Users/mollykressler/Documents/data_phd/habitat_model/no ghosts of 6hr threshold')
	all<-read.csv('habmodel_data_noGHOSTS_all_w3_psd7_noMtide_sept22.csv')
		{
		all<-all%>%dplyr::mutate(prop_unkwn=ifelse(is.na(prop_unkwn),0,prop_unkwn))
		all$time<-as.Date(all$time)
		all$PIT<-as_factor(all$PIT)
		all$sex<-as_factor(all$sex)
		all$tidephase<-as_factor(all$tidephase)
		all$month<-as_factor(all$month)
		all$dorn<-as_factor(all$dorn)
		all$use<-as_factor(all$use)
		} # combining pseudos and real detections with habitat into one dateframe for modelling. psd vs real identifiable via 'use' (0/1).

	head(all,3)
	summary(all)


	## plot of random slope lines for dist.cmg
	cmg<-readRDS('distance2centralmangrove_glmer_habmodel_use_w7_GHOSTSremoved_dec22.RDS')

	cols<-c('Tag 1'='#723747','Tag 2'='#d1044b','Tag 3'='#6b6561','Tag 4'='#497f7b','Tag 5'='#e804a7','Tag 6'='#5e5627','Tag 7'='#3090a8','Tag 8'='#b584af','Tag 9'='#7a4a6e','Tag 10'='#c68e13','Tag 11'='#021d47','Tag 12'='#8bf429','Tag 13'='#c4c40b','Tag 14'='#c4450f','Tag 15'='#cba7ce','Tag 16'='#871f4c')# visually distinct
	## need to anonymise the PT tags to Tags 1-6

	me<-as.data.frame(ggpredict(cmg,terms=c('dist_cmg[all]','PIT'),type='random'))
	me$group<-as_factor(me$group)
	dist.fixed<-as.data.frame(ggpredict(cmg,terms=c('dist_cmg[all]'),type='random'))

	dist.plot<-ggplot()+geom_line(data=me,aes(x=x,y=predicted,group=group),col='grey66',size=.6,linetype=1,alpha=1)+ylab('Probability of Use')+xlab('Distance from Central Mangrove Forest (m)')+theme_bw()+geom_line(data=dist.fixed,aes(x=x,y=predicted),col='black',size=.6,linetype=1)+expand_limits(y=c(0,1))


	### Translate Tags to PITs

	setwd('/Users/mollykressler/Documents/data_phd')
	meta<-read.csv('metadata_sharks_used_in_models_n16_habmodelsRSPandDBBMM.csv')
	meta

	################# plot MARGINAL EFFECTS OF RANDOM EFFECTS - random slop lines for bare sand, medium density seagrass, urban and rocky, sargassum and deepwater - could probably do these in grayscale..
	bs<-readRDS('baresand_glmer_habmodel_use_w7_GHOSTSremoved_dec22.RDS')
	mds<-readRDS('mediumdensityseagrass_glmer_habmodel_use_w7_GHOSTSremoved_dec22.RDS')
	sar<-readRDS('sargassum_glmer_habmodel_use_w7_GHOSTSremoved_dec22.RDS')
	urb<-readRDS('urbanandrockyoutcrops_glmer_habmodel_use_w7_GHOSTSremoved_dec22.RDS')
	deep<-readRDS('deepwater_glmer_habmodel_use_w7_GHOSTSremoved_dec22.RDS')


		# deep 
		me3<-as.data.frame(ggpredict(deep, terms=c('prop_deep[all]','PIT'),type='random'))
		me3$group<-as_factor(me3$group)
		deep.fixed<-as.data.frame(ggpredict(deep,terms='prop_deep[all]',type='random'))
		max(detts$prop_deep)
		deep.p<-me3%>%filter(x<=0.117)
		# subset it for only Tags 7,8,12,15
		deep.f2<-deep.fixed%>%filter(x<=0.117)
		deep.plot<-ggplot()+geom_line(data=deep.p,aes(x=x,y=predicted,group=group),size=.6,linetype=1,alpha=1,col='gray66')+ylab('Probability of Use')+xlab('Proportion of Deep Water')+theme_bw()+geom_line(data=deep.f2,aes(x=x,y=predicted),col='black',size=.6,linetype=1)+expand_limits(y=c(0,1)) # in grayscale, black as the mean calculated separatley using ggpredict

		# bare sand
		bare<-as.data.frame(ggpredict(bs, terms=c('prop_brs[all]','PIT'),type='random'))
		bare$group<-as_factor(bare$group)
		bs.fixed<-as.data.frame(ggpredict(bs,terms='prop_brs[all]',type='random'))
		max(detts$prop_brs)
		sand.p<-bare%>%filter(x<=max(detts$prop_brs))
		sand.p2<-sand.p%>%filter(group %in% targets)
		sand.f2<-bs.fixed%>%filter(x<=max(detts$prop_brs))
		sand.plot<-ggplot()+geom_line(data=sand.p,aes(x=x,y=predicted,group=group),size=.6,linetype=1,alpha=1,col='gray66')+ylab('Probability of Use')+xlab('Proportion of Bare Sand')+theme_bw()+geom_line(data=sand.f2,aes(x=x,y=predicted),col='black',size=.6,linetype=1)+expand_limits(y=c(0,1)) # in grayscale, black as the mean calculated separatley using ggpredict


		# medium density seagrass 
		me7<-as.data.frame(ggpredict(mds, terms=c('prop_medsg[all]','PIT'),type='random'))
		me7$group<-as_factor(me7$group)
		med.fixed<-as.data.frame(ggpredict(mds,terms='prop_medsg[all]',type='random'))
		max(detts$prop_medsg)
		med.p<-me7%>%filter(x<=max(detts$prop_medsg))
		med.f2<-med.fixed%>%filter(x<=max(detts$prop_medsg))	
		med.plot<-ggplot()+geom_line(data=med.p,aes(x=x,y=predicted,group=group),size=.6,linetype=1,alpha=1,col='gray66')+ylab('Probability of Use')+xlab('Proportion of Medium Density Seagrass')+geom_line(data=med.f2,aes(x=x,y=predicted),col='black',size=.6,linetype=1)+expand_limits(y=c(0,1))+theme_bw() # in grayscale, black as the mean calculated separatley using ggpredict

		# urban and rocky 
		me5<-as.data.frame(ggpredict(urb, terms=c('prop_urb_r[all]','PIT'),type='random'))
		me5$group<-as_factor(me5$group)
		urb.fixed<-as.data.frame(ggpredict(urb,terms='prop_urb_r[all]',type='random'))
		max(detts$prop_urb_r)
		urb.p<-me5%>%filter(x<=max(detts$prop_urb_r))
		urb.f2<-urb.fixed%>%filter(x<=max(detts$prop_urb_r))	
		urb.plot<-ggplot()+geom_line(data=urb.p,aes(x=x,y=predicted,group=group),size=.6,linetype=1,alpha=1,col='gray66')+ylab('Probability of Use')+xlab('Proportion of Urban & Rocky Outcrops')+theme_bw()+geom_line(data=urb.f2,aes(x=x,y=predicted),col='black',size=.6,linetype=1)+expand_limits(y=c(0,1))# in grayscale, black as the mean calculated separatley using ggpredict


		# sargassum 
		me6<-as.data.frame(ggpredict(sar, terms=c('prop_sarg[all]','PIT'),type='random'))
		sar.fixed<-as.data.frame(ggpredict(sar,terms='prop_sarg[all]',type='random'))
		me6$group<-as_factor(me6$group)
		max(detts$prop_sarg)
		sarg.p<-me6%>%filter(x<=max(detts$prop_sarg))
		sarg.f2<-sar.fixed%>%filter(x<=max(detts$prop_sarg))	
		sarg.plot<-ggplot()+geom_line(data=sarg.p,aes(x=x,y=predicted,group=group),size=.6,linetype=1,alpha=1,col='gray66')+ylab('Probability of Use')+xlab('Proportion of Sargassum')+theme_bw()+geom_line(data=sarg.f2,aes(x=x,y=predicted),col='black',size=.6,linetype=1)+expand_limits(y=c(0,1))#grayscale, black as the mean calculated separatley using ggpredict


	### put them all together 

		individual.slopes<-grid.arrange(dist.plot,deep.plot,sand.plot,med.plot,urb.plot,sarg.plot,ncol=3)
		ggsave(file='marginal_effects_random_effects_glmers_habmodel_withGHOSTSremoved_dec22.pdf',individual.slopes, device='pdf',units='mm',height=200,width=300,dpi=600)

######## 
## - Model summary flextable & AICs of seagrass densitys (flex tables)
	pacman::p_load(tidyverse,modelsummary,flextable,sf)
	setwd('/Users/mollykressler/Documents/data_phd/habitat_model/no ghosts of 6hr threshold')	
	cmg<-readRDS('distance2centralmangrove_glmer_habmodel_use_w7_GHOSTSremoved_dec22.RDS')
	bs<-readRDS('baresand_glmer_habmodel_use_w7_GHOSTSremoved_dec22.RDS')
	mds<-readRDS('mediumdensityseagrass_glmer_habmodel_use_w7_GHOSTSremoved_dec22.RDS')
	lds<-readRDS('lowdensityseagrass_glmer_habmodel_use_w7_GHOSTSremoved_dec22.RDS')
	hds<-readRDS('highdensityseagrass_glmer_habmodel_use_w7_GHOSTSremoved_dec22.RDS')
	sar<-readRDS('sargassum_glmer_habmodel_use_w7_GHOSTSremoved_dec22.RDS')
	urb<-readRDS('urbanandrockyoutcrops_glmer_habmodel_use_w7_GHOSTSremoved_dec22.RDS')
	deep<-readRDS('deepwater_glmer_habmodel_use_w7_GHOSTSremoved_dec22.RDS')
	models<-c(bs,lds,mds,hds,sar,urb,deep,cmg)

	summary<-modelsummary(models,coef_rename=c('tidephaseL'='Tide State','prop_brs'='Habitat Type','prop_ldsg'='Habitat Type','prop_medsg'='Habitat Type','prop_hdsg'='Habitat Type','prop_sarg'='Habitat Type','prop_urb_r'='Habitat Type','prop_deep'='Habitat Type','dist_cmg'='Habitat Type','prop_brstidephaseL'='Habitat-Tide Interaction','prop_ldsgtidephaseL'='Habitat-Tide Interaction','prop_medsgtidephaseL'='Habitat-Tide Interaction','prop_hdsgtidephaseL'='Habitat-Tide Interaction','prop_sargtidephaseL'='Habitat-Tide Interaction','prop_urb_rtidephaseL'='Habitat-Tide Interaction','prop_deeptidephaseL'='Habitat-Tide Interaction','dist_cmgtidephaseL'='Habitat-Tide Interaction','SD (prop_brs PIT)'='SD (Habitat Individual)','Cor (Intercept~prop_brs PIT)'='Cor (Intercept~Habitat Individual)','SD (prop_ldsg PIT)'='SD (Habitat Individual)','Cor (Intercept~prop_ldsg PIT)'='Cor (Intercept~Habitat Individual)','SD (prop_medsg PIT)'='SD (Habitat Individual)','Cor (Intercept~prop_medsg PIT)'='Cor (Intercept~Habitat Individual)','SD (prop_hdsg PIT)'='SD (Habitat Individual)','Cor (Intercept~prop_hdsg PIT)'='Cor (Intercept~Habitat Individual)','SD (prop_sarg PIT)'='SD (Habitat Individual)','Cor (Intercept~prop_sarg PIT)'='Cor (Intercept~Habitat Individual)','SD (prop_urb_r PIT)'='SD (Habitat Individual)','Cor (Intercept~prop_urb_r PIT)'='Cor (Intercept~Habitat Individual)','SD (prop_deep PIT)'='SD (Habitat Individual)','Cor (Intercept~prop_deep PIT)'='Cor (Intercept~Habitat Individual)','SD (dist_cmg PIT)'='SD (Habitat Individual)','Cor (Intercept~dist_cmg PIT)'='Cor (Intercept~Habitat Individual)','SD (Intercept PIT)' = 'SD (Group Intercept)'), fmt=3,estimate='estimate', statistic='conf.int',stars=FALSE,conf_level=0.95,output='flextable')

	summary_flex<-summary%>%theme_alafoli()%>%align(align = 'center', part = 'all')%>%font(fontname = 'Times', part = 'all')%>%fontsize(size = 11, part = 'all')%>%autofit()
	save_as_image(summary_flex,'summary_glmms_flextable_ghostsfiltered_2019to2020_noMtide_pcl60to100.png',webshot='webshot2')


	aic.seagrasses<-AIC(lds,mds,hds)%>%add_column(Density=c('Low','Medium','High'),.before='df') # AIC confirms 

	aics<-flextable(aic.seagrasses)%>%theme_alafoli()%>%align(align = 'center', part = 'all')%>%font(fontname = 'Times', part = 'all')%>%fontsize(size = 11, part = 'all')%>%autofit()
	save_as_image(aics,'aic_seagrass_models_dec22_glmers.png',webshot='webshot2',dpi=600)


################
## Prediction shapefile and dataframe
################
# the following section rely heavily upon this shapefile. It contains predictions calculated in the R code file 'evaluate_and_predict_KresslerTrevailetal_inprep.R'

# as a shapefile 
setwd('/Users/mollykressler/Documents/data_phd/habitat_model/no ghosts of 6hr threshold')
pred.hex2<-st_as_sf(st_read('habitatmodel_preds_method5_withANDwithoutREFUGE_ghostsremoved_glmers_dec22_updatedhabitat.shp'))%>%rename(AFTERuse=AFTERus,AIC_m5_use=AIC_m5_)


######## 
## - Habitat Selection, model averaged probability of 'use'
	# in the ms we are showing only the final result of low tide. 
		preds_at_low<-pred.hex2%>%filter(tidephs=='L')

		lowbox<-st_as_sf(st_make_valid((st_boundary(st_union(preds_at_low)))))
		ggplot()+geom_sf(data=lowbox)
	# plot it (method 5)

		method5.glmers<-ggplot()+geom_sf(data=lowbox,alpha=0,col='#FFFFFF')+geom_sf(data=pred.hex2,aes(fill=AIC_m5_use),col=NA)+scale_fill_distiller(palette='RdPu',direction=1,guide=guide_colourbar(title='Probability of Use'))+facet_grid(cols=vars(tidephs))+theme_minimal()+geom_sf(data=land[3,],fill='gray98')+geom_sf(data=land[4,],fill='gray98')+geom_sf(data=land[2,],fill='gray98')+geom_sf(data=land[1,],fill='gray98')+theme_bw()
		for_pub_lowonly<-ggplot()+geom_sf(data=pred.hex2,aes(fill=AIC_m5_use),col=NA)+scale_fill_distiller(palette='RdPu',direction=1,guide=guide_colourbar(title='Probability of Use'))+geom_sf(data=land[3,],fill='gray98')+geom_sf(data=land[4,],fill='gray98')+geom_sf(data=land[2,],fill='gray98')+geom_sf(data=land[1,],fill='gray98')+theme_bw()



######## 
## - Updating Habitat Selection, accounting for strong selection for refugia

	# outlines/boundaries data file

	outlines50all<-st_as_sf(st_read('outlines_of_use50s_beforeANDafter_updating_habselction_good4plotting_ghostsremoved_dec22.shp'))

	# Supplementary Figure: facetted predictons post-updating selection model averaging 
		AFTER50<-st_as_sf(st_read('preds_AFTER50_in_BEFORE50cureusearea_ghostsremoved_noMtide_april23.shp'),crs='WGS84')
		selection.in.use50<-ggplot()+geom_sf(data=AFTER50,aes(fill=AFTERuse),col=NA)+scale_fill_distiller(palette='RdPu',direction=1,limits=c(0,1),guide=guide_colourbar(title='Probability of Use'))+facet_grid(cols=vars(tidephs),labeller=(labels))+theme_bw()+geom_sf(data=land,fill='gray98') 
		selection.in.use50.LOWONLY<-ggplot()+geom_sf(data=AFTER50%>%filter(tidephs=='L'),aes(fill=AFTERuse),col=NA)+scale_fill_distiller(palette='RdPu',direction=1,limits=c(0,1),guide=guide_colourbar(title='Probability of Use'))+theme_bw()+geom_sf(data=land,fill='gray98')+theme(axis.text.y = element_blank(),axis.ticks.y = element_blank())
		#ggsave(selection.in.use50.LOWONLY,file='updatedselection_ghostsremoved_LOWtideonly_mar23_formultipanel.png',device='png',units='mm',dpi=1200,height=150,width=250)	
	

	# supplementary figure: outlines pre/post updating at High and Low tide facetted. 
		extent.50use.beforeANDafterUPDATING.noPreds<-ggplot()+geom_sf(data=land,col='grey72',fill='grey82')+geom_sf(data=outlines50all,aes(linetype=upd,col=upd),lwd=1.2)+scale_linetype(labels=c('after','before'),guide=guide_legend(title='Updating'))+scale_color_manual(labels=c('after','before'),values=c('goldenrod2','navyblue'),guide=guide_legend(title='Updating'))+facet_grid(cols=vars(tidephase),labeller=(labels))+theme_bw() ## separate for high and low tide, good for supplementary materials. 
		
		#ggsave(extent.50use.beforeANDafterUPDATING.noPreds,file='outlines_habsel_withinUSE50_beforeANDafterUpdating_LOWandHIGHtide_ghostsremoved_dec22.png',device='png',units='mm',dpi=1200,height=150,width=250)	
 
	# results figure: pre/ppst updated minimum convex hull outline of area where selection was greater than or equal to 50%. Data shown here is for the low tide results, as this was the most conservative. 
		lowoutlines50<-outlines50all%>%filter(tidephase=='L')
		simplified.extent.50use.beforeANDafterUPDATING.noPreds<-ggplot()+geom_sf(data=land,col='grey72',fill='grey82')+geom_sf(data=lowoutlines50,aes(linetype=upd,col=upd),lwd=1.5)+scale_linetype(labels=c('after','before'),guide=guide_legend(title='Updating'))+scale_color_manual(labels=c('after','before'),values=c('goldenrod2','navyblue'),guide=guide_legend(title='Updating'))+theme_bw() ## for in text results, final figure.
		#ggsave(simplified.extent.50use.beforeANDafterUPDATING.noPreds,file='outlines_habsel_withinUSE50_beforeANDafterUpdating_onlyLOWtide_ghostsremoved_dec22.png',device='png',units='mm',dpi=1200,height=150,width=250)	


######## Figure 2 Multi-panel predictions and MPA areas
## - updated based on feedback on layout from co-A Vital Heim

	# zoom out B (preds in use50 low only) and C (before and after updating boundaries) panels in the multi-panel figure to match the zoom of A (methd5 preds) - April 2023 

		lowbox<-st_as_sf(st_make_valid((st_boundary(st_union(preds_at_low)))))
		ggplot()+geom_sf(data=lowbox)

	# plot each one with lowbox as the first layer at alpha = 0 
		# panel A - selection across whole hab
		for_pub_lowonly<-ggplot()+geom_sf(data=lowbox,alpha=0,col='#FFFFFF')+
			geom_sf(data=preds_at_low,aes(fill=AIC_m5_use),col=NA)+
			scale_fill_distiller(palette='RdPu',direction=1,guide=guide_colourbar(title='Probability of Use'))+
			geom_sf(data=land,col='grey72',fill='grey82')+
			theme_bw()

		# panel B - selection in (before) use50 
		selection.in.use50.LOWONLY<-ggplot()+
			geom_sf(data=lowbox,alpha=0,col='#FFFFFF')+
			geom_sf(data=AFTER50%>%filter(tidephs=='L'),aes(fill=AFTERuse),col=NA)+
			geom_sf(data=land,col='grey72',fill='grey82')+
			scale_fill_distiller(palette='RdPu',direction=1,limits=c(0,1),guide=guide_colourbar(title='Probability of Use'))+
			theme_bw()+
			theme(legend.position='bottom')

		# panel C - selection in (before) use50 
		lowoutlines50<-outlines50all%>%filter(tidephase=='L')
		simplified.extent.50use.beforeANDafterUPDATING.noPreds<-ggplot()+
			geom_sf(data=lowbox,alpha=0,col='#FFFFFF')+
			geom_sf(data=land,col='grey72',fill='grey82')+
			geom_sf(data=lowoutlines50,aes(linetype=upd,col=upd),lwd=1.5)+
			scale_linetype(labels=c('after','before'),guide=guide_legend(title='Updating'))+
			scale_color_manual(labels=c('after','before'),values=c('goldenrod2','navyblue'),guide=guide_legend(title='Updating'))+
			theme_bw() ## for in text results, final figure.

		## line them up - (1) two rows, (2) one column
		# (1)
			a1<-for_pub_lowonly+
			theme(axis.text.x=element_blank(),axis.ticks.x = element_blank(),legend.position='none')
			b1<-selection.in.use50.LOWONLY+
			theme(axis.text.y = element_blank(),axis.ticks.y = element_blank(),legend.position='none')
			c1<-simplified.extent.50use.beforeANDafterUPDATING.noPreds+
			theme(legend.position='none')

			legenda<-cowplot::get_legend(a1)
			legendb<-cowplot::get_legend(selection.in.use50.LOWONLY)
			legendc<-cowplot::get_legend(simplified.extent.50use.beforeANDafterUPDATING.noPreds)
			legends<-cowplot::plot_grid(legendb,legendc,align='c',nrow=2)

			panel.figure2<-a1 /c1 | {(b1 / legends)}  + plot_layout(ncol=1,nrow=2, width=c(1),height=c(1,1))& theme(legend.position='none') ## I DID IT
		ggsave(panel.figure2,file='full A4page_panels_results_habselectionmodels_sharks_bimini_april23.png',device='png',units='mm',dpi=1800,height=200,width=180)	



		# (2) change theme settings to: theme(axis.text.x = element_blank(),axis.ticks.x = element_blank()) 
			a2<-for_pub_lowonly+
			theme(axis.text.x=element_blank(),axis.ticks.x = element_blank(),legend.position='none')
			b2<-selection.in.use50.LOWONLY+
			theme(axis.text.x = element_blank(),axis.ticks.x = element_blank(),legend.position='none')
			c2<-simplified.extent.50use.beforeANDafterUPDATING.noPreds+
			theme(legend.position='none')

			stacked_panels<-a2/b2/c2+legends

			# VH would prefer the Pr(use) legend underneath the first two, and the Updatign legend after the third. Going to try to reconfigure plot (2).
				# legenda = Pr(use)
				# legendc = Updating 
			stacked_panels2<-a2/b2/legendb/c2/legendc + plot_layout(heights=c(4,4,1,4,1)) 

		ggsave(stacked_panels2,file='stacked_panels_results_habselectionmodels_sharks_bimini_april23.png',device='png',units='mm',dpi=1200,height=500,width=125)	
 

######## 
## - AIC weights tables, before and after updating for the 6 then 5 models
	pacman::p_load(tidyverse,modelsummary,flextable)
	setwd('/Users/mollykressler/Documents/data_phd/habitat_model/no ghosts of 6hr threshold')	
	cmg<-readRDS('distance2centralmangrove_glmer_habmodel_use_w7_GHOSTSremoved_dec22.RDS')
	bs<-readRDS('baresand_glmer_habmodel_use_w7_GHOSTSremoved_dec22.RDS')
	mds<-readRDS('mediumdensityseagrass_glmer_habmodel_use_w7_GHOSTSremoved_dec22.RDS')
	lds<-readRDS('lowdensityseagrass_glmer_habmodel_use_w7_GHOSTSremoved_dec22.RDS')
	hds<-readRDS('highdensityseagrass_glmer_habmodel_use_w7_GHOSTSremoved_dec22.RDS')
	sar<-readRDS('sargassum_glmer_habmodel_use_w7_GHOSTSremoved_dec22.RDS')
	urb<-readRDS('urbanandrockyoutcrops_glmer_habmodel_use_w7_GHOSTSremoved_dec22.RDS')
	deep<-readRDS('deepwater_glmer_habmodel_use_w7_GHOSTSremoved_dec22.RDS')

	models<-c(bs,mds,sar,urb,deep,cmg)

	aicw<-read.csv('aic_weights_ghostsremoved_2019_2020_glmers_habitatmodels_dec22.csv')
	upd<-read.csv('aic_weights_withOUTrefugeCMG_ghostsremoved_2019_2020_glmers_habitatmodels_dec22.csv')
	df1<-aicw[,c(8,7)]%>%add_column(Updating='before')
	df2<-upd[,c(8,7)]%>%add_column(Updating='after')
	df<-rbind(df1,df2)%>%rename('Model'='model.name','Aic Weight'='AICc.Weights')

	df4<-df%>%pivot_wider(names_from='Updating',values_from='Aic Weight')
	# rename models to match manuscript formatting. refuge = 8, meddensg=3, bare ssand =1, sargassum = 5, urban = 6, deep water = 7, 
	df5<-df4%>%mutate(Model=case_when(Model=='refuge'~8,Model=='medensg'~3,Model=='baresand'~1,Model=='sargassum'~5,Model=='urban'~6,Model=='deepwater'~7,))%>%rename(Before=before,After=after)
	df5$Model<-as.character(df5$Model)
	aics.updating<-flextable(df5)%>%theme_alafoli()
	%>%align(align = 'center', part = 'all')%>%
	font(fontname = 'Times', part = 'all')%>%
	fontsize(size = 11, part = 'all')%>%colformat_double(digits = 8) %>%autofit()
	save_as_image(aics.updating,'aic__models_dec22_glmers_beforeAfterUpdating.png',webshot='webshot2')







######## MAYBE LEAVE OUT....
## - Individual spatial behaviour, dBBMMs and habitat composition averages   