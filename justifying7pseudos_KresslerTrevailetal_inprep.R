## Justufying 7 pseudo detections
#### Manuscript: 'Habitat or safety? Drivers and management implications of juvenile lemon shark space use in mangrove nursery', Kressler, Trevail, et al. (in prep)

# unless otherwise noted, code written by Molly M Kressler

pacman::p_load(sf,tidyverse,dplyr,ggplot2,gridExtra,lubridate,remotes,effects,lme4,modelsummary,performance,beepr,boot,pROC,caret,LMERConvenienceFunctions,MuMIn,bbmle,pathwork,ggpubr,arm)

setwd('/Users/mollykressler/Documents/data_phd/habitat_model/no ghosts of 6hr threshold')
detts<-read.csv('detections20192020__NOghosts_withUPDATEDhabitat_sept22.csv')%>%rename(jcode=FID)%>%add_column(use=1,.before='pcl')%>%dplyr::select(time,PIT,GPS_W,GPS_N,use,tidephs,jcode,dist_cmg)%>%mutate(w7=7,w3=3,w5=5,w9=9,w11=11,w13=13,w15=15) # real detections, with habitat
	detts$time<-as.Date(detts$time)
	detts<-detts%>%rename(tidephase=tidephs)
detts.sf<-st_as_sf(detts,coords=c('GPS_W','GPS_N'),crs='WGS84')

setwd('/Users/mollykressler/Documents/data_phd/habitat_model/no ghosts of 6hr threshold')
p7<-read.csv('pseudos_GHOSTSremoved_n7_20192020_withUPDATEDhabitat_habmodel_dec22.csv')%>%rename(area=grid_ar,dist_cmg=dist.cmg)
	p7$time<-as.Date(p7$time)

## cut more pseudo shapefiles 
	setwd('/Users/mollykressler/Documents/data_phd')
	land<-st_as_sf(st_read('bim_onlyland_noDots.kml'),crs='WGS84')	
	final.hab<-st_as_sf(st_read('hexagon_grid_withUPDATEDhabitat_sept22.shp'),crs='WGS84')
	hab.noland<-st_difference(st_make_valid(final.hab),st_union(land))
	available.forpseudos<-st_union(hab.noland)
	#st_write(available.forpseudos,'availablehabitat_updatedSept2022.kml')
	# 3
		empty3<-detts.sf%>%dplyr::select(time,PIT,tidephase)%>%add_column(use=0,.after='PIT')%>%dplyr::slice(rep(1:n(),each=3)) 
		p3<-st_as_sf(st_geometry(st_sample(available.forpseudos,size=(nrow(detts.sf)*3),type='random')))
		p3<-bind_cols(p3,empty3)
		p3<-st_join(p3,final.hab)
		p3<-p3%>%add_column(GPS_W=st_coordinates(.)[,1],.before='PIT')%>%add_column(GPS_N=st_coordinates(.)[,2],.after='GPS_W')
		p3<-p3%>%dplyr::select(time,PIT,use,tidephase,jcode,dist_cmg,GPS_W,GPS_N)%>%add_column(w7=7,w3=3,w5=5,w9=9,w11=11,w13=13,w15=15)
	# 5
		empty5<-detts.sf%>%dplyr::select(time,PIT,tidephase)%>%add_column(use=0,.after='PIT')%>%dplyr::slice(rep(1:n(),each=5)) 
		p5<-st_as_sf(st_geometry(st_sample(available.forpseudos,size=(nrow(detts.sf)*5),type='random')))
		p5<-bind_cols(p5,empty5)
		p5<-st_join(p5,final.hab)
		p5<-p5%>%add_column(GPS_W=st_coordinates(.)[,1],.before='PIT')%>%add_column(GPS_N=st_coordinates(.)[,2],.after='GPS_W')
		p5<-p5%>%dplyr::select(time,PIT,use,tidephase,jcode,dist_cmg,GPS_W,GPS_N)%>%add_column(w7=7,w3=3,w5=5,w9=9,w11=11,w13=13,w15=15)
	# 9
		empty9<-detts.sf%>%dplyr::select(time,PIT,tidephase)%>%add_column(use=0,.after='PIT')%>%dplyr::slice(rep(1:n(),each=9)) 
		p9<-st_as_sf(st_geometry(st_sample(available.forpseudos,size=(nrow(detts.sf)*9),type='random')))
		p9<-bind_cols(p9,empty9)
		p9<-st_join(p9,final.hab)
		p9<-p9%>%add_column(GPS_W=st_coordinates(.)[,1],.before='PIT')%>%add_column(GPS_N=st_coordinates(.)[,2],.after='GPS_W')
		p9<-p9%>%dplyr::select(time,PIT,use,tidephase,jcode,dist_cmg,GPS_W,GPS_N)%>%add_column(w7=7,w3=3,w5=5,w9=9,w11=11,w13=13,w15=15)

	# 11
		empty11<-detts.sf%>%dplyr::select(time,PIT,tidephase)%>%add_column(use=0,.after='PIT')%>%dplyr::slice(rep(1:n(),each=11)) 
		p11<-st_as_sf(st_geometry(st_sample(available.forpseudos,size=(nrow(detts.sf)*11),type='random')))
		p11<-bind_cols(p11,empty11)
		p11<-st_join(p11,final.hab)
		p11<-p11%>%add_column(GPS_W=st_coordinates(.)[,1],.before='PIT')%>%add_column(GPS_N=st_coordinates(.)[,2],.after='GPS_W')
		p11<-p11%>%dplyr::select(time,PIT,use,tidephase,jcode,dist_cmg,GPS_W,GPS_N)%>%add_column(w7=7,w3=3,w5=5,w9=9,w11=11,w13=13,w15=15)

	# 13
		empty13<-detts.sf%>%dplyr::select(time,PIT,tidephase)%>%add_column(use=0,.after='PIT')%>%dplyr::slice(rep(1:n(),each=13)) 
		p13<-st_as_sf(st_geometry(st_sample(available.forpseudos,size=(nrow(detts.sf)*13),type='random')))
		p13<-bind_cols(p13,empty13)
		p13<-st_join(p13,final.hab)
		p13<-p13%>%add_column(GPS_W=st_coordinates(.)[,1],.before='PIT')%>%add_column(GPS_N=st_coordinates(.)[,2],.after='GPS_W')
		p13<-p13%>%dplyr::select(time,PIT,use,tidephase,jcode,dist_cmg,GPS_W,GPS_N)%>%add_column(w7=7,w3=3,w5=5,w9=9,w11=11,w13=13,w15=15)

	# 15
		empty15<-detts.sf%>%dplyr::select(time,PIT,tidephase)%>%add_column(use=0,.after='PIT')%>%dplyr::slice(rep(1:n(),each=15)) 
		p15<-st_as_sf(st_geometry(st_sample(available.forpseudos,size=(nrow(detts.sf)*15),type='random')))
		p15<-bind_cols(p15,empty15)
		p15<-st_join(p15,final.hab)
		p15<-p15%>%add_column(GPS_W=st_coordinates(.)[,1],.before='PIT')%>%add_column(GPS_N=st_coordinates(.)[,2],.after='GPS_W')
		p15<-p15%>%dplyr::select(time,PIT,use,tidephase,jcode,dist_cmg,GPS_W,GPS_N)%>%add_column(w7=7,w3=3,w5=5,w9=9,w11=11,w13=13,w15=15)
	setwd('/Users/mollykressler/Documents/data_phd/habitat_model/validating_no_of_pseudos')
	st_write(p3,'psd3_20192020data_ghostsremoved_sept22habitat.csv',driver='CSV')
	st_write(p5,'psd5_20192020data_ghostsremoved_sept22habitat.csv',driver='CSV')
	st_write(p9,'psd9_20192020data_ghostsremoved_sept22habitat.csv',driver='CSV')
	st_write(p11,'psd11_20192020data_ghostsremoved_sept22habitat.csv',driver='CSV')
	st_write(p13,'psd13_20192020data_ghostsremoved_sept22habitat.csv',driver='CSV')
	st_write(p15,'psd15_20192020data_ghostsremoved_sept22habitat.csv',driver='CSV')

## make model dataframes

pa3<-read.csv('psd3_20192020data_ghostsremoved_sept22habitat.csv')
pa5<-read.csv('psd5_20192020data_ghostsremoved_sept22habitat.csv')
pa9<-read.csv('psd9_20192020data_ghostsremoved_sept22habitat.csv')
pa11<-read.csv('psd11_20192020data_ghostsremoved_sept22habitat.csv')
pa13<-read.csv('psd13_20192020data_ghostsremoved_sept22habitat.csv')
pa15<-read.csv('psd15_20192020data_ghostsremoved_sept22habitat.csv')

pa3$time<-as.Date(pa3$time)
pa5$time<-as.Date(pa5$time)
pa9$time<-as.Date(pa9$time)
pa11$time<-as.Date(pa11$time)
pa13$time<-as.Date(pa13$time)
pa15$time<-as.Date(pa15$time)

a3<-bind_rows(detts,pa3)
a5<-bind_rows(detts,pa5)
a9<-bind_rows(detts,pa9)
a11<-bind_rows(detts,pa11)
a13<-bind_rows(detts,pa13)
a15<-bind_rows(detts,pa15)

## run dist_cmg models for each dataset

m3<-glmer(use~dist_cmg+dist_cmg*tidephase+(dist_cmg|PIT),family=binomial(link=logit),weights=w3,data=a3)
m5<-glmer(use~dist_cmg+dist_cmg*tidephase+(dist_cmg|PIT),family=binomial(link=logit),weights=w5,data=a5)
m9<-glmer(use~dist_cmg+dist_cmg*tidephase+(dist_cmg|PIT),family=binomial(link=logit),weights=w9,data=a9) 
m11<-glmer(use~dist_cmg+dist_cmg*tidephase+(dist_cmg|PIT),family=binomial(link=logit),weights=w11,data=a11) 
m13<-glmer(use~dist_cmg+dist_cmg*tidephase+(dist_cmg|PIT),family=binomial(link=logit),weights=w13,data=a13)
m15<-glmer(use~dist_cmg+dist_cmg*tidephase+(dist_cmg|PIT),family=binomial(link=logit),weights=w15,data=a15)

setwd('/Users/mollykressler/Documents/data_phd/habitat_model/no ghosts of 6hr threshold')
m7<-readRDS('distance2centralmangrove_glmer_habmodel_use_w7_GHOSTSremoved_dec22.RDS')






