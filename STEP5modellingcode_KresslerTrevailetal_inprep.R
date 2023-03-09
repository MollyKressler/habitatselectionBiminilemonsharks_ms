## Modelling code
## Manuscript: 'Habitat or safety? Drivers and management implications of juvenile lemon shark space use in mangrove nursery', Kressler, Trevail, et al. (in prep)

# unless otherwise noted, code written by Molly M Kressler

##################
## Prepare Workspace
##################

## Load packages 

pacman::p_load(sf,tidyverse,dplyr,ggplot2,gridExtra,lubridate,remotes,effects,lme4,modelsummary,performance,beepr,boot,pROC,caret,LMERConvenienceFunctions,arm,flextable,webshot2)

## Load Data 

# detections
	vars<-c('day','date','lat','lon','sunrise','sunset','ghost','station','location','transmitter_id','acoustic_serial','download','buffID','buff_ar') # superfluous in the data frame. don't need so we'll select to remove them. 
	detts<-read.csv('testing/detections20192020__NOghosts_withUPDATEDhabitat_sept22.csv')%>%
		add_column(use=1,.before='pcl')%>%
		filter(Tidepeak_type!='M')%>%
		rename(tidephase=Tidepeak_type,dist_cmg=dist.cmg)%>%
		dplyr::select(-all_of(vars))
		# filter intermediate tidephase, add column that tells model its a real detection 
		detts$time<-as_datetime(detts$time)

# pseudo-detections
	psd<-read.csv('testing/pseudos_GHOSTSremoved_n7_20192020_withUPDATEDhabitat_habmodel_dec22.csv')%>%
	filter(tidephase!='M')%>%
	dplyr::select(-jcode,-grid_ar,-prop_spong)
	# filter pseudo-detectiosn drawm at intermediate tidephase
		psd$time<-as_datetime(psd$time)

stopifnot(names(psd)==names(detts)) # check 

# hexagon grid across study site 
	hexagon_grid<-st_as_sf(st_read('testing/hexagon_grid_wthUPDATEDhabitat_FORMODELLING_sept22.shp'),crs='WGS84')

##################
## Format dataframe for models
##################

## make model dataframe
	all<-bind_rows(detts,psd)
		{
		all<-all%>%dplyr::mutate(prop_unkwn=ifelse(is.na(prop_unkwn),0,prop_unkwn)) # double checkign no NAs in this variable
		all$time<-as.Date(all$time)
		all$PIT<-as_factor(all$PIT)
		all$sex<-as_factor(all$sex)
		all$tidephase<-as_factor(all$tidephase)
		all$dorn<-as_factor(all$dorn)
		all$use<-as_factor(all$use)
		} # combining pseudos and real detections with habitat into one dateframe for modelling. psd vs real identifiable via 'use' (0/1).

stopifnot(names(all)==names(detts)) # check 
stopifnot(nrow(all)==(nrow(psd)+nrow(detts))) # check 

## weighting pseudos versus reals 

	all$w7<-ifelse(all$use==1,7,1) # 7 because there are 7 pseudos to each real detection


	## save all data without M tide and with weighting column. 
	write.csv(all, 'testing/habmodel_data_noGHOSTS_all_w7_psd7_noMtide_sept22.csv')

##################
## Run Generalised Linear Mixed Models 
##################
## glmers 

	bs<-glmer(use~prop_brs+prop_brs*tidephase+(prop_brs|PIT),family=binomial(link=logit),weights=w7,data=all) 

	lds<-glmer(use~prop_ldsg+prop_ldsg*tidephase+(prop_ldsg|PIT),family=binomial(link=logit),weights=w7,data=all)
	
	mds<-glmer(use~prop_medsg+prop_medsg*tidephase+(prop_medsg|PIT),family=binomial(link=logit),weights=w7,data=all) 
	
	hds<-glmer(use~prop_hdsg+prop_hdsg*tidephase+(prop_hdsg|PIT),family=binomial(link=logit),weights=w7,data=all)
	
	sar<-glmer(use~prop_sarg+prop_sarg*tidephase+(prop_sarg|PIT),family=binomial(link=logit),weights=w7,data=all)
	
	urb<-glmer(use~prop_urb_r+prop_urb_r*tidephase+(prop_urb_r|PIT),family=binomial(link=logit),weights=w7,data=all)
	
	deep<-glmer(use~prop_deep+prop_deep*tidephase+(prop_deep|PIT),family=binomial(link=logit),weights=w7,data=all)
	
	dist.cmg<-glmer(use~dist_cmg+dist_cmg*tidephase+(dist_cmg|PIT),family=binomial(link=logit),weights=w7,data=all)

## save model objects

	saveRDS(dist.cmg,'testing/distance2centralmangrove_glmer_habmodel_use_w7_GHOSTSremoved_dec22.RDS')
	saveRDS(bs,'testing/baresand_glmer_habmodel_use_w7_GHOSTSremoved_dec22.RDS')
	saveRDS(mds,'testing/mediumdensityseagrass_glmer_habmodel_use_w7_GHOSTSremoved_dec22.RDS')
	saveRDS(lds,'testing/lowdensityseagrass_glmer_habmodel_use_w7_GHOSTSremoved_dec22.RDS')
	saveRDS(hds,'testing/highdensityseagrass_glmer_habmodel_use_w7_GHOSTSremoved_dec22.RDS')
	saveRDS(sar,'testing/sargassum_glmer_habmodel_use_w7_GHOSTSremoved_dec22.RDS')
	saveRDS(urb,'urbanandrockyoutcrops_glmer_habmodel_use_w7_GHOSTSremoved_dec22.RDS')
	saveRDS(deep,'testing/deepwater_glmer_habmodel_use_w7_GHOSTSremoved_dec22.RDS')

## summaries
	cmg<-readRDS('testing/distance2centralmangrove_glmer_habmodel_use_w7_GHOSTSremoved_dec22.RDS')
	bs<-readRDS('testing/baresand_glmer_habmodel_use_w7_GHOSTSremoved_dec22.RDS')
	mds<-readRDS('testing/mediumdensityseagrass_glmer_habmodel_use_w7_GHOSTSremoved_dec22.RDS')
	lds<-readRDS('testing/lowdensityseagrass_glmer_habmodel_use_w7_GHOSTSremoved_dec22.RDS')
	hds<-readRDS('testing/highdensityseagrass_glmer_habmodel_use_w7_GHOSTSremoved_dec22.RDS')
	sar<-readRDS('testing/sargassum_glmer_habmodel_use_w7_GHOSTSremoved_dec22.RDS')
	urb<-readRDS('urbanandrockyoutcrops_glmer_habmodel_use_w7_GHOSTSremoved_dec22.RDS')
	deep<-readRDS('testing/deepwater_glmer_habmodel_use_w7_GHOSTSremoved_dec22.RDS')
	models<-c(bs,lds,mds,hds,sar,urb,deep,cmg)

	summary<-modelsummary(models,coef_rename=c('tidephaseL'='Tide State','prop_brs'='Habitat Type','prop_ldsg'='Habitat Type','prop_medsg'='Habitat Type','prop_hdsg'='Habitat Type','prop_sarg'='Habitat Type','prop_urb_r'='Habitat Type','prop_deep'='Habitat Type','dist_cmg'='Habitat Type','prop_brstidephaseH'='Habitat-Tide Interaction','prop_ldsgtidephaseH'='Habitat-Tide Interaction','prop_medsgtidephaseH'='Habitat-Tide Interaction','prop_hdsgtidephaseH'='Habitat-Tide Interaction','prop_sargtidephaseH'='Habitat-Tide Interaction','prop_urb_rtidephaseH'='Habitat-Tide Interaction','prop_deeptidephaseH'='Habitat-Tide Interaction','dist_cmgtidephaseH'='Habitat-Tide Interaction','SD (prop_brs PIT)'='SD (Habitat Individual)','Cor (Intercept~prop_brs PIT)'='Cor (Intercept~Habitat Individual)','SD (prop_ldsg PIT)'='SD (Habitat Individual)','Cor (Intercept~prop_ldsg PIT)'='Cor (Intercept~Habitat Individual)','SD (prop_medsg PIT)'='SD (Habitat Individual)','Cor (Intercept~prop_medsg PIT)'='Cor (Intercept~Habitat Individual)','SD (prop_hdsg PIT)'='SD (Habitat Individual)','Cor (Intercept~prop_hdsg PIT)'='Cor (Intercept~Habitat Individual)','SD (prop_sarg PIT)'='SD (Habitat Individual)','Cor (Intercept~prop_sarg PIT)'='Cor (Intercept~Habitat Individual)','SD (prop_urb_r PIT)'='SD (Habitat Individual)','Cor (Intercept~prop_urb_r PIT)'='Cor (Intercept~Habitat Individual)','SD (prop_deep PIT)'='SD (Habitat Individual)','Cor (Intercept~prop_deep PIT)'='Cor (Intercept~Habitat Individual)','SD (dist_cmg PIT)'='SD (Habitat Individual)','Cor (Intercept~dist_cmg PIT)'='Cor (Intercept~Habitat Individual)','SD (Intercept PIT)' = 'SD (Group Intercept)'), fmt=3,estimate='estimate', statistic='conf.int',stars=FALSE,conf_level=0.95,output='flextable')

	summary_flex<-summary%>%theme_alafoli()%>%align(align = 'center', part = 'all')%>%font(fontname = 'Times', part = 'all')%>%fontsize(size = 11, part = 'all')%>%autofit()

	save_as_image(summary_flex, 'modelsummaries_glmers_ghostsfiltered_habselmodels_dec22.png', webshot = 'webshot2')


##################
## Investigate which density of seagrass to use
##################

## which seagrass - ROCs say medium

aic.seagrasses<-AIC(lds,mds,hds)%>%add_column(Density=c('Low','Medium','High'),.before='df') # AIC confirms 

aics<-flextable(aic.seagrasses)%>%theme_alafoli()%>%align(align = 'center', part = 'all')%>%font(fontname = 'Times', part = 'all')%>%fontsize(size = 11, part = 'all')%>%autofit()
save_as_image(aics,'testing/aic_seagrass_models_dec22_glmers.png',webshot='webshot2')





