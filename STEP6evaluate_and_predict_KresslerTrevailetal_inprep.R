## Evaluating and Predicting, Code 
#### Manuscript: 'Habitat or safety? Drivers and management implications of juvenile lemon shark space use in mangrove nursery', Kressler, Trevail, et al. (in prep)

# unless otherwise noted, code written by Molly M Kressler

##################
## Prepare Workspace
##################

## Load packages 

pacman::p_load(tidyverse,ggplot2,gridExtra,lubridate,sf,ggeffects,arm)

## Load Data 

# detections
	detts<-read.csv('detections20192020__NOghosts_withUPDATEDhabitat_sept22.csv')%>%add_column(use=1,.before='pcl')%>%filter(tidephs!='M')%>%rename(tidephase=tidephs)
		# filter intermediate tidephase, add column that tells model its a real detection 
		detts$time<-as.Date(detts$time)
		detts<-detts[,c(1:8,10,17:32)] # simplify data frame to only what you need

# pseudo-detections
	psd<-read.csv('pseudos_GHOSTSremoved_n7_20192020_withUPDATEDhabitat_habmodel_dec22.csv')%>%filter(tidephase!='M')
	# filter pseudo-detectiosn drawm at intermediate tidephase
		psd$time<-as.Date(psd$time)

## dataframe with detections and pseudos 
	all<-bind_rows(detts,psd)
		{
		all<-all%>%dplyr::mutate(prop_unkwn=ifelse(is.na(prop_unkwn),0,prop_unkwn)) # double checkign no NAs in this variable
		all$time<-as.Date(all$time)
		all$PIT<-as_factor(all$PIT)
		all$sex<-as_factor(all$sex)
		all$tidephase<-as_factor(all$tidephase)
		all$month<-as_factor(all$month)
		all$dorn<-as_factor(all$dorn)
		all$use<-as_factor(all$use)
		} # combining pseudos and real detections with habitat into one dateframe for modelling. psd vs real identifiable via 'use' (0/1).

# hexagon grid across study site 
	hex.grid<-st_as_sf(st_read('hexagon_grid_withUPDATEDhabitat_sept22.shp'),crs='WGS84')

# land shape 
	land<-st_as_sf(st_union(st_read('bim_onlyland_noDots.kml')),crs='WGS84')

# Model RDS outputs 
	cmg<-readRDS('distance2centralmangrove_glmer_habmodel_use_w7_GHOSTSremoved_dec22.RDS')
	bs<-readRDS('baresand_glmer_habmodel_use_w7_GHOSTSremoved_dec22.RDS')
	mds<-readRDS('mediumdensityseagrass_glmer_habmodel_use_w7_GHOSTSremoved_dec22.RDS')
	sar<-readRDS('sargassum_glmer_habmodel_use_w7_GHOSTSremoved_dec22.RDS')
	urb<-readRDS('urbanandrockyoutcrops_glmer_habmodel_use_w7_GHOSTSremoved_dec22.RDS')
	deep<-readRDS('deepwater_glmer_habmodel_use_w7_GHOSTSremoved_dec22.RDS')

##################
## Marginal Effects Plots
##################

## Individual Plots 
	# for each of the predictor variables, use ggpredict with type = 'random' to produce marginal effects plots which take the random effect into account. For each model... 
		# 1st: predict such that individual marginal effects are calculated into a dataframe using the model RDS . 
		# 2nd: predict but only for the population level effect. 
		# 3rd: identify the maximum value of the predictor variable observed in the study system, we don't want to visualise predictions beyond this maximum. 
		# 4th: plot the individual effects in grey and the population in black
		# repeat steps 1-4 for each predictor variable
		# then arrange all six plots and save

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


	# Sargassum 
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


##################
## Predicting into the Study Area Hexagon Grid
##################

## Aim: Produce heatmaps of the Probability of Use across the study site hexagon grid. It will take three steps:
	# 1. Draw estimates across the hexagon grid for each GLMM
	# 2. (2.1) Calculate the AIC weights for each GLMM (before updating) and (2.2) model average using Probability of use estimates adjusted as per the AIC weights table. (2.3) Plot the estimates
	# 3. (3.1) Calculate the AIC weights for each GLMM excluding the 'cmg'/refuge model and calculate the model averaged probability of use ('after' updating). (3.2) Plot updated estimates into Core use area from 2.2. (3.3) Caluclate the new core use area boundary (after updating), and make several plots illustrating the updated/secondary selection, and the before and after boundaries of core use. 

## 1. Draw estimates across the hexagon grid for each GLMM

	# add tdephase to hex.grid, add column tidephase then set equal to L, do the same for H on a duplicate grid set and bind rows. 
	hex.grid.L<-hex.grid%>%add_column(tidephase='L')
	hex.grid.H<-hex.grid%>%add_column(tidephase='H')
	hex.grid<-bind_rows(hex.grid.L,hex.grid.H)
	hex.grid$tidephase<-as_factor(hex.grid$tidephase)

	# cut land out 
	hex.grid<-st_difference(hex.grid,land)
	ggplot()+geom_sf(data=hex.grid,alpha=0.2,fill='cadetblue2')+theme_minimal()

	# Predict un-adjusted probability of use estimates into the hex.grid, making pred.hex.
	pred.hex<-hex.grid%>%add_column(bs.use=invlogit(predict(bs,.,re.form=NA)),.before='geometry')%>%add_column(mds.use=invlogit(predict(mds,hex.grid,re.form=NA)),.before='geometry')%>%add_column(sar.use=invlogit(predict(sar,hex.grid,re.form=NA)),.before='geometry')%>%add_column(urb.use=invlogit(predict(urb,hex.grid,re.form=NA)),.before='geometry')%>%add_column(deep.use=invlogit(predict(deep,hex.grid,re.form=NA)),.before='geometry')%>%add_column(cmg.use=invlogit(predict(cmg,hex.grid,re.form=NA)),.before='geometry')

	# plots per glmm
		bs.plot<-ggplot()+geom_sf(data=pred.hex,aes(fill=bs.use),col=NA)+scale_fill_distiller(palette='RdPu',direction=1,limits=c(0,1),guide=guide_colourbar(title='Probability of Use'))+geom_sf(data=land,col='grey72',fill='grey82')+facet_grid(cols=vars(tidephase))+theme_void()
		mds.plot<-ggplot()+geom_sf(data=pred.hex,aes(fill=mds.use),col=NA)+scale_fill_distiller(palette='RdPu',direction=1,limits=c(0,1),guide=guide_colourbar(title='Probability of Use'))+geom_sf(data=land,col='grey72',fill='grey82')+facet_grid(cols=vars(tidephase))+theme_void()
		sar.plot<-ggplot()+geom_sf(data=pred.hex,aes(fill=sar.use),col=NA)+scale_fill_distiller(palette='RdPu',direction=1,limits=c(0,1),guide=guide_colourbar(title='Probability of Use'))+geom_sf(data=land,col='grey72',fill='grey82')+facet_grid(cols=vars(tidephase))+theme_void()
		urb.plot<-ggplot()+geom_sf(data=pred.hex,aes(fill=urb.use),col=NA)+scale_fill_distiller(palette='RdPu',direction=1,limits=c(0,1),guide=guide_colourbar(title='Probability of Use'))+geom_sf(data=land,col='grey72',fill='grey82')+facet_grid(cols=vars(tidephase))+theme_void()
		deep.plot<-ggplot()+geom_sf(data=pred.hex,aes(fill=deep.use),col=NA)+scale_fill_distiller(palette='RdPu',direction=1,limits=c(0,1),guide=guide_colourbar(title='Probability of Use'))+geom_sf(data=land,col='grey72',fill='grey82')+facet_grid(cols=vars(tidephase))+theme_void()
		cmg.plot<-ggplot()+geom_sf(data=pred.hex,aes(fill=cmg.use),col=NA)+scale_fill_distiller(palette='RdPu',direction=1,limits=c(0,1),guide=guide_colourbar(title='Probability of Use'))+geom_sf(data=land,col='grey72',fill='grey82')+facet_grid(cols=vars(tidephase))+theme_void()

# 2. 
## (2.1) Calculate the AIC weights for each GLMM (before updating) - code provided by Richard B Sherley and modified by Molly M Kressler 
	N<-1000
	M<-6
	AicM<-matrix(rep(0.0,(M*7)),nrow=M,dimnames=list(c(),c("Model.AIC","K","AICc","Delta.AICc","NULL","AICc.Weights","Model.Number"))) #matrix for AIC weighting (a table essentially)
		#fill in the AicM with each row for one model and the first column = deviance(model) and the second column the degrees of freedom
		AicM[1,1]<-deviance(bs)*1/10000
		AicM[1,2]<-7
		AicM[2,1]<-deviance(mds)*1/10000
		AicM[2,2]<-7
		AicM[3,1]<-deviance(sar)*1/10000
		AicM[3,2]<-7
		AicM[4,1]<-deviance(urb)*1/10000
		AicM[4,2]<-7
		AicM[5,1]<-deviance(deep)*1/10000
		AicM[5,2]<-7
		AicM[6,1]<-deviance(cmg)*1/10000
		AicM[6,2]<-7
		i <- 1
		while(i > 0 & i < (length(AicM[,1])+1)){
		AicM[i,3] <- (AicM[i,1]+(((2*AicM[i,2])*(AicM[i,2]+1))/(N-AicM[i,2]-1)))
		i <- i + 1
		}
		lowest <- min(AicM[,3])
		i <- 1
		while(i > 0 & i < (length(AicM[,1])+1)){
		AicM[i,4] <- AicM[i,3] - lowest
		AicM[i,5] <- exp(-0.5*AicM[i,4])
		i <- i + 1
		}
		i <- 1
		while(i > 0 & i < (length(AicM[,1])+1)){
		AicM[i,6] <- AicM[i,5]/sum(AicM[,5])
		AicM[i,7] <- i
		i <- i + 1
		}
		AicM <- as.data.frame(AicM)
		attach(AicM)
		AicFinal <- cbind(Model.Number,Model.AIC,K,AICc,Delta.AICc,AICc.Weights)
		AicFinal <- as.data.frame(AicFinal)
		AicFinal <- AicFinal[with(AicFinal, order(-AICc.Weights)), ]
		AicFinal

		## add model name to datafile. THIS WILL CHANGE IF MODELS OR DATA CHANGE
			AicFinal$model.name<-c('refuge','medensg','deepwater','baresand','sargassum','urban')

		## save 
			write.csv(AicFinal,'aic_weights_ghostsremoved_2019_2020_glmers_habitatmodels_dec22.csv')
			AicFinal.with<-read.csv('aic_weights_ghostsremoved_2019_2020_glmers_habitatmodels_dec22.csv')%>%dplyr::select(-X)
## (2.2) model average using Probability of use estimates adjusted as per the AIC weights table. 

	## add aic weights to hex data frame
		pred.hex$AIC.w.bs<-AicFinal.with[4,6]
		pred.hex$AIC.w.mds<-AicFinal.with[2,6]
		pred.hex$AIC.w.sar<-AicFinal.with[5,6]
		pred.hex$AIC.w.urb<-AicFinal.with[6,6]
		pred.hex$AIC.w.deep<-AicFinal.with[3,6]
		pred.hex$AIC.w.cmg<-AicFinal.with[1,6]


  	# calculate weighted probability of use estimates 

		pred.hex<-pred.hex%>%mutate(adj.bs.use=bs.use*AIC.w.bs,adj.mds.use=mds.use*AIC.w.mds,adj.sar.use=sar.use*AIC.w.sar,adj.urb.use=urb.use*AIC.w.urb,adj.deep.use=deep.use*AIC.w.deep,adj.cmg.use=cmg.use*AIC.w.cmg,.before='geometry')%>%mutate(AIC.m5.use=(adj.bs.use+adj.mds.use+adj.sar.use+adj.urb.use+adj.deep.use+adj.cmg.use),.before='geometry')

		# for simplicity in the ms we are showing only the final result of low tide. 
		preds_at_low<-pred.hex%>%filter(tidephase=='L')


## (2.3) Plot the estimates

	method5.glmers<-ggplot()+geom_sf(data=pred.hex,aes(fill=AIC.m5.use),col=NA)+scale_fill_distiller(palette='RdPu',direction=1,guide=guide_colourbar(title='Probability of Use'))+facet_grid(cols=vars(tidephase))+theme_minimal()+geom_sf(data=land[3,],fill='gray98')+geom_sf(data=land[4,],fill='gray98')+geom_sf(data=land[2,],fill='gray98')+geom_sf(data=land[1,],fill='gray98')+theme_bw()
	for_pub_lowonly<-ggplot()+geom_sf(data=preds_at_low,aes(fill=AIC.m5.use),col=NA)+scale_fill_distiller(palette='RdPu',direction=1,guide=guide_colourbar(title='Probability of Use'))+geom_sf(data=land[3,],fill='gray98')+geom_sf(data=land[4,],fill='gray98')+geom_sf(data=land[2,],fill='gray98')+geom_sf(data=land[1,],fill='gray98')+theme_bw()

		ggsave(file='method5_bothtides_predictions_updatedhabitat_GHOSTSremoved_glmers_dec22.pdf',method5.glmers,device='pdf',units='mm',width=250,height=150,dpi=500)

# 3. Updating Habitat Selection, acounting for strong selection for refugia 
##(3.1) Calculate the AIC weights for each GLMM excluding the 'cmg'/refuge model and calculate the model averaged probability of use ('after' updating) - code provided by Richard B Sherley and modified by Molly M Kressler  

	N<-1000
	M<-5
	AicM<-matrix(rep(0.0,(M*7)),nrow=M,dimnames=list(c(),c("Model.AIC","K","AICc","Delta.AICc","NULL","AICc.Weights","Model.Number"))) #matrix for AIC weighting (a table essentially)
		#fill in the AicM with each row for one model and the first column = deviance(model) and the second column the degrees of freedom
		AicM[1,1]<-deviance(bs)*1/10000
		AicM[1,2]<-7
		AicM[2,1]<-deviance(mds)*1/10000
		AicM[2,2]<-7
		AicM[3,1]<-deviance(sar)*1/10000
		AicM[3,2]<-7
		AicM[4,1]<-deviance(urb)*1/10000
		AicM[4,2]<-7
		AicM[5,1]<-deviance(deep)*1/10000
		AicM[5,2]<-7

		i <- 1
		while(i > 0 & i < (length(AicM[,1])+1)){
		AicM[i,3] <- (AicM[i,1]+(((2*AicM[i,2])*(AicM[i,2]+1))/(N-AicM[i,2]-1)))
		i <- i + 1
		}
		lowest <- min(AicM[,3])
		i <- 1
		while(i > 0 & i < (length(AicM[,1])+1)){
		AicM[i,4] <- AicM[i,3] - lowest
		AicM[i,5] <- exp(-0.5*AicM[i,4])
		i <- i + 1
		}
		i <- 1
		while(i > 0 & i < (length(AicM[,1])+1)){
		AicM[i,6] <- AicM[i,5]/sum(AicM[,5])
		AicM[i,7] <- i
		i <- i + 1
		}
		AicM <- as.data.frame(AicM)
		attach(AicM)
		AicFinal <- cbind(Model.Number,Model.AIC,K,AICc,Delta.AICc,AICc.Weights)
		AicFinal <- as.data.frame(AicFinal)
		AicFinal <- AicFinal[with(AicFinal, order(-AICc.Weights)), ]
		AicFinal

	## add model name to datafile. 
		AicFinal$model.name<-c('medensg','deepwater','baresand','sargassum','urban') 
		#write.csv(AicFinal,'aic_weights_withOUTrefugeCMG_ghostsremoved_2019_2020_glmers_habitatmodels_dec22.csv')
		aic.norefuge<-read.csv('aic_weights_withOUTrefugeCMG_ghostsremoved_2019_2020_glmers_habitatmodels_dec22.csv')%>%dplyr::select(-X)

		pred.hex$AICwoRbs<-aic.norefuge[3,6]
		pred.hex$AICwoRmds<-aic.norefuge[1,6]
		pred.hex$AICwoRsar<-aic.norefuge[4,6]
		pred.hex$AICwoRurb<-aic.norefuge[5,6]
		pred.hex$AICwoRdeep<-aic.norefuge[2,6]

		pred.hex<-pred.hex%>%mutate(updAIC.bs=aic.norefuge[3,6],updAIC.mds=aic.norefuge[1,6],updAIC.sar=aic.norefuge[4,6],updAIC.urb=aic.norefuge[5,6],updAIC.deep=aic.norefuge[2,6],.before='geometry')%>%mutate(AFTERuse=(bs.use*updAIC.bs)+(mds.use*updAIC.mds)+(sar.use*updAIC.sar)+(urb.use*updAIC.urb)+(deep.use*updAIC.deep),.before='geometry')

## (3.2) Plot updated estimates into Core use area from 2.2. 
	labels<-as_labeller(c('H'='High Tide','L'='Low Tide'))
	
	BEFORE50<-pred.hex2%>%filter(AIC_m5_use>=0.50)
		beforedata<-BEFORE50%>%rename(tidephase=tidephs)%>%dplyr::select(jcode,tidephase,AIC_m5_use,geometry)
		BEFOREdata.low<-beforedata%>%filter(tidephase=='L')
		BEFOREdata.high<-beforedata%>%filter(tidephase!='H')
		BEFORE_low_outline<-st_as_sf(st_boundary(st_convex_hull(st_union(BEFOREdata.low))))%>%add_column(tidephase='L')
		BEFORE_high_outline<-st_as_sf(st_boundary(st_convex_hull(st_union(BEFOREdata.high))))%>%add_column(tidephase='H')
		BEFORE_outlines50<-bind_rows(BEFORE_low_outline,BEFORE_high_outline)%>%add_column(refuge='without')%>%add_column(updating='before')%>%add_column(upd='B')
	
	# cut the AFTERuse estimates to the use50 area (aka where AIC.m5.use>=50) and plot 
		AFTER.in.use50<-BEFORE50%>%filter(AIC_m5_use>=0.50)
		ggplot()+geom_sf(data=AFTER.in.use50)+theme_bw() # should be grey hexagons around the central mangrove forest 

	# save - these are the predictons after updatign in the before-core use area (50%)
		st_write(AFTER.in.use50,'preds_AFTER50_in_BEFORE50cureusearea_ghostsremoved_noMtide_april23.shp',driver='ESRI Shapefile')

## (3.3) Caluclate the new core use area boundary (after updating), and make several plots illustrating the updated/secondary selection, and the before and after boundaries of core use. 

	AFTERuse50<-AFTER.in.use50%>%filter(AFTERuse>=0.50)

	# define boundaries
		AFTERdata.low<-AFTERuse50%>%filter(tidephase=='L')
		AFTERdata.high<-AFTERuse50%>%filter(tidephase!='H')
		AFTER_low_outline<-st_as_sf(st_boundary(st_convex_hull(st_union(AFTERdata.low))))%>%add_column(tidephase='L')
		AFTER_high_outline<-st_as_sf(st_boundary(st_convex_hull(st_union(AFTERdata.high))))%>%add_column(tidephase='H')
	AFTER_outlines50<-bind_rows(AFTER_low_outline,AFTER_high_outline)%>%add_column(refuge='without')%>%add_column(updating='after')%>%add_column(upd='A')
	# bind rows and save 
	outlines50all<-bind_rows(BEFORE_outlines50,AFTER_outlines50)%>%rename(geometry=x)



	# Supplementary Figure: facetted predictons post-updating selection model averaging 
		selection.in.use50<-ggplot()+geom_sf(data=AFTERuse50,aes(fill=AFTERuse),col=NA)+scale_fill_distiller(palette='RdPu',direction=1,limits=c(0,1),guide=guide_colourbar(title='Probability of Use'))+facet_grid(cols=vars(tidephase),labeller=(labels))+theme_bw()+geom_sf(data=land,fill='gray98') 
		selection.in.use50.LOWONLY<-ggplot()+geom_sf(data=AFTERuse50%>%filter(tidephase=='L'),aes(fill=AFTERuse),col=NA)+scale_fill_distiller(palette='RdPu',direction=1,limits=c(0,1),guide=guide_colourbar(title='Probability of Use'))+theme_bw()+geom_sf(data=land,fill='gray98')+theme(axis.text.y = element_blank(),axis.ticks.y = element_blank())
		ggsave(selection.in.use50.LOWONLY,file='updatedselection_ghostsremoved_LOWtideonly_mar23_formultipanel.png',device='png',units='mm',dpi=1200,height=150,width=250)	
	

	# supplementary figure: outlines pre/post updating at High and Low tide facetted. 
		extent.50use.beforeANDafterUPDATING.noPreds<-ggplot()+geom_sf(data=land,col='grey72',fill='grey82')+geom_sf(data=outlines50all,aes(linetype=upd,col=upd),lwd=1.2)+scale_linetype(labels=c('after','before'),guide=guide_legend(title='Updating'))+scale_color_manual(labels=c('after','before'),values=c('goldenrod2','navyblue'),guide=guide_legend(title='Updating'))+facet_grid(cols=vars(tidephase),labeller=(labels))+theme_bw() ## separate for high and low tide, good for supplementary materials. 
		
		ggsave(extent.50use.beforeANDafterUPDATING.noPreds,file='outlines_habsel_withinUSE50_beforeANDafterUpdating_LOWandHIGHtide_ghostsremoved_dec22.png',device='png',units='mm',dpi=1200,height=150,width=250)	
 
	# results figure: pre/ppst updated minimum convex hull outline of area where selection was greater than or equal to 50%. Data shown here is for the low tide results, as this was the most conservative. 
		lowoutlines50<-outlines50all%>%filter(tidephase=='L')
		simplified.extent.50use.beforeANDafterUPDATING.noPreds<-ggplot()+geom_sf(data=land,col='grey72',fill='grey82')+geom_sf(data=lowoutlines50,aes(linetype=upd,col=upd),lwd=1.5)+scale_linetype(labels=c('after','before'),guide=guide_legend(title='Updating'))+scale_color_manual(labels=c('after','before'),values=c('goldenrod2','navyblue'),guide=guide_legend(title='Updating'))+theme_bw() ## for in text results, final figure.
		ggsave(simplified.extent.50use.beforeANDafterUPDATING.noPreds,file='outlines_habsel_withinUSE50_beforeANDafterUpdating_onlyLOWtide_ghostsremoved_dec22.png',device='png',units='mm',dpi=1200,height=150,width=250)	


## save the predictions dateframe/simple features file and that for the outlines
	st_write(pred.hex,'habitatmodel_preds_method5_withANDwithoutREFUGE_ghostsremoved_glmers_dec22_updatedhabitat.shp',append=FALSE)
	st_write(outlines50all,'outlines_of_use50s_beforeANDafter_updating_habselction_good4plotting_ghostsremoved_dec22.shp',append=FALSE)



##################
## Calculate the area of the core use area before and after updating
##################

	out<-outlines50.beforeandafter
pacman::p_load(units)
	polys<-st_cast(out,'POLYGON')%>%mutate(area.km=set_units(st_area(.),km^2),.before='geometry')


	# make it into a flex table. use 'polys', make a table of area with the tidephase and updatign column
	polys2<-dplyr::select(as.data.frame(polys),-geometry)
	polys2$area.km<-as.numeric(polys2$area.km)
	polys2$area.km2<-round(polys2$area.km,3)
	polys3<-polys2%>%rename('Tide State'= tidephase, Updating = updating, 'Area (km^2)  '= area.km2)
	ft.area.outlines<-flextable(polys3[,c(1,3,6)])%>%theme_alafoli()%>%align(align = 'center', part = 'all')%>%font(fontname = 'Times', part = 'all')%>%fontsize(size = 11, part = 'all')%>%colformat_double(digits=3)%>%autofit()
	#save_as_image(ft.area.outlines,path='habitat_model/no ghosts of 6hr threshold/outlines50_beforeandafterupdating_area.png',webshot='webshot')
	lowpolys<-polys2%>%filter(tidephase=='L')
	t.test(lowpolys$area.km) # t = 18.87, df = 1, p-value = 0.0337


out<-st_as_sf(st_read('habitat_model/no ghosts of 6hr threshold/outlines_of_use50s_beforeANDafter_updating_habselction_good4plotting_ghostsremoved_dec22.shp'))



















