### JUSTIFICATION for ghosts threshold 'n' hours
#### Manuscript: 'Habitat or safety? Drivers and management implications of juvenile lemon shark space use in mangrove nursery', Kressler, Trevail, et al. (in prep)

# unless otherwise noted, code written by Molly M Kressler

	
##################
## Prepare Workspace
##################

## Load Packages 

pacman::p_load(tidyverse,ggplot2,lubridate)

## Load Data 

	data<-read.csv('detections_2019to2020_pcl60to100_withMtide_cleaned_sept22.csv')
	data<-data%>%select(time,PIT,GPS_N,GPS_W,pcl,sex,Tidepeak_type,dorn,station,month)
	head(data,2)
	  {
	  data$time<-as_datetime(data$time,tz='EST')
	  data$PIT<-as_factor(data$PIT)
	  data$dorn<-as_factor(data$dorn)
	  data$month<-as_factor(data$month)
	  data$Tidepeak_type<-as_factor(data$Tidepeak_type)
	  data$sex<-as_factor(data$sex)     
	  }
	df_diagnostic <- data
	nrow(df_diagnostic)



##################
## Investigate the number of ghosts classified at different values for the threshold
##################

# the bulk/base of this code is provided by Alice Trevail. It identifies whether the difference in time between detections is greater than a threshold value, and then I've added some lines to calculate whether a detection is a 'ghost' or not. 

## examine the asymptote curve, x = threshold values ('n' hours) & y= number of ghosts classified 


# calculate the duration between detections, per individual

	# define maximum gap in data using time threshold
	n0.5=0.5*60*60
	n2=2*60*60
	n4=4*60*60
	n6=6*60*60
	n8=8*60*60
	n10=10*60*60
	n12=12*60*60
	n14=14*60*60
	n16=16*60*60
	n18=18*60*60
	n20=20*60*60

	
	threshold_time <- as.period(n0.5, unit = "secs") 
	units_df_datetime <- "secs"
	threshold_points <- 2
	options(tibble.width=Inf)
	df_diagnostic <-  df_diagnostic %>%group_by(PIT) %>%mutate(difftime = difftime(time, lag(time), units="secs")  )
	# convert the time threshold to the same units as difftime in df_diagnostic
	threshold_units <- as.numeric(as.period(threshold_time, units_df_datetime))

	df_segments <- df_diagnostic %>%
	  group_by(PIT) %>%
	  mutate(segment_start = case_when(row_number()==1 ~ TRUE,
	                                   difftime > threshold_units ~ TRUE),
	         segment_row_number = case_when(segment_start == TRUE ~ cur_group_rows())) %>%
	  fill(segment_row_number) %>%
	  ungroup() %>% group_split(PIT) %>%
	  purrr::map_df(~.x %>% group_by(segment_row_number) %>% mutate(segment_num = cur_group_id())) %>% 
	  ungroup() %>% select(-c(segment_row_number)) %>% 
	  mutate(SegmentID = paste0(PIT, "_", segment_num))

	#diagnostic tools
	summary(df_segments)
	head(df_segments,20)
	print(df_segments[490:600,c(1,2,11:12)],n=50)


## mark a detection as ghost if IT and its LEAD (the next) row == TRUE (based on segment.start col)

	df_segments2<-df_segments%>%group_by(PIT)%>%mutate(ghost=case_when((segment_start==TRUE & lag(segment_start)==TRUE) ~ TRUE,TRUE~FALSE))
	summary(df_segments2)
	head(df_segments2,20)

	ghosts_removed<-df_segments2%>%filter(ghost==FALSE)
	nrow(ghosts_removed)

## how many ghosts, if n=...? Repeat the above lines at increments of 2 from n=2 to n=20 
	### DO NOT RUN IN A BLOCK - didn't write a loop for this whole process (look we all have regrets), so you have to go through each value of n above through the corresponding line in ths section, before moving to making a dataframe. 
	
	ghosts_n0<-nrow(df_diagnostic) #n=0, aka no threshold, just the data
	ghosts_n0.5<-nrow(df_segments2%>%filter(ghost==TRUE)) #n=30 minutes (0.5 hr)
	ghosts_n2<-nrow(df_segments2%>%filter(ghost==TRUE)) #n=2 
	ghosts_n4<-nrow(df_segments2%>%filter(ghost==TRUE)) #n=4
	ghosts_n6<-nrow(df_segments2%>%filter(ghost==TRUE)) #n=6 
	ghosts_n8<-nrow(df_segments2%>%filter(ghost==TRUE)) #n=8 
	ghosts_n10<-nrow(df_segments2%>%filter(ghost==TRUE)) #n=10
	ghosts_n12<-nrow(df_segments2%>%filter(ghost==TRUE)) #n=12
	ghosts_n14<-nrow(df_segments2%>%filter(ghost==TRUE)) #n=14
	ghosts_n16<-nrow(df_segments2%>%filter(ghost==TRUE)) #n=16
	ghosts_n18<-nrow(df_segments2%>%filter(ghost==TRUE)) #n=18
	ghosts_n20<-nrow(df_segments2%>%filter(ghost==TRUE)) #n=20

### Did you see the wanring above? About not running it in a block? If yes, continue. If no, scroll back, 

## make a df with number of ghosts and 'n'.
	g<-c(ghosts_n0,ghosts_n0.5,ghosts_n2,ghosts_n4,ghosts_n6,ghosts_n8,ghosts_n10,ghosts_n12,ghosts_n14,ghosts_n16,ghosts_n18,ghosts_n20)
	n=c(0,0.5,seq(2,20,2))
	a<-as.data.frame(cbind(n,g))

	a.plot<-ggplot(data=a,aes(n,g))+geom_point(pch=20,size=3,col='black')+geom_path(stat='identity',position='identity',lineend='butt',linejoin='round')+ylab('Ghost Detection, count')+xlab('Threshold')+geom_point(data=a[5,1:2],pch=19,size=5,col='goldenrod2')+theme_bw()
	ggsave(a.plot,file='asymptotic_curve_GHOSTS_if_thresholdequals2to20.pdf',device='pdf',units='mm',dpi=200,height=150,width=250)


























