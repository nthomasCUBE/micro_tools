options(stringsAsFactors=FALSE)
source("alpha_diversity.R")
source("beta_diversity.R")

otu_table_cleaned="4_filter_otu_cleaned_raw_6_sep.csv"
meta_file="4_filter_otu_cleaned_raw_6_sep_mapping.txt"
timepoint="Visit..day1..day2..."

#########################################################################
TIME_POINT_SELECTED=c("5")
TIME_POINT_SELECTED=c(0,1,2,3,4,5,6,7,8)
CONDITION_SELECTED="HE"
ALPHA_OR_BETA="beta"
column_selected=9
CUR_WIDTH=10
CUR_HEIGHT=2
#########################################################################

#
#	Beta diversity plots Part 2
#
beta_plot_2=function(){
	data=read.csv(meta_file,sep="\t",header=T)
	x0=0
	for(cur_fig in 1:8){
		print(c("current step=",x))
		data_filt=subset(data,data[,"Visit..day1..day2..."]%in%c(x0,cur_fig))
		data_filt=subset(data_filt,data_filt[,"Health.status"]==CONDITION_SELECTED)
		allowed_samples=c()
		for(x in 1:dim(data_filt)[2]){
			u_n=unique(data_filt[,x])
			for(y in 1:length(u_n)){
				uid=paste0(colnames(data_filt)[x],"#",u_n[y])
				cs=0
				if(u_n[y]=="_" | u_n[y]=="none_" | u_n[y]=="10"){
					cs=cs+1
				}
				if(length(my_ignore_list)>0){
					for(z in 1:length(my_ignore_list)){
						cs=cs+length(grep(my_ignore_list[z],uid))
					}
				}
				if(cs==0){
					allowed_samples=c(allowed_samples,uid)
				}
			}
		}
		svg(paste0(ALPHA_OR_BETA,"_timepoint_0_VS_",cur_fig,"_condition_selected_",CONDITION_SELECTED,".svg"),width=CUR_WIDTH,height=CUR_HEIGHT)
		print(data_filt)
		p=beta_diversity(otu_table_cleaned,meta_file,column_selected,allowed_samples)
		plot(p)
		dev.off()
	}
}

#
#
#
draw_plot=function(MY_TIME_POINT_SELECTED, ALPHA_OR_BETA){
	allowed_samples=c()
	data=read.csv(meta_file,sep="\t",header=T)
	if(MY_TIME_POINT_SELECTED!=""){	data=subset(data,data["Visit..day1..day2..."]==MY_TIME_POINT_SELECTED)	}
	if(CONDITION_SELECTED!=""){		data=subset(data,data["Health.status"]==CONDITION_SELECTED)	}
	my_ignore_list=c()
	for(x in 1:dim(data)[2]){
		u_n=unique(data[,x])
		for(y in 1:length(u_n)){
			uid=paste0(colnames(data)[x],"#",u_n[y])
			cs=0
			if(u_n[y]=="_" | u_n[y]=="none_" | u_n[y]=="10"){
				cs=cs+1
			}
			if(length(my_ignore_list)>0){
				for(z in 1:length(my_ignore_list)){
					cs=cs+length(grep(my_ignore_list[z],uid))
				}
			}
			if(cs==0){
				allowed_samples=c(allowed_samples,uid)
			}
		}
	}
	svg(paste0(ALPHA_OR_BETA,"_timepoint_",MY_TIME_POINT_SELECTED,"_condition_selected_",CONDITION_SELECTED,".svg"),width=CUR_WIDTH,height=CUR_HEIGHT)
	if(ALPHA_OR_BETA=="alpha"){
		p=alpha_diversity(otu_table_cleaned,meta_file,column_selected,allowed_samples,timepoint)
	}else{
		p=beta_diversity(otu_table_cleaned,meta_file,column_selected,allowed_samples)
	}
	plot(p)
	dev.off()
}
draw_plot("","alpha")


