options(stringsAsFactors=FALSE)
source("C:/Users/nussbath/Dropbox/microbIEM/microbIEM/alpha_diversity.R")
source("C:/Users/nussbath/Dropbox/microbIEM/microbIEM/beta_diversity.R")

otu_table_cleaned="4_filter_otu_cleaned_raw_6_sep.csv"
meta_file="4_filter_otu_cleaned_raw_6_sep_mapping.txt"

column_selected=12
timepoint="Visit..day1..day2..."

allowed_samples=c()
data=read.csv(meta_file,sep="\t",header=T)

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

svg("alpha_diversity_analysis.svg",width=20,height=20)
#p=alpha_diversity(otu_table_cleaned,meta_file,column_selected,allowed_samples,timepoint)
p=beta_diversity(otu_table_cleaned,meta_file,column_selected,allowed_samples)
plot(p)
dev.off()
