#!/usr/local/bin/Rscript

	### read config file
	filepath = '/mnt/data03_4_153/cluster_data/result/new_panel/'
	filepath_raw = 'panel_rawdata_cluster_demographic/'
	#filepath_cluster = filepath + filepath_raw + 'category_cluster_result.dat'
	#filepath_demo = filepath + filepath_raw + 'demographic_cluster_result.dat'
	filepath_cluster <- paste(filepath, filepath_raw, "category_cluster_result.dat", sep="")
	filepath_demo <- paste(filepath, filepath_raw, "demographic_cluster_result.dat", sep="")
	
	
	categorySet <- read.table(file=filepath_cluster, header=FALSE, 
				sep='|', col.names=c("cid", "uv"))
	demoSet <- read.table(file=filepath_demo, header=FALSE, 
				sep='|', col.names=c("cid", "gender", "age", "income", "edu"))

	category_demo_uv <- merge(categorySet, demoSet, sort=TRUE);

	### no need to delete the dataset of demo like "|1|1|1|1|"
	#category_demo_uv <- category_demo_uv[!(category_demo_uv$gender==1
	#									&	category_demo_uv$age==1
	#									&	category_demo_uv$income==1
	#									&	category_demo_uv$edu==1),];
	

	### set the config for R
	gender <- list()
	for(i in 1:2){
		tmp <- category_demo_uv[category_demo_uv$gender==i,];
		gender[[i]] <- quantile(tmp$uv, seq(0, 1, 0.05));
	}

	age <- list()
	for(i in 1:7){
		tmp <- category_demo_uv[category_demo_uv$age==i,];
		age[[i]] <- quantile(tmp$uv, seq(0, 1, 0.05));
	}

	income <- list()
	for(i in 1:8){
		tmp <- category_demo_uv[category_demo_uv$income==i,];
		income[[i]] <- quantile(tmp$uv, seq(0, 1, 0.05));
	}

	edu <- list()
	for(i in 1:5){
		tmp <- category_demo_uv[category_demo_uv$edu==i,];
		edu[[i]] <- quantile(tmp$uv, seq(0, 1, 0.05));
	}


	### Identify the attribute of gender
	category_demo_uv_gender1 <- category_demo_uv[(category_demo_uv$uv >= gender[[1]][2]) 
						& (category_demo_uv$uv <= gender[[1]][19]) 
						& (category_demo_uv$gender == 1),]
	category_demo_uv_gender2 <- category_demo_uv[(category_demo_uv$uv >= gender[[2]][6]) 
						& (category_demo_uv$uv <= gender[[2]][15]) 
						& (category_demo_uv$gender == 2),]
	category_demo_uv_gender <- rbind(category_demo_uv_gender1, 
						category_demo_uv_gender2)


	### Identify the attribute of age
	category_demo_uv_age1 <- category_demo_uv[(category_demo_uv$uv >= age[[1]][11])
						& (category_demo_uv$uv >= age[[1]][15])
						& (category_demo_uv$age == 1),]
	category_demo_uv_age2 <- category_demo_uv[(category_demo_uv$uv >= age[[2]][1])
						& (category_demo_uv$uv >= age[[2]][21])
						& (category_demo_uv$age == 2),]
	category_demo_uv_age3 <- category_demo_uv[(category_demo_uv$uv >= age[[3]][1])
						& (category_demo_uv$uv >= age[[3]][21])
						& (category_demo_uv$age == 3),]
	category_demo_uv_age4 <- category_demo_uv[(category_demo_uv$uv >= age[[4]][1])
						& (category_demo_uv$uv >= age[[4]][21])
						& (category_demo_uv$age == 4),]
	category_demo_uv_age5 <- category_demo_uv[(category_demo_uv$uv >= age[[5]][10])
						& (category_demo_uv$uv >= age[[5]][13])
						& (category_demo_uv$age == 5),]
	category_demo_uv_age6 <- category_demo_uv[(category_demo_uv$uv >= age[[6]][9])
						& (category_demo_uv$uv >= age[[6]][11])
						& (category_demo_uv$age == 6),]
	category_demo_uv_age7 <- category_demo_uv[(category_demo_uv$uv >= age[[7]][12])
						& (category_demo_uv$uv >= age[[7]][13])
						& (category_demo_uv$age == 7),]
	category_demo_uv_age <- rbind(category_demo_uv_age1,
					category_demo_uv_age2,
					category_demo_uv_age3,
					category_demo_uv_age4,
					category_demo_uv_age5,
					category_demo_uv_age6,
					category_demo_uv_age7)


	### Identify the attribute of income
	category_demo_uv_income1 <- category_demo_uv[(category_demo_uv$uv >= income[[1]][1])
						& (category_demo_uv$uv >= income[[1]][21])
						& (category_demo_uv$income == 1),]
	category_demo_uv_income2 <- category_demo_uv[(category_demo_uv$uv >= income[[2]][1])
						& (category_demo_uv$uv >= income[[2]][21])
						& (category_demo_uv$income == 2),]
	category_demo_uv_income3 <- category_demo_uv[(category_demo_uv$uv >= income[[3]][1])
						& (category_demo_uv$uv >= income[[3]][21])
						& (category_demo_uv$income == 3),]
	category_demo_uv_income4 <- category_demo_uv[(category_demo_uv$uv >= income[[4]][1])
						& (category_demo_uv$uv >= income[[4]][21])
						& (category_demo_uv$income == 4),]
    	category_demo_uv_income5 <- category_demo_uv[(category_demo_uv$uv >= income[[5]][1])
						& (category_demo_uv$uv >= income[[5]][21])
						& (category_demo_uv$income == 5),]
	category_demo_uv_income6 <- category_demo_uv[(category_demo_uv$uv >= income[[6]][13])
						& (category_demo_uv$uv >= income[[6]][17])
						& (category_demo_uv$income == 6),]
	category_demo_uv_income7 <- category_demo_uv[(category_demo_uv$uv >= income[[7]][13])
						& (category_demo_uv$uv >= income[[7]][17])
						& (category_demo_uv$income == 7),]
	category_demo_uv_income8 <- category_demo_uv[(category_demo_uv$uv >= income[[8]][13])
						& (category_demo_uv$uv >= income[[8]][17])
						& (category_demo_uv$income == 8),]
	category_demo_uv_income <- rbind(category_demo_uv_income1,
						category_demo_uv_income2,
						category_demo_uv_income3,
						category_demo_uv_income4,
						category_demo_uv_income5,
						category_demo_uv_income6,
						category_demo_uv_income7,
						category_demo_uv_income8)


	### Identify the attribute of education
	category_demo_uv_edu1 <- category_demo_uv[(category_demo_uv$uv >= edu[[1]][9])
						& (category_demo_uv$uv >= edu[[1]][15])
	                                        & (category_demo_uv$education == 1),]
    	category_demo_uv_edu2 <- category_demo_uv[(category_demo_uv$uv >= edu[[2]][5])
                                                & (category_demo_uv$uv >= edu[[2]][17])
	                                        & (category_demo_uv$education == 2),]
	category_demo_uv_edu3 <- category_demo_uv[(category_demo_uv$uv >= edu[[3]][1])
                                                & (category_demo_uv$uv >= edu[[3]][21])
	                                        & (category_demo_uv$education == 3),]
	category_demo_uv_edu4 <- category_demo_uv[(category_demo_uv$uv >= edu[[4]][1])
                                            	& (category_demo_uv$uv >= edu[[4]][21])
                                            	& (category_demo_uv$education == 4),]   
	category_demo_uv_edu5 <- category_demo_uv[(category_demo_uv$uv >= edu[[5]][9])
	                                        & (category_demo_uv$uv >= edu[[5]][12])
	                                        & (category_demo_uv$education == 5),]
	category_demo_uv_edu <- rbind(category_demo_uv_edu1,
					category_demo_uv_edu2,
					category_demo_uv_edu3,
					category_demo_uv_edu4,
					category_demo_uv_edu5)


	### combine the demo info
	tmp <- merge(category_demo_uv_gender, category_demo_uv_age, sort=TRUE) ; 
	tmp <- merge(tmp, category_demo_uv_income, sort=TRUE) ; 
	category_demo_uv_merge <- merge(tmp, category_demo_uv_edu, sort=TRUE) ; 
	category_demo_uv_merge <- category_demo_uv_merge[,c('cid','gender','age','income','edu')]
					    
	#filepath_panel = filepath + filepath_raw + 'pannel_cluster_demographic.dat'
	filepath_panel <- paste(filepath, filepath_raw, "panel_cluster_result.dat", sep="")	

	if (file.exists(filepath_panel)) file.remove(filepath_panel)
	write.table(category_demo_uv_merge,
				file=filepath_panel,
				row.names=F, col.names=F,
				quote=F, sep="|");

	sx <- list()	
	sx[[1]] <- category_demo_uv_merge[(category_demo_uv_merge$gender == 1), 'cid'] 
	sx[[2]] <- category_demo_uv_merge[(category_demo_uv_merge$gender == 2), 'cid']
	sx[[3]] <- category_demo_uv_merge[(category_demo_uv_merge$age == 1), 'cid']
	sx[[4]] <- category_demo_uv_merge[(category_demo_uv_merge$age == 2), 'cid']
	sx[[5]] <- category_demo_uv_merge[(category_demo_uv_merge$age == 3), 'cid']
	sx[[6]] <- category_demo_uv_merge[(category_demo_uv_merge$age == 4), 'cid']
	sx[[7]] <- category_demo_uv_merge[(category_demo_uv_merge$age == 5), 'cid']
	sx[[8]] <- category_demo_uv_merge[(category_demo_uv_merge$age == 6), 'cid']
	sx[[9]] <- category_demo_uv_merge[(category_demo_uv_merge$age == 7), 'cid']
	sx[[10]] <- category_demo_uv_merge[(category_demo_uv_merge$income == 1), 'cid']
	sx[[11]] <- category_demo_uv_merge[(category_demo_uv_merge$income == 2), 'cid']
	sx[[12]] <- category_demo_uv_merge[(category_demo_uv_merge$income == 3), 'cid']
	sx[[13]] <- category_demo_uv_merge[(category_demo_uv_merge$income == 4), 'cid']
	sx[[14]] <- category_demo_uv_merge[(category_demo_uv_merge$income == 5), 'cid']
	sx[[15]] <- category_demo_uv_merge[(category_demo_uv_merge$income == 6), 'cid']
	sx[[16]] <- category_demo_uv_merge[(category_demo_uv_merge$income == 7), 'cid']
	sx[[17]] <- category_demo_uv_merge[(category_demo_uv_merge$income == 8), 'cid']
	sx[[18]] <- category_demo_uv_merge[(category_demo_uv_merge$edu == 1), 'cid']
	sx[[19]] <- category_demo_uv_merge[(category_demo_uv_merge$edu == 2), 'cid']
	sx[[20]] <- category_demo_uv_merge[(category_demo_uv_merge$edu == 3), 'cid']
	sx[[21]] <- category_demo_uv_merge[(category_demo_uv_merge$edu == 4), 'cid']
	sx[[22]] <- category_demo_uv_merge[(category_demo_uv_merge$edu == 5), 'cid']


	for (i in 1:22){
		sx[[i]]<-as.character(sx[[i]])
	}
	
	a <- c("1000000001","1000000002","1000000003","1000000004","1000000005","1000000006",
		 	"1000000007","1000000008","1000000009","1000000010","1000000011","1000000012",
		 	"1000000013","1000000014","1000000015","1000000016","1000000017","1000000018",
		 	"1000000019","1000000020","1000000021","1000000022")
	
	sx_ad1 <- c(a[1], sx[[1]])
	sx_ad2 <- c(a[2], sx[[2]])
	sx_ad3 <- c(a[3], sx[[3]])
	sx_ad4 <- c(a[4], sx[[4]])
	sx_ad5 <- c(a[5], sx[[5]])
	sx_ad6 <- c(a[6], sx[[6]])
	sx_ad7 <- c(a[7], sx[[7]])
	sx_ad8 <- c(a[8], sx[[8]])
	sx_ad9 <- c(a[9], sx[[9]])
	sx_ad10 <- c(a[10], sx[[10]])
	sx_ad11 <- c(a[11], sx[[11]])
	sx_ad12 <- c(a[12], sx[[12]])
	sx_ad13 <- c(a[13], sx[[13]])
	sx_ad14 <- c(a[14], sx[[14]])
	sx_ad15 <- c(a[15], sx[[15]])
	sx_ad16 <- c(a[16], sx[[16]])
	sx_ad17 <- c(a[17], sx[[17]])
	sx_ad18 <- c(a[18], sx[[18]])
	sx_ad19 <- c(a[19], sx[[19]])
	sx_ad20 <- c(a[20], sx[[20]])
	sx_ad21 <- c(a[21], sx[[21]])
	sx_ad22 <- c(a[22], sx[[22]])


	#filepath_panel_cluster = filepath + filepath_raw + 'pannel_audience_cluster.dat'
	filepath_panel_cluster <- paste(filepath, filepath_raw, "panel_audience_cluster.dat", sep="")

	if (file.exists(filepath_panel_cluster)) file.remove(filepath_panel_cluster)

	
	write.table(t(sx_ad1), file=filepath_panel_cluster, col.names=F,
				row.names=F, append = T,sep='|',quote=F)
	write.table(t(sx_ad2), file=filepath_panel_cluster, col.names=F,
				row.names=F, append = T,sep='|',quote=F)
	write.table(t(sx_ad3), file=filepath_panel_cluster, col.names=F,
				row.names=F, append = T,sep='|',quote=F)
	write.table(t(sx_ad4), file=filepath_panel_cluster, col.names=F,
				row.names=F, append = T,sep='|',quote=F)
	write.table(t(sx_ad5), file=filepath_panel_cluster, col.names=F,
				row.names=F, append = T,sep='|',quote=F)
	write.table(t(sx_ad6), file=filepath_panel_cluster, col.names=F,
				row.names=F, append = T,sep='|',quote=F)
	write.table(t(sx_ad7), file=filepath_panel_cluster, col.names=F,
				row.names=F, append = T,sep='|',quote=F)
	write.table(t(sx_ad8), file=filepath_panel_cluster, col.names=F,
				row.names=F, append = T,sep='|',quote=F)
	write.table(t(sx_ad9), file=filepath_panel_cluster, col.names=F,
				row.names=F, append = T,sep='|',quote=F)
	write.table(t(sx_ad10), file=filepath_panel_cluster, col.names=F,
				row.names=F, append = T,sep='|',quote=F)
	write.table(t(sx_ad11), file=filepath_panel_cluster, col.names=F,
				row.names=F, append = T,sep='|',quote=F)
	write.table(t(sx_ad12), file=filepath_panel_cluster, col.names=F,
				row.names=F, append = T,sep='|',quote=F)
	write.table(t(sx_ad13), file=filepath_panel_cluster, col.names=F,
				row.names=F, append = T,sep='|',quote=F)
	write.table(t(sx_ad14), file=filepath_panel_cluster, col.names=F,
				row.names=F, append = T,sep='|',quote=F)
	write.table(t(sx_ad15), file=filepath_panel_cluster, col.names=F,
				row.names=F, append = T,sep='|',quote=F)
	write.table(t(sx_ad16), file=filepath_panel_cluster, col.names=F,
				row.names=F, append = T,sep='|',quote=F)
	write.table(t(sx_ad17), file=filepath_panel_cluster, col.names=F,
				row.names=F, append = T,sep='|',quote=F)
	write.table(t(sx_ad18), file=filepath_panel_cluster, col.names=F,
				row.names=F, append = T,sep='|',quote=F)
	write.table(t(sx_ad19), file=filepath_panel_cluster, col.names=F,
				row.names=F, append = T,sep='|',quote=F)
	write.table(t(sx_ad20), file=filepath_panel_cluster, col.names=F,
				row.names=F, append = T,sep='|',quote=F)
	write.table(t(sx_ad21), file=filepath_panel_cluster, col.names=F,
				row.names=F, append = T,sep='|',quote=F)
	write.table(t(sx_ad22), file=filepath_panel_cluster, col.names=F,
				row.names=F, append = T,sep='|',quote=F)


