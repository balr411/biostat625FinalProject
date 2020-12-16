library("dplyr")

dat9900<-read.table("finalTables/finalTable9900.txt", header=T)
dat0102<-read.table("finalTables/finalTable0102.txt", header=T)
dat0304<-read.table("finalTables/finalTable0304.txt", header=T)
dat0506<-read.table("finalTables/finalTable0506.txt", header=T)
dat0708<-read.table("finalTables/finalTable0708.txt", header=T)
dat0910<-read.table("finalTables/finalTable0910.txt", header=T)
dat1112<-read.table("finalTables/finalTable1112.txt", header=T)
dat1314<-read.table("finalTables/finalTable1314.txt", header=T)
dat1516<-read.table("finalTables/finalTable1516.txt", header=T)
dat1718<-read.table("finalTables/finalTable1718.txt", header=T)

#See how many columns are common 
common_cols<-intersect(names(dat9900), names(dat0102))
common_cols<-intersect(common_cols, names(dat0304))
common_cols<-intersect(common_cols, names(dat0506))
common_cols<-intersect(common_cols, names(dat0708))
common_cols<-intersect(common_cols, names(dat0910))
common_cols<-intersect(common_cols, names(dat1112))
common_cols<-intersect(common_cols, names(dat1314))
common_cols<-intersect(common_cols, names(dat1516))
common_cols<-intersect(common_cols, names(dat1718))

dat9900<-subset(dat9900, select=common_cols)
dat0102<-subset(dat0102, select=common_cols)
dat0304<-subset(dat0304, select=common_cols)
dat0506<-subset(dat0506, select=common_cols)
dat0708<-subset(dat0708, select=common_cols)
dat0910<-subset(dat0910, select=common_cols)
dat1112<-subset(dat1112, select=common_cols)
dat1314<-subset(dat1314, select=common_cols)
dat1516<-subset(dat1516, select=common_cols)
dat1718<-subset(dat1718, select=common_cols)

final_table<-rbind(dat1718, dat1516)
final_table<-rbind(final_table, dat1314)
final_table<-rbind(final_table, dat1112)
final_table<-rbind(final_table, dat0910)
final_table<-rbind(final_table, dat0708)
final_table<-rbind(final_table, dat0506)
final_table<-rbind(final_table, dat0304)
final_table<-rbind(final_table, dat0102)
final_table<-rbind(final_table, dat9900)

#investigate data
#Note that DIQ010 is our outcome of interest
sum(final_table$DIQ010[!is.na(final_table$DIQ010)]==2 | final_table$DIQ010[!is.na(final_table$DIQ010)]==1)

#95547 observations, drop all others

final_table<-subset(final_table, subset=!is.na(final_table$DIQ010))
final_table<-subset(final_table, subset=(final_table$DIQ010==1 | final_table$DIQ010==2))

#Check the number of NAs in each column 
num_na<-vector(length=dim(final_table)[2])
for(i in 1:length(num_na)){
  num_na[i]<-sum(is.na(final_table[[i]]))
}

sum(num_na>50000)
#101 columns have more than 50,000 missing values

sum(num_na>20000)
#146 columns have more than 20,000 missing values

sum(num_na>10000)
#152 columns have more than 10,000 missing values 

sum(num_na>5000)
#161 columns have more than 5,000 missing values

#Make sure there are no variables missing for one whole cycle
idx<-which(num_na>5000 & num_na<10000)
tab<-final_table[,idx]
View(tab[is.na(final_table[[idx[1]]]), ])
View(tab[is.na(final_table[[idx[2]]]), ])
View(tab[is.na(final_table[[idx[3]]]), ])
View(tab[is.na(final_table[[idx[4]]]), ])
View(tab[is.na(final_table[[idx[5]]]), ])
View(tab[is.na(final_table[[idx[6]]]), ])
View(tab[is.na(final_table[[idx[7]]]), ])
View(tab[is.na(final_table[[idx[8]]]), ])
View(tab[is.na(final_table[[idx[9]]]), ])

#Doesn't appear to be the case

#Try deleting columns with more than 10,000 missing values
to_delete<-which(num_na>10000)
final_table<-final_table[,-to_delete]

#Delete rows with NA in them
for(i in 1:dim(final_table)[2]){
  final_table<-final_table[!is.na(final_table[[i]]),]
}

dim(final_table)[1]

write.table(final_table, "finalTables/fullFinalTableAllAttributes.txt")
