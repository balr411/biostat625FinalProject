library(SASxport)

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

#Check the descriptions for the variables we have left
#Easiest way is to re-create table from 2017-2018 and match names to find descriptions
#Hence recreate this table
ACQ<- read.xport("nnhanes1718/ACQ_J.XPT")
AUQ<- read.xport("nnhanes1718/AUQ_J.XPT")
BMX<- read.xport("nnhanes1718/BMX_J.XPT")
BPQ<- read.xport("nnhanes1718/BPQ_J.XPT")
BPX<- read.xport("nnhanes1718/BPX_J.XPT")
CDQ<- read.xport("nnhanes1718/CDQ_J.XPT")
DBQ<- read.xport("nnhanes1718/DBQ_J.XPT")
DEMO<- read.xport("nnhanes1718/DEMO_J.XPT")
DEQ<- read.xport("nnhanes1718/DEQ_J.XPT")
DIQ<- read.xport("nnhanes1718/DIQ_J.XPT")
DLQ<- read.xport("nnhanes1718/DLQ_J.XPT")
DPQ<- read.xport("nnhanes1718/DPQ_J.XPT")
DSQTOT <- read.xport("nnhanes1718/DSQTOT_J.XPT")
DUQ <- read.xport("nnhanes1718/DUQ_J.XPT")
ECQ <- read.xport("nnhanes1718/ECQ_J.XPT")
HEQ <- read.xport("nnhanes1718/HEQ_J.XPT")
HIQ <- read.xport("nnhanes1718/HIQ_J.XPT")
HSQ <- read.xport("nnhanes1718/HSQ_J.XPT")
HUQ <- read.xport("nnhanes1718/HUQ_J.XPT")
IMQ <- read.xport("nnhanes1718/IMQ_J.XPT")
KIQ <- read.xport("nnhanes1718/KIQ_U_J.XPT")
MCQ <- read.xport("nnhanes1718/MCQ_J.XPT")
OCQ <- read.xport("nnhanes1718/OCQ_J.XPT")
OHQ <- read.xport("nnhanes1718/OHQ_J.XPT")
OSQ <- read.xport("nnhanes1718/OSQ_J.XPT")  
PAQ <- read.xport("nnhanes1718/PAQ_J.XPT")
PAQY <- read.xport("nnhanes1718/PAQY_J.XPT")  
PFQ <- read.xport("nnhanes1718/PFQ_J.XPT")
PUQMEC <- read.xport("nnhanes1718/PUQMEC_J.XPT")
RHQ <- read.xport("nnhanes1718/RHQ_J.XPT")
RXQASA <- read.xport("nnhanes1718/RXQASA_J.XPT")
SLQ <- read.xport("nnhanes1718/SLQ_J.XPT")
SMQ <- read.xport("nnhanes1718/SMQ_J.XPT")
SMQFAM <- read.xport("nnhanes1718/SMQFAM_J.XPT")
SMQRTU <- read.xport("nnhanes1718/SMQRTU_J.XPT")
SMQSHS <- read.xport("nnhanes1718/SMQSHS_J.XPT")
WHQ <- read.xport("nnhanes1718/WHQ_J.XPT")
WHQMEC <- read.xport("nnhanes1718/WHQMEC_J.XPT")
AtoD <- merge( DEMO, ACQ, by ="SEQN", all = TRUE)
AtoD <- merge( AtoD, AUQ, by ="SEQN", all = TRUE)
AtoD <- merge( AtoD, BMX, by ="SEQN", all = TRUE)
AtoD <- merge( AtoD, BPQ, by ="SEQN", all = TRUE)
AtoD <- merge( AtoD, BPX, by ="SEQN", all = TRUE)
AtoD <- merge( AtoD, CDQ, by ="SEQN", all = TRUE)
AtoD <- merge( AtoD, DBQ, by ="SEQN", all = TRUE)
AtoD <- merge( AtoD, DEMO, by ="SEQN", all = TRUE)
AtoD <- merge( AtoD, DEQ, by ="SEQN", all = TRUE)
AtoD <- merge( AtoD, DIQ, by ="SEQN", all = TRUE)
AtoD <- merge( AtoD, DLQ, by ="SEQN", all = TRUE)
AtoD <- merge( AtoD, DPQ, by ="SEQN", all = TRUE)
DtoP <- merge(AtoD, DSQTOT, by="SEQN", all=TRUE)
DtoP <- merge(DtoP, AtoD, BY ="SEQN", all = TRUE)
DtoP <- merge(DtoP, DUQ, BY ="SEQN", all = TRUE)
DtoP <- merge(DtoP, ECQ, BY ="SEQN", all = TRUE)
DtoP <- merge(DtoP, HEQ, BY ="SEQN", all = TRUE)
DtoP <- merge(DtoP, HIQ, BY ="SEQN", all = TRUE)
DtoP <- merge(DtoP, HSQ, BY ="SEQN", all = TRUE)
DtoP <- merge(DtoP, HUQ, BY ="SEQN", all = TRUE)
DtoP <- merge(DtoP, IMQ, BY ="SEQN", all = TRUE)
DtoP <- merge(DtoP, KIQ, BY ="SEQN", all = TRUE)
DtoP <- merge(DtoP, MCQ, BY ="SEQN", all = TRUE)
DtoP <- merge(DtoP, OCQ, BY ="SEQN", all = TRUE)
DtoP <- merge(DtoP, OHQ, BY ="SEQN", all = TRUE)
DtoP <- merge(DtoP, OSQ, BY ="SEQN", all = TRUE)
DtoP <- merge(DtoP, PAQ, BY ="SEQN", all = TRUE)
DtoP <- merge(DtoP, PAQY, BY ="SEQN", all = TRUE)
DtoP <- merge(DtoP, PFQ, BY ="SEQN", all = TRUE)
DtoP <- merge(DtoP, PUQMEC, BY ="SEQN", all = TRUE)
RtoW <- merge(DtoP, RHQ, by = "SEQN",all = TRUE)
RtoW <- merge(RtoW, RXQASA, by = "SEQN",all = TRUE)
RtoW <- merge(RtoW, SLQ, by = "SEQN",all = TRUE)
RtoW <- merge(RtoW, SMQ, by = "SEQN",all = TRUE)
RtoW <- merge(RtoW, SMQFAM, by = "SEQN",all = TRUE)
RtoW <- merge(RtoW, SMQRTU, by = "SEQN",all = TRUE)
RtoW <- merge(RtoW, SMQSHS, by = "SEQN",all = TRUE)
RtoW <- merge(RtoW, WHQ, by = "SEQN",all = TRUE)
RtoW <- merge(RtoW, WHQMEC, by = "SEQN",all = TRUE)
final1718 <- RtoW


for(i in 1:length(names(final_table))){
  print(attr(final1718[[which(names(final_table)[i]==names(final1718))]], "label"))
}

#Not enough useful variables here, after investigation we decide to use data from 2007 on only 
#Read in files again 
dat0708<-read.table("finalTables/finalTable0708.txt", header=T)
dat0910<-read.table("finalTables/finalTable0910.txt", header=T)
dat1112<-read.table("finalTables/finalTable1112.txt", header=T)
dat1314<-read.table("finalTables/finalTable1314.txt", header=T)
dat1516<-read.table("finalTables/finalTable1516.txt", header=T)
dat1718<-read.table("finalTables/finalTable1718.txt", header=T)


#Find common columns
common_cols<-intersect(names(dat1516), names(dat1718))
common_cols<-intersect(common_cols, names(dat0708))
common_cols<-intersect(common_cols, names(dat0910))
common_cols<-intersect(common_cols, names(dat1112))
common_cols<-intersect(common_cols, names(dat1314))

#Subset tables based on common cols
dat0708<-subset(dat0708, select=common_cols)
dat0910<-subset(dat0910, select=common_cols)
dat1112<-subset(dat1112, select=common_cols)
dat1314<-subset(dat1314, select=common_cols)
dat1516<-subset(dat1516, select=common_cols)
dat1718<-subset(dat1718, select=common_cols)

#Combine all data frames
final_table<-rbind(dat1718, dat1516)
final_table<-rbind(final_table, dat1314)
final_table<-rbind(final_table, dat1112)
final_table<-rbind(final_table, dat0910)
final_table<-rbind(final_table, dat0708)

#Re-investigate data
#DIQ010 is our outcome of interest
sum(final_table$DIQ010[!is.na(final_table$DIQ010)]==2 | final_table$DIQ010[!is.na(final_table$DIQ010)]==1)

#56463 observations, drop all others

final_table<-subset(final_table, subset=!is.na(final_table$DIQ010))
final_table<-subset(final_table, subset=(final_table$DIQ010==1 | final_table$DIQ010==2))

#Check the number of NAs in each column 
num_na<-vector(length=dim(final_table)[2])
for(i in 1:length(num_na)){
  num_na[i]<-sum(is.na(final_table[[i]]))
}

sum(num_na>50000)
#171 columns have more than 50,000 missing values

sum(num_na>20000)
#374 columns have more than 20,000 missing values

sum(num_na>10000)
#420 columns have more than 10,000 missing values 

sum(num_na>5000)
#430 columns have more than 5,000 missing values

#Try deleting columns with more than 20,000 missing values
to_delete<-which(num_na>20000)
final_table<-final_table[,-to_delete]

#Check variables that are left
for(i in 1:length(names(final_table))){
  print(attr(final1718[[which(names(final_table)[i]==names(final1718))]], "label"))
}

#Decide to use gender, age, race, ratio of family income to poverty, BMI, high blood pressure, "how healthy is the diet", 
#general health condition, asthma,  minutes sedentary activity, "ever told doctor had trouble sleeping", 
#pulse regular or irregular, Seen mental health professional/past yr, "taking treatment for anemia/past 3 mos" - 14 predictors

#These variables correspond to the names RIAGENDR, RIDAGEYR, RIDRETH1, INDFMPIR, BMXBMI, BPQ020, BPXPULS, DBQ700, HSD010, HUQ090,
#MCQ010, MCQ053, PAD680, and SLQ050

final_names<-c("DIQ010", "RIAGENDR.x", "RIDAGEYR.x", "RIDRETH1.x", "INDFMPIR.x", "BMXBMI", "BPQ020", "BPXPULS", "DBQ700", "HSD010", "HUQ090", "MCQ010", "MCQ053", "PAD680", "SLQ050")

final_table<-subset(final_table, select = final_names)

colnames(final_table)<-c("DIQ010", "RIAGENDR", "RIDAGEYR", "RIDRETH1", "INDFMPIR", "BMXBMI", "BPQ020", "BPXPULS", "DBQ700", "HSD010", "HUQ090", "MCQ010", "MCQ053", "PAD680", "SLQ050")

#Delete rows with NA in them
for(i in 1:dim(final_table)[2]){
  final_table<-final_table[!is.na(final_table[[i]]),]
}

dim(final_table)[1]

#Need to remove individuals with certain values of each predictor
final_table<-subset(final_table, subset=(final_table$BPQ020==1 | final_table$BPQ020==2))
final_table<-subset(final_table, subset=(final_table$DBQ700 %in% 1:5))
final_table<-subset(final_table, subset=(final_table$HSD010 %in% 1:5))
final_table<-subset(final_table, subset=(final_table$HUQ090==1 | final_table$HUQ090==2))
final_table<-subset(final_table, subset=(final_table$MCQ010==1 | final_table$MCQ010==2))
final_table<-subset(final_table, subset=(final_table$MCQ053==1 | final_table$MCQ053==2))
final_table<-subset(final_table, subset=(final_table$PAD680!=7777 | final_table$PAD680!=9999))
final_table<-subset(final_table, subset=(final_table$SLQ050==1 | final_table$SLQ050==2))

dim(final_table)
#28656 observations left 

sum(final_table$DIQ010==1)
#3558 with diabetes

prop<-sum(final_table$DIQ010==1)/dim(final_table)[1]

#split into training and test data
#use 70% train, 30% test
num_test<-as.integer(0.3*dim(final_table)[1])
num_test_diabetes<-as.integer(prop*num_test)
idx_test<-sample(which(final_table$DIQ010==1), num_test_diabetes)
idx_test<-c(idx_test, sample(which(final_table$DIQ010==2), num_test-num_test_diabetes))
test_set<-final_table[idx_test,]
train_set<-final_table[-idx_test,]

#write all tables
write.table(final_table, "finalTables/fullFinalTable.txt")
write.table(test_set, "finalTables/test.txt")
write.table(train_set, "finalTables/train.txt")
