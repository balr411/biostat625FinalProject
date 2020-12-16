library(SASxport)
ACQ<- read.xport("nnhanes9900/ACQ.XPT")
ALQ<- read.xport("nnhanes9900/ALQ.XPT")
AUQ<- read.xport("nnhanes9900/AUQ.XPT")
BAQ<- read.xport("nnhanes9900/BAQ.XPT")
BMX<- read.xport("nnhanes9900/BMX.XPT")
BPQ<- read.xport("nnhanes9900/BPQ.XPT")
BPX<- read.xport("nnhanes9900/BPX.XPT")
CIQGAD<- read.xport("nnhanes9900/CIQGAD.XPT")
# check CIQMDEP AND CIQPANIC
CIQDEP<- read.xport("nnhanes9900/CIQMDEP.XPT")
CIQPAN<- read.xport("nnhanes9900/CIQPANIC.XPT")
CDQ<- read.xport("nnhanes9900/CDQ.XPT")
CFQ<- read.xport("nnhanes9900/CFQ.XPT")
DBQ<- read.xport("nnhanes9900/DBQ.XPT")
DEMO<- read.xport("nnhanes9900/DEMO.XPT")
DEQ<- read.xport("nnhanes9900/DEQ.XPT")
DIQ<- read.xport("nnhanes9900/DIQ.XPT")
DSQ1 <- read.xport("nnhanes9900/DSQFILE1.XPT")
DSQ2 <- read.xport("nnhanes9900/DSQFILE2.XPT")
DUQ <- read.xport("nnhanes9900/DUQ.XPT")
ECQ <- read.xport("nnhanes9900/ECQ.XPT")
FSQ <- read.xport("nnhanes9900/FSQ.XPT")
HIQ <- read.xport("nnhanes9900/HIQ.XPT")
HOQ <- read.xport("nnhanes9900/HOQ.XPT")
HSQ <- read.xport("nnhanes9900/HSQ.XPT")
HUQ <- read.xport("nnhanes9900/HUQ.XPT")
IMQ <- read.xport("nnhanes9900/IMQ.XPT")
#check KIQ variable
KIQ <- read.xport("nnhanes9900/KIQ.XPT")
MCQ <- read.xport("nnhanes9900/MCQ.XPT")
MPQ <- read.xport("nnhanes9900/MPQ.XPT")
OCQ <- read.xport("nnhanes9900/OCQ.XPT")
OHQ <- read.xport("nnhanes9900/OHQ.XPT")
OSQ <- read.xport("nnhanes9900/OSQ.XPT")
PAQ <- read.xport("nnhanes9900/PAQ.XPT")
PAQIAF <- read.xport("nnhanes9900/PAQIAF.XPT")
PFQ <- read.xport("nnhanes9900/PFQ.XPT")
PUQ <- read.xport("nnhanes9900/PUQ.XPT")
PUQ <- read.xport("nnhanes9900/PUQ.XPT")
RDQ <- read.xport("nnhanes9900/RDQ.XPT")
RHQ <- read.xport("nnhanes9900/RHQ.XPT")
RXQANA <- read.xport("nnhanes9900/RXQ_ANA.XPT")
SMQ <- read.xport("nnhanes9900/SMQ.XPT")
SMQFAM <- read.xport("nnhanes9900/SMQFAM.XPT")
SMQMEC <- read.xport("nnhanes9900/SMQMEC.XPT")
SSQ <- read.xport("nnhanes9900/SSQ.XPT")
SXQ <- read.xport("nnhanes9900/SXQ.XPT")
SXQ <- read.xport("nnhanes9900/SXQ.XPT")
TBQ <- read.xport("nnhanes9900/TBQ.XPT")
VIQ <- read.xport("nnhanes9900/VIQ.XPT")
WHQ <- read.xport("nnhanes9900/WHQ.XPT")

AtoD <- merge( DEMO, ACQ, by ="SEQN", all = TRUE)
#AtoD <- merge( AtoD, AUQ, by ="SEQN", all = TRUE) ###doesn't exist for 13/14
AtoD <- merge( AtoD, BMX, by ="SEQN", all = TRUE) 
AtoD <- merge( AtoD, BPQ, by ="SEQN", all = TRUE)
AtoD <- merge( AtoD, BPX, by ="SEQN", all = TRUE) 
AtoD <- merge( AtoD, CDQ, by ="SEQN", all = TRUE)
AtoD <- merge( AtoD, DBQ, by ="SEQN", all = TRUE)
AtoD <- merge( AtoD, DEMO, by ="SEQN", all = TRUE)
#AtoD <- merge( AtoD, DEQ, by ="SEQN", all = TRUE) ###doesn't exist for 07/08
AtoD <- merge( AtoD, DIQ, by ="SEQN", all = TRUE)
#AtoD <- merge( AtoD, DLQ, by ="SEQN", all = TRUE) ###doesn't exist for 11/12
#AtoD <- merge( AtoD, DPQ, by ="SEQN", all = TRUE) ###doesn't exist for 03/04
#DtoP <- merge(AtoD, DSQTOT, by="SEQN", all=TRUE)  ###doesn't exist for 05/06
DtoP <- merge(DUQ, AtoD, BY ="SEQN", all = TRUE)
DtoP <- merge(DtoP, DUQ, BY ="SEQN", all = TRUE)
DtoP <- merge(DtoP, ECQ, BY ="SEQN", all = TRUE)
#DtoP <- merge(DtoP, HEQ, BY ="SEQN", all = TRUE) ###doesn't exist for 11/12
DtoP <- merge(DtoP, HIQ, BY ="SEQN", all = TRUE)
DtoP <- merge(DtoP, HSQ, BY ="SEQN", all = TRUE)
DtoP <- merge(DtoP, HUQ, BY ="SEQN", all = TRUE)
DtoP <- merge(DtoP, IMQ, BY ="SEQN", all = TRUE)
DtoP <- merge(DtoP, KIQ, BY ="SEQN", all = TRUE)
DtoP <- merge(DtoP, MCQ, BY ="SEQN", all = TRUE)
DtoP <- merge(DtoP, OCQ, BY ="SEQN", all = TRUE)
DtoP <- merge(DtoP, OHQ, BY ="SEQN", all = TRUE)
#DtoP <- merge(DtoP, OSQ, BY ="SEQN", all = TRUE)  #######doesn't exist for 15/16
DtoP <- merge(DtoP, PAQ, BY ="SEQN", all = TRUE)
#DtoP <- merge(DtoP, PAQY, BY ="SEQN", all = TRUE)  #####doesn't exist for 15/16
DtoP <- merge(DtoP, PFQ, BY ="SEQN", all = TRUE)
#DtoP <- merge(DtoP, PUQMEC, BY ="SEQN", all = TRUE) ####doesn't exist for 03/04
RtoW <- merge(DtoP, RHQ, by = "SEQN",all = TRUE)
#RtoW <- merge(RtoW, RXQASA, by = "SEQN",all = TRUE) ####doesn't exist for 09/10
#RtoW <- merge(RtoW, SLQ, by = "SEQN",all = TRUE)  ####doesn't exist for 03/04
RtoW <- merge(RtoW, SMQ, by = "SEQN",all = TRUE)
RtoW <- merge(RtoW, SMQFAM, by = "SEQN",all = TRUE)
#RtoW <- merge(RtoW, SMQRTU, by = "SEQN",all = TRUE) ####doesn't exist for 03/04
#RtoW <- merge(RtoW, SMQSHS, by = "SEQN",all = TRUE)  ####doesn't exist for 11/12
RtoW <- merge(RtoW, WHQ, by = "SEQN",all = TRUE)
#RtoW <- merge(RtoW, WHQMEC, by = "SEQN",all = TRUE)  ####doesn't exist for 03/04
final9900 <- RtoW
write.table(final9900, file="finalTables/finalTable9900.txt")
