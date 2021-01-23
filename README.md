# R - Extraction-loop-Lumicycle-Gentaur-period-analysis-and-graphing


###Loads Library
{
	library(MetaCycle)
	library(gdata, verbose=FALSE)
	library(RSEIS)
	}
	
### Read and Extract data from Excel file
{
	###Access data

	clist<-c("A - Page 1","B - Page 1", "C - Page 1", "D - Page 1", "E - Page 1", "F - Page 1", "G - Page 1", "H - Page 1")
	d<- as.list(rep("",8))
	for (i in clist) {
		d[[i]]<-read.xls("backup.xlsx", i , na.strings=c("NA","#DIV/0!"))
		}

	T<-d$`A - Page 1`[9:3000,2]	
	A<-d$`A - Page 1`[9:3000,4]
	B<-d$`B - Page 1`[9:3000,4]
	C<-d$`C - Page 1`[9:3000,4]
	D<-d$`D - Page 1`[9:3000,4]
	E<-d$`E - Page 1`[9:3000,4]
	F<-d$`F - Page 1`[9:3000,4]
	G<-d$`G - Page 1`[9:3000,4]
	H<-d$`H - Page 1`[9:3000,4]

	####Create Data frames
{
T<-data.frame(T)
A<-data.frame(A)
B<-data.frame(B)
C<-data.frame(C)
D<-data.frame(D)
E<-data.frame(E)
F<-data.frame(F)
G<-data.frame(G)
H<-data.frame(H)
}

### Combine Data Frame

d<-cbind(T,A)	
d<-cbind(d,B)	
d<-cbind(d,C)	
d<-cbind(d,D)	
d<-cbind(d,E)	
d<-cbind(d,F)	
d<-cbind(d,G)
d<-cbind(d,H)

### Save file in Days for Graphs
d$T<-as.numeric(as.character(d$T)) /86400
write.table(d, file="Newanalysis.txt", sep="\t", row.names=FALSE, quote=FALSE)

### Save file in Hours for Period analysis
d$T<-as.numeric(as.character(d$T))*24
t<-t(d)
t<-t[ , colSums(is.na(t)) == 0]	
write.table(format(t, digits=6), file="Newanalysis.csv", sep=",", col.names=FALSE, quote=FALSE)
}

#### Graph data
{
d <- read.table('Newanalysis.txt',header=TRUE, sep="\t")
pdf (file=paste ("RplotLumino", Sys.Date(), ".pdf", sep=""))
par(mfrow = c(2,1))

###Plots Raw data

{
Ymax<-max(d,na.rm=TRUE)
Xmax<-max(d$T,na.rm=TRUE)
		
plot(d$T, d$A,xlab = 'Days', ylab = ' Luminescence (counts/10sec)',type="n",xlim=c(0,Xmax+1),ylim=c(0,Ymax+500), las=1, bty='n')
legend(Xmax,Ymax+Ymax/5, c("A", "B", "C", "D", "E", "F", "G", "H"),lty=c(1,1,1,1,1,1,1,1),lwd=c(1,1,1,1,1,1,1,1),col=c("red", "yellow", "green", "blue", "magenta", "orange", "black", "hotpink"), bty='n')

	lines(d$T, d$A,lwd=1, col="red")
	lines(d$T, d$B,lwd=1, col="yellow")
	lines(d$T, d$C,lwd=1, col="green")
	lines(d$T, d$D,lwd=1, col="blue")
	lines(d$T, d$E,lwd=1, col="magenta")
	lines(d$T, d$F,lwd=1, col="orange")
	lines(d$T, d$G,lwd=1, col="black")
	lines(d$T, d$H,lwd=1, col="hotpink")
}

### Detrended data		
### Calculates Trends


{
	detrended.trajectory1 <- detrend(d$A)
	detrended.trajectory2 <- detrend(d$B)
	detrended.trajectory3 <- detrend(d$C)
	detrended.trajectory4 <- detrend(d$D)
	detrended.trajectory5 <- detrend(d$E)
	detrended.trajectory6 <- detrend(d$F)
	detrended.trajectory7 <- detrend(d$G)
	detrended.trajectory8 <- detrend(d$H)

}

### Graphs Detrended Data
{
	Ydtmax<-max(detrended.trajectory1, detrended.trajectory2, detrended.trajectory3, detrended.trajectory4, detrended.trajectory5, detrended.trajectory6, detrended.trajectory7,detrended.trajectory8, na.rm=TRUE)
	Ydtmin<-min(detrended.trajectory1, detrended.trajectory2, detrended.trajectory3, detrended.trajectory4, detrended.trajectory5, detrended.trajectory6, detrended.trajectory7,detrended.trajectory8, na.rm=TRUE)
	AbsYdt<-max(Ydtmax,-Ydtmin,na.rm=FALSE)
	plot(d$Y1,xlab = 'Days', ylab = 'Rel. Luminescence', type="n",xlim=c(0,Xmax+1),ylim=c(-AbsYdt-200,AbsYdt+200), las=1,bty='n')
	legend(Xmax,AbsYdt+AbsYdt/5, c("A", "B", "C", "D", "E", "F", "G", "H"),lty=c(1,1,1,1,1,1,1,1),lwd=c(1,1,1,1,1,1,1,1),col=c("red", "yellow", "green", "blue", "magenta", "orange", "black", "hotpink"), bty='n')


lines(d$T,detrended.trajectory1, lwd=1, col="red")
lines(d$T,detrended.trajectory2, lwd=1, col="yellow")
lines(d$T,detrended.trajectory3, lwd=1, col="green")
lines(d$T,detrended.trajectory4, lwd=1, col="blue")
lines(d$T,detrended.trajectory5, lwd=1, col="magenta")
lines(d$T,detrended.trajectory6, lwd=1, col="orange")
lines(d$T,detrended.trajectory7, lwd=1, col="black")
lines(d$T,detrended.trajectory8, lwd=1, col="hotpink")
}

dev.off()

}

### Analyse data
{

meta2d("Newanalysis.csv", outdir = "metaout", "csv", "line1", minper = 20,
      maxper = 26, cycMethod = c("ARS", "JTK", "LS"),
      analysisStrategy = "auto", outputFile = TRUE, outIntegration = "both",
      adjustPhase = "predictedPer", combinePvalue = "fisher",
      weightedPerPha = FALSE, ARSmle = "auto", ARSdefaultPer = 24,
      outRawData = FALSE, releaseNote = TRUE, outSymbol = "")}	
