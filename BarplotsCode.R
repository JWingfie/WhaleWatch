###################################################################
######### Humpback Environmental Barplots for each bmode ##########
###################################################################

sstplot<-read.table("Barplots.txt",header=TRUE)    #even though it is called sstplot, it contains ALL environment columns (just easier to leave it named this)

breaks<-c(6,8,10,12,14,16,18,20,22,24,26,28,30)                #to ensure all three bmodes break into 12 equal bins
h<-hist(sstplot$SST1,main="SST for transiting",col="black",las=2,breaks=breaks)         #simple frequency histogram
s<-sum(h$counts)                                   #the counts are the number of observations in each bin, this command sums them (total observations)
l<-length(h$counts)                                 #Length in the total number of bins (should be 12)
fp1<-numeric(l)                                     #making "l" into a numeric value for use in the for loop
for (i in 1:l) {                                    #For loop which takes the number of observations in each bin and divides it by the total number of observations
  fp1[i]<-(h$counts[i]/s)*100                         #multiplying the number by 100 gives the percentage of the total observations in each bin
}                                                    #You now have % Frequency, instead of just a count

h<-hist(sstplot$SST2,main="SST for transiting",col="black",las=2,breaks=breaks)             #Repeat the process above for the two other categories below
s<-sum(h$counts)
l<-length(h$counts)
fp2<-numeric(l)
for (i in 1:l) {
  fp2[i]<-(h$counts[i]/s)*100
}

h<-hist(sstplot$SST3,main="SST for transiting",col="black",las=2,breaks=breaks)
s<-sum(h$counts)
l<-length(h$counts)
fp3<-numeric(l)
for (i in 1:l) {
  fp3[i]<-(h$counts[i]/s)*100
}
windows()
##########SST#################
png(filename = "Humpbacks_SSTBmodepercent.png",  width = 600, height = 600, units = "px", pointsize = 14, bg = "white")
##Labelling the barplot is important as it is necessary to properly label the x axis later on
x2<-barplot( height=fp1, col=rgb(0,0,1,1/4),xlab="Sea Surface Temperature (°C)",ylab="% Frequency",main="",ylim=c(0,100),yaxt="n")  # first histogram- Transiting (blue)
box(which="plot",lty="solid")   #puts a box around the whole plot
x<-c("6-8","8-10","10-12","12-14","14-16","16-18","18-20","20-22","22-24","24-26","26-28","28-30")  #labes for the x axis
labels1<-x
text(x2,par("usr")[3] - 0.25,srt=45,adj=c(1,1.5),labels=labels1,xpd=TRUE,cex=1.0) #custom labelling the x-axis
axis(side=2,at=seq(0,100,20),labels=seq(0,100,20),las=1,par(cex.axis=1))    #labelling the y-axis
barplot( height=fp2, col=rgb(1,0,0,1/4),add=T,yaxt="n")  # second- foraging  (red)
barplot(height=fp3, col=rgb(0,1,0,1/4),add=T,yaxt="n")  # third - breeding   (blue)
cols<-c(col=rgb(0,0,1,1/4), col=rgb(1,0,0,1/4),col=rgb(0,1,0,1/4))       #binding the colours together to use in the legend
legend('topleft',c('Transiting','Foraging','Breeding'), fill = cols,bty="n")
dev.off()

##############CHL % Frequency##############
sstplot<-read.table("Barplots.txt",header=TRUE)    #even though it is called sstplot, it contains ALL environment columns (just easier to leave it named this)

breaks<-c(-4,-3.5,-3,-2.5,-2,-1.5,-1.0,-0.5,0,0.5,1.0,1.5,2,2.5,3,3.5,4)                #to ensure all three bmodes break into 12 equal bins
h<-hist(log(sstplot$CHL1),main="CHL for transiting",col="black",las=2,breaks=breaks)         #simple frequency histogram
s<-sum(h$counts)                                   #the counts are the number of observations in each bin, this command sums them (total observations)
l<-length(h$counts)                                 #Length in the total number of bins (should be 12)
fp1<-numeric(l)                                     #making "l" into a numeric value for use in the for loop
for (i in 1:l) {                                    #For loop which takes the number of observations in each bin and divides it by the total number of observations
  fp1[i]<-(h$counts[i]/s)*100                         #multiplying the number by 100 gives the percentage of the total observations in each bin
}                                                    #You now have % Frequency, instead of just a count

h<-hist(log(sstplot$CHL2),main="SST for transiting",col="black",las=2,breaks=breaks)             #Repeat the process above for the two other categories below
s<-sum(h$counts)
l<-length(h$counts)
fp2<-numeric(l)
for (i in 1:l) {
  fp2[i]<-(h$counts[i]/s)*100
}

h<-hist(log(sstplot$CHL3),main="SST for transiting",col="black",las=2,breaks=breaks)
s<-sum(h$counts)
l<-length(h$counts)
fp3<-numeric(l)
for (i in 1:l) {
  fp3[i]<-(h$counts[i]/s)*100
}                   

##########CHL, % freqeuncy#################
png(filename = "Humpbacks_logCHLBmode.png",  width = 600, height = 600, units = "px", pointsize = 14, bg = "white")
##Labelling the barplot is important as it is necessary to properly label the x axis later on
x2<-barplot( height=fp1, col=rgb(0,0,1,1/4),xlab="log(Chlorophyll a)",ylab="% Frequency",main="",ylim=c(0,100),las=1,yaxt="n")  # first histogram- Transiting (blue)
box(which="plot",lty="solid")   #puts a box around the whole plot
x<-c("-4- -3.5","-3.5- -3","-3- -2.5","-2.5- -2","-2- -1.5","-1.5- -1","-1- -0.5","-0.5- 0","0-0.5","0.5-1","1-1.5","1.5-2","2-2.5","2.5-3","3-3.5","3.5-4")  #custom labelling the x axis
labels1<-x
text(x2,par("usr")[3] - 0.25,srt=45,adj=c(1.1,1.5),labels=labels1,xpd=TRUE,cex=1.0)
axis(side=2,at=seq(0,100,20),labels=seq(0,100,20),las=1,par(cex.axis=1))
barplot( height=fp2, col=rgb(1,0,0,1/4),add=T,yaxt="n")  # second- foraging  (red)
barplot(height=fp3, col=rgb(0,1,0,1/4),add=T,yaxt="n")  # third - breeding   (green)
cols<-c(col=rgb(0,0,1,1/4), col=rgb(1,0,0,1/4),col=rgb(0,1,0,1/4))       #binding the colours together to use in the legend
legend('topleft',c('Transiting','Foraging','Breeding'), fill = cols,bty="n")
dev.off()


##################PP % Frequency####################

sstplot<-read.table("Barplots.txt",header=TRUE)    #even though it is called sstplot, it contains ALL environment columns (just easier to leave it named this)

breaks<-c(0,500,1000,1500,2000,2500,3000,3500,4000,4500,5000,5500,6000,6500,7000)                #to ensure all three bmodes break into 12 equal bins
h<-hist(sstplot$PP1,main="PP for transiting",col="black",las=2,breaks=breaks)         #simple frequency histogram
s<-sum(h$counts)                                   #the counts are the number of observations in each bin, this command sums them (total observations)
l<-length(h$counts)                                 #Length in the total number of bins (should be 12)
fp1<-numeric(l)                                     #making "l" into a numeric value for use in the for loop
for (i in 1:l) {                                    #For loop which takes the number of observations in each bin and divides it by the total number of observations
  fp1[i]<-(h$counts[i]/s)*100                         #multiplying the number by 100 gives the percentage of the total observations in each bin
}                                                    #You now have % Frequency, instead of just a count

h<-hist(sstplot$PP2,main="SST for transiting",col="black",las=2,breaks=breaks)             #Repeat the process above for the two other categories below
s<-sum(h$counts)
l<-length(h$counts)
fp2<-numeric(l)
for (i in 1:l) {
  fp2[i]<-(h$counts[i]/s)*100
}

h<-hist(sstplot$PP3,main="SST for transiting",col="black",las=2,breaks=breaks)
s<-sum(h$counts)
l<-length(h$counts)
fp3<-numeric(l)
for (i in 1:l) {
  fp3[i]<-(h$counts[i]/s)*100
}
windows()
##########PP % Frequency#################
png(filename = "Humpbacks_PPBmodepercent.png",  width = 600, height = 600, units = "px", pointsize = 14, bg = "white")
##Labelling the barplot is important as it is necessary to properly label the x axis later on
x2<-barplot( height=fp1, col=rgb(0,0,1,1/4),xlab="",ylab="% Frequency",main="",ylim=c(0,100),yaxt="n",cex.lab=1.0)  # first histogram- Transiting (blue)
box(which="plot",lty="solid")   #puts a box around the whole plot
x<-c("0-500","500-1000","1000-1500","1500-2000","2000-2500","2500-3000","3000-3500","3500-4000","4000-4500","4500-5000","5000-5500","5500-6000","6000-6500","6500-7000")  #custom labelling the x axis
labels1<-x
mtext(side = 1, text = "Primary Productivity", line = 4)
text(x2,par("usr")[3] - 0.25,srt=45,adj=c(1.1,0.7),labels=labels1,xpd=TRUE,cex=0.95)
axis(side=2,at=seq(0,100,20),labels=seq(0,100,20),las=1)
barplot( height=fp2, col=rgb(1,0,0,1/4),add=T,yaxt="n")  # second- foraging  (red)
barplot(height=fp3, col=rgb(0,1,0,1/4),add=T,yaxt="n")  # third - breeding   (blue)
cols<-c(col=rgb(0,0,1,1/4), col=rgb(1,0,0,1/4),col=rgb(0,1,0,1/4))       #binding the colours together to use in the legend
legend('topright',c('Transiting','Foraging','Breeding'), fill = cols,bty="n",cex=1.0)
dev.off()


##################SSH % Frequency####################

sstplot<-read.table("Barplots.txt",header=TRUE)    #even though it is called sstplot, it contains ALL environment columns (just easier to leave it named this)

breaks<-c(-0.20,-0.15,-0.10,-0.05,0,0.05,0.10)                #to ensure all three bmodes break into 12 equal bins
h<-hist(sstplot$SSH1,main="SSH for transiting",col="black",las=2,breaks=breaks)         #simple frequency histogram
s<-sum(h$counts)                                   #the counts are the number of observations in each bin, this command sums them (total observations)
l<-length(h$counts)                                 #Length in the total number of bins (should be 12)
fp1<-numeric(l)                                     #making "l" into a numeric value for use in the for loop
for (i in 1:l) {                                    #For loop which takes the number of observations in each bin and divides it by the total number of observations
  fp1[i]<-(h$counts[i]/s)*100                         #multiplying the number by 100 gives the percentage of the total observations in each bin
}                                                    #You now have % Frequency, instead of just a count

h<-hist(sstplot$SSH2,main="SSH for transiting",col="black",las=2,breaks=breaks)             #Repeat the process above for the two other categories below
s<-sum(h$counts)
l<-length(h$counts)
fp2<-numeric(l)
for (i in 1:l) {
  fp2[i]<-(h$counts[i]/s)*100
}

h<-hist(sstplot$SSH3,main="SST for transiting",col="black",las=2,breaks=breaks)
s<-sum(h$counts)
l<-length(h$counts)
fp3<-numeric(l)
for (i in 1:l) {
  fp3[i]<-(h$counts[i]/s)*100
}


##########SSH % Frequency#################
png(filename = "Humpbacks_SSHBmodepercent.png",  width = 600, height = 600, units = "px", pointsize = 14, bg = "white")
##Labelling the barplot is important as it is necessary to properly label the x axis later on
x2<-barplot( height=fp1, col=rgb(0,0,1,1/4),xlab="Sea Surface Height",ylab="% Frequency",main="",ylim=c(0,100),yaxt="n",cex.lab=1.0)  # first histogram- Transiting (blue)
box(which="plot",lty="solid")   #puts a box around the whole plot
x<-c("-0.20- -0.15","-0.15- -0.10","-0.10- -0.05","-0.05- 0","0- 0.05","0.05-0.10")  #custom labelling the x axis
labels1<-x
text(x2,par("usr")[3] - 0.25,srt=45,adj=c(1.1,0.7),labels=labels1,xpd=TRUE,cex=0.9)
axis(side=2,at=seq(0,100,20),labels=seq(0,100,20),las=1)
barplot( height=fp2, col=rgb(1,0,0,1/4),add=T,yaxt="n")  # second- foraging  (red)
barplot(height=fp3, col=rgb(0,1,0,1/4),yaxt="n",add=T)  # third - breeding   (blue)
cols<-c(col=rgb(0,0,1,1/4), col=rgb(1,0,0,1/4),col=rgb(0,1,0,1/4))       #binding the colours together to use in the legend
legend('topright',c('Transiting','Foraging','Breeding'), fill = cols,bty="n",cex=1.0)
dev.off()


##################Ekman % Frequency####################

sstplot<-read.table("Barplots.txt",header=TRUE)    #even though it is called sstplot, it contains ALL environment columns (just easier to leave it named this)

breaks<-c(-0.00005,-0.00004,-0.00003,-0.00002,-0.00001,0,0.00001,0.00002,0.00003,0.00004,0.00005)                #to ensure all three bmodes break into 12 equal bins
h<-hist(sstplot$Ekman1,main="SSH for transiting",col="black",las=2,breaks=breaks)         #simple frequency histogram
s<-sum(h$counts)                                   #the counts are the number of observations in each bin, this command sums them (total observations)
l<-length(h$counts)                                 #Length in the total number of bins (should be 12)
fp1<-numeric(l)                                     #making "l" into a numeric value for use in the for loop
for (i in 1:l) {                                    #For loop which takes the number of observations in each bin and divides it by the total number of observations
  fp1[i]<-(h$counts[i]/s)*100                         #multiplying the number by 100 gives the percentage of the total observations in each bin
}                                                    #You now have % Frequency, instead of just a count

h<-hist(sstplot$Ekman2,main="SSH for transiting",col="black",las=2,breaks=breaks)             #Repeat the process above for the two other categories below
s<-sum(h$counts)
l<-length(h$counts)
fp2<-numeric(l)
for (i in 1:l) {
  fp2[i]<-(h$counts[i]/s)*100
}

h<-hist(sstplot$Ekman3,main="SST for transiting",col="black",las=2,breaks=breaks)
s<-sum(h$counts)
l<-length(h$counts)
fp3<-numeric(l)
for (i in 1:l) {
  fp3[i]<-(h$counts[i]/s)*100
}


##########Ekman % Frequency#################
png(filename = "Humpbacks_EkmanBmodepercent.png",  width = 600, height = 600, units = "px", pointsize = 14, bg = "white")
##Labelling the barplot is important as it is necessary to properly label the x axis later on
x2<-barplot( height=fp1, col=rgb(0,0,1,1/4),xlab="",ylab="% Frequency",main="",ylim=c(0,100),yaxt="n",cex.lab=1.0)  # first histogram- Transiting (blue)
box(which="plot",lty="solid")   #puts a box around the whole plot
x<-c("-5.0e-5- -4.0e-5","-4.0e-5- -3.0e-5","-3.0e-5- -2.0e-5","-2.0e-5- -1.0e-5","-1.0e-5-0","0-1.0e-5","1.0e-5-2.0e-5","2.0e-5-3.0e-5","3.0e-5-4.0e-5","4.0e-5-5.0e-5")  #custom labelling the x axis
labels1<-x
mtext(side = 1, text = "Ekman Upwelling", line = 4)   #line argument allows you to move x label away from the axis
text(x2,par("usr")[3] - 0.25,srt=45,adj=c(1.1,0.7),labels=labels1,xpd=TRUE,cex=0.9)
axis(side=2,at=seq(0,100,20),labels=seq(0,100,20),las=1)
barplot( height=fp2, col=rgb(1,0,0,1/4),add=T,yaxt="n")  # second- foraging  (red)
barplot(height=fp3, col=rgb(0,1,0,1/4),yaxt="n",add=T)  # third - breeding   (blue)
cols<-c(col=rgb(0,0,1,1/4), col=rgb(1,0,0,1/4),col=rgb(0,1,0,1/4))       #binding the colours together to use in the legend
legend('topright',c('Transiting','Foraging','Breeding'), fill = cols,bty="n",cex=1.0)
dev.off()


##################Narrow Ekman % Frequency####################

sstplot<-read.table("Barplots.txt",header=TRUE)    #even though it is called sstplot, it contains ALL environment columns (just easier to leave it named this)

breaks<-c(-0.00001,-0.000005,0,0.000005,0.00001,0.000015,0.00002)               #to ensure all three bmodes break into 12 equal bins
Ekman1narrow<-subset(sstplot,Ekman1>=(-0.00001)&Ekman1<=(0.00002))
h<-hist(Ekman1narrow$Ekman1,main="SSH for transiting",col="black",las=2,breaks=breaks)         #simple frequency histogram
s<-sum(h$counts)                                   #the counts are the number of observations in each bin, this command sums them (total observations)
l<-length(h$counts)                                 #Length in the total number of bins (should be 12)
fp1<-numeric(l)                                     #making "l" into a numeric value for use in the for loop
for (i in 1:l) {                                    #For loop which takes the number of observations in each bin and divides it by the total number of observations
  fp1[i]<-(h$counts[i]/s)*100                         #multiplying the number by 100 gives the percentage of the total observations in each bin
}                                                    #You now have % Frequency, instead of just a count

Ekman2narrow<-subset(sstplot,Ekman2>=(-0.00001)&Ekman2<=(0.00002))
h<-hist(Ekman2narrow$Ekman2,main="SSH for transiting",col="black",las=2,breaks=breaks)             #Repeat the process above for the two other categories below
s<-sum(h$counts)
l<-length(h$counts)
fp2<-numeric(l)
for (i in 1:l) {
  fp2[i]<-(h$counts[i]/s)*100
}

Ekman3narrow<-subset(sstplot,Ekman3>=(-0.00001)&Ekman3<=(0.00002))
h<-hist(Ekman3narrow$Ekman3,main="SST for transiting",col="black",las=2,breaks=breaks)
s<-sum(h$counts)
l<-length(h$counts)
fp3<-numeric(l)
for (i in 1:l) {
  fp3[i]<-(h$counts[i]/s)*100
}


##########Narrow Ekman % Frequency#################
png(filename = "Humpbacks_NarrowEkmanBmodepercent.png",  width = 600, height = 600, units = "px", pointsize = 14, bg = "white")
##Labelling the barplot is important as it is necessary to properly label the x axis later on
x2<-barplot( height=fp1, col=rgb(0,0,1,1/4),xlab="",ylab="% Frequency",main="",ylim=c(0,100),yaxt="n",cex.lab=1.0)  # first histogram- Transiting (blue)
box(which="plot",lty="solid")   #puts a box around the whole plot
x<-c("-1.0e-5- -0.5e-5","-0.5e-5-0","0- 0.5e-5","0.5e-1.0e-5","1.0e-5-1.5e-5","1.5e-2.0e-5")  #custom labelling the x axis
labels1<-x
mtext(side = 1, text = "Ekman Upwelling", line = 4)   #line argument allows you to move x label away from the axis
text(x2,par("usr")[3] - 0.25,srt=45,adj=c(1.1,0.7),labels=labels1,xpd=TRUE,cex=0.9)
axis(side=2,at=seq(0,100,20),labels=seq(0,100,20),las=1)
barplot( height=fp2, col=rgb(1,0,0,1/4),add=T,yaxt="n")  # second- foraging  (red)
barplot(height=fp3, col=rgb(0,1,0,1/4),yaxt="n",add=T)  # third - breeding   (blue)
cols<-c(col=rgb(0,0,1,1/4), col=rgb(1,0,0,1/4),col=rgb(0,1,0,1/4))       #binding the colours together to use in the legend
legend('topright',c('Transiting','Foraging','Breeding'), fill = cols,bty="n",cex=1.0)
dev.off()


##################Bottom Depth % Frequency####################

sstplot<-read.table("Barplots.txt",header=TRUE)    #even though it is called sstplot, it contains ALL environment columns (just easier to leave it named this)

breaks<-c(-5500,-5000,-4500,-4000,-3500,-3000,-2500,-2000,-1500,-1000,-500,0)                #to ensure all three bmodes break into 12 equal bins
h<-hist(sstplot$Depth1,main="SSH for transiting",col="black",las=2,breaks=breaks)         #simple frequency histogram
s<-sum(h$counts)                                   #the counts are the number of observations in each bin, this command sums them (total observations)
l<-length(h$counts)                                 #Length in the total number of bins (should be 12)
fp1<-numeric(l)                                     #making "l" into a numeric value for use in the for loop
for (i in 1:l) {                                    #For loop which takes the number of observations in each bin and divides it by the total number of observations
  fp1[i]<-(h$counts[i]/s)*100                         #multiplying the number by 100 gives the percentage of the total observations in each bin
}                                                    #You now have % Frequency, instead of just a count

h<-hist(sstplot$Depth2,main="SSH for transiting",col="black",las=2,breaks=breaks)             #Repeat the process above for the two other categories below
s<-sum(h$counts)
l<-length(h$counts)
fp2<-numeric(l)
for (i in 1:l) {
  fp2[i]<-(h$counts[i]/s)*100
}

h<-hist(sstplot$Depth3,main="SST for transiting",col="black",las=2,breaks=breaks)
s<-sum(h$counts)
l<-length(h$counts)
fp3<-numeric(l)
for (i in 1:l) {
  fp3[i]<-(h$counts[i]/s)*100
}


##########Bottom Depth % Frequency#################
png(filename = "Humpbacks_DepthBmodepercent.png",  width = 600, height = 600, units = "px", pointsize = 14, bg = "white")
##Labelling the barplot is important as it is necessary to properly label the x axis later on
x2<-barplot( height=fp1, col=rgb(0,0,1,1/4),xlab="",ylab="% Frequency",main="",ylim=c(0,100),yaxt="n",cex.lab=1.0)  # first histogram- Transiting (blue)
box(which="plot",lty="solid")   #puts a box around the whole plot
x<-c("-5500--5000","-5000--4500","-4500--4000","-4000--3500","-3500--3000","-3000--2500","-2500--2000","-2000--1500","-1500--1000","-1000--500","-500-0")  #custom labelling the x axis
labels1<-x
mtext(side = 1, text = "Bottom Depth (m)", line = 4)
text(x2,par("usr")[3] - 0.25,srt=45,adj=c(1.1,0.7),labels=labels1,xpd=TRUE,cex=0.9)
axis(side=2,at=seq(0,100,20),labels=seq(0,100,20),las=1)
barplot( height=fp2, col=rgb(1,0,0,1/4),add=T,yaxt="n")  # second- foraging  (red)
barplot(height=fp3, col=rgb(0,1,0,1/4),yaxt="n",add=T)  # third - breeding   (blue)
cols<-c(col=rgb(0,0,1,1/4), col=rgb(1,0,0,1/4),col=rgb(0,1,0,1/4))       #binding the colours together to use in the legend
legend('topleft',c('Transiting','Foraging','Breeding'), fill = cols,bty="n",cex=1.0)
dev.off()


###################################################################################
#########Gray Whale Environmental Barplots for each bmode #########################
###################################################################################

sstplot<-read.table("Barplots_GW.txt",header=TRUE)    #even though it is called sstplot, it contains ALL environment columns (just easier to leave it named this)

breaks<-c(-2,0,2,4,6,8,10,12,14,16,18,20,22,24)                #to ensure all three bmodes break into equal bins
h<-hist(sstplot$SST1,main="SST for transiting",col="black",las=2,breaks=breaks)         #simple frequency histogram
s<-sum(h$counts)                                   #the counts are the number of observations in each bin, this command sums them (total observations)
l<-length(h$counts)                                 #Length in the total number of bins (should be 12)
fp1<-numeric(l)                                     #making "l" into a numeric value for use in the for loop
for (i in 1:l) {                                    #For loop which takes the number of observations in each bin and divides it by the total number of observations
  fp1[i]<-(h$counts[i]/s)*100                         #multiplying the number by 100 gives the percentage of the total observations in each bin
}                                                    #You now have % Frequency, instead of just a count

h<-hist(sstplot$SST2,main="SST for transiting",col="black",las=2,breaks=breaks)             #Repeat the process above for the two other categories below
s<-sum(h$counts)
l<-length(h$counts)
fp2<-numeric(l)
for (i in 1:l) {
  fp2[i]<-(h$counts[i]/s)*100
}

h<-hist(sstplot$SST3,main="SST for transiting",col="black",las=2,breaks=breaks)
s<-sum(h$counts)
l<-length(h$counts)
fp3<-numeric(l)
for (i in 1:l) {
  fp3[i]<-(h$counts[i]/s)*100
}
windows()

##########SST#################

png(filename = "GrayWhale_SSTBmodepercent.png",  width = 600, height = 600, units = "px", pointsize = 14, bg = "white")
##Labelling the barplot is important as it is necessary to properly label the x axis later on
x2<-barplot( height=fp1, col=rgb(0,0,1,1/4),xlab="Sea Surface Temperature (°C)",ylab="% Frequency",main="",ylim=c(0,100),yaxt="n")  # first histogram- Transiting (blue)
box(which="plot",lty="solid")   #puts a box around the whole plot
x<-c("-2-0","0-2","2-4","4-6","6-8","8-10","10-12","12-14","14-16","16-18","18-20","20-22","22-14")  #labes for the x axis
labels1<-x
text(x2,par("usr")[3] - 0.25,srt=45,adj=c(1,1),labels=labels1,xpd=TRUE,cex=1.0) #custom labelling the x-axis
axis(side=2,at=seq(0,100,20),labels=seq(0,100,20),las=1,par(cex.axis=1))    #labelling the y-axis
barplot( height=fp2, col=rgb(1,0,0,1/4),add=T,yaxt="n")  # second- foraging  (red)
barplot(height=fp3, col=rgb(0,1,0,1/4),add=T,yaxt="n")  # third - breeding   (blue)
cols<-c(col=rgb(0,0,1,1/4), col=rgb(1,0,0,1/4),col=rgb(0,1,0,1/4))       #binding the colours together to use in the legend
legend('topleft',c('Transiting','Foraging','Breeding'), fill = cols,bty="n")
dev.off()


##############CHL % Frequency#################

sstplot<-read.table("Barplots_GW.txt",header=TRUE)    #even though it is called sstplot, it contains ALL environment columns (just easier to leave it named this)

breaks<-c(-2,-1.5,-1.0,-0.5,0,0.5,1.0,1.5,2,2.5,3,3.5,4)     #to ensure all three bmodes break into equal bins
h<-hist(log(sstplot$CHL1),main="CHL for transiting",col="black",las=2,breaks=breaks)         #simple frequency histogram
s<-sum(h$counts)                                   #the counts are the number of observations in each bin, this command sums them (total observations)
l<-length(h$counts)                                 #Length in the total number of bins (should be 12)
fp1<-numeric(l)                                     #making "l" into a numeric value for use in the for loop
for (i in 1:l) {                                    #For loop which takes the number of observations in each bin and divides it by the total number of observations
  fp1[i]<-(h$counts[i]/s)*100                         #multiplying the number by 100 gives the percentage of the total observations in each bin
}                                                    #You now have % Frequency, instead of just a count

h<-hist(log(sstplot$CHL2),main="SST for transiting",col="black",las=2,breaks=breaks)             #Repeat the process above for the two other categories below
s<-sum(h$counts)
l<-length(h$counts)
fp2<-numeric(l)
for (i in 1:l) {
  fp2[i]<-(h$counts[i]/s)*100
}

h<-hist(log(sstplot$CHL3),main="SST for transiting",col="black",las=2,breaks=breaks)
s<-sum(h$counts)
l<-length(h$counts)
fp3<-numeric(l)
for (i in 1:l) {
  fp3[i]<-(h$counts[i]/s)*100
}                   



##########CHL, % freqeuncy#################

png(filename = "GrayWhale_logCHLBmode.png",  width = 600, height = 600, units = "px", pointsize = 14, bg = "white")
##Labelling the barplot is important as it is necessary to properly label the x axis later on
x2<-barplot( height=fp1, col=rgb(0,0,1,1/4),xlab="log(Chlorophyll a Concentration)",ylab="% Frequency",main="",ylim=c(0,100),las=1,yaxt="n")  # first histogram- Transiting (blue)
box(which="plot",lty="solid")   #puts a box around the whole plot
x<-c("-2.0- -1.5","-1.5- -1.0","-1.0- -0.5","-0.5-0","0- 0.5","0.5- 1.0","1.0- 1.5","1.5- 2.0","2.0-2.5","2.5-3.0","3.0-3.5","3.5-4.0")  #custom labelling the x axis
labels1<-x
text(x2,par("usr")[3] - 0.25,srt=45,adj=c(1.1,1.5),labels=labels1,xpd=TRUE,cex=1.0)
axis(side=2,at=seq(0,100,20),labels=seq(0,100,20),las=1,par(cex.axis=1))
barplot( height=fp2, col=rgb(1,0,0,1/4),add=T,yaxt="n")  # second- foraging  (red)
barplot(height=fp3, col=rgb(0,1,0,1/4),add=T,yaxt="n")  # third - breeding   (green)
cols<-c(col=rgb(0,0,1,1/4), col=rgb(1,0,0,1/4),col=rgb(0,1,0,1/4))       #binding the colours together to use in the legend
legend('topleft',c('Transiting','Foraging','Breeding'), fill = cols,bty="n")
dev.off()

##################PP % Frequency####################

sstplot<-read.table("Barplots_GW.txt",header=TRUE)    #even though it is called sstplot, it contains ALL environment columns (just easier to leave it named this)

breaks<-c(0,500,1000,1500,2000,2500,3000,3500,4000,4500,5000,5500,6000,6500,7000,7500)     #to ensure all three bmodes break into equal bins
h<-hist(sstplot$PP1,main="PP for transiting",col="black",las=2,breaks=breaks)         #simple frequency histogram
s<-sum(h$counts)                                   #the counts are the number of observations in each bin, this command sums them (total observations)
l<-length(h$counts)                                 #Length in the total number of bins
fp1<-numeric(l)                                     #making "l" into a numeric value for use in the for loop
for (i in 1:l) {                                    #For loop which takes the number of observations in each bin and divides it by the total number of observations
  fp1[i]<-(h$counts[i]/s)*100                         #multiplying the number by 100 gives the percentage of the total observations in each bin
}                                                    #You now have % Frequency, instead of just a count

h<-hist(sstplot$PP2,main="SST for transiting",col="black",las=2,breaks=breaks)             #Repeat the process above for the two other categories below
s<-sum(h$counts)
l<-length(h$counts)
fp2<-numeric(l)
for (i in 1:l) {
  fp2[i]<-(h$counts[i]/s)*100
}

h<-hist(sstplot$PP3,main="SST for transiting",col="black",las=2,breaks=breaks)
s<-sum(h$counts)
l<-length(h$counts)
fp3<-numeric(l)
for (i in 1:l) {
  fp3[i]<-(h$counts[i]/s)*100
}
windows()

##########PP % Frequency#################

png(filename = "GrayWhale_PPBmodepercent.png",  width = 600, height = 600, units = "px", pointsize = 14, bg = "white")
##Labelling the barplot is important as it is necessary to properly label the x axis later on
x2<-barplot( height=fp1, col=rgb(0,0,1,1/4),xlab="",ylab="% Frequency",main="",ylim=c(0,100),yaxt="n",cex.lab=1.0)  # first histogram- Transiting (blue)
box(which="plot",lty="solid")   #puts a box around the whole plot
x<-c("0-500","500-1000","1000-1500","1500-2000","2000-2500","2500-3000","3000-3500","3500-4000","4000-4500","4500-5000","5000-5500","5500-6000","6000-6500","6500-7000","7000-7500")  #custom labelling the x axis
labels1<-x
mtext(side = 1, text = "Primary Productivity", line = 4)
text(x2,par("usr")[3] - 0.25,srt=45,adj=c(1.1,0.7),labels=labels1,xpd=TRUE,cex=0.95)
axis(side=2,at=seq(0,100,20),labels=seq(0,100,20),las=1)
barplot( height=fp2, col=rgb(1,0,0,1/4),add=T,yaxt="n")  # second- foraging  (red)
barplot(height=fp3, col=rgb(0,1,0,1/4),add=T,yaxt="n")  # third - breeding   (blue)
cols<-c(col=rgb(0,0,1,1/4), col=rgb(1,0,0,1/4),col=rgb(0,1,0,1/4))       #binding the colours together to use in the legend
legend('topright',c('Transiting','Foraging','Breeding'), fill = cols,bty="n",cex=1.0)
dev.off()


##################SSH % Frequency####################

sstplot<-read.table("Barplots_GW.txt",header=TRUE)    #even though it is called sstplot, it contains ALL environment columns (just easier to leave it named this)

breaks<-c(-0.30,-0.25,-0.20,-0.15,-0.10,-0.05,0,0.05,0.10,0.15,0.20,0.25,0.30,0.35,0.40)   #to ensure all three bmodes break into equal bins
h<-hist(sstplot$SSH1,main="SSH for transiting",col="black",las=2,breaks=breaks)         #simple frequency histogram
s<-sum(h$counts)                                   #the counts are the number of observations in each bin, this command sums them (total observations)
l<-length(h$counts)                                 #Length in the total number of bins
fp1<-numeric(l)                                     #making "l" into a numeric value for use in the for loop
for (i in 1:l) {                                    #For loop which takes the number of observations in each bin and divides it by the total number of observations
  fp1[i]<-(h$counts[i]/s)*100                         #multiplying the number by 100 gives the percentage of the total observations in each bin
}                                                    #You now have % Frequency, instead of just a count

h<-hist(sstplot$SSH2,main="SSH for transiting",col="black",las=2,breaks=breaks)             #Repeat the process above for the two other categories below
s<-sum(h$counts)
l<-length(h$counts)
fp2<-numeric(l)
for (i in 1:l) {
  fp2[i]<-(h$counts[i]/s)*100
}

h<-hist(sstplot$SSH3,main="SST for transiting",col="black",las=2,breaks=breaks)
s<-sum(h$counts)
l<-length(h$counts)
fp3<-numeric(l)
for (i in 1:l) {
  fp3[i]<-(h$counts[i]/s)*100
}


##########SSH % Frequency#################

png(filename = "GrayWhale_SSHBmodepercent.png",  width = 600, height = 600, units = "px", pointsize = 14, bg = "white")
##Labelling the barplot is important as it is necessary to properly label the x axis later on
x2<-barplot( height=fp1, col=rgb(0,0,1,1/4),xlab="",ylab="% Frequency",main="",ylim=c(0,100),yaxt="n",cex.lab=1.0)  # first histogram- Transiting (blue)
box(which="plot",lty="solid")   #puts a box around the whole plot
x<-c("-0.30- -0.25","-0.25- -0.20","-0.20- -0.15","-0.15- -0.10","-0.10- -0.05","-0.05- 0","0-0.05","0.05-0.10","0.10-0.15","0.15-0.20","0.20-0.25","0.25-0.30","0.30-0.35","0.35-4.0")  #custom labelling the x axis
labels1<-x
mtext(side = 1, text = "Sea Surface Height", line = 4) 
text(x2,par("usr")[3] - 0.25,srt=45,adj=c(1.1,0.7),labels=labels1,xpd=TRUE,cex=0.9)
axis(side=2,at=seq(0,100,20),labels=seq(0,100,20),las=1)
barplot( height=fp2, col=rgb(1,0,0,1/4),add=T,yaxt="n")  # second- foraging  (red)
barplot(height=fp3, col=rgb(0,1,0,1/4),yaxt="n",add=T)  # third - breeding   (blue)
cols<-c(col=rgb(0,0,1,1/4), col=rgb(1,0,0,1/4),col=rgb(0,1,0,1/4))       #binding the colours together to use in the legend
legend('topright',c('Transiting','Foraging','Breeding'), fill = cols,bty="n",cex=1.0)
dev.off()


##################Ekman % Frequency####################

sstplot<-read.table("Barplots_GW.txt",header=TRUE)    #even though it is called sstplot, it contains ALL environment columns (just easier to leave it named this)

breaks<-c(-0.00005,-0.00004,-0.00003,-0.00002,-0.00001,0,0.00001,0.00002,0.00003,0.00004,0.00005,0.00006,0.00007,0.00008)                #to ensure all three bmodes break into equal bins
h<-hist(sstplot$Ekman1,main="SSH for transiting",col="black",las=2,breaks=breaks)         #simple frequency histogram
s<-sum(h$counts)                                   #the counts are the number of observations in each bin, this command sums them (total observations)
l<-length(h$counts)                                 #Length in the total number of bins 
fp1<-numeric(l)                                     #making "l" into a numeric value for use in the for loop
for (i in 1:l) {                                    #For loop which takes the number of observations in each bin and divides it by the total number of observations
  fp1[i]<-(h$counts[i]/s)*100                         #multiplying the number by 100 gives the percentage of the total observations in each bin
}                                                    #You now have % Frequency, instead of just a count

h<-hist(sstplot$Ekman2,main="SSH for transiting",col="black",las=2,breaks=breaks)             #Repeat the process above for the two other categories below
s<-sum(h$counts)
l<-length(h$counts)
fp2<-numeric(l)
for (i in 1:l) {
  fp2[i]<-(h$counts[i]/s)*100
}

h<-hist(sstplot$Ekman3,main="SST for transiting",col="black",las=2,breaks=breaks)
s<-sum(h$counts)
l<-length(h$counts)
fp3<-numeric(l)
for (i in 1:l) {
  fp3[i]<-(h$counts[i]/s)*100
}


##########Ekman % Frequency#################

png(filename = "GrayWhale_EkmanBmodepercent.png",  width = 600, height = 600, units = "px", pointsize = 14, bg = "white")
##Labelling the barplot is important as it is necessary to properly label the x axis later on
x2<-barplot( height=fp1, col=rgb(0,0,1,1/4),xlab="",ylab="% Frequency",main="",ylim=c(0,100),yaxt="n",cex.lab=1.0)  # first histogram- Transiting (blue)
box(which="plot",lty="solid")   #puts a box around the whole plot
x<-c("-5.0e-5- -4.0e-5","-4.0e-5- -3.0e-5","-3.0e-5- -2.0e-5","-2.0e-5- -1.0e-5","-1.0e-5-0","0-1.0e-5","1.0e-5-2.0e-5","2.0e-5-3.0e-5","3.0e-5-4.0e-5","4.0e-5-5.0e-5","5.0e-5-6.0e-5","6.0e-5-7.0e-5","7.0e-5- 8.0e-5")  #custom labelling the x axis
labels1<-x
mtext(side = 1, text = "Ekman Upwelling", line = 4)   #line argument allows you to move x label away from the axis
text(x2,par("usr")[3] - 0.25,srt=45,adj=c(1.1,0.7),labels=labels1,xpd=TRUE,cex=0.9)
axis(side=2,at=seq(0,100,20),labels=seq(0,100,20),las=1)
barplot( height=fp2, col=rgb(1,0,0,1/4),add=T,yaxt="n")  # second- foraging  (red)
barplot(height=fp3, col=rgb(0,1,0,1/4),yaxt="n",add=T)  # third - breeding   (blue)
cols<-c(col=rgb(0,0,1,1/4), col=rgb(1,0,0,1/4),col=rgb(0,1,0,1/4))       #binding the colours together to use in the legend
legend('topright',c('Transiting','Foraging','Breeding'), fill = cols,bty="n",cex=1.0)
dev.off()

#############Ekman, only plotting from -1.0e-5 to 2e-5 to elucidate trends##########


breaks<-c(-0.00001,-0.000005,0,0.000005,0.00001,0.000015,0.00002)                #to ensure all three bmodes break into equal bins
Ekman1narrow<-subset(sstplot,Ekman1>=(-0.00001)&Ekman1<=(0.00002))
h<-hist(Ekman1narrow$Ekman1,main="SSH for transiting",col="black",las=2,breaks=breaks)         #simple frequency histogram
s<-sum(h$counts)                                   #the counts are the number of observations in each bin, this command sums them (total observations)
l<-length(h$counts)                                 #Length in the total number of bins 
fp1<-numeric(l)                                     #making "l" into a numeric value for use in the for loop
for (i in 1:l) {                                    #For loop which takes the number of observations in each bin and divides it by the total number of observations
  fp1[i]<-(h$counts[i]/s)*100                         #multiplying the number by 100 gives the percentage of the total observations in each bin
}                                                    #You now have % Frequency, instead of just a count

Ekman2narrow<-subset(sstplot,Ekman2>=(-0.00001)&Ekman2<=(0.00002))
h<-hist(Ekman2narrow$Ekman2,main="SSH for transiting",col="black",las=2,breaks=breaks)             #Repeat the process above for the two other categories below
s<-sum(h$counts)
l<-length(h$counts)
fp2<-numeric(l)
for (i in 1:l) {
  fp2[i]<-(h$counts[i]/s)*100
}

Ekman3narrow<-subset(sstplot,Ekman3>=(-0.00001)&Ekman3<=(0.00002))
h<-hist(Ekman3narrow$Ekman3,main="SST for transiting",col="black",las=2,breaks=breaks)
s<-sum(h$counts)
l<-length(h$counts)
fp3<-numeric(l)
for (i in 1:l) {
  fp3[i]<-(h$counts[i]/s)*100
}


##########Ekman % Frequency#################

png(filename = "GrayWhale_NarrowEkmanBmodepercent.png",  width = 600, height = 600, units = "px", pointsize = 14, bg = "white")
##Labelling the barplot is important as it is necessary to properly label the x axis later on
x2<-barplot( height=fp1, col=rgb(0,0,1,1/4),xlab="",ylab="% Frequency",main="",ylim=c(0,100),yaxt="n",cex.lab=1.0)  # first histogram- Transiting (blue)
box(which="plot",lty="solid")   #puts a box around the whole plot
x<-c("-1.0e-5- -0.5e-5","-0.5e-5-0","0- 0.5e-5","0.5e-1.0e-5","1.0e-5-1.5e-5","1.5e-2.0e-5")  #custom labelling the x axis
labels1<-x
mtext(side = 1, text = "Ekman Upwelling", line = 4)   #line argument allows you to move x label away from the axis
text(x2,par("usr")[3] - 0.25,srt=45,adj=c(1.1,0.7),labels=labels1,xpd=TRUE,cex=0.9)
axis(side=2,at=seq(0,100,20),labels=seq(0,100,20),las=1)
barplot( height=fp2, col=rgb(1,0,0,1/4),add=T,yaxt="n")  # second- foraging  (red)
barplot(height=fp3, col=rgb(0,1,0,1/4),yaxt="n",add=T)  # third - breeding   (blue)
cols<-c(col=rgb(0,0,1,1/4), col=rgb(1,0,0,1/4),col=rgb(0,1,0,1/4))       #binding the colours together to use in the legend
legend('topright',c('Transiting','Foraging','Breeding'), fill = cols,bty="n",cex=1.0)
dev.off()

##################Bottom Depth % Frequency####################

sstplot<-read.table("Barplots_GW.txt",header=TRUE)    #even though it is called sstplot, it contains ALL environment columns (just easier to leave it named this)

breaks<-c(-2500,-2250,-2000,-1750,-1500,-1250,-1000,-750,-500,-250,0)                #to ensure all three bmodes break into 12 equal bins
h<-hist(sstplot$Depth1,main="SSH for transiting",col="black",las=2,breaks=breaks)         #simple frequency histogram
s<-sum(h$counts)                                   #the counts are the number of observations in each bin, this command sums them (total observations)
l<-length(h$counts)                                 #Length in the total number of bins (should be 12)
fp1<-numeric(l)                                     #making "l" into a numeric value for use in the for loop
for (i in 1:l) {                                    #For loop which takes the number of observations in each bin and divides it by the total number of observations
  fp1[i]<-(h$counts[i]/s)*100                         #multiplying the number by 100 gives the percentage of the total observations in each bin
}                                                    #You now have % Frequency, instead of just a count

h<-hist(sstplot$Depth2,main="SSH for transiting",col="black",las=2,breaks=breaks)             #Repeat the process above for the two other categories below
s<-sum(h$counts)
l<-length(h$counts)
fp2<-numeric(l)
for (i in 1:l) {
  fp2[i]<-(h$counts[i]/s)*100
}

h<-hist(sstplot$Depth3,main="SST for transiting",col="black",las=2,breaks=breaks)
s<-sum(h$counts)
l<-length(h$counts)
fp3<-numeric(l)
for (i in 1:l) {
  fp3[i]<-(h$counts[i]/s)*100
}


##########Bottom Depth % Frequency#################

png(filename = "GrayWhale_DepthBmodepercent.png",  width = 600, height = 600, units = "px", pointsize = 14, bg = "white")
##Labelling the barplot is important as it is necessary to properly label the x axis later on
x2<-barplot( height=fp1, col=rgb(0,0,1,1/4),xlab="",ylab="% Frequency",main="",ylim=c(0,100),yaxt="n",cex.lab=1.0)  # first histogram- Transiting (blue)
box(which="plot",lty="solid")   #puts a box around the whole plot
x<-c("-2500--2250","-2250--2000","-2000--1750","-1750--1500","-1500--1250","-1250--1000","-1000--750","-750--500","-500--250","-250-0")  #custom labelling the x axis
labels1<-x
mtext(side = 1, text = "Bottom Depth (m)", line = 4)
text(x2,par("usr")[3] - 0.25,srt=45,adj=c(1.1,0.7),labels=labels1,xpd=TRUE,cex=0.9)
axis(side=2,at=seq(0,100,20),labels=seq(0,100,20),las=1)
barplot( height=fp2, col=rgb(1,0,0,1/4),add=T,yaxt="n")  # second- foraging  (red)
barplot(height=fp3, col=rgb(0,1,0,1/4),yaxt="n",add=T)  # third - breeding   (blue)
cols<-c(col=rgb(0,0,1,1/4), col=rgb(1,0,0,1/4),col=rgb(0,1,0,1/4))       #binding the colours together to use in the legend
legend('topleft',c('Transiting','Foraging','Breeding'), fill = cols,bty="n",cex=1.0)
dev.off()


#############Only plotting from -500 depth to elucidate trends##########

breaks<-c(-500,-450,-400,-350,-300,-250,-200,-150,-100,-50,0)                #to ensure all three bmodes break into 12 equal bins
Depth1shallow<-subset(sstplot,Depth1>=-500)
h<-hist(Depth1shallow$Depth1,main="SSH for transiting",col="black",las=2,breaks=breaks)         #simple frequency histogram
s<-sum(h$counts)                                   #the counts are the number of observations in each bin, this command sums them (total observations)
l<-length(h$counts)                                 #Length in the total number of bins (should be 12)
fp1<-numeric(l)                                     #making "l" into a numeric value for use in the for loop
for (i in 1:l) {                                    #For loop which takes the number of observations in each bin and divides it by the total number of observations
  fp1[i]<-(h$counts[i]/s)*100                         #multiplying the number by 100 gives the percentage of the total observations in each bin
}                                                    #You now have % Frequency, instead of just a count

Depth2shallow<-subset(sstplot,Depth2>=-500)
h<-hist(Depth2shallow$Depth2,main="SSH for transiting",col="black",las=2,breaks=breaks)             #Repeat the process above for the two other categories below
s<-sum(h$counts)
l<-length(h$counts)
fp2<-numeric(l)
for (i in 1:l) {
  fp2[i]<-(h$counts[i]/s)*100
}

Depth3shallow<-subset(sstplot,Depth3>=-500)
h<-hist(Depth3shallow$Depth3,main="SST for transiting",col="black",las=2,breaks=breaks)
s<-sum(h$counts)
l<-length(h$counts)
fp3<-numeric(l)
for (i in 1:l) {
  fp3[i]<-(h$counts[i]/s)*100
}


##########Bottom Depth % Frequency#################

png(filename = "GrayWhale_ShallowDepthBmodepercent.png",  width = 600, height = 600, units = "px", pointsize = 14, bg = "white")
##Labelling the barplot is important as it is necessary to properly label the x axis later on
x2<-barplot( height=fp1, col=rgb(0,0,1,1/4),xlab="",ylab="% Frequency",main="",ylim=c(0,100),yaxt="n",cex.lab=1.0)  # first histogram- Transiting (blue)
box(which="plot",lty="solid")   #puts a box around the whole plot
x<-c("-500--450","-450--400","-400--350","-350--300","-300--250","-250--200","-200--150","-150--100","-100--50","-50-0")  #custom labelling the x axis
labels1<-x
mtext(side = 1, text = "Bottom Depth (m)", line = 4)
text(x2,par("usr")[3] - 0.25,srt=45,adj=c(1.1,0.7),labels=labels1,xpd=TRUE,cex=0.9)
axis(side=2,at=seq(0,100,20),labels=seq(0,100,20),las=1)
barplot( height=fp2, col=rgb(1,0,0,1/4),add=T,yaxt="n")  # second- foraging  (red)
barplot(height=fp3, col=rgb(0,1,0,1/4),yaxt="n",add=T)  # third - breeding   (blue)
cols<-c(col=rgb(0,0,1,1/4), col=rgb(1,0,0,1/4),col=rgb(0,1,0,1/4))       #binding the colours together to use in the legend
legend('topleft',c('Transiting','Foraging','Breeding'), fill = cols,bty="n",cex=1.0)
dev.off()

#########################################################################################
########################Gray Whale 2005 and 2009 Separate################################
##########################################################################################

#######SST##########

sstplot<-read.table("GrayWhaleBarplot_YearsSeparate.txt",header=TRUE)

breaks<-c(-2,0,2,4,6,8,10,12,14,16,18)                #to ensure all three bmodes break into equal bins
h<-hist(sstplot$SST20052,main="SST for transiting",col="black",las=2,breaks=breaks)         #simple frequency histogram
s<-sum(h$counts)                                   #the counts are the number of observations in each bin, this command sums them (total observations)
l<-length(h$counts)                                 #Length in the total number of bins 
fp1<-numeric(l)                                     #making "l" into a numeric value for use in the for loop
for (i in 1:l) {                                    #For loop which takes the number of observations in each bin and divides it by the total number of observations
  fp1[i]<-(h$counts[i]/s)*100                         #multiplying the number by 100 gives the percentage of the total observations in each bin
}                                                    #You now have % Frequency, instead of just a count

h<-hist(sstplot$SST20092,main="SST for transiting",col="black",las=2,breaks=breaks)             #Repeat the process above for the two other categories below
s<-sum(h$counts)
l<-length(h$counts)
fp2<-numeric(l)
for (i in 1:l) {
  fp2[i]<-(h$counts[i]/s)*100
}


##########SST#################

png(filename = "GrayWhale_SSTBmodepercentYear.png",  width = 600, height = 600, units = "px", pointsize = 14, bg = "white")
##Labelling the barplot is important as it is necessary to properly label the x axis later on
x2<-barplot( height=fp1, col=rgb(0,0,1,1/4),xlab="Sea Surface Temperature (°C)",ylab="% Frequency",main="",ylim=c(0,100),yaxt="n")  # first histogram- Transiting (blue)
box(which="plot",lty="solid")   #puts a box around the whole plot
x<-c("-2-0","0-2","2-4","4-6","6-8","8-10","10-12","12-14","14-16","16-18")  #labes for the x axis
labels1<-x
text(x2,par("usr")[3] - 0.25,srt=45,adj=c(1,1),labels=labels1,xpd=TRUE,cex=1.0) #custom labelling the x-axis
axis(side=2,at=seq(0,100,20),labels=seq(0,100,20),las=1,par(cex.axis=1))    #labelling the y-axis
barplot( height=fp2, col=rgb(1,0,0,1/4),add=T,yaxt="n")  # second- foraging  (red)
cols<-c(col=rgb(0,0,1,1/4), col=rgb(1,0,0,1/4))       #binding the colours together to use in the legend
legend('topleft',c('Foraging 2005','Foraging 2009'), fill = cols,bty="n")
dev.off()


#######CHL##########

sstplot<-read.table("GrayWhaleBarplot_YearsSeparate.txt",header=TRUE)

breaks<-c(-1.5,-1.0,-0.5,0,0.5,1.0,1.5,2.0,2.5,3.0,3.5)                #to ensure all three bmodes break into equal bins
h<-hist(log(sstplot$CHL20052),main="SST for transiting",col="black",las=2,breaks=breaks)         #simple frequency histogram
s<-sum(h$counts)                                   #the counts are the number of observations in each bin, this command sums them (total observations)
l<-length(h$counts)                                 #Length in the total number of bins 
fp1<-numeric(l)                                     #making "l" into a numeric value for use in the for loop
for (i in 1:l) {                                    #For loop which takes the number of observations in each bin and divides it by the total number of observations
  fp1[i]<-(h$counts[i]/s)*100                         #multiplying the number by 100 gives the percentage of the total observations in each bin
}                                                    #You now have % Frequency, instead of just a count

h<-hist(log(sstplot$CHL20092),main="SST for transiting",col="black",las=2,breaks=breaks)             #Repeat the process above for the two other categories below
s<-sum(h$counts)
l<-length(h$counts)
fp2<-numeric(l)
for (i in 1:l) {
  fp2[i]<-(h$counts[i]/s)*100
}


##########CHL#################

png(filename = "GrayWhale_CHLBmodepercentYear.png",  width = 600, height = 600, units = "px", pointsize = 14, bg = "white")
##Labelling the barplot is important as it is necessary to properly label the x axis later on
x2<-barplot( height=fp1, col=rgb(0,0,1,1/4),xlab="log(Chlorophyll a concentration)",ylab="% Frequency",main="",ylim=c(0,100),yaxt="n")  # first histogram- Transiting (blue)
box(which="plot",lty="solid")   #puts a box around the whole plot
x<-c("-1.5--1.0","-1.0--0.5","-0.5-0","0-0.5","0.5-1.0","1.0-1.5","1.5-2.0","2.0-2.5","2.5-3.0","3.0-3.5")  #labes for the x axis
labels1<-x
text(x2,par("usr")[3] - 0.25,srt=45,adj=c(1,1),labels=labels1,xpd=TRUE,cex=1.0) #custom labelling the x-axis
axis(side=2,at=seq(0,100,20),labels=seq(0,100,20),las=1,par(cex.axis=1))    #labelling the y-axis
barplot( height=fp2, col=rgb(1,0,0,1/4),add=T,yaxt="n")  # second- foraging  (red)
cols<-c(col=rgb(0,0,1,1/4), col=rgb(1,0,0,1/4))       #binding the colours together to use in the legend
legend('topleft',c('Foraging 2005','Foraging 2009'), fill = cols,bty="n")
dev.off()

#######SSH##########

sstplot<-read.table("GrayWhaleBarplot_YearsSeparate.txt",header=TRUE)

breaks<-c(-0.3,-0.2,-0.1,0,0.1,0.2,0.3)                #to ensure all three bmodes break into equal bins
h<-hist(sstplot$SSH20052,col="black",las=2,breaks=breaks)         #simple frequency histogram
s<-sum(h$counts)                                   #the counts are the number of observations in each bin, this command sums them (total observations)
l<-length(h$counts)                                 #Length in the total number of bins 
fp1<-numeric(l)                                     #making "l" into a numeric value for use in the for loop
for (i in 1:l) {                                    #For loop which takes the number of observations in each bin and divides it by the total number of observations
  fp1[i]<-(h$counts[i]/s)*100                         #multiplying the number by 100 gives the percentage of the total observations in each bin
}                                                    #You now have % Frequency, instead of just a count

h<-hist(sstplot$SSH20092,col="black",las=2,breaks=breaks)             #Repeat the process above for the two other categories below
s<-sum(h$counts)
l<-length(h$counts)
fp2<-numeric(l)
for (i in 1:l) {
  fp2[i]<-(h$counts[i]/s)*100
}


##########SSH#################

png(filename = "GrayWhale_SSHBmodepercentYear.png",  width = 600, height = 600, units = "px", pointsize = 14, bg = "white")
##Labelling the barplot is important as it is necessary to properly label the x axis later on
x2<-barplot( height=fp1, col=rgb(0,0,1,1/4),xlab="Sea Surface Height",ylab="% Frequency",main="",ylim=c(0,100),yaxt="n")  # first histogram- Transiting (blue)
box(which="plot",lty="solid")   #puts a box around the whole plot
x<-c("-0.3--0.2","-0.2--0.1","-0.1-0","0-0.1","0.1-0.2","0.2-0.3")  #labes for the x axis
labels1<-x
text(x2,par("usr")[3] - 0.25,srt=45,adj=c(1,1),labels=labels1,xpd=TRUE,cex=1.0) #custom labelling the x-axis
axis(side=2,at=seq(0,100,20),labels=seq(0,100,20),las=1,par(cex.axis=1))    #labelling the y-axis
barplot( height=fp2, col=rgb(1,0,0,1/4),add=T,yaxt="n")  # second- foraging  (red)
cols<-c(col=rgb(0,0,1,1/4), col=rgb(1,0,0,1/4))       #binding the colours together to use in the legend
legend('topleft',c('Foraging 2005','Foraging 2009'), fill = cols,bty="n")
dev.off()


#######PP##########

sstplot<-read.table("GrayWhaleBarplot_YearsSeparate.txt",header=TRUE)

breaks<-c(0,500,1000,1500,2000,2500,3000,3500,4000,4500,5000,5500)                #to ensure all three bmodes break into equal bins
h<-hist(sstplot$PP20052,col="black",las=2,breaks=breaks)         #simple frequency histogram
s<-sum(h$counts)                                   #the counts are the number of observations in each bin, this command sums them (total observations)
l<-length(h$counts)                                 #Length in the total number of bins 
fp1<-numeric(l)                                     #making "l" into a numeric value for use in the for loop
for (i in 1:l) {                                    #For loop which takes the number of observations in each bin and divides it by the total number of observations
  fp1[i]<-(h$counts[i]/s)*100                         #multiplying the number by 100 gives the percentage of the total observations in each bin
}                                                    #You now have % Frequency, instead of just a count

h<-hist(sstplot$PP20092,col="black",las=2,breaks=breaks)             #Repeat the process above for the two other categories below
s<-sum(h$counts)
l<-length(h$counts)
fp2<-numeric(l)
for (i in 1:l) {
  fp2[i]<-(h$counts[i]/s)*100
}


##########PP#################

png(filename = "GrayWhale_PPmodepercentYear.png",  width = 600, height = 600, units = "px", pointsize = 14, bg = "white")
##Labelling the barplot is important as it is necessary to properly label the x axis later on
x2<-barplot( height=fp1, col=rgb(0,0,1,1/4),xlab="Primary Productivity",ylab="% Frequency",main="",ylim=c(0,100),yaxt="n")  # first histogram- Transiting (blue)
box(which="plot",lty="solid")   #puts a box around the whole plot
x<-c("0-500","500-1000","1000-1500","1500-2000","2000-2500","2500-3000","3000-3500","3500-4000","4000-4500","4500-5000","5000-5500")  #labes for the x axis
labels1<-x
text(x2,par("usr")[3] - 0.25,srt=45,adj=c(1,1),labels=labels1,xpd=TRUE,cex=1.0) #custom labelling the x-axis
axis(side=2,at=seq(0,100,20),labels=seq(0,100,20),las=1,par(cex.axis=1))    #labelling the y-axis
barplot( height=fp2, col=rgb(1,0,0,1/4),add=T,yaxt="n")  # second- foraging  (red)
cols<-c(col=rgb(0,0,1,1/4), col=rgb(1,0,0,1/4))       #binding the colours together to use in the legend
legend('topleft',c('Foraging 2005','Foraging 2009'), fill = cols,bty="n")
dev.off()


#######Depth##########

sstplot<-read.table("GrayWhaleBarplot_YearsSeparate.txt",header=TRUE)

breaks<-c(-650,-600,-550,-500,-450,-400,-350,-300,-250,-200,-150,-100,-50,0)                #to ensure all three bmodes break into equal bins
h<-hist(sstplot$Depth20052,col="black",las=2,breaks=breaks)         #simple frequency histogram
s<-sum(h$counts)                                   #the counts are the number of observations in each bin, this command sums them (total observations)
l<-length(h$counts)                                 #Length in the total number of bins 
fp1<-numeric(l)                                     #making "l" into a numeric value for use in the for loop
for (i in 1:l) {                                    #For loop which takes the number of observations in each bin and divides it by the total number of observations
  fp1[i]<-(h$counts[i]/s)*100                         #multiplying the number by 100 gives the percentage of the total observations in each bin
}                                                    #You now have % Frequency, instead of just a count

h<-hist(sstplot$Depth20092,col="black",las=2,breaks=breaks)             #Repeat the process above for the two other categories below
s<-sum(h$counts)
l<-length(h$counts)
fp2<-numeric(l)
for (i in 1:l) {
  fp2[i]<-(h$counts[i]/s)*100
}


##########Depth#################

png(filename = "GrayWhale_DepthmodepercentYear.png",  width = 600, height = 600, units = "px", pointsize = 14, bg = "white")
##Labelling the barplot is important as it is necessary to properly label the x axis later on
x2<-barplot( height=fp1, col=rgb(0,0,1,1/4),xlab="Bottom Depth (m)",ylab="% Frequency",main="",ylim=c(0,100),yaxt="n")  # first histogram- Transiting (blue)
box(which="plot",lty="solid")   #puts a box around the whole plot
x<-c("-650--600","-600--550","-550--500","-500--450","-450--400","-400--350","-350--300","-300--250","-250--200","-200--150","-150--100","-100--50","-50-0")  #labes for the x axis
labels1<-x
text(x2,par("usr")[3] - 0.25,srt=45,adj=c(1,1),labels=labels1,xpd=TRUE,cex=1.0) #custom labelling the x-axis
axis(side=2,at=seq(0,100,20),labels=seq(0,100,20),las=1,par(cex.axis=1))    #labelling the y-axis
barplot( height=fp2, col=rgb(1,0,0,1/4),add=T,yaxt="n")  # second- foraging  (red)
cols<-c(col=rgb(0,0,1,1/4), col=rgb(1,0,0,1/4))       #binding the colours together to use in the legend
legend('topleft',c('Foraging 2005','Foraging 2009'), fill = cols,bty="n")
dev.off()

#######Shallow Depth##########

sstplot<-read.table("GrayWhaleBarplot_YearsSeparate.txt",header=TRUE)

breaks<-c(-150,-140,-130,-120,-110,-100,-90,-80,-70,-60,-50,-40,-30,-20,-10,0)                #to ensure all three bmodes break into equal bins
Depth1shallow<-subset(sstplot,Depth20052>=-150)
h<-hist(Depth1shallow$Depth20052,col="black",las=2,breaks=breaks)         #simple frequency histogram
s<-sum(h$counts)                                   #the counts are the number of observations in each bin, this command sums them (total observations)
l<-length(h$counts)                                 #Length in the total number of bins 
fp1<-numeric(l)                                     #making "l" into a numeric value for use in the for loop
for (i in 1:l) {                                    #For loop which takes the number of observations in each bin and divides it by the total number of observations
  fp1[i]<-(h$counts[i]/s)*100                         #multiplying the number by 100 gives the percentage of the total observations in each bin
}                                                    #You now have % Frequency, instead of just a count

Depth2shallow<-subset(sstplot,Depth20092>=-150)
h<-hist(Depth2shallow$Depth20092,col="black",las=2,breaks=breaks)             #Repeat the process above for the two other categories below
s<-sum(h$counts)
l<-length(h$counts)
fp2<-numeric(l)
for (i in 1:l) {
  fp2[i]<-(h$counts[i]/s)*100
}


##########Shallow Depth#################

png(filename = "GrayWhale_DepthmodepercentYearShallow.png",  width = 600, height = 600, units = "px", pointsize = 14, bg = "white")
##Labelling the barplot is important as it is necessary to properly label the x axis later on
x2<-barplot( height=fp1, col=rgb(0,0,1,1/4),xlab="Bottom Depth (m)",ylab="% Frequency",main="",ylim=c(0,100),yaxt="n")  # first histogram- Transiting (blue)
box(which="plot",lty="solid")   #puts a box around the whole plot
x<-c("-150--140","-140--130","-130--120","-120--110","-110--100","-100--90","-90--80","-80--70","-70--60","-60--50","-50--40","-40--30","-30--20","-20--10","-10-0")  #labes for the x axis
labels1<-x
text(x2,par("usr")[3] - 0.25,srt=45,adj=c(1,1),labels=labels1,xpd=TRUE,cex=1.0) #custom labelling the x-axis
axis(side=2,at=seq(0,100,20),labels=seq(0,100,20),las=1,par(cex.axis=1))    #labelling the y-axis
barplot( height=fp2, col=rgb(1,0,0,1/4),add=T,yaxt="n")  # second- foraging  (red)
cols<-c(col=rgb(0,0,1,1/4), col=rgb(1,0,0,1/4))       #binding the colours together to use in the legend
legend('topleft',c('Foraging 2005','Foraging 2009'), fill = cols,bty="n")
dev.off()

#######Ekman##########

sstplot<-read.table("GrayWhaleBarplot_YearsSeparate.txt",header=TRUE)

breaks<-c(-0.000045,-0.000035,-0.000025,-0.000015,-0.000005,0.000005,0.000015,0.000025,0.000035)                #to ensure all three bmodes break into equal bins
h<-hist(sstplot$Ekman20052,col="black",las=2,breaks=breaks)         #simple frequency histogram
s<-sum(h$counts)                                   #the counts are the number of observations in each bin, this command sums them (total observations)
l<-length(h$counts)                                 #Length in the total number of bins 
fp1<-numeric(l)                                     #making "l" into a numeric value for use in the for loop
for (i in 1:l) {                                    #For loop which takes the number of observations in each bin and divides it by the total number of observations
  fp1[i]<-(h$counts[i]/s)*100                         #multiplying the number by 100 gives the percentage of the total observations in each bin
}                                                    #You now have % Frequency, instead of just a count

h<-hist(sstplot$Ekman20092,col="black",las=2,breaks=breaks)             #Repeat the process above for the two other categories below
s<-sum(h$counts)
l<-length(h$counts)
fp2<-numeric(l)
for (i in 1:l) {
  fp2[i]<-(h$counts[i]/s)*100
}


##########Ekman#################

png(filename = "GrayWhale_EkmanmodepercentYear.png",  width = 600, height = 600, units = "px", pointsize = 14, bg = "white")
##Labelling the barplot is important as it is necessary to properly label the x axis later on
x2<-barplot( height=fp1, col=rgb(0,0,1,1/4),xlab="",ylab="% Frequency",main="",ylim=c(0,100),yaxt="n")  # first histogram- Transiting (blue)
box(which="plot",lty="solid")   #puts a box around the whole plot
x<-c("-4.5e-5--3.5e-5","-3.5e-5- -2.5e-5","-2.5e-5--1.5e-5","-1.5e--0.5e-5","-0.5e-5-0.5e-5","0.5e-5-1.5e-5","1.5e-5-2.5e-5","2.5e-5-3.5e-5")  #labes for the x axis
labels1<-x
mtext(side=1,text="Ekman Upwelling",line=4)
text(x2,par("usr")[3] - 0.25,srt=45,adj=c(1,1),labels=labels1,xpd=TRUE,cex=1.0) #custom labelling the x-axis
axis(side=2,at=seq(0,100,20),labels=seq(0,100,20),las=1,par(cex.axis=1))    #labelling the y-axis
barplot( height=fp2, col=rgb(1,0,0,1/4),add=T,yaxt="n")  # second- foraging  (red)
cols<-c(col=rgb(0,0,1,1/4), col=rgb(1,0,0,1/4))       #binding the colours together to use in the legend
legend('topleft',c('Foraging 2005','Foraging 2009'), fill = cols,bty="n")
dev.off()

###########################################################################
##################Gray Whale 2005 Behaviour Only###########################
###########################################################################

#######SST##########

sstplot<-read.table("Barplots_GW.txt",header=TRUE)
sstplot<-subset(sstplot,Year==2005)                 #So the data is for 2005 only
breaks<-c(-2,0,2,4,6,8,10,12,14,16,18,20,22,24)                #to ensure all three bmodes break into equal bins
h<-hist(sstplot$SST1,col="black",las=2,breaks=breaks)         #simple frequency histogram
s<-sum(h$counts)                                   #the counts are the number of observations in each bin, this command sums them (total observations)
l<-length(h$counts)                                 #Length in the total number of bins 
fp1<-numeric(l)                                     #making "l" into a numeric value for use in the for loop
for (i in 1:l) {                                    #For loop which takes the number of observations in each bin and divides it by the total number of observations
  fp1[i]<-(h$counts[i]/s)*100                         #multiplying the number by 100 gives the percentage of the total observations in each bin
}                                                    #You now have % Frequency, instead of just a count

h<-hist(sstplot$SST2,main="SST for transiting",col="black",las=2,breaks=breaks)             #Repeat the process above for the two other categories below
s<-sum(h$counts)
l<-length(h$counts)
fp2<-numeric(l)
for (i in 1:l) {
  fp2[i]<-(h$counts[i]/s)*100
}

h<-hist(sstplot$SST3,main="SST for transiting",col="black",las=2,breaks=breaks)             #Repeat the process above for the two other categories below
s<-sum(h$counts)
l<-length(h$counts)
fp3<-numeric(l)
for (i in 1:l) {
  fp3[i]<-(h$counts[i]/s)*100
}


##########SST#################

png(filename = "GrayWhale_SSTBmodepercent2005.png",  width = 600, height = 600, units = "px", pointsize = 14, bg = "white")
##Labelling the barplot is important as it is necessary to properly label the x axis later on
x2<-barplot( height=fp1, col=rgb(0,0,1,1/4),xlab="Sea Surface Temperature (°C)",ylab="% Frequency",main="",ylim=c(0,100),yaxt="n")  # first histogram- Transiting (blue)
box(which="plot",lty="solid")   #puts a box around the whole plot
x<-c("-2-0","0-2","2-4","4-6","6-8","8-10","10-12","12-14","14-16","16-18","18-20","20-22","22-24")  #labes for the x axis
labels1<-x
text(x2,par("usr")[3] - 0.25,srt=45,adj=c(1,1),labels=labels1,xpd=TRUE,cex=1.0) #custom labelling the x-axis
axis(side=2,at=seq(0,100,20),labels=seq(0,100,20),las=1,par(cex.axis=1))    #labelling the y-axis
barplot( height=fp2, col=rgb(1,0,0,1/4),add=T,yaxt="n")  # second- foraging  (red)
barplot( height=fp3, col=rgb(0,1,0,1/4),add=T,yaxt="n")  # third- breeding  (green)
cols<-c(col=rgb(0,0,1,1/4), col=rgb(1,0,0,1/4),col=rgb(0,1,0,1/4))       #binding the colours together to use in the legend
legend('topleft',c('Transiting','Foraging','Breeding'), fill = cols,bty="n")
dev.off()


##############CHL % Frequency#################

breaks<-c(-2,-1.5,-1.0,-0.5,0,0.5,1.0,1.5,2,2.5,3,3.5,4)     #to ensure all three bmodes break into equal bins
h<-hist(log(sstplot$CHL1),main="CHL for transiting",col="black",las=2,breaks=breaks)         #simple frequency histogram
s<-sum(h$counts)                                   #the counts are the number of observations in each bin, this command sums them (total observations)
l<-length(h$counts)                                 #Length in the total number of bins (should be 12)
fp1<-numeric(l)                                     #making "l" into a numeric value for use in the for loop
for (i in 1:l) {                                    #For loop which takes the number of observations in each bin and divides it by the total number of observations
  fp1[i]<-(h$counts[i]/s)*100                         #multiplying the number by 100 gives the percentage of the total observations in each bin
}                                                    #You now have % Frequency, instead of just a count

h<-hist(log(sstplot$CHL2),main="SST for transiting",col="black",las=2,breaks=breaks)             #Repeat the process above for the two other categories below
s<-sum(h$counts)
l<-length(h$counts)
fp2<-numeric(l)
for (i in 1:l) {
  fp2[i]<-(h$counts[i]/s)*100
}

h<-hist(log(sstplot$CHL3),main="SST for transiting",col="black",las=2,breaks=breaks)
s<-sum(h$counts)
l<-length(h$counts)
fp3<-numeric(l)
for (i in 1:l) {
  fp3[i]<-(h$counts[i]/s)*100
}                   



##########CHL, % freqeuncy#################

png(filename = "GrayWhale_logCHLBmodepercent2005.png",  width = 600, height = 600, units = "px", pointsize = 14, bg = "white")
##Labelling the barplot is important as it is necessary to properly label the x axis later on
x2<-barplot( height=fp1, col=rgb(0,0,1,1/4),xlab="log(Chlorophyll a Concentration)",ylab="% Frequency",main="",ylim=c(0,100),las=1,yaxt="n")  # first histogram- Transiting (blue)
box(which="plot",lty="solid")   #puts a box around the whole plot
x<-c("-2.0- -1.5","-1.5- -1.0","-1.0- -0.5","-0.5-0","0- 0.5","0.5- 1.0","1.0- 1.5","1.5- 2.0","2.0-2.5","2.5-3.0","3.0-3.5","3.5-4.0")  #custom labelling the x axis
labels1<-x
text(x2,par("usr")[3] - 0.25,srt=45,adj=c(1.1,1.5),labels=labels1,xpd=TRUE,cex=1.0)
axis(side=2,at=seq(0,100,20),labels=seq(0,100,20),las=1,par(cex.axis=1))
barplot( height=fp2, col=rgb(1,0,0,1/4),add=T,yaxt="n")  # second- foraging  (red)
barplot(height=fp3, col=rgb(0,1,0,1/4),add=T,yaxt="n")  # third - breeding   (green)
cols<-c(col=rgb(0,0,1,1/4), col=rgb(1,0,0,1/4),col=rgb(0,1,0,1/4))       #binding the colours together to use in the legend
legend('topleft',c('Transiting','Foraging','Breeding'), fill = cols,bty="n")
dev.off()

##################PP % Frequency####################

breaks<-c(0,500,1000,1500,2000,2500,3000,3500,4000,4500,5000,5500,6000,6500,7000,7500)     #to ensure all three bmodes break into equal bins
h<-hist(sstplot$PP1,main="PP for transiting",col="black",las=2,breaks=breaks)         #simple frequency histogram
s<-sum(h$counts)                                   #the counts are the number of observations in each bin, this command sums them (total observations)
l<-length(h$counts)                                 #Length in the total number of bins
fp1<-numeric(l)                                     #making "l" into a numeric value for use in the for loop
for (i in 1:l) {                                    #For loop which takes the number of observations in each bin and divides it by the total number of observations
  fp1[i]<-(h$counts[i]/s)*100                         #multiplying the number by 100 gives the percentage of the total observations in each bin
}                                                    #You now have % Frequency, instead of just a count

h<-hist(sstplot$PP2,main="SST for transiting",col="black",las=2,breaks=breaks)             #Repeat the process above for the two other categories below
s<-sum(h$counts)
l<-length(h$counts)
fp2<-numeric(l)
for (i in 1:l) {
  fp2[i]<-(h$counts[i]/s)*100
}

h<-hist(sstplot$PP3,main="SST for transiting",col="black",las=2,breaks=breaks)
s<-sum(h$counts)
l<-length(h$counts)
fp3<-numeric(l)
for (i in 1:l) {
  fp3[i]<-(h$counts[i]/s)*100
}
windows()

##########PP % Frequency#################

png(filename = "GrayWhale_PPBmodepercent2005.png",  width = 600, height = 600, units = "px", pointsize = 14, bg = "white")
##Labelling the barplot is important as it is necessary to properly label the x axis later on
x2<-barplot( height=fp1, col=rgb(0,0,1,1/4),xlab="",ylab="% Frequency",main="",ylim=c(0,100),yaxt="n",cex.lab=1.0)  # first histogram- Transiting (blue)
box(which="plot",lty="solid")   #puts a box around the whole plot
x<-c("0-500","500-1000","1000-1500","1500-2000","2000-2500","2500-3000","3000-3500","3500-4000","4000-4500","4500-5000","5000-5500","5500-6000","6000-6500","6500-7000","7000-7500")  #custom labelling the x axis
labels1<-x
mtext(side = 1, text = "Primary Productivity", line = 4)
text(x2,par("usr")[3] - 0.25,srt=45,adj=c(1.1,0.7),labels=labels1,xpd=TRUE,cex=0.95)
axis(side=2,at=seq(0,100,20),labels=seq(0,100,20),las=1)
barplot( height=fp2, col=rgb(1,0,0,1/4),add=T,yaxt="n")  # second- foraging  (red)
barplot(height=fp3, col=rgb(0,1,0,1/4),add=T,yaxt="n")  # third - breeding   (blue)
cols<-c(col=rgb(0,0,1,1/4), col=rgb(1,0,0,1/4),col=rgb(0,1,0,1/4))       #binding the colours together to use in the legend
legend('topright',c('Transiting','Foraging','Breeding'), fill = cols,bty="n",cex=1.0)
dev.off()


##################SSH % Frequency####################


breaks<-c(-0.30,-0.25,-0.20,-0.15,-0.10,-0.05,0,0.05,0.10,0.15,0.20,0.25,0.30,0.35)   #to ensure all three bmodes break into equal bins
h<-hist(sstplot$SSH1,main="SSH for transiting",col="black",las=2,breaks=breaks)         #simple frequency histogram
s<-sum(h$counts)                                   #the counts are the number of observations in each bin, this command sums them (total observations)
l<-length(h$counts)                                 #Length in the total number of bins
fp1<-numeric(l)                                     #making "l" into a numeric value for use in the for loop
for (i in 1:l) {                                    #For loop which takes the number of observations in each bin and divides it by the total number of observations
  fp1[i]<-(h$counts[i]/s)*100                         #multiplying the number by 100 gives the percentage of the total observations in each bin
}                                                    #You now have % Frequency, instead of just a count

h<-hist(sstplot$SSH2,main="SSH for transiting",col="black",las=2,breaks=breaks)             #Repeat the process above for the two other categories below
s<-sum(h$counts)
l<-length(h$counts)
fp2<-numeric(l)
for (i in 1:l) {
  fp2[i]<-(h$counts[i]/s)*100
}

h<-hist(sstplot$SSH3,main="SST for transiting",col="black",las=2,breaks=breaks)
s<-sum(h$counts)
l<-length(h$counts)
fp3<-numeric(l)
for (i in 1:l) {
  fp3[i]<-(h$counts[i]/s)*100
}


##########SSH % Frequency#################

png(filename = "GrayWhale_SSHBmodepercent2005.png",  width = 600, height = 600, units = "px", pointsize = 14, bg = "white")
##Labelling the barplot is important as it is necessary to properly label the x axis later on
x2<-barplot( height=fp1, col=rgb(0,0,1,1/4),xlab="",ylab="% Frequency",main="",ylim=c(0,100),yaxt="n",cex.lab=1.0)  # first histogram- Transiting (blue)
box(which="plot",lty="solid")   #puts a box around the whole plot
x<-c("-0.30- -0.25","-0.25- -0.20","-0.20- -0.15","-0.15- -0.10","-0.10- -0.05","-0.05- 0","0-0.05","0.05-0.10","0.10-0.15","0.15-0.20","0.20-0.25","0.25-0.30","0.30-0.35")  #custom labelling the x axis
labels1<-x
mtext(side = 1, text = "Sea Surface Height", line = 4) 
text(x2,par("usr")[3] - 0.25,srt=45,adj=c(1.1,0.7),labels=labels1,xpd=TRUE,cex=0.9)
axis(side=2,at=seq(0,100,20),labels=seq(0,100,20),las=1)
barplot( height=fp2, col=rgb(1,0,0,1/4),add=T,yaxt="n")  # second- foraging  (red)
barplot(height=fp3, col=rgb(0,1,0,1/4),yaxt="n",add=T)  # third - breeding   (blue)
cols<-c(col=rgb(0,0,1,1/4), col=rgb(1,0,0,1/4),col=rgb(0,1,0,1/4))       #binding the colours together to use in the legend
legend('topright',c('Transiting','Foraging','Breeding'), fill = cols,bty="n",cex=1.0)
dev.off()


##################Ekman % Frequency####################

breaks<-c(-0.00005,-0.00004,-0.00003,-0.00002,-0.00001,0,0.00001,0.00002,0.00003,0.00004,0.00005,0.00006,0.00007,0.00008)                #to ensure all three bmodes break into equal bins
h<-hist(sstplot$Ekman1,main="SSH for transiting",col="black",las=2,breaks=breaks)         #simple frequency histogram
s<-sum(h$counts)                                   #the counts are the number of observations in each bin, this command sums them (total observations)
l<-length(h$counts)                                 #Length in the total number of bins 
fp1<-numeric(l)                                     #making "l" into a numeric value for use in the for loop
for (i in 1:l) {                                    #For loop which takes the number of observations in each bin and divides it by the total number of observations
  fp1[i]<-(h$counts[i]/s)*100                         #multiplying the number by 100 gives the percentage of the total observations in each bin
}                                                    #You now have % Frequency, instead of just a count

h<-hist(sstplot$Ekman2,main="SSH for transiting",col="black",las=2,breaks=breaks)             #Repeat the process above for the two other categories below
s<-sum(h$counts)
l<-length(h$counts)
fp2<-numeric(l)
for (i in 1:l) {
  fp2[i]<-(h$counts[i]/s)*100
}

h<-hist(sstplot$Ekman3,main="SST for transiting",col="black",las=2,breaks=breaks)
s<-sum(h$counts)
l<-length(h$counts)
fp3<-numeric(l)
for (i in 1:l) {
  fp3[i]<-(h$counts[i]/s)*100
}


##########Ekman % Frequency#################

png(filename = "GrayWhale_EkmanBmodepercent2005.png",  width = 600, height = 600, units = "px", pointsize = 14, bg = "white")
##Labelling the barplot is important as it is necessary to properly label the x axis later on
x2<-barplot( height=fp1, col=rgb(0,0,1,1/4),xlab="",ylab="% Frequency",main="",ylim=c(0,100),yaxt="n",cex.lab=1.0)  # first histogram- Transiting (blue)
box(which="plot",lty="solid")   #puts a box around the whole plot
x<-c("-5.0e-5- -4.0e-5","-4.0e-5- -3.0e-5","-3.0e-5- -2.0e-5","-2.0e-5- -1.0e-5","-1.0e-5-0","0-1.0e-5","1.0e-5-2.0e-5","2.0e-5-3.0e-5","3.0e-5-4.0e-5","4.0e-5-5.0e-5","5.0e-5-6.0e-5","6.0e-5-7.0e-5","7.0e-5- 8.0e-5")  #custom labelling the x axis
labels1<-x
mtext(side = 1, text = "Ekman Upwelling", line = 4)   #line argument allows you to move x label away from the axis
text(x2,par("usr")[3] - 0.25,srt=45,adj=c(1.1,0.7),labels=labels1,xpd=TRUE,cex=0.9)
axis(side=2,at=seq(0,100,20),labels=seq(0,100,20),las=1)
barplot( height=fp2, col=rgb(1,0,0,1/4),add=T,yaxt="n")  # second- foraging  (red)
barplot(height=fp3, col=rgb(0,1,0,1/4),yaxt="n",add=T)  # third - breeding   (blue)
cols<-c(col=rgb(0,0,1,1/4), col=rgb(1,0,0,1/4),col=rgb(0,1,0,1/4))       #binding the colours together to use in the legend
legend('topright',c('Transiting','Foraging','Breeding'), fill = cols,bty="n",cex=1.0)
dev.off()

#############Ekman, only plotting from -1.0e-5 to 2e-5 to elucidate trends##########


breaks<-c(-0.00001,-0.000005,0,0.000005,0.00001,0.000015,0.00002)                #to ensure all three bmodes break into equal bins
Ekman1narrow<-subset(sstplot,Ekman1>=(-0.00001)&Ekman1<=(0.00002))
h<-hist(Ekman1narrow$Ekman1,main="SSH for transiting",col="black",las=2,breaks=breaks)         #simple frequency histogram
s<-sum(h$counts)                                   #the counts are the number of observations in each bin, this command sums them (total observations)
l<-length(h$counts)                                 #Length in the total number of bins 
fp1<-numeric(l)                                     #making "l" into a numeric value for use in the for loop
for (i in 1:l) {                                    #For loop which takes the number of observations in each bin and divides it by the total number of observations
  fp1[i]<-(h$counts[i]/s)*100                         #multiplying the number by 100 gives the percentage of the total observations in each bin
}                                                    #You now have % Frequency, instead of just a count

Ekman2narrow<-subset(sstplot,Ekman2>=(-0.00001)&Ekman2<=(0.00002))
h<-hist(Ekman2narrow$Ekman2,main="SSH for transiting",col="black",las=2,breaks=breaks)             #Repeat the process above for the two other categories below
s<-sum(h$counts)
l<-length(h$counts)
fp2<-numeric(l)
for (i in 1:l) {
  fp2[i]<-(h$counts[i]/s)*100
}

Ekman3narrow<-subset(sstplot,Ekman3>=(-0.00001)&Ekman3<=(0.00002))
h<-hist(Ekman3narrow$Ekman3,main="SST for transiting",col="black",las=2,breaks=breaks)
s<-sum(h$counts)
l<-length(h$counts)
fp3<-numeric(l)
for (i in 1:l) {
  fp3[i]<-(h$counts[i]/s)*100
}


##########Ekman % Frequency#################

png(filename = "GrayWhale_NarrowEkmanBmodepercent2005.png",  width = 600, height = 600, units = "px", pointsize = 14, bg = "white")
##Labelling the barplot is important as it is necessary to properly label the x axis later on
x2<-barplot( height=fp1, col=rgb(0,0,1,1/4),xlab="",ylab="% Frequency",main="",ylim=c(0,100),yaxt="n",cex.lab=1.0)  # first histogram- Transiting (blue)
box(which="plot",lty="solid")   #puts a box around the whole plot
x<-c("-1.0e-5- -0.5e-5","-0.5e-5-0","0- 0.5e-5","0.5e-1.0e-5","1.0e-5-1.5e-5","1.5e-2.0e-5")  #custom labelling the x axis
labels1<-x
mtext(side = 1, text = "Ekman Upwelling", line = 4)   #line argument allows you to move x label away from the axis
text(x2,par("usr")[3] - 0.25,srt=45,adj=c(1.1,0.7),labels=labels1,xpd=TRUE,cex=0.9)
axis(side=2,at=seq(0,100,20),labels=seq(0,100,20),las=1)
barplot( height=fp2, col=rgb(1,0,0,1/4),add=T,yaxt="n")  # second- foraging  (red)
barplot(height=fp3, col=rgb(0,1,0,1/4),yaxt="n",add=T)  # third - breeding   (blue)
cols<-c(col=rgb(0,0,1,1/4), col=rgb(1,0,0,1/4),col=rgb(0,1,0,1/4))       #binding the colours together to use in the legend
legend('topright',c('Transiting','Foraging','Breeding'), fill = cols,bty="n",cex=1.0)
dev.off()

##################Bottom Depth % Frequency####################

breaks<-c(-2500,-2250,-2000,-1750,-1500,-1250,-1000,-750,-500,-250,0)                #to ensure all three bmodes break into 12 equal bins
h<-hist(sstplot$Depth1,main="SSH for transiting",col="black",las=2,breaks=breaks)         #simple frequency histogram
s<-sum(h$counts)                                   #the counts are the number of observations in each bin, this command sums them (total observations)
l<-length(h$counts)                                 #Length in the total number of bins (should be 12)
fp1<-numeric(l)                                     #making "l" into a numeric value for use in the for loop
for (i in 1:l) {                                    #For loop which takes the number of observations in each bin and divides it by the total number of observations
  fp1[i]<-(h$counts[i]/s)*100                         #multiplying the number by 100 gives the percentage of the total observations in each bin
}                                                    #You now have % Frequency, instead of just a count

h<-hist(sstplot$Depth2,main="SSH for transiting",col="black",las=2,breaks=breaks)             #Repeat the process above for the two other categories below
s<-sum(h$counts)
l<-length(h$counts)
fp2<-numeric(l)
for (i in 1:l) {
  fp2[i]<-(h$counts[i]/s)*100
}

h<-hist(sstplot$Depth3,main="SST for transiting",col="black",las=2,breaks=breaks)
s<-sum(h$counts)
l<-length(h$counts)
fp3<-numeric(l)
for (i in 1:l) {
  fp3[i]<-(h$counts[i]/s)*100
}


##########Bottom Depth % Frequency#################

png(filename = "GrayWhale_DepthBmodepercent2005.png",  width = 600, height = 600, units = "px", pointsize = 14, bg = "white")
##Labelling the barplot is important as it is necessary to properly label the x axis later on
x2<-barplot( height=fp1, col=rgb(0,0,1,1/4),xlab="",ylab="% Frequency",main="",ylim=c(0,100),yaxt="n",cex.lab=1.0)  # first histogram- Transiting (blue)
box(which="plot",lty="solid")   #puts a box around the whole plot
x<-c("-2500--2250","-2250--2000","-2000--1750","-1750--1500","-1500--1250","-1250--1000","-1000--750","-750--500","-500--250","-250-0")  #custom labelling the x axis
labels1<-x
mtext(side = 1, text = "Bottom Depth (m)", line = 4)
text(x2,par("usr")[3] - 0.25,srt=45,adj=c(1.1,0.7),labels=labels1,xpd=TRUE,cex=0.9)
axis(side=2,at=seq(0,100,20),labels=seq(0,100,20),las=1)
barplot( height=fp2, col=rgb(1,0,0,1/4),add=T,yaxt="n")  # second- foraging  (red)
barplot(height=fp3, col=rgb(0,1,0,1/4),yaxt="n",add=T)  # third - breeding   (blue)
cols<-c(col=rgb(0,0,1,1/4), col=rgb(1,0,0,1/4),col=rgb(0,1,0,1/4))       #binding the colours together to use in the legend
legend('topleft',c('Transiting','Foraging','Breeding'), fill = cols,bty="n",cex=1.0)
dev.off()


#############Only plotting from -250 depth to elucidate trends##########

breaks<-c(-250,-200,-150,-100,-50,0)                #to ensure all three bmodes break into 12 equal bins
Depth1shallow<-subset(sstplot,Depth1>=-250)
h<-hist(Depth1shallow$Depth1,main="SSH for transiting",col="black",las=2,breaks=breaks)         #simple frequency histogram
s<-sum(h$counts)                                   #the counts are the number of observations in each bin, this command sums them (total observations)
l<-length(h$counts)                                 #Length in the total number of bins (should be 12)
fp1<-numeric(l)                                     #making "l" into a numeric value for use in the for loop
for (i in 1:l) {                                    #For loop which takes the number of observations in each bin and divides it by the total number of observations
  fp1[i]<-(h$counts[i]/s)*100                         #multiplying the number by 100 gives the percentage of the total observations in each bin
}                                                    #You now have % Frequency, instead of just a count

Depth2shallow<-subset(sstplot,Depth2>=-250)
h<-hist(Depth2shallow$Depth2,main="SSH for transiting",col="black",las=2,breaks=breaks)             #Repeat the process above for the two other categories below
s<-sum(h$counts)
l<-length(h$counts)
fp2<-numeric(l)
for (i in 1:l) {
  fp2[i]<-(h$counts[i]/s)*100
}

Depth3shallow<-subset(sstplot,Depth3>=-250)
h<-hist(Depth3shallow$Depth3,main="SST for transiting",col="black",las=2,breaks=breaks)
s<-sum(h$counts)
l<-length(h$counts)
fp3<-numeric(l)
for (i in 1:l) {
  fp3[i]<-(h$counts[i]/s)*100
}


##########Bottom Depth % Frequency#################

png(filename = "GrayWhale_ShallowDepthBmodepercent2005.png",  width = 600, height = 600, units = "px", pointsize = 14, bg = "white")
##Labelling the barplot is important as it is necessary to properly label the x axis later on
x2<-barplot( height=fp1, col=rgb(0,0,1,1/4),xlab="",ylab="% Frequency",main="",ylim=c(0,100),yaxt="n",cex.lab=1.0)  # first histogram- Transiting (blue)
box(which="plot",lty="solid")   #puts a box around the whole plot
x<-c("-250--200","-200--150","-150--100","-100--50","-50-0")  #custom labelling the x axis
labels1<-x
mtext(side = 1, text = "Bottom Depth (m)", line = 4)
text(x2,par("usr")[3] - 0.25,srt=45,adj=c(1.1,0.7),labels=labels1,xpd=TRUE,cex=0.9)
axis(side=2,at=seq(0,100,20),labels=seq(0,100,20),las=1)
barplot( height=fp2, col=rgb(1,0,0,1/4),add=T,yaxt="n")  # second- foraging  (red)
barplot(height=fp3, col=rgb(0,1,0,1/4),yaxt="n",add=T)  # third - breeding   (blue)
cols<-c(col=rgb(0,0,1,1/4), col=rgb(1,0,0,1/4),col=rgb(0,1,0,1/4))       #binding the colours together to use in the legend
legend('topleft',c('Transiting','Foraging','Breeding'), fill = cols,bty="n",cex=1.0)
dev.off()

######################################################################################
#############4 Species SST plot#######################################################
#####################################################################################

###########For Pathfnder 5 day data#############

sstplot<-read.table("4speciesSST.txt",header=TRUE)    #even though it is called sstplot, it contains ALL environment columns (just easier to leave it named this)

breaks<-c(-2,0,2,4,6,8,10,12,14,16,18,20,22,24,26,28,30,32)                #to ensure all three bmodes break into 12 equal bins
h<-hist(sstplot$SSTGW,main="SST for transiting",col="black",las=2,breaks=breaks)         #simple frequency histogram
s<-sum(h$counts)                                   #the counts are the number of observations in each bin, this command sums them (total observations)
l<-length(h$counts)                                 #Length in the total number of bins (should be 12)
fp1<-numeric(l)                                     #making "l" into a numeric value for use in the for loop
for (i in 1:l) {                                    #For loop which takes the number of observations in each bin and divides it by the total number of observations
  fp1[i]<-(h$counts[i]/s)*100                         #multiplying the number by 100 gives the percentage of the total observations in each bin
}                                                    #You now have % Frequency, instead of just a count

h<-hist(sstplot$SSTHB,main="SST for transiting",col="black",las=2,breaks=breaks)             #Repeat the process above for the two other categories below
s<-sum(h$counts)
l<-length(h$counts)
fp2<-numeric(l)
for (i in 1:l) {
  fp2[i]<-(h$counts[i]/s)*100
}

h<-hist(sstplot$SSTFW,main="SST for transiting",col="black",las=2,breaks=breaks)
s<-sum(h$counts)
l<-length(h$counts)
fp3<-numeric(l)
for (i in 1:l) {
  fp3[i]<-(h$counts[i]/s)*100
}

h<-hist(sstplot$SSTBW,main="SST for transiting",col="black",las=2,breaks=breaks)
s<-sum(h$counts)
l<-length(h$counts)
fp4<-numeric(l)
for (i in 1:l) {
  fp4[i]<-(h$counts[i]/s)*100
}

##########SST#################

png(filename = "4Species_SSTpercent.png",bg = "white")

par(mfrow=c(4,1),mar=c(4,4,0.5,2))   #######Each plot will be separate, so trends are easier to see#######

#HB
barplot(height=fp2, col=rgb(0,0,1,1/4),yaxt="n",xlab="",ylim=c(0,50),ylab="%Frequency")  # second- foraging  (red)
box(which="plot",lty="solid")   #puts a box around the whole plot
x<-c("-2-0","0-2","2-4","4-6","6-8","8-10","10-12","12-14","14-16","16-18","18-20","20-22","22-24","24-26","26-28","28-30","30-32")  #labes for the x axis
labels1<-x
text(x2,par("usr")[3] - 0.25,srt=45,adj=c(1,1.5),labels=labels1,xpd=TRUE,cex=1.0) #custom labelling the x-axis
axis(side=2,at=seq(0,50,10),labels=seq(0,50,10),las=1,par(cex.axis=1))    #labelling the y-axis
text(-1,45,labels="Humpback",pos=4)

#FW
barplot(height=fp3, col=rgb(0,0,1,1/4),yaxt="n",xlab="",ylim=c(0,50),ylab="% Frequency")  # third - breeding   (blue)
box(which="plot",lty="solid")   #puts a box around the whole plot
x<-c("-2-0","0-2","2-4","4-6","6-8","8-10","10-12","12-14","14-16","16-18","18-20","20-22","22-24","24-26","26-28","28-30","30-32")  #labes for the x axis
labels1<-x
text(x2,par("usr")[3] - 0.25,srt=45,adj=c(1,1.5),labels=labels1,xpd=TRUE,cex=1.0) #custom labelling the x-axis
axis(side=2,at=seq(0,50,10),labels=seq(0,50,10),las=1,par(cex.axis=1))    #labelling the y-axis
text(-1,45,labels="Fin",pos=4)

#BW
barplot(height=fp4, col=rgb(0,0,1,1/4),yaxt="n",xlab="",ylim=c(0,50),ylab="% Frequency")  # third - breeding   (blue)
box(which="plot",lty="solid")   #puts a box around the whole plot
x<-c("-2-0","0-2","2-4","4-6","6-8","8-10","10-12","12-14","14-16","16-18","18-20","20-22","22-24","24-26","26-28","28-30","30-32")  #labes for the x axis
labels1<-x
text(x2,par("usr")[3] - 0.25,srt=45,adj=c(1,1.5),labels=labels1,xpd=TRUE,cex=1.0) #custom labelling the x-axis
axis(side=2,at=seq(0,50,10),labels=seq(0,50,10),las=1,par(cex.axis=1))    #labelling the y-axis
text(-1,45,labels="Blue",pos=4)

#GW
x2<-barplot( height=fp1, col=rgb(0,0,1,1/4),xlab="",ylab="% Frequency",main="",ylim=c(0,50),yaxt="n")  # first histogram- Transiting (blue)
box(which="plot",lty="solid")   #puts a box around the whole plot
x<-c("-2-0","0-2","2-4","4-6","6-8","8-10","10-12","12-14","14-16","16-18","18-20","20-22","22-24","24-26","26-28","28-30","30-32")  #labes for the x axis
labels1<-x
text(x2,par("usr")[3] - 0.25,srt=45,adj=c(1,1.5),labels=labels1,xpd=TRUE,cex=1.0) #custom labelling the x-axis
axis(side=2,at=seq(0,50,10),labels=seq(0,50,10),las=1,par(cex.axis=1))    #labelling the y-axis
text(-1,45,labels="Gray",pos=4)
mtext(side = 1, text = "Sea Surface Temperature (°C)", line = 2.5,cex=0.7)
dev.off()


###########For Belnded 5 day data (except blue whale, only given pathfinder)#############

sstplot<-read.table("4speciesSST.txt",header=TRUE)    #even though it is called sstplot, it contains ALL environment columns (just easier to leave it named this)

breaks<-c(-2,0,2,4,6,8,10,12,14,16,18,20,22,24,26,28,30,32)                #to ensure all three bmodes break into 12 equal bins
h<-hist(sstplot$SSTGW2,main="SST for transiting",col="black",las=2,breaks=breaks)         #simple frequency histogram
s<-sum(h$counts)                                   #the counts are the number of observations in each bin, this command sums them (total observations)
l<-length(h$counts)                                 #Length in the total number of bins (should be 12)
fp1<-numeric(l)                                     #making "l" into a numeric value for use in the for loop
for (i in 1:l) {                                    #For loop which takes the number of observations in each bin and divides it by the total number of observations
  fp1[i]<-(h$counts[i]/s)*100                         #multiplying the number by 100 gives the percentage of the total observations in each bin
}                                                    #You now have % Frequency, instead of just a count

h<-hist(sstplot$SSTHB2,main="SST for transiting",col="black",las=2,breaks=breaks)             #Repeat the process above for the two other categories below
s<-sum(h$counts)
l<-length(h$counts)
fp2<-numeric(l)
for (i in 1:l) {
  fp2[i]<-(h$counts[i]/s)*100
}

h<-hist(sstplot$SSTFW2,main="SST for transiting",col="black",las=2,breaks=breaks)
s<-sum(h$counts)
l<-length(h$counts)
fp3<-numeric(l)
for (i in 1:l) {
  fp3[i]<-(h$counts[i]/s)*100
}

h<-hist(sstplot$SSTBW,main="SST for transiting",col="black",las=2,breaks=breaks)
s<-sum(h$counts)
l<-length(h$counts)
fp4<-numeric(l)
for (i in 1:l) {
  fp4[i]<-(h$counts[i]/s)*100
}

##########SST#################

png(filename = "4Species_SSTpercent.png",bg = "white")

par(mfrow=c(4,1),mar=c(4,4,0.5,2))   #######Each plot will be separate, so trends are easier to see#######

#HB
barplot(height=fp2, col=rgb(0,0,1,1/4),yaxt="n",xlab="",ylim=c(0,50),ylab="%Frequency")  # second- foraging  (red)
box(which="plot",lty="solid")   #puts a box around the whole plot
x<-c("-2-0","0-2","2-4","4-6","6-8","8-10","10-12","12-14","14-16","16-18","18-20","20-22","22-24","24-26","26-28","28-30","30-32")  #labes for the x axis
labels1<-x
text(x2,par("usr")[3] - 0.25,srt=45,adj=c(1,1.5),labels=labels1,xpd=TRUE,cex=1.0) #custom labelling the x-axis
axis(side=2,at=seq(0,50,10),labels=seq(0,50,10),las=1,par(cex.axis=1))    #labelling the y-axis
text(-1,45,labels="Humpback",pos=4)

#FW
barplot(height=fp3, col=rgb(0,0,1,1/4),yaxt="n",xlab="",ylim=c(0,50),ylab="% Frequency")  # third - breeding   (blue)
box(which="plot",lty="solid")   #puts a box around the whole plot
x<-c("-2-0","0-2","2-4","4-6","6-8","8-10","10-12","12-14","14-16","16-18","18-20","20-22","22-24","24-26","26-28","28-30","30-32")  #labes for the x axis
labels1<-x
text(x2,par("usr")[3] - 0.25,srt=45,adj=c(1,1.5),labels=labels1,xpd=TRUE,cex=1.0) #custom labelling the x-axis
axis(side=2,at=seq(0,50,10),labels=seq(0,50,10),las=1,par(cex.axis=1))    #labelling the y-axis
text(-1,45,labels="Fin",pos=4)

#BW
barplot(height=fp4, col=rgb(0,0,1,1/4),yaxt="n",xlab="",ylim=c(0,50),ylab="% Frequency")  # third - breeding   (blue)
box(which="plot",lty="solid")   #puts a box around the whole plot
x<-c("-2-0","0-2","2-4","4-6","6-8","8-10","10-12","12-14","14-16","16-18","18-20","20-22","22-24","24-26","26-28","28-30","30-32")  #labes for the x axis
labels1<-x
text(x2,par("usr")[3] - 0.25,srt=45,adj=c(1,1.5),labels=labels1,xpd=TRUE,cex=1.0) #custom labelling the x-axis
axis(side=2,at=seq(0,50,10),labels=seq(0,50,10),las=1,par(cex.axis=1))    #labelling the y-axis
text(-1,45,labels="Blue",pos=4)

#GW
x2<-barplot( height=fp1, col=rgb(0,0,1,1/4),xlab="",ylab="% Frequency",main="",ylim=c(0,50),yaxt="n")  # first histogram- Transiting (blue)
box(which="plot",lty="solid")   #puts a box around the whole plot
x<-c("-2-0","0-2","2-4","4-6","6-8","8-10","10-12","12-14","14-16","16-18","18-20","20-22","22-24","24-26","26-28","28-30","30-32")  #labes for the x axis
labels1<-x
text(x2,par("usr")[3] - 0.25,srt=45,adj=c(1,1.5),labels=labels1,xpd=TRUE,cex=1.0) #custom labelling the x-axis
axis(side=2,at=seq(0,50,10),labels=seq(0,50,10),las=1,par(cex.axis=1))    #labelling the y-axis
text(-1,45,labels="Gray",pos=4)
mtext(side = 1, text = "Sea Surface Temperature (°C)", line = 2.5,cex=0.7)
dev.off()

###################################################################################
###############Density Plots for SST 4 species####################################
#################################################################################
# Kernel Density Plot
d <- density(sstplot$SSTHB,na.rm=TRUE) # returns the density data 
e <- density(sstplot$SSTFW,na.rm=TRUE) # returns the density data 
f<- density(sstplot$SSTBW,na.rm=TRUE) 
g<- density(sstplot$SSTGW,na.rm=TRUE) 
par(mfrow=c(2,2))
plot(d,main="",xlab="Sea Surface Temperature (°C)",col="blue") # plots the results
plot(e,main="",xlab="Sea Surface Temperature (°C)",col="green")
lines(f,main="",xlab="Sea Surface Temperature (°C)",col="red")
plot(g,main="",xlab="Sea Surface Temperature (°C)",col="black")


