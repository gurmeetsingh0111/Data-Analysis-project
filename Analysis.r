library(readr)
library(ggplot2)
library(dplyr)


Details <- read_csv("Downloads/IndividualDetails.csv")
  

data = Details[,c(1,3,4,5,8,9,10,11)]

d_subset = na.omit(data)
write.csv(d_subset, file="file.csv")

d_subset<-as.data.frame(d_subset)
d_subset$diagnosed_date<-as.Date(d_subset$diagnosed_date,'%d/%m/%Y')

fm<-table(d_subset$gender)
totalfemale<-fm[1]
totalmale<-fm[2]

slices <- c(fm[1],fm[2])
pct <- round(slices/sum(slices)*100)
lbls <- c("Female", "Male")
lbls <- paste(lbls, pct) # add percents to labels
lbls <- paste(lbls,"%",sep="") # ad % to labels
pie(slices,labels = lbls, col=rainbow(length(lbls)),radius=1,border=1,
    main="Gender % of affected people")

age<-read_csv("Downloads/AgeGroupDetails.csv")
age<-as.data.frame(age)
lbls<-c("0-9","10-19","20-29","30-39","40-49","50-59","60-69","70-79",">=80")
pct<-c(age$Percentage)
lbls<-paste(lbls,pct,sep = "->")
pie(age$TotalCases,labels=lbls,col=rainbow(length(age$AgeGroup)),radius=1.2,border=1,main="Age group wise distribution")

status<-table(d_subset$current_status)
status
barplot(status,col=c("red","orange","green"),xlab = "Current Status",ylim=c(0,1000),ylab = "No. of patients",main = "Status of patients")
sum(status)

spect<-round(status/sum(status)*100)
lbls<-c("Deceasead","Hospitalized","Recovered")
lbls<-paste(lbls,spect,sep = "->")
lbls<-paste(lbls,"%",sep = "")
pie(status,labels=lbls,col=rainbow(3),radius=1,border=1,main="Condition of people")

recovered1<-filter(d_subset,d_subset$gender=='M' & d_subset$current_status=="Recovered")
recovered2<-filter(d_subset,d_subset$gender=='F' & d_subset$current_status=="Recovered")
rc1<-as.integer(count(recovered1))
rc2<-as.integer(count(recovered2))
deceased1<-filter(d_subset,d_subset$gender=='M' & d_subset$current_status=="Deceased")
deceased2<-filter(d_subset,d_subset$gender=='F' & d_subset$current_status=="Deceased")
dc1<-as.integer(count(deceased1))
dc2<-as.integer(count(deceased2))

c1<-c(rc1,rc2)
c1
c2<-c(dc1,dc2)
c2
rd<-c("Males","Females")
barplot(c1, main="People Recovered",names.arg = rd,
        ylab="no of patients",ylim =c(0,50), col=c("darkblue","red"),
         )
barplot(c2, main="People Deceased",names.arg = rd,
        ylab="no of patients",ylim =c(0,50), col=c("darkblue","red"),
)

pfr<-round((rc2/totalfemale)*100)
pmr<-round((rc1/totalmale)*100)
pct <- c(pmr,pfr)
lbls <- c("Males Recovered", "Females Recovered")
lbls <- paste(lbls, pct,sep="->") # add percents to labels
lbls <- paste(lbls,"%",sep="") # ad % to labels
pie(pct,labels = lbls, col=rainbow(length(lbls)),
    main="Recovery percenatge of females vs males")

pfd<-round((dc2/totalfemale)*100)
pmd<-round((dc1/totalmale)*100)
pct <- c(pfd,pmd)
lbls <- c("Females Deceased", "Males Deceased")
lbls <- paste(lbls, pct,sep="->") # add percents to labels
lbls <- paste(lbls,"%",sep="") # ad % to labels
pie(pct,labels = lbls, col=rainbow(length(lbls)),
    main="Deceased percenatge of females vs males")

q1<-filter(d_subset,d_subset$gender=='M' & d_subset$current_status=="Hospitalized")
q2<-filter(d_subset,d_subset$gender=='F' & d_subset$current_status=="Hospitalized")
qc1<-as.integer(count(q1))
qc1
qc2<-as.integer(count(q2))
qc2
q<-c(qc2,qc1)
pfq<-round((qc2/totalfemale)*100)
pmq<-round((qc1/totalmale)*100)
pct <- c(pfq,pmq)
lbls <- c("Females Hospitilized", "Males Hospitilized")
lbls <- paste(lbls, pct,sep="->") # add percents to labels
lbls <- paste(lbls,"%",sep="") # ad % to labels
pie(pct,labels = lbls, col=rainbow(length(lbls)),
    main="Hospitilized percenatge of females vs males")

ad<- d_subset %>%
  group_by(diagnosed_date)%>%
  summarise(n_distinct(id))
ad
colnames(ad)<-c("date","cases")

ad$date<-as.Date(ad$date,'%d/%m/%Y')
require(ggplot2)
ggplot(data = ad,aes(date,cases))+geom_line()+labs(title = "Cases every day")

m=max(ad[,2])
r=0
for(i in 1:nrow(ad))
  if(ad[i,2]!=m)
    {r=r+1}else{break}
print("Maximum cases on a single day were on ")
print(ad[r+1,1])
ad[r+1,2]

State<- d_subset %>%
  group_by(detected_state)%>%
  summarise(n_distinct(id))
colnames(State)<-c("state","cases")

barplot(State$cases,col=rainbow(29),names.arg = State$state,las=2,ylab = "No. of patients",main = "No. of Patient State wise")

nation=d_subset%>%
  group_by(nationality)%>%
  summarise(n_distinct(id))
colnames(nation)<-c("nationality","cases")
row_to_keep = c(FALSE,FALSE, TRUE, TRUE, TRUE, TRUE, TRUE,TRUE, TRUE)
nation = nation[row_to_keep,]
barplot(nation$cases,col=rainbow(8),names.arg = nation$nationality,las=2,ylab = "No. of patients",main = "No. of NRI Patient")


tdeaths=0
fdeaths=0
mdeaths=0
for(i in 1:nrow(d_subset)){
  if(d_subset[i,7]=="Deceased")
    tdeaths=tdeaths+1
}

for(i in 1:nrow(d_subset)){
  if(d_subset[i,7]=="Deceased"&&d_subset[i,4]=="F")
    fdeaths=fdeaths+1
  
}
for(i in 1:nrow(d_subset)){
  if(d_subset[i,7]=="Deceased"&&d_subset[i,4]=="M")
    mdeaths=mdeaths+1
}
mdeaths
fdeaths
tdeaths
#fatality rate
fratefemale=fdeaths/totalfemale*100
fratefemale
fratemale=mdeaths/totalmale*100
fratemale
frate=tdeaths/1047*100
frate
rate<-c(fratefemale,fratemale)
lbls<-c("Female fatality rate","Male fatality rate")
pct<-round(rate)
lbls<-paste(lbls,pct,sep = "->")
lbls<-paste(lbls,"%",sep = "")
pie(rate,labels=lbls,col=rainbow(2),radius=1,border=1,main="Gender wise Fatality rate")
