#Applied Data Analysis Project
#Erin Readling Fall 2023

##################################################
#Load in Project Data
##################################################
load("/Volumes/courses/QAC/qac201/Studies and Codebooks/GSS/Data/gss_2021.RData")

#Variables I'm interested in

#spkhomo - Should an individual who declares himself a homosexual be allowed to speak in public (1=yes, 2=no)
#coinc - Household income (Since this variable is based on categorical data income is not continuous but based on categorical mid-points and imputations.I=Not Applicable)
#degree- Degrees held by respondent (0=less than high school,1=high school,2=associates/junior college, 3=bachelors, 4=graduate)
#sexnow1- Sex of Respondent (1=male, 2=female, 3=transgender, 4=non of these)
#fund- Fundamentalism/liberalism of respondents religion (1=fundamentalist, 2=moderate, 3=liberal)

#Create subset of relevant variables
var.to.keep<-c("spkhomo","coninc","degree","sexnow1","fund")
SUBDATA<-gss2021[,var.to.keep]

#Load in necessary libraries
library(ggplot2)
library(descr)
install.packages("wesanderson")
library(wesanderson)

######################################
#Data Management and Frequency tables
######################################

#freq(SUBDATA$spkhomo)
#freq(SUBDATA$coninc)
#summary(SUBDATA$coninc)
#freq(data_no_NA$income)
#freq(SUBDATA$degree)
#freq(SUBDATA$sex)
#freq(SUBDATA$fund)

# Make a new variable called homophobia and set it to "yes" if someone is homophobic and "no" if they are not
SUBDATA$homophobia[SUBDATA$spkhomo == 1] <-"No"
SUBDATA$homophobia[SUBDATA$spkhomo == 2] <-"Yes"

# Make a new variable which shows female or male for sex
SUBDATA$sex1[SUBDATA$sexnow1 == 1] <-"Male"
SUBDATA$sex1[SUBDATA$sexnow1 == 2] <-"Female"
SUBDATA$sex1[SUBDATA$sexnow1 == 3] <-"Transgender"
SUBDATA$sex1[SUBDATA$sexnow1 == 4] <-"None of these"

# Make new variable with descriptions of highest degree i.e. "less than high school"
SUBDATA$highestdegree[SUBDATA$degree==0]<-"< high school"
SUBDATA$highestdegree[SUBDATA$degree==1]<-"high school"
SUBDATA$highestdegree[SUBDATA$degree==2]<-"associates"
SUBDATA$highestdegree[SUBDATA$degree==3]<-"bachelors"
SUBDATA$highestdegree[SUBDATA$degree==4]<-"graduate"

#make highest degree an ordered factor
SUBDATA$highestdegree<-factor(SUBDATA$highestdegree, levels=c("< high school","high school", "associates", "bachelors", "graduate"),ordered=TRUE)

#make a new variable with descriptions of fundamentalist level.
SUBDATA$fundamentalistlvl[SUBDATA$fund==1]<-"fundamentalist"
SUBDATA$fundamentalistlvl[SUBDATA$fund==2]<-"moderate"
SUBDATA$fundamentalistlvl[SUBDATA$fund==3]<-"liberal"

#make a new variable which makes income a categorical variable (0-Q1, Q1-Median, Median-Q2, >Q2)
SUBDATA$income[SUBDATA$coninc<=21840]<- "< $21,840"
SUBDATA$income[SUBDATA$coninc<=45360 & SUBDATA$coninc >21840]<- "$21,840 - $45,360"
SUBDATA$income[SUBDATA$coninc<=67200 & SUBDATA$coninc > 45360]<- "$45,360 - $67,200"
SUBDATA$income[SUBDATA$coninc>67200]<- "> $67,200"

#make income an ordered factor
SUBDATA$income<-factor(SUBDATA$income, levels=c("< $21,840","$21,840 - $45,360","$45,360 - $67,200","> $67,200"),ordered=TRUE)

#make fundamentalistlvl an ordered factor
SUBDATA$fundamentalistlvl<-factor(SUBDATA$fundamentalistlvl, levels=c("fundamentalist","moderate","liberal"),ordered=TRUE)

#freq(SUBDATA$highestdegree)
#freq(SUBDATA$fundamentalistlvl)
#freq(SUBDATA$homophobia)

# Make a new variable called test_homo and set it equal to 1 if someone is homophobic and 0 if they are not. Remove N/As
SUBDATA$homo[SUBDATA$spkhomo == 1] <-0
SUBDATA$homo[SUBDATA$spkhomo == 2] <-1

#flag responses with missing values for fundamentalism and homopohobia
MISSING <- is.na(SUBDATA$homo) |
  is.na(SUBDATA$fundamentalistlvl) |
  is.na(SUBDATA$sex1) |
  is.na(SUBDATA$highestdegree)|
  is.na(SUBDATA$coninc)

#count # rows marked for deletion
sum(MISSING)

#create new database with only complete responses
data_no_NA <- subset(SUBDATA, subset = !MISSING)

############################
#Data visualizations
############################

graph_homophobia<-na.omit(SUBDATA[,c("homophobia")])
graph_fundamentalistlvl<-na.omit(SUBDATA[,c("fundamentalistlvl")])
graph_highestdegree<-na.omit(SUBDATA[,c("highestdegree")])
graph_familyincome<-na.omit(SUBDATA[,c("coninc")])
graph_sex1<-na.omit(SUBDATA[,c("sex1")])

# ggplot(data=graph_homophobia)+
#   geom_bar(aes(x=homophobia))+
#   ggtitle("Homophobia")+
#   xlab("Should a self-proclaimed gay man be able to give a speech in a public setting?")
# 
# ggplot(data=graph_fundamentalistlvl)+
#   geom_bar(aes(x=fundamentalistlvl))+
#   ggtitle("Religious Identification")+
#   xlab("Fundamentalist Level of Respondent's Religion")
# 
# ggplot(data=graph_highestdegree)+
#   geom_bar(aes(x=highestdegree))+
#   ggtitle("Education Level")+
#   xlab("Respondent's Highest Degree of Education")
# 
# ggplot(data=graph_sex1)+
#   geom_bar(aes(x=sex1))+
#   ggtitle("Sex of Respondent")+
#   xlab("Sex")
# 
# ggplot(data=graph_familyincome)+
#   geom_histogram(aes(x=coninc))+
#   ggtitle("Respondent's Yearly Family Income")+
#   xlab("Income")
  
###################
#bivariate graphing
###################

#graph religious identification vs homophobia

require(ggplot2)
# ggplot(data = data_no_NA, aes(x = fundamentalistlvl, fill = homophobia)) +
#   geom_bar(position = "fill") + ylab("proportion") +xlab("religious identification") +
#   ggtitle("Religious Identification vs. Homophobia")

ggplot(data=data_no_NA)+
  stat_summary(aes(x=fundamentalistlvl, y=homo),
               fun="mean", geom="bar", fill="lightblue",
               color="black")+
  ylab("Proportion that exhibit homophobia")

tab1 <- table(data_no_NA$homo, data_no_NA$fundamentalistlvl)
tab1_colProp <- prop.table(tab1, 2) # column proportions
tab1_colProp

#graph sex vs homophobia
ggplot(data = data_no_NA, aes(x = sex1, fill = homophobia)) +
  geom_bar(position = "fill") + ylab("Proportion") +xlab("Sex")

#################################
#Multivariate Graphing
###############################

#graphing fundamentalism vs homophobia with degree

ggplot(data=data_no_NA)+
  stat_summary(aes(x=fundamentalistlvl, fill=highestdegree, y=homo),
               fun="mean", geom="bar", position="dodge", alpha=0.4)+
  ylab("Proportion that exhibit homophobia")+
  ggtitle("Proportion of Subjects who exhibit homophobia based on religious identification")

#graphing fundamentalism vs homophobia with income

ggplot(data=data_no_NA)+
  stat_summary(aes(x=fundamentalistlvl, fill=income, y=homo),
               fun="mean", geom="bar", position="dodge", alpha=0.4)+
  ylab("Proportion that exhibit homophobia")+
  ggtitle("Proportion of Subjects who exhibit homophobia based on religious identification")

########################################
#Statistical Analysis
#######################################
#Performing Chi Squared Analysis (sex vs. Homophobia)

myChi5 <- chisq.test(data_no_NA$homophobia, data_no_NA$sex1) 
myChi5 

Observed_table5<-myChi5$observed

prop.table(myChi5$observed, 2)

source("https://raw.githubusercontent.com/PassionDrivenStatistics/R/master/ChiSquarePostHoc.R")
chisq.post.hoc(Observed_table5, popsInRows=FALSE, control="bonferroni")

#Performing Chi Squared Analysis (Fundamentalism vs Homophobia)
myChi <- chisq.test(data_no_NA$homophobia, data_no_NA$fundamentalistlvl) 
myChi 

Observed_table<-myChi$observed

prop.table(myChi$observed, 2)

source("https://raw.githubusercontent.com/PassionDrivenStatistics/R/master/ChiSquarePostHoc.R")
chisq.post.hoc(Observed_table, popsInRows=FALSE, control="bonferroni")

#Performing Chi Squared Analysis (Degree vs. Homophobia)

myChi2 <- chisq.test(data_no_NA$homophobia, data_no_NA$highestdegree) 
myChi2 

Observed_table2<-myChi2$observed

prop.table(myChi2$observed, 2)

source("https://raw.githubusercontent.com/PassionDrivenStatistics/R/master/ChiSquarePostHoc.R")
chisq.post.hoc(Observed_table2, popsInRows=FALSE, control="bonferroni")

data_no_NA$fundamentalistlvl<- as.character(data_no_NA$fundamentalistlvl)
data_no_NA$highestdegree<- as.character(data_no_NA$highestdegree)

#Performing Logistic Regression (Fundamentalism vs. Homophobia)
my.logreg <- glm(homo ~ fundamentalistlvl, data = data_no_NA, family = "binomial")
summary(my.logreg)  # for p-values 
exp(my.logreg$coefficients)  # for odds ratios 
exp(confint(my.logreg))  # for confidence intervals on the odds ratios

#graph logistic regression

graphdata<-expand.grid(fundamentalistlvl=c("fundamentalist","moderate","liberal")) #Fill in with interesting values of Explanatory1

graphdata<-cbind(graphdata, predict(my.logreg, newdata=graphdata, type="link", se=TRUE))

graphdata<-cbind(graphdata, PredictedProb=plogis(graphdata$fit),
                 LL=plogis(graphdata$fit-1.96*graphdata$se.fit),
                 UL=plogis(graphdata$fit+1.96*graphdata$se.fit))

ggplot(data=graphdata)+
  geom_errorbar(aes(x=fundamentalistlvl, y=PredictedProb, ymin=LL, color=fundamentalistlvl, ymax=UL),width=0.3, size=2)+
  geom_point(aes(x=fundamentalistlvl, y=PredictedProb, color=fundamentalistlvl), size=3)

#Performing Logistic Regression (Fundamentlism vs. Homophobia) wrt degree
my.logreg2<-glm(homo~fundamentalistlvl+highestdegree, family="binomial", data=data_no_NA)
summary(my.logreg2)
exp(my.logreg2$coefficients)  # for odds ratios 

#graphing
graphdata2<-expand.grid(fundamentalistlvl=c("fundamentalist","moderate","liberal"),
                        highestdegree=c("< high school","high school","associates", "bachelors", "graduate"))

graphdata2<-cbind(graphdata2, predict(my.logreg2, newdata=graphdata2, type="link", se=TRUE))

graphdata2<-cbind(graphdata2, PredictedProb=plogis(graphdata2$fit),
                  LL=plogis(graphdata2$fit-1.96*graphdata2$se.fit),
                  UL=plogis(graphdata2$fit+1.96*graphdata2$se.fit))
graphdata2$highestdegree=as.factor(graphdata2$highestdegree)
seep=ggplot(data=graphdata2)+
  geom_errorbar(aes(x=fundamentalistlvl, y=PredictedProb, color=highestdegree, ymin=LL, ymax=UL),width=0.5, size=2, position=position_dodge(width=0.6))+
  geom_point(aes(x=fundamentalistlvl, y=PredictedProb, color=highestdegree,group=as.factor(highestdegree)), size=3, position=position_dodge(width=0.6))+
  scale_fill_manual(values=wes_palette(n=5, name="Moonrise3"))+
  xlab("Religous Identification")+
  ylab("Predicted Probability of Homophobia")
seep+scale_color_brewer(palette="Set3")

#Performing Logistic Regression (Fundamentlism vs. Homophobia) wrt income
my.logreg3<-glm(homo~fundamentalistlvl+income, family="binomial", data=data_no_NA)
summary(my.logreg3)
exp(my.logreg3$coefficients)  
#graphing
graphdata3<-expand.grid(fundamentalistlvl=c("fundamentalist","moderate","liberal"),
                        income=c("< $21,840","$21,840 - $45,360","$45,360 - $67,200","> $67,200"))

graphdata3<-cbind(graphdata3, predict(my.logreg3, newdata=graphdata3, type="link", se=TRUE))

graphdata3<-cbind(graphdata3, PredictedProb=plogis(graphdata3$fit),
                  LL=plogis(graphdata3$fit-1.96*graphdata3$se.fit),
                  UL=plogis(graphdata3$fit+1.96*graphdata3$se.fit))

graphdata3$income=as.factor(graphdata3$income)

ggplot(data=graphdata3)+
  geom_errorbar(aes(x=fundamentalistlvl, y=PredictedProb, color=income, ymin=LL, ymax=UL),width=0.5, size=2, position=position_dodge(width=0.6))+
  geom_point(aes(x=fundamentalistlvl, y=PredictedProb, color=as.factor(income),group=as.factor(income)), size=3, position=position_dodge(width=0.6))+
  xlab("Religious Identification")+
  ylab("Predicted Probability of Homophobia")

