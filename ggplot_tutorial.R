# install.packages("tidyr")
# install.packages("readxl")
# install.packages("ggplot2")
# install.packages("dplyr")

library(tidyr)
library(readxl)
library(ggplot2)
library(dplyr) 


### SET WORKING DIRECTORY/MAKE PROJECT

### Reading the data
onstein <- read_xlsx('~/Dropbox/AndeanWorkshop/Onstein_data.xlsx',
                     sheet = 'Matrix for analysis')

## Check what is inside the dataset, was it read fine?
head(onstein)

## Check how it was read, is it a column with numbers (aka a quantitative variable?)
onstein$Log_Fruit_length_avg
 #good practice, what data am I missing?

## Make it a quantitative variable that we can work with
onstein$Log_Fruit_length_avg <- as.numeric(onstein$Log_Fruit_length_avg)
is.na(onstein$Log_Fruit_length_avg)

## It is a log data (later we will discuss why sometimes we have to log variables, but for now let's
## put it back in regular scale

fruit_length_average= exp(onstein$Log_Fruit_length_avg)

## Add it to your dataset- Here discuss why it is really good to "add here" instead of modifying your original data.
## Reproducibility practices
onstein<-cbind(onstein, fruit_length_average)
head(onstein)

#### HISTOGRAMS
### discuss what are histograms, how they are built, make sure they know how to read
ggplot(onstein, aes(x=fruit_length_average)) + geom_histogram()


### Changing the looks
ggplot(onstein, aes(x=fruit_length_average))+ geom_histogram(color="darkblue", fill="hotpink")


###  Histograms per discrete variable
ggplot(onstein, aes(x=fruit_length_average, color=Shrub)) + # REMEMBER SHRUB WAS ONE OF OUR COLUMNS
  geom_histogram(fill="white")
# Overlaid histograms
##ggplot(onstein, aes(x=fruit_length_average, color=Shrub)) +
  ##geom_histogram(fill="white", alpha=0.5, position="identity")

## Calculating the mean per group
mu<-onstein %>% group_by(Shrub) %>% 
  summarize(mean_fl=mean(fruit_length_average, na.rm=TRUE),
            .groups = 'drop')
mu

## The dimension of mu, what do you notice??
dim(mu)
 ### (there are NAs this will be annoying later on, so we are going to remove)
mu<-mu[2:3,] ## discuss here how R reads tables and matrices as rows and columns!!!!

## Saving your histogram as an object  and adding the mean we calculated
## WHY WE SAVE P-HISTOGRAM? EXPLAIN
p_histogram<-ggplot(onstein, aes(x=fruit_length_average, color=Shrub)) +
  geom_histogram(fill="white", position="dodge")+
  geom_vline(data=mu, aes(xintercept=mean_fl, color=Shrub),
             linetype="dashed")+
  theme(legend.position="top")
## I want my own colors how do I do this?

p_histogram<- p_histogram+scale_color_manual(values=c("hotpink", "#56B4E9")) #EXPLAIN

## I want to make it publication quality 
p_histogram<- p_histogram+xlab("Average Fruit Length")

p_histogram+theme_classic()


#### DENSITY PLOTS, what are they and why we need them?

ggplot(onstein, aes(x=fruit_length_average)) + geom_density()


###### NOW I'M DOING DENSITY WITHOUT NA

p_density<-ggplot(data=subset(onstein,!is.na(fruit_length_average)), aes(x=fruit_length_average)) + geom_density()

#### Adding the mean but removing those pesky NAs- do one with and one without NAs
mean.fruit=mean(onstein$fruit_length_average,na.rm=TRUE)

p_density+ geom_vline(aes(xintercept=mean.fruit),
              color="blue", linetype="dashed", size=1)

## Make it pretty colors
ggplot(onstein, aes(x=fruit_length_average))+
  geom_density(color="darkblue", fill="lightblue")


# Change density plot line colors by groups
ggplot(onstein, aes(x=fruit_length_average, color=Shrub)) +
  geom_density() + geom_vline(data=mu, aes(xintercept=mean_fl, color=Shrub),
             linetype="dashed")
p_density<-ggplot(onstein, aes(x=fruit_length_average, fill=Shrub)) + geom_density(alpha=0.4)+ geom_vline(data=mu, aes(xintercept=mean_fl, color=Shrub),
                                                                                                              linetype="dashed")
## Adding my color scheme (tolk about coolors here)
p_density<-p_density+scale_fill_manual(values=c("hotpink", "#56B4E9"))

## Making it publication style
p_density<- p_density+ xlab("Average Fruit Length")+theme_classic()


############# Making boxplots 

ggplot(data=onstein, aes(x=fruit_length_average)) + 
  geom_boxplot()

## How do we make it vertical???


## How do we make it by group

#ggplot(onstein, aes(y=fruit_length_average, color=Shrub)) +
  geom_boxplot()

## How do we change the color
ggplot(onstein, aes(x=Shrub,y=fruit_length_average, color=Shrub)) +
  geom_boxplot()+scale_color_manual(values=c("hotpink", "#56B4E9"))


### How do we fill them in
p2<-ggplot(onstein, aes(x=Shrub,y=fruit_length_average, fill=Shrub)) +
  geom_boxplot()

p2+scale_fill_manual(values=c("hotpink", "#56B4E9"))


### Eliminating those pesky NAs from plots

p_boxplot<-ggplot(data=subset(onstein, !is.na(fruit_length_average)), aes(x=Shrub,y=fruit_length_average, color=Shrub)) +
  geom_boxplot()+scale_color_manual(values=c("hotpink", "#56B4E9"))

## How do we add the mean?

p_boxplot<-p_boxplot + geom_point(data=mu,aes(x=Shrub,y=mean_fl),shape=23, size=4)

## Good labels

p_boxplot<-p_boxplot+xlab("Shrub status")+ylab("Average fruit length")

## How do we add all the sample
p_boxplot<-p_boxplot + geom_jitter(shape=16, position=position_jitter(0.2))

## All pretty
p_boxplot<-p_boxplot+theme_classic()


##### SCATTERPLOTS
onstein$Log_Fruit_width_avg
onstein$Log_Fruit_width_avg <- as.numeric(onstein$Log_Fruit_width_avg)

fruit_width_average= exp(onstein$Log_Fruit_width_avg)

onstein<-cbind(onstein, fruit_width_average)

head(onstein)






ggplot(onstein, aes(x=fruit_length_average, y=fruit_width_average))+ geom_point()

ggplot(onstein, aes(x=fruit_length_average, y=fruit_width_average)) + 
  geom_point()+
  geom_smooth(method=lm)

ggplot(onstein, aes(x=fruit_length_average, y=fruit_width_average, color=Shrub)) +
  geom_point() + 
  geom_smooth(method=lm, se=FALSE, fullrange=TRUE)
