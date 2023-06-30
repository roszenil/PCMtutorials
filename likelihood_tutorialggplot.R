## loading our libraries
library(dplyr)

### Reading the data
bird_alarms <- read.csv("birdalarms.csv")
head(bird_alarms)

p_scatter<-ggplot(bird_alarms, aes(x=Forage,y=Response, color=Forage))+geom_point()+theme_classic()+ylab("Seconds until unfreezing")


### The likelihood function
likelihood.function<- function(parameter, observations){
  probabilities <-dexp(observations, rate=1/parameter,log=FALSE)
  L <- prod(probabilities)
  return(-L)
}


## The maximum likelihood with two observations X=2,10
mle<-(2+10)/2

##### Optimization for likelihood
likelihood.optimization <- optimize(f=likelihood.function, interval=c(0,10), observations=c(2,10))
likelihood.optimization

#### dead-leaf gleaners
dl.forager <- bird_alarms %>% filter(Forage == "DL") %>% pull(Response)
dl.forager

### flycatchers
f.forager <- bird_alarms %>% filter(Forage =="F") %>% pull(Response) 
f.forager

### Optimization for likelihood of dead-leaf gleaners
dl.optimization <- optimize(f=likelihood.function, interval=c(10,30), observations=dl.forager)
dl.optimization
str(dl.optimization)

mle.dl <- dl.optimization$minimum
likelihoodval.dl <- -dl.optimization$objective
likelihoodval.dl


### Optimization for likelihood of flycatchers 
mle.f <- mean(f.forager)
mle.f
f.optimization<-optimize(f=likelihood.function, interval=c(0,10), observations=f.forager)
f.optimization
likelihoodval.f <- -f.optimization$objective



### Plotting the Likelihood and the relative likelihood functions

parameter.vals<-seq(0.0001,50,0.01) #creating an interval for possible values for the likelihood
long<-length(parameter.vals)

p.likelihooddl<-rep(0,long)
for (i in 1:long){
  p.likelihooddl[i]<- -likelihood.function(parameter.vals[i],observations=dl.forager)
}

relative_likelihood= p.likelihooddl/likelihoodval.dl

dl_results<-data.frame(forage=rep("DL",long), theta=parameter.vals, likelihood=p.likelihooddl, relative_likelihood)

p_likely_dl<-ggplot(dl_results, aes(x=theta,y=likelihood))+geom_line(color="blue")+theme_classic()

p_rellikely_dl<-ggplot(dl_results, aes(x=theta,y=relative_likelihood))+geom_line(color="blue")+theme_classic()+geom_vline(xintercept=mle.dl, linetype="dashed", color="hotpink")

p_rellikely_dl



### Plotting the likelihood of flycatcher
p.likelihoodf<-rep(0,long)
for (i in 1:long){
  p.likelihoodf[i]<- -likelihood.function(parameter.vals[i],observations=f.forager) # Remeber is negative so we need to add a sign
}


relative_likelihood= p.likelihoodf/likelihoodval.f

f_results<-data.frame(forage=rep("F",long), theta=parameter.vals, likelihood=p.likelihoodf, relative_likelihood)

p_likely_f<-ggplot(f_results, aes(x=theta,y=likelihood))+geom_line(color="red")+theme_classic()

p_rellikely_f<-ggplot(f_results, aes(x=theta,y=relative_likelihood))+geom_line(color="red")+theme_classic()+geom_vline(xintercept=mle.f, linetype="dashed", color="darkblue")

p_rellikely_f


### All together to test for differences

results<-rbind(dl_results,f_results)

p_all<-ggplot(results,aes(x=theta,y=relative_likelihood,group=forage))+geom_line(aes(color=forage,linetype=forage))+theme_classic()

p_all


p_all<- p_all+scale_color_manual(values=c("blue","red"))+geom_vline(xintercept=mle.f, linetype="dashed", color="darkblue")+geom_vline(xintercept=mle.dl, linetype="dashed", color="hotpink")

