## Simple Random Sample (SRS)
srs<-read.csv("SRS.csv")

n<-nrow(srs)
max<-max(srs$SIZE)
min<-min(srs$SIZE)
srs.size_m<-mean(srs$SIZE)
srs.size_var<-var(srs$SIZE)
srs.size_se<-sqrt(srs.size_var/n)
CI_upper<-srs.size_m+1.96*srs.size_se
CI_lower<-srs.size_m-1.96*srs.size_se
c(CI_lower,CI_upper)

srs.gender_m<-mean(srs$GENDER)
pf=1-srs.gender_m
srs.gender_se<-sqrt(srs.gender_m*(1-srs.gender_m)/n)
CI_upper<-pf+1.96*srs.gender_se
CI_lower<-pf-1.96*srs.gender_se
c(CI_lower,CI_upper)

srs.sizeC_m<-mean(srs$SIZE_CAT)
srs.sizeC_se<-sqrt(srs.sizeC_m*(1-srs.sizeC_m)/n)
CI_upper<-srs.sizeC_m+1.96*srs.sizeC_se
CI_lower<-srs.sizeC_m-1.96*srs.sizeC_se
c(CI_lower,CI_upper)


## Stratified Sampling
as<-read.csv("appliedscience_S.csv")
arts<-read.csv("arts_S.csv")
cb<-read.csv("commerce&business_S.csv")
sci<-read.csv("science_S.csv")

#calculate sample sizes for each strata
N_as=5491 
N_arts=14541  
N_sci=8496
N_cb=4076
N=N_as+N_arts+N_sci+N_cb

p_as=N_as/N
p_arts=N_arts/N
p_sci=N_sci/N
p_cb=N_cb/N

n=40
n_as=round(p_as*n)
n_arts=round(p_arts*n)
n_sci=round(p_sci*n)
n_cb=round(p_cb*n)

#CLASS SIZE
#applied science
as.size_m<-mean(as$SIZE)
as.size_var<-var(as$SIZE)
as.size_se<-sqrt(as.size_var/n_as)
as.CI_upper<-as.size_m+1.96*as.size_se
as.CI_lower<-as.size_m-1.96*as.size_se
c(as.CI_lower,as.CI_upper)

#arts
arts.size_m<-mean(arts$SIZE)
arts.size_var<-var(arts$SIZE)
arts.size_se<-sqrt(arts.size_var/n_arts)
arts.CI_upper<-arts.size_m+1.96*arts.size_se
arts.CI_lower<-arts.size_m-1.96*arts.size_se
c(arts.CI_lower,arts.CI_upper)

#science
sci.size_m<-mean(sci$SIZE)
sci.size_var<-var(sci$SIZE)
sci.size_se<-sqrt(sci.size_var/n_sci)
sci.CI_upper<-sci.size_m+1.96*sci.size_se
sci.CI_lower<-sci.size_m-1.96*sci.size_se
c(sci.CI_lower,sci.CI_upper)

#commerce&business
cb.size_m<-mean(cb$SIZE)
cb.size_var<-var(cb$SIZE)
cb.size_se<-sqrt(cb.size_var/n_cb)
cb.CI_upper<-cb.size_m+1.96*cb.size_se
cb.CI_lower<-cb.size_m-1.96*cb.size_se
c(cb.CI_lower,cb.CI_upper)

#estimate
size_str_m<-p_as*as.size_m+p_sci*sci.size_m+p_arts*arts.size_m+p_cb*cb.size_m
size_str_var<-p_as^2*as.size_var/n_as+p_sci^2*sci.size_var/n_sci+p_arts^2*arts.size_var/n_arts+p_cb^2*cb.size_var/n_cb
size_str_se<-sqrt(size_str_var)
CI_upper<-size_str_m+1.96*size_str_se
CI_lower<-size_str_m-1.96*size_str_se
c(CI_lower,CI_upper)

#GENDER PROPORTION
#applied science
as.gender_m<-mean(as$GENDER)
as.gender_se<-sqrt(as.gender_m*(1-as.gender_m)/n_as)
as.CI_upper_g<-as.gender_m+1.96*as.gender_se
as.CI_lower_g<-as.gender_m-1.96*as.gender_se
c(as.CI_lower_g,as.CI_upper_g)

#arts
arts.gender_m<-mean(arts$GENDER)
arts.gender_se<-sqrt(arts.gender_m*(1-arts.gender_m)/n_arts)
arts.CI_upper_g<-arts.gender_m+1.96*arts.gender_se
arts.CI_lower_g<-arts.gender_m-1.96*arts.gender_se
c(arts.CI_lower_g,arts.CI_upper_g)

#science
sci.gender_m<-mean(sci$GENDER)
sci.gender_se<-sqrt(sci.gender_m*(1-sci.gender_m)/n_sci)
sci.CI_upper_g<-sci.gender_m+1.96*sci.gender_se
sci.CI_lower_g<-sci.gender_m-1.96*sci.gender_se
c(sci.CI_lower_g,sci.CI_upper_g)

#commerce &business
cb.gender_m<-mean(cb$GENDER)
cb.gender_se<-sqrt(cb.gender_m*(1-cb.gender_m)/n_cb)
cb.CI_upper_g<-cb.gender_m+1.96*cb.gender_se
cb.CI_lower_g<-cb.gender_m-1.96*cb.gender_se
c(cb.CI_lower_g,cb.CI_upper_g)

#estimate
gender_str_m<-p_as*as.gender_m+p_sci*sci.gender_m+p_arts*arts.gender_m+p_cb*cb.gender_m
gender_str_se<-sqrt(p_as^2*as.gender_se^2+p_sci^2*sci.gender_se^2+p_arts^2*arts.gender_se^2+p_cb^2*cb.gender_se^2)
CI_upper_g<-gender_str_m+1.96*gender_str_se
CI_lower_g<-gender_str_m-1.96*gender_str_se
c(CI_lower_g,CI_upper_g)

#GENDER PROPORTION
#applied science
as.sizeC_m<-mean(as$SIZE_CAT)
as.sizeC_se<-sqrt(as.sizeC_m*(1-as.sizeC_m)/n_as)
as.CI_upper_sizeC<-as.sizeC_m+1.96*as.sizeC_se
as.CI_lower_sizeC<-as.sizeC_m-1.96*as.sizeC_se
c(as.CI_lower_sizeC,as.CI_upper_sizeC)

#arts
arts.sizeC_m<-mean(arts$SIZE_CAT)
arts.sizeC_se<-sqrt(arts.sizeC_m*(1-arts.sizeC_m)/n_arts)
arts.CI_upper_sizeC<-arts.sizeC_m+1.96*arts.sizeC_se
arts.CI_lower_sizeC<-arts.sizeC_m-1.96*arts.sizeC_se
c(arts.CI_lower_sizeC,arts.CI_upper_sizeC)

#science
sci.sizeC_m<-mean(sci$SIZE_CAT)
sci.sizeC_se<-sqrt(sci.sizeC_m*(1-sci.sizeC_m)/n_sci)
sci.CI_upper_sizeC<-sci.sizeC_m+1.96*sci.sizeC_se
sci.CI_lower_sizeC<-sci.sizeC_m-1.96*sci.sizeC_se
c(sci.CI_lower_sizeC,sci.CI_upper_sizeC)

#commerce &business
cb.sizeC_m<-mean(cb$SIZE_CAT)
cb.sizeC_se<-sqrt(cb.sizeC_m*(1-cb.sizeC_m)/n_cb)
cb.CI_upper_sizeC<-cb.sizeC_m+1.96*cb.sizeC_se
cb.CI_lower_sizeC<-cb.sizeC_m-1.96*cb.sizeC_se
c(cb.CI_lower_sizeC,cb.CI_upper_sizeC)

#estimate
sizeC_str_m<-p_as*as.sizeC_m+p_sci*sci.sizeC_m+p_arts*arts.sizeC_m+p_cb*cb.sizeC_m
sizeC_str_se<-sqrt(p_as^2*as.sizeC_se^2+p_sci^2*sci.sizeC_se^2+p_arts^2*arts.sizeC_se^2+p_cb^2*cb.sizeC_se^2)
CI_upper_sizeC<-sizeC_str_m+1.96*sizeC_str_se
CI_lower_sizeC<-sizeC_str_m-1.96*sizeC_str_se
c(CI_lower_sizeC,CI_upper_sizeC)
