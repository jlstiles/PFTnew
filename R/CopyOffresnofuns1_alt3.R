#' @export
date.fill = function(date)
{
  # datep = c(0,4,NA,NA,NA,NA,98)
  na = which(is.na(date)==TRUE)
  m = length(na)
  n = length(date)
  max(na)==n
  if (max(na)==n)
  {
    a=1
    x=1
    while (x>0) {
      if (a==m) {a=a+1;break}
      else {x = ifelse(na[(m-a)]==(na[m]-a),1,0)
      a=a+1
      }}
    a=a-1
    #' #   date[na[(m-a+1):m]]=vapply(1:a,FUN = function(x) date[n-a]+180*x,FUN.VALUE=4)
    #' # date
    #' # na
    #' # c(m,n,a)
    #' 

    na = which(is.na(date[1:(n-a)])==TRUE)
    if (length(na)==0) date=date
    else
    {
      m = length(na)
      n = length(date[1:(n-a)])
      h = rep(1,m)

      for (x in 1:m)
      {
        if (x==1) h[x]=1
        else {h[x]=ifelse(na[x]==(na[(x-1)]+1),h[x-1],h[x-1]+1)}
      }
      for(a in 1:max(h)){
        timediff = date[na[max(which(h==a))]+1]-date[na[min(which(h==a))]-1]
        date[na[which(h==a)]]=vapply(1:length(which(h==a)),FUN = function(x) {
          round(date[na[min(which(h==a))]-1]+timediff*x/(length(which(h==a))+1),0)}, FUN.VALUE=7)}
    }
  }else
  {
    h = rep(1,m)
    for (x in 1:m)
    {
      if (x==1) h[x]=1
      else {h[x]=ifelse(na[x]==(na[(x-1)]+1),h[x-1],h[x-1]+1)}
    }
    for(a in 1:max(h)){
      timediff = date[na[max(which(h==a))]+1]-date[na[min(which(h==a))]-1]
      date[na[which(h==a)]]=vapply(1:length(which(h==a)),FUN = function(x) {
        round(date[na[min(which(h==a))]-1]+timediff*x/(length(which(h==a))+1),0)}, FUN.VALUE=7)}}
  return(date)
}

#' @export
avebtwndate = function(dates,date,measure)
{
  n = length(dates)
  aves = vapply(1:(n-1),FUN = function(x) mean(measure[date<=dates[x+1]&date>=dates[x]],na.rm=TRUE),
                FUN.VALUE=4)
  aves[n]=NA
  return(aves)
}

#' @export
bintimes = function(df,prefixes)
{
  datecols = grep(prefixes[1],colnames(df))
  indices = vapply(1:length(datecols),FUN = function(x) as.numeric(all(is.na(df[,datecols[x]])==TRUE)),
                   FUN.VALUE = 2)
  indices = which(indices==0)
  m = vapply(prefixes,FUN = function(x) grep(x,colnames(df))[indices],
             FUN.VALUE = rep(1,length(indices)))
  m = as.data.frame(m)
  useful =apply(m,2,FUN = function(vec)
  {
    vapply(1:length(vec),FUN = function(x) max(df[,vec[x]],na.rm=TRUE), FUN.VALUE=3)
  })
  useful = as.data.frame(useful)
  for (a in 1:length(prefixes))
  {
    h = grep(prefixes[a],colnames(df))
    df = df[-h]
  }
  df[,prefixes]=useful
  m = nrow(useful)
  df$times = rep(NA,m)
  if (m>1)
  {
    df$times[1]=0
    for (a in 2:m)
    {
      df$times[a]=df$times[a-1]+round((df$date[a]-df$date[a-1])/180)
    }
  } else {df$times[1] = 0}
  return(df)
}

#' @export
fill = function(x,v,col,others)
{
  # make a blank dataframe with a row for each element in v
  pete= as.data.frame(matrix(rep(NA,(ncol(x)*length(v))),ncol=ncol(x)))
  # retain the colnames of the dataframe--we are just addding blank
  # rows for missing visits
  colnames(pete)=colnames(x)
  # get row numbers where the visitnumber matches one of the prescribed visits
  visitrows = vapply(x[,col],FUN = function(y) which(v==y), FUN.VALUE=2)
  # Fill in what you got, otherwise it is an NA--handles factors annoyance
  for(a in 1:ncol(x))
  {
    if (is.factor(x[,a])==TRUE)
    {
      pete[,a]=as.factor(rep(NA,length(v)))
      levels(pete[,a])=levels(x[,a])
      pete[,a][visitrows]=x[,a]
    }
    else {
      pete[,a][visitrows]=x[,a]
    }
  }
  # Make the visitcolumn reflect the visitnumber
  pete[,col]=v
  #repeat race info, sex etc--anything in others
  for (a in 1:length(others))
  {
    pete[,others[a]]=min(x[,others[a]])
  }
  return(pete)
}

#' @export
smash = function(df,prefix)
{
  VISIT = c(1,6,12,18,24,30,36,42,48,54,101,102,103,104)
  # df=subset(df.morgan,ID==6)
  timept = vapply(df$VISITNUM,FUN = function(x) which(VISIT==x),FUN.VALUE=2)
  # prefix="pretime"

  lab = grep(prefix,colnames(df))
  keep = lab[timept]
  df[,prefix] = rep(NA,nrow(df))
  df[,prefix]=vapply(keep,FUN = function(x){if (all(is.na(df[,x])==TRUE)){NA} else {max(df[,x],na.rm=TRUE)}},FUN.VALUE=1)
  return(df)
}

#' @export
prepare = function(x,names,censornames,exposure,outcome)
{
  # 
  x=df.morgan
  names=names
  censornames=censornames
  exposure=exposure
  outcome=outcome
  # endtime=T
  # limit main dataframe to names selected
  df.morganall = x[,names]
  
  # perpare twoweek illness for time varying

  df.morganall = ddply(df.morganall,.(id),.fun = function(x)
  {
    # x = subset(df.morganPAH,id==25)
    date = x[,"date"]
    age = x[,"ageV"]
    if (all(is.na(age)==FALSE)) {
      return(x)
    } else {
      age.na = which(is.na(age)==TRUE)
      for (a in 1:length(age.na))
      {
        age[age.na[a]]=age[age.na[a]-1]+(date[age.na[a]]-date[age.na[a]-1])/365
      }
      x[,"ageV"][age.na]=age[age.na]
      return(x)
    }})
  # subset(df.morganall,id%in%throwaways)
  # length(unique(df.morganall$id))
  # get rid of those who never showed and got no initial measurements
  # subset(df.morganall,select=c('height','id'))

    df.morganall = ddply(df.morganall,.(id),.fun = function(x)
    {
      # x=subset(df.morganall,id==226)
      if (all(is.na(x[,outcome]))) return(x) else {
      max.oc.vis = max(which(!is.na(x[,outcome])))-1
      # max.oc.vis = min(max.oc.vis,4)
      for(a in 1:max.oc.vis){
      if (is.na(x[,outcome][1]))
      {
        x[1:16,names!="times"]=x[2:17,names != "times"]
        x[17,"date"]=x[17,"date"]+180
        x[17,"ageV"]=x[17,"ageV"]+.5
      }}
      return(x)
    }})

    noshows = vapply(unique(df.morganall$ID) ,
                     FUN=function(x) is.na(subset(df.morganall,ID==x&times==0)[,outcome]),
                     FUN.VALUE = TRUE)
    
    
    keepers = unique(df.morganall$ID)[!noshows]
    discards = unique(df.morganall$ID)[noshows]

    df.morganall = df.morganall[df.morganall$ID%in%keepers,]  

#     df.morganall = ddply(df.morganall,.(ID),.fun = function(x)
#     {
#       if (!all(is.na(x[,exposure])|is.na(x[,outcome]))) {
#         m = min(which(!is.na(x[,exposure])&!is.na(x[,outcome])))
#         if (m==1|m>4) return(x) else {
#           for(a in 1:(m-1)){
#             if (x[1,"ageV"]<17&is.na(lvdate[lastday$id==x[1,"ID"]]))
#             {
#               x[1:16,names!="times"]=x[2:17,names != "times"]
#               x[17,"date"]=x[17,"date"]+180
#               x[17,"ageV"]=x[17,"ageV"]+.5
#             } else {
#               if (x[1,"ageV"]<17&lvdate[lastday$id==x[1,"ID"]]>x[2,"date"]) {
#                 x[1:16,names!="times"]=x[2:17,names != "times"]
#                 x[17,"date"]=x[17,"date"]+180
#                 x[17,"ageV"]=x[17,"ageV"]+.5
#               }
#             }
#             
#           }
#         }
#       } 
#       return(x)
#     })
# 
# 
#     noshows = vapply(unique(df.morganall$ID) ,
#                      FUN=function(x) is.na(subset(df.morganall,ID==x&times==0)[,outcome]),
#                      FUN.VALUE = TRUE)
#     
#     keepers = unique(df.morganall$ID)[noshows==FALSE]
#     length(keepers)
#     
#     df.morganall = df.morganall[df.morganall$ID%in%keepers,]
# # 
#     subset(df.morganall,ID%in%c(345,404,497,616,622,9404,9453),select=c("ID","mnPAH","mnfev2"))
# 
#     subset(df.morganall,times==0&is.na(mnPAH),select=c("ID","mnPAH","mnfev2"))
  df.morganall[,exposure]=NULL
           # df.morganall[is.na(df.morganall$height),"height"]=50
  # create censoring dataframe for outcomes, attach to main df and cut out the nothings
  censordf = apply(df.morganall[,censornames],2,
                   FUN=function(a) as.numeric(is.na(a)))
  colnames(censordf)=paste0("cL",censornames)
  df.morganall = data.frame(df.morganall,censordf)
  
  # convert twoweek to time-varying
  df.morganall$Ltwoweek = df.morganall$twoweek
  df.morganall$twoweek = NULL
  
  # adjust age to be in years

  # adjust height to be in cm and convert to time-varying name
  df.morganall$height.cm = df.morganall$height*2.54
  
  # load in age height projections
  proj_ht = read.csv("C:/Users/jlstiles/Desktop/pulmonary-fresno/LongitudinalPFT_Levy/statage.csv")
  male = subset(proj_ht,Sex==1)
  male = as.data.frame(lapply(male,FUN=as.numeric))
  fem = subset(proj_ht,Sex==2)
  fem = as.data.frame(lapply(fem,as.numeric))

  # for both height and weight we track with percentiles
  df.morganall = ddply(df.morganall,.(id),.fun = function(x)
  {
    # x = subset(df.morganall,id==1)
    C = max(which(is.na(x[,"date"])==FALSE))
    age.mo = 12*x[,"ageV"]
    height.cm = x[,"height.cm"]
    height.na = which(is.na(height.cm))
    height.na = height.na[height.na<=C]
    x$SEX
    if (x[,"SEX"][1]==1)
    {
      for (a in height.na)
      {
        if (age.mo[a-1]>234) age.mo[a-1]=234
        age.diff = abs(male$Agemos-age.mo[a-1])
        mindiff= min(age.diff,na.rm=TRUE)
        ind = max(which(age.diff==mindiff))
        perc = abs(x[,"height.cm"][a-1]-
                     male[,c("P3","P5","P10","P25","P50","P75","P90","P95","P97")][ind,])
        p = which(perc==min(perc))[[1]]+5
        x[,"height.cm"][a]=male[(ind+6),p]
      }
    }  else
    {
      for (a in height.na)
      {
        if (age.mo[a-1]>234) age.mo[a-1]=234
        age.diff = abs(fem$Agemos-age.mo[a-1])
        mindiff= min(age.diff,na.rm=TRUE)
        ind = max(which(age.diff==mindiff))
        perc = abs(x[,"height.cm"][a-1]-
                     fem[,c("P3","P5","P10","P25","P50","P75","P90","P95","P97")][ind,])
        p = which(perc==min(perc))[1]+5
        x[,"height.cm"][a]=fem[(ind+6),p]
      }
    }
    return(x)
  })

  # load in weight projections and track by percentile
  df.morganall$weight.kg = df.morganall$weight/2.24
  proj_wt = read.csv("C:/Users/jlstiles/Desktop/pulmonary-fresno/LongitudinalPFT_Levy/proj_wt.csv")
  is.data.frame(proj_wt)
  male = subset(proj_wt,Sex==1)
  male = as.data.frame(lapply(male,FUN=as.numeric))
  fem = subset(proj_wt,Sex==2)
  fem = as.data.frame(lapply(fem,as.numeric))

  df.morganall = ddply(df.morganall,.(id),.fun = function(x)
  {
    C = max(which(is.na(x[,"date"])==FALSE))
    # x=as.data.frame(lapply(y,as.numeric))
    age.mo = 12*x[,"ageV"]
    weight.kg = x[,"weight.kg"]
    weight.na = which((is.na(weight.kg)==TRUE))
    weight.na = weight.na[weight.na<=C]
    if (x[,"SEX"][1]==1)
    {
      for (a in weight.na)
      {
        if (age.mo[a-1]>234) age.mo[a-1]=234
        age.diff = abs(male$Agemos-age.mo[a-1])
        mindiff= min(age.diff,na.rm=TRUE)
        ind = max(which(age.diff==mindiff))
        perc = abs(x[,"weight.kg"][a-1]-
                     male[,c("P3","P5","P10","P25","P50","P75","P90","P95","P97")][ind,])
        p = which(perc==min(perc))[[1]]+5
        x[,"weight.kg"][a]=male[(ind+6),p]
      }
    }  else
    {
      for (a in weight.na)
      {
        if (age.mo[a-1]>234) age.mo[a-1]=234
        age.diff = abs(fem$Agemos-age.mo[a-1])
        mindiff= min(age.diff,na.rm=TRUE)
        ind = max(which(age.diff==mindiff))
        perc = abs(x[,"weight.kg"][a-1]-
                     fem[,c("P3","P5","P10","P25","P50","P75","P90","P95","P97")][ind,])
        p = which(perc==min(perc))[1]+5
        x[,"weight.kg"][a]=fem[(ind+6),p]
      }
    }
    return(x)
  })
  
  # convert to time-varying names drop old columns
  tvs = names(df.morganall)%in%c("height.cm","weight.kg","season")
  old = names(df.morganall)[tvs] 
  new = paste0("L",old)
  names(df.morganall)[tvs]=new
  head(df.morganall)  
  df.morganall[,"height"]=df.morganall[,"weight"]=df.morganall[,"cLseason"]=NULL

  # imputation is based on Hankinson
  prediction_coeff = list()
  prediction_coeff$male=list()
  prediction_coeff$male$mnfvc2$high=data.frame(race1=c(-.7571,-.09520,.006619,.00017823),
                                               race2=c(-.4971,-.15497,.007701,.00016643),
                                               race3=c(-.2584,-.20415,.010133,.00018642))

  prediction_coeff$male$mnfev2$high=data.frame(race1=c(-.8218,-.04248,.004291,.00015104),
                                               race2=c(-.7048,-.05711,.004316,.00013194),
                                               race3=c(-.7453,-.04106,.004477,.00014098))

  prediction_coeff$female$mnfvc2$high=data.frame(race1=c(-1.2507,.07501,0,.00014246),
                                                 race2=c(-.6166,-.04687,.003602,.00013606),
                                                 race3=c(-1.2082,.05916,0,.00014815))
  
  prediction_coeff$female$mnfev2$high=data.frame(race1=c(-.9641,.0649,0,.00012154),
                                                 race2=c(-.963,-.05799,0,.00010846),
                                                 race3=c(-.8710,.06537,0,.00011496))
  
  # impute tracking on percentage of Hank predicted
  df.morganallex = ddply(df.morganall,.(id),.fun = function(x)
  {
    # x=subset(df.morganallex,id==298)
    race = x[,"race_cat"][1]
    ht = x[,"Lheight.cm"]
    age = x[,"ageV"]
    oc = x[,outcome]
    sex = x[,"SEX"][1]

    if (sex==1)
    {
      ind = which(names(prediction_coeff$male) %in% outcome)
      des = matrix(c(rep(1,length(age)),age,age^2,ht^2),byrow = FALSE,ncol=4)
      meas1 = des %*% prediction_coeff$male[[ind]]$high[,paste0("race",race)]
    
    } else
    {
      ind = which(names(prediction_coeff$female) %in% outcome)
      des = matrix(c(rep(1,length(age)),age,age^2,ht^2),byrow = FALSE,ncol=4)
      meas1 = des %*% prediction_coeff$female[[ind]]$high[,paste0("race",race)]
    }
    x[,"meas1"]=as.vector(meas1)
    fillinds = which((is.na(x[,outcome])==TRUE|(x[,outcome]==0)))
    for (i in fillinds)
    {
      perc = oc[i-1]/meas1[i-1]
      oc[i] = perc*meas1[i]
    }
    x[,outcome]=oc
    return(x)
  })
  
  df.morganallex$L=df.morganallex$Y
  df.morganallex$meas1=NULL
  return(df.morganallex)
}

#' @export
get.regimes = function(df,sum.measure,sum.names)
{
  Anodes = grep("A_",colnames(df))
  Cnodes = grep("C_", colnames(df))
  T=length(Cnodes)
  ID = df$ID
  finishers = vapply(ID,FUN = function(x)
  {
    # x=ID[2]
    if (all(df[ID==x,Cnodes]=="uncensored")) return(1)  else return(0)
  },FUN.VALUE = 1)
  if (all(finishers==0)) {return("no one completed a regime")} else {
    fin = ID[finishers==1]}
  
    regimes.final = df[finishers==1,Anodes]
    
    regimes.final = as.matrix(unique(regimes.final, incomparables = FALSE, MARGIN = 1,
                           fromLast = FALSE))
    B = nrow(regimes.final)
    N=length(ID)
    regimes = array(dim=c(N,T,B))
    for (a in 1:B)
    {
      regimes[,,a]=matrix(rep(regimes.final[a,],N),byrow=TRUE,nrow=N)
    }
    
    L = length(sum.names)
    summary.measures = array(dim=c(B,L,1))
    dimnames(summary.measures)[[2]]=sum.names
    summary.measures[,,1]=t(apply(regimes.final,1,FUN=function(x) sum.measure(x)))
    
    return(list(regimes=regimes,summary.measures=summary.measures, regimes.final=regimes.final))
}

sum.measure = function(x) sum(x)
sum.measure1= function(x) {
  abar = sum(x)
  vec = rep(0,(length(x)+1))
  vec[(abar+1)]=1
  return(vec)
}

#' @export
blprepare = function(outcome,exposure,df,DF,bigDF)
{
  # outcome = "mnfev2"
  df.bl =read.csv("C:/Users/jlstiles/Desktop/pulmonary-fresno/LongitudinalPFT_Levy/df.bl.csv")
  df.bl$X = NULL
  names.bl = names(df.bl)
  names.bl

  # df.bl[,c("anymedA","anymedB","anymedBC","anymedC")]
  
  ill.1 = grep("C1H_1B",names.bl)
  ill.1 = c("C1H_1",names.bl[ill.1])
  ill.1
  df.bl[,c("id",ill.1)][c(57,209,264,272),]

  df.bl$ill_1 = 2-df.bl$C1H_1
  which(is.na(df.bl$ill_1)==TRUE)
  df.bl$id = 1:315

  df.bl$cat=with(df.bl,as.numeric(is.na(C1D_9AC)==FALSE&C1D_9AC==1))
  df.bl$cat

  df.bl$cat_p1[which(is.na(df.bl$cat_p)==TRUE)]=0
  df.bl$cat_p1[which(is.na(df.bl$cat_p)==FALSE)]=df.bl$cat_p[which(is.na(df.bl$cat_p)==FALSE)]
  df.bl$cat_p1

  df.bl$cat=df.bl$cat*df.bl$cat_p1
  df.bl$cat

  df.bl$dog=with(df.bl,as.numeric(is.na(C1D_9AD)==FALSE&C1D_9AD==1))
  df.bl$dog

  df.bl$dog_p1[which(is.na(df.bl$dog_p)==TRUE)]=0
  df.bl$dog_p1[which(is.na(df.bl$dog_p)==FALSE)]=df.bl$dog_p[which(is.na(df.bl$dog_p)==FALSE)]
  df.bl$dog_p1

  df.bl$dog=df.bl$dog*df.bl$dog_p1
  df.bl$dog

  df.bl$pet = df.bl$dog+df.bl$cat
  df.bl$pet
# 
#   names = names(df.morgan)
#   names = names(df.morgan)[c(1:3,5:9,which(names==outcome),93,98,99,118,119:123)]

  
  # bldates = bigDF
  # bldates = ddply(bldates,.(ID),.fun = function(x){
  #   beg.date = DF[DF$ID==x$ID[1],"date"]
  #   M = max(x$date[x$date<=beg.date],na.rm = TRUE)
  #   return(x[x$date==M,])
  # })
  
  # bldates$height = bldates$height*2.54
  # bldates$weight = bldates$weight/2.24

  # expdate = read.csv("df.dailypers.csv")
  # which(expdate$date==min(expdate$date,na.rm=TRUE))
  # expdate[,c("YR","MON","DA","date")][1,]
  # expdate$date = expdate$date-14976
  # bigDF=DF
  date = bigDF[bigDF$times==0,"date"]
  months = months(as.Date(date,origin = "2001-01-01"))
  months
  season1 = which(months %in% c("February","March","April","May"))
  season2 = which(months %in% c("June","July","August","September"))
  season3 = which(months %in% c("October","November","December","January"))
  season=c()
  season[season1]="season1"
  season[season2]="season2"
  season[season3]="season3"
  season
  #   Feb-May
  # June-September
  # October-January

  df.baseline = df.bl[,c("ID","id","pet","atopic",
                         "ginasx","C1S_13","C1E_5","C1E_7","C1E_8","anymedB")]
  colnames(df.baseline)[6:9] = c("smoke","income","momschool","popschool")

  
  relevant = unique(bigDF$ID)
  length(relevant)
  
  df.baseline = subset(df.baseline,ID %in% relevant)
  nrow(df.baseline)
  df.baseline$season = factor(season)
  df.baseline
  # There were only 3 with both NA--put them in 0 category.  Otherwise, both college
  # got 4, one college got 3, both highschool got 2, one highschool got 1, rest 0
  momsch = df.baseline$momschool
  popsch = df.baseline$popschool

  education = function(x) {ifelse((x[1]==5|x[1]==6)&(x[2]==5|x[2]==6),4,
                                  ifelse((x[1]==5|x[1]==6)|(x[2]==5|x[2]==6),3,
                                         ifelse((x[1]==4|x[1]==3)&(x[2]==3|x[2]==4),2,
                                                ifelse((x[1]==3|x[1]==4)|(x[2]==3|x[2]==4),1,0))))}

  school = apply(subset(df.baseline,select=c("momschool","popschool")),1,education)
  both = which(is.na(popsch)==TRUE&is.na(momsch)==TRUE)
  school[both]=c(0,0,0)
  mom.na = which((is.na(popsch)==FALSE&is.na(momsch)==TRUE))
  pop.na = which((is.na(popsch)==TRUE&is.na(momsch)==FALSE))

  school[mom.na]=vapply(mom.na,FUN=function(x)
  {
    ifelse((popsch[x]==5|popsch[x]==6),3,
           ifelse((popsch[x]==4|popsch[x]==3),1,0))
  },FUN.VALUE = 3)

  school[pop.na]=vapply(pop.na,FUN=function(x)
  {
    ifelse((momsch[x]==5|momsch[x]==6),3,
           ifelse((momsch[x]==4|momsch[x]==3),1,0))
  },FUN.VALUE = 3)

  df.baseline$school=school

  # one missing info was put in the "never around smoke category"
  df.baseline$smoke[which(is.na(df.baseline$smoke)==TRUE)]=0
  df.baseline$smoke[which(df.baseline$smoke==3|df.baseline$smoke==4)]=2

  # 11 missing gets put in own 0 category
  df.baseline$income[which(is.na(df.baseline$income)==TRUE)]=1
  df.baseline$momschool=df.baseline$popschool=NULL

  # 42 missing for atopic (no skin tests get category 2)
  df.baseline$atopic[which(is.na(df.baseline$atopic)==TRUE)]=2

  # group the 8 gina score 4's into gina score 3's
  df.baseline$ginasx[which(df.baseline$ginasx==4)]=3
  df.baseline$ginasx
  # group smoke 4 category into the 3's
  # df.baseline$smoke[which(df.baseline$smoke==4)]=3
  # df.baseline
  names(df.baseline)
  df.baseline = ddply(df.baseline, .(ID), .fun = function(x) {
    # x=subset(df.baseline,ID==2)
    x[,c("race","sex","age",outcome,"twoweek")]= 
      bigDF[bigDF$ID==x$ID[1]&bigDF$times==0,c("race_cat","SEX","ageV",outcome,"Ltwoweek")]
    x[,c("weight.kg","height.cm","aRH24","aT24","aWS2")]=
      bigDF[bigDF$ID==x$ID[1]&bigDF$times==0,c("Lweight.kg","Lheight.cm","aRH24","aT24","aWS2")]
    return(x)    
  })
  # df.baseline[,c("race","sex","age",outcome,"twoweek")]=subset(bldates,times==0,
                                                               # select = c("race_cat","SEX","ageV",outcome,"twoweek"))

  # df.baseline[,c("weight.kg","height.cm","aRH24","aT24","aWS2")]=
    # subset(bldates,times==0, select = c("weight","height","aRH24","aT24","aWS2"))
  names(df.baseline)
  df.baselinefinal = df.baseline[,c(1,3:20)]
  return(df.baselinefinal)
}

#' @export
getwideform = function(T,blvars,blfactors,timevars,df.morganallex,
                       df.baselinefinal,outcome,outlier.remove)
{
  # T=6
  # df.baselinefinal = BL.PAHfev1
  # df.morganallex = DF.PAHfev1
  # blvars = names(df.baselinefinal)[c(3:12,14,15)]
  # blfactors = names(df.baselinefinal)[c(3:10)]
  # timevars = names(df.morganallex)[c(3,4,5,7,8)]
  # outcome = oes[[2]][2]
  outlier.remove=FALSE  
  
  # make the outcome censoring name
  df.long = df.morganallex

  # cut out those uncensored subjects whose outcome is an outlier by standard definition
  # only done if outlier.remove is TRUE
  if (outlier.remove==TRUE) {
    id.u = unique(df.long$id)
    ocs = vapply(id.u,FUN = function(x) {
      dude = subset(df.long,id==x)
      if (all(dude[,"C"]==0)) return(NA) else return(dude[T,"Y"])
    },FUN.VALUE=5)
    
    Q1 = quantile(ocs,na.rm=TRUE,probs=c(.25,.75))[1]
    Q3 = quantile(ocs,na.rm=TRUE,probs=c(.25,.75))[2]
    IQR = Q3-Q1
    
    df.long = ddply(df.long,.(id), .fun = function(x) 
      if (all(x[,"C"]==0)&(x[T,"Y"]<(Q1-1.5*IQR)|x[T,"Y"]>(Q3+1.5*IQR))) return(data.frame()) else return(x))
  }
  
  # order the long form to get ready for wide form so it is properly time ordered
  keepies = c("ID","times","C","A",timevars,"Y")
  df.long = df.long[,keepies]
  
  # Put in wide form
  df.long = melt(df.long,id=c("ID","times"))
  df.wide <- dcast(df.long,ID~times+variable, value.var="value")
  
  #make season a factor variable--the only time-varying factor
  fnames = names(df.wide)[grep("Lseason",names(df.wide))]
  df.wide[,names(df.wide)%in%fnames] = lapply(df.wide[,names(df.wide)%in%fnames],factor)
  df.wide[,!(names(df.wide)%in%fnames)] = lapply(df.wide[,!(names(df.wide)%in%fnames)],FUN = function(x) as.numeric(x))
  
  # cut out those uncensored whose outcome was an outlier--
  # effects nothing if outlier.remove is FALSE
  df.baselinefinal=df.baselinefinal[df.baselinefinal$ID %in% df.wide$ID,]
  
  # Make factors where appropriate
  df = merge(df.baselinefinal[,c("ID",blvars)],df.wide,by="ID")
  
  if (length(blfactors)==1)
  {
    df[,blfactors]=as.factor(df[,blfactors])
  } else
  {df[,blfactors]=lapply(df[,blfactors],factor)}
  
  #cut out the id variable
  # df = df[,1:ncol(df)]
  
  # prepare the names
  S <- names(df)
  S <- strsplit(S, split='_', fixed=TRUE)
  # S
  LL = length(S)
  colnames(df)=vapply(1:LL,FUN = function(x)
  {
    if (is.na(S[[x]][2])==TRUE) S[[x]][1] else
    {paste(S[[x]][2],S[[x]][1], sep = "_", collapse = NULL)}
  },
  FUN.VALUE = "chchc")
  
  # cut out non outcome Ynodes and Lnodes after the outcome
  T_name = as.factor(T)
  lastL = paste(timevars,T_name,sep = "_")
  Ys = paste0("Y_",1:(T-1))
  df = df[,!(names(df) %in% c(Ys,lastL))]
  # df = df[,!(names(df) %in% c(lastL))]
  
  # Set the nodes for LTMLE
  Anodes = grep("A",names(df))
  names(df)[Anodes]
  Cnodes = grep("C",names(df))
  names(df)[Cnodes]
  Lnodes = grep("L",names(df))
  names(df)[Lnodes]
  Ynodes = grep("Y",names(df))
  names(df)[Ynodes]
  
  # use built factor prep
  # df[,Cnodes]=lapply(df[,Cnodes],FUN = function(x) as.numeric(x))
  df[,Cnodes]=lapply(df[,Cnodes],FUN = function(x)
    BinaryToCensoring(is.censored=x))
  
  for (a in 1:length(Cnodes)) if (all(df[,Cnodes[a]]=="uncensored")) df[,Cnodes[a]]=NULL
  
  #make NA's for all cells after censoring
  cols = 1:ncol(df)
  for (a in 1:nrow(df)){
    censored.col = min(which(df[a,]=="censored"))
    if (censored.col==Inf) df[a,]=df[a,] else{
      Cn = grep("C_",colnames(df))
      names1 = colnames(df)[cols>censored.col&!(cols%in%Cn)]
      df[a,names1]=NA
    }
  }
  
  cols = 1:ncol(df)
  for (a in 1:nrow(df)){
    censored.col = min(which(df[a,]=="censored"))
    if (censored.col==Inf) df[a,]=df[a,] else{
      na.ind = cols>censored.col
      df[a,na.ind]=NA
    }
  }
 
  return(df)
}

#' @export
getlongform = function(df.morganallex,exposure,exposuredf,outcome){
  
  # Do exposures for different time periods with 75% cut-off for censoring
  # We need a censoring variable to at least censor after their last exposure
  
  # FIRST get rid of no shows with no baseline
  df.morganallex$C = 0
  C = vapply(unique(df.morganallex$ID),FUN= function(x) {
    df = subset(df.morganallex,ID==x)
    m = max(exposuredf[exposuredf$id==x,"date"],na.rm=TRUE)
    mm =lvdate[lastday$id==x]
    M = min(c(m,mm),na.rm=TRUE)
    C = df$date>=M
    return(C)
  },FUN.VALUE=rep(TRUE,17))
  
  # Get rid of anybody whose start date is beyond their leave date. 
  df.morganallex$C= as.numeric(as.vector(C))
  goods = subset(df.morganallex,times==0&C!=1,select="ID")$ID
  df.morganallex = df.morganallex[df.morganallex$ID%in%goods,]
  unique(df.morganallex$ID)
  
  # compute averages for exposures
  pd = 182
  df.morganallex1 = ddply(df.morganallex,.(ID),.fun = function(x){
    # x=subset(df.morganallex,ID==84)
    d = x$date
    # d = d[x$C==0]
    d = c(d,(d[length(d)]+pd))
    newexp = rep(NA,length(x$date))
    for (a in 1:(length(d)-1)) {
      ee = subset(exposuredf,select=c("date",exposure),date>d[a]&date<=d[a+1]&id==x$ID[1])
      nas = is.na(ee[,exposure])
      newexp[a]=ifelse(mean(nas)>.25,NA,mean(ee[,exposure],na.rm=TRUE))
      # newexp[a]=mean(ee[,exposure],na.rm=TRUE)
    }
    x[,exposure]=newexp
    return(x)
  })
 
  # Move up people who may have lots of exposure info but missing one of the 
  DF = df.morganallex1 
  head(DF)
  subset(DF,ID==622)
  subset(DF,ID==84)
  subset(df.morganallex1,ID==84)
  names = names(DF)
  DF = ddply(DF,.(ID),.fun = function(x)
        {
          if (!all(is.na(x[,exposure])|x[,paste0("cL",outcome)]==1)) {
            m = min(which(!is.na(x[,exposure])&x[,paste0("cL",outcome)]==0))
            if (m==1|m>4) return(x) else {
              for(a in 1:(m-1)){
                if (x[1,"ageV"]<17&is.na(lvdate[lastday$id==x[1,"ID"]]))
                {
                  x[1:16,names!="times"]=x[2:17,names != "times"]
                  x[17,"date"]=x[17,"date"]+180
                  x[17,"ageV"]=x[17,"ageV"]+.5
                } else {
                  if (x[1,"ageV"]<17&lvdate[lastday$id==x[1,"ID"]]>x[2,"date"]) {
                    x[1:16,names!="times"]=x[2:17,names != "times"]
                    x[17,"date"]=x[17,"date"]+180
                    x[17,"ageV"]=x[17,"ageV"]+.5
                  }
                }

              }
            }
          }
          return(x)
        })


  C1 = vapply(unique(DF$ID),FUN= function(x) {
    # x=14
    df = subset(DF,ID==x)
    LL = ifelse(is.na(lvdate[lastday$id==x]),10e7,lvdate[lastday$id==x])
    C = rep(0,length(df$ID))
    if (!any(is.na(df[,exposure])&(df$times%%1==0))) C = df$date>=LL else{
      C = df$date>=LL
      MM = min(which(is.na(df[,exposure])&(df$times%%1==0)))
      if (all(C==FALSE)) {
        ind = MM
        C[ind:length(C)]=1}
      else {
        mm = min(which(C==TRUE))
        ind = min(mm,MM)
        C[ind:length(C)]=1}
      return(C)
    }},FUN.VALUE=rep(1,17))
  # censored after first missing exposure.   
  
  DF$C1= as.numeric(as.vector(C1))
  
  min.dateDF = ddply(DF, .(ID), .fun = function(x) {
    if (any(x$C1==0)) m = min(x[x$C1==0,"date"],na.rm=TRUE) else m = x$date[1]
    d = data.frame(date=m,ID=x$ID[1]) 
    return(d)
  }) 
  
  # head(DF,7)
  # test = subset(DF,times==5|times==6,select = c("cLmnfev2","C1","ID"))
  # sum(vapply(unique(DF$ID),FUN = function(ids) {
  #   # ids=2
  #   censored = subset(DF, ID==ids&times==5)$C1==1
  #   missing = subset(DF, ID==ids&times==6)$cLmnfev2==1
  #   return(missing)},FUN.VALUE=TRUE))
  
  
  
  peter= read.csv("C:/Users/jlstiles/Desktop/pulmonary-fresno/LongitudinalPFT_Levy/df.bl.csv")
  df.baselinefinal = blprepare(outcome,exposure,peter, DF=min.dateDF,bigDF = DF)
  
  pd = 365
  DF = ddply(DF,.(ID),.fun = function(x){
    # x=subset(DF,ID==2)
    f = x[x$times%%2==0,"date"]
    f = c(f,(f[length(f)]+pd))
    ans = rep(NA,(length(f)-1))
    for (a in 1:(length(f)-1)) {
      if (f[a]<max(exposuredf[exposuredf$id==x$ID[1],"date"])){
      ee = subset(exposuredf,select=c("date",exposure),date>f[a]&date<=f[a+1]&id==x$ID[1])
      nas = is.na(ee[,exposure])
      ans[a] = ifelse(mean(nas)>.25,NA,mean(ee[,exposure],na.rm=TRUE))}
    }
    
    newexp = rep(NA,length(x$date))
    newexp[x$times%%2==0]=ans
    x[,paste0(exposure,2)]=newexp
    return(x)
  })
  
  names = names(DF)
  DF = ddply(DF,.(ID),.fun = function(x)
  {
    j = x[x$times%%2==0,]
    if (!all(is.na(j[,exposure])|is.na(j[,paste0("cL",outcome)]))) {
      m = min(which(!is.na(j[,exposure])&!is.na(j[,paste0("cL",outcome)])))
      if (m==1|(2*m)>4) return(x) else {
        for(a in 1:(2*m-1)){
          if (x[1,"ageV"]<17&is.na(lvdate[lastday$id==x[1,"ID"]]))
          {
            x[1:16,names!="times"]=x[2:17,names != "times"]
            x[17,"date"]=x[17,"date"]+180
            x[17,"ageV"]=x[17,"ageV"]+.5
          } else {
            if (x[1,"ageV"]<17&lvdate[lastday$id==x[1,"ID"]]>x[2,"date"]) {
              x[1:16,names!="times"]=x[2:17,names != "times"]
              x[17,"date"]=x[17,"date"]+180
              x[17,"ageV"]=x[17,"ageV"]+.5
            }
          }
          
        }
      }
    }
    return(x)
  })
  
  C2 = vapply(unique(DF$ID),FUN= function(x) {
    # x=14
    df = subset(DF,ID==x)
    LL = ifelse(is.na(lvdate[lastday$id==x]),10e7,lvdate[lastday$id==x])
    C = rep(0,length(df$ID))
    if (!any(is.na(df[,paste0(exposure,2)])&(df$times%%2==0))) C = df$date>=LL else{
      C = df$date>=LL
      MM = min(which(is.na(df[,paste0(exposure,2)])&(df$times%%2==0)))
      if (all(C==FALSE)) {
        ind = MM
        C[ind:length(C)]=1}
      else {
        mm = min(which(C==TRUE))
        ind = min(mm,MM)
        C[ind:length(C)]=1}
      return(C)
    }},FUN.VALUE=rep(1,17))
  

  
  DF$C2= as.numeric(as.vector(C2))
  
  # Get minimum dates to use for baseline
  min.dateDF = ddply(DF[DF$times%%2==0,], .(ID), .fun = function(x) {
    if (any(x$C2==0)) m = min(x[x$C2==0,"date"],na.rm=TRUE) else m = x$date[1]
    d = data.frame(date=m,ID=x$ID[1]) 
    return(d)
  }) 
  
  df.baselinefinal2 = blprepare(outcome,exposure,peter, DF=min.dateDF,bigDF=DF)
  
  pd = 365+183
  DF = ddply(DF,.(ID),.fun = function(x){
    # x=subset(DF,ID==2)
    f = x[x$times%%3==0,"date"]
    f = c(f,(f[length(f)]+pd))
    ans = rep(NA,(length(f)-1))
    for (a in 1:(length(f)-1)) {
      if (f[a]<max(exposuredf[exposuredf$id==x$ID[1],"date"])){
        ee = subset(exposuredf,select=c("date",exposure),date>f[a]&date<=f[a+1]&id==x$ID[1])
        nas = is.na(ee[,exposure])
        ans[a]=ifelse(mean(nas)>.25,NA,mean(ee[,exposure],na.rm=TRUE))}
    }
    
    newexp = rep(NA,length(x$date))
    newexp[x$times%%3==0]=ans
    x[,paste0(exposure,3)]=newexp
    return(x)
  })
  
  names = names(DF)
  DF = ddply(DF,.(ID),.fun = function(x)
  {
    j = x[x$times%%3==0,]
    if (!all(is.na(j[,exposure])|is.na(j[,paste0("cL",outcome)]))) {
      m = min(which(!is.na(j[,exposure])&!is.na(j[,paste0("cL",outcome)])))
      if (m==1|(3*m)>4) return(x) else {
        for(a in 1:(3*m-1)){
          if (x[1,"ageV"]<17&is.na(lvdate[lastday$id==x[1,"ID"]]))
          {
            x[1:16,names!="times"]=x[2:17,names != "times"]
            x[17,"date"]=x[17,"date"]+180
            x[17,"ageV"]=x[17,"ageV"]+.5
          } else {
            if (x[1,"ageV"]<17&lvdate[lastday$id==x[1,"ID"]]>x[2,"date"]) {
              x[1:16,names!="times"]=x[2:17,names != "times"]
              x[17,"date"]=x[17,"date"]+180
              x[17,"ageV"]=x[17,"ageV"]+.5
            }
          }
          
        }
      }
    }
    return(x)
  })
  
  
  C3 = vapply(unique(DF$ID),FUN= function(x) {
    # x=14
    df = subset(DF,ID==x)
    LL = ifelse(is.na(lvdate[lastday$id==x]),10e7,lvdate[lastday$id==x])
    C = rep(0,length(df$ID))
    if (!any(is.na(df[,paste0(exposure,3)])&(df$times%%3==0))) C = df$date>=LL else{
      C = df$date>=LL
      MM = min(which(is.na(df[,paste0(exposure,3)])&(df$times%%3==0)))
      if (all(C==FALSE)) {
        ind = MM
        C[ind:length(C)]=1}
      else {
        mm = min(which(C==TRUE))
        ind = min(mm,MM)
      C[ind:length(C)]=1}
    return(C)
  }},FUN.VALUE=rep(1,17))
  
  
  
  DF$C3= as.numeric(as.vector(C3))
  
  # Get minimum dates to use for baseline
  min.dateDF = ddply(DF[DF$times%%3==0,], .(ID), .fun = function(x) {
    if (any(x$C3==0)) m = min(x[x$C3==0,"date"],na.rm=TRUE) else m = x$date[1]
    d = data.frame(date=m,ID=x$ID[1]) 
    return(d)
  }) 
  
  df.baselinefinal3 = blprepare(outcome,exposure,peter, DF=min.dateDF, bigDF=DF)
  
  
  pd = 730
  DF = ddply(DF,.(ID),.fun = function(x){
    # x=subset(DF,ID==2)
    f = x[x$times%%4==0,"date"]
    f = c(f,(f[length(f)]+pd))
    ans = rep(NA,(length(f)-1))
    for (a in 1:(length(f)-1)) {
      if (f[a]<max(exposuredf[exposuredf$id==x$ID[1],"date"])){
        ee = subset(exposuredf,select=c("date",exposure),date>f[a]&date<=f[a+1]&id==x$ID[1])
        nas = is.na(ee[,exposure])
        ans[a]=ifelse(mean(nas)>.25,NA,mean(ee[,exposure],na.rm=TRUE))}
    }
    
    newexp = rep(NA,length(x$date))
    newexp[x$times%%4==0]=ans
    x[,paste0(exposure,4)]=newexp
    return(x)
  })
  
  names=names(DF)
  DF = ddply(DF,.(ID),.fun = function(x)
  {
    j = x[x$times%%4==0,]
    if (!all(is.na(j[,exposure])|is.na(j[,paste0("cL",outcome)]))) {
      m = min(which(!is.na(j[,exposure])&!is.na(j[,paste0("cL",outcome)])))
      if (m==1|(4*m)>4) return(x) else {
        for(a in 1:(4*m-1)){
          if (x[1,"ageV"]<17&is.na(lvdate[lastday$id==x[1,"ID"]]))
          {
            x[1:16,names!="times"]=x[2:17,names != "times"]
            x[17,"date"]=x[17,"date"]+180
            x[17,"ageV"]=x[17,"ageV"]+.5
          } else {
            if (x[1,"ageV"]<17&lvdate[lastday$id==x[1,"ID"]]>x[2,"date"]) {
              x[1:16,names!="times"]=x[2:17,names != "times"]
              x[17,"date"]=x[17,"date"]+180
              x[17,"ageV"]=x[17,"ageV"]+.5
            }
          }
          
        }
      }
    }
    return(x)
  })
  
  
  C4 = vapply(unique(DF$ID),FUN= function(x) {
    # x=14
    df = subset(DF,ID==x)
    LL = ifelse(is.na(lvdate[lastday$id==x]),10e7,lvdate[lastday$id==x])
    C = rep(0,length(df$ID))
    if (!any(is.na(df[,paste0(exposure,4)])&(df$times%%4==0))) C = df$date>=LL else{
      C = df$date>=LL
      MM = min(which(is.na(df[,paste0(exposure,4)])&(df$times%%4==0)))
      if (all(C==FALSE)) {
        ind = MM
        C[ind:length(C)]=1}
      else {
        mm = min(which(C==TRUE))
        ind = min(mm,MM)
        C[ind:length(C)]=1}
      return(C)
    }},FUN.VALUE=rep(1,17))
  
  
  
  DF$C4= as.numeric(as.vector(C4))
  
  # Get minimum dates to use for baseline
  min.dateDF = ddply(DF[DF$times%%4==0,], .(ID), .fun = function(x) {
    if (any(x$C4==0)) m = min(x[x$C4==0,"date"],na.rm=TRUE) else m = x$date[1]
    d = data.frame(date=m,ID=x$ID[1]) 
    return(d)
  }) 
  
  df.baselinefinal4 = blprepare(outcome,exposure,peter, DF=min.dateDF, bigDF=DF)
  
 
  pd = 1095
  DF = ddply(DF,.(ID),.fun = function(x){
    # x=subset(DF,ID==2)
    f = x[x$times%%6==0,"date"]
    f = c(f,(f[length(f)]+pd))
    ans = rep(NA,(length(f)-1))
    for (a in 1:(length(f)-1)) {
      if (f[a]<max(exposuredf[exposuredf$id==x$ID[1],"date"])){
        ee = subset(exposuredf,select=c("date",exposure),date>f[a]&date<=f[a+1]&id==x$ID[1])
        nas = is.na(ee[,exposure])
        ans[a]=ifelse(mean(nas)>.25,NA,mean(ee[,exposure],na.rm=TRUE))}
    }
    
    newexp = rep(NA,length(x$date))
    newexp[x$times%%6==0]=ans
    x[,paste0(exposure,6)]=newexp
    return(x)
  })
  
  names = names(DF)
  DF = ddply(DF,.(ID),.fun = function(x)
  {
    j = x[x$times%%6==0,]
    if (!all(is.na(j[,exposure])|is.na(j[,paste0("cL",outcome)]))) {
      m = min(which(!is.na(j[,exposure])&!is.na(j[,paste0("cL",outcome)])))
      if (m==1|(6*m)>4) return(x) else {
        for(a in 1:(6*m-1)){
          if (x[1,"ageV"]<17&is.na(lvdate[lastday$id==x[1,"ID"]]))
          {
            x[1:16,names!="times"]=x[2:17,names != "times"]
            x[17,"date"]=x[17,"date"]+180
            x[17,"ageV"]=x[17,"ageV"]+.5
          } else {
            if (x[1,"ageV"]<17&lvdate[lastday$id==x[1,"ID"]]>x[2,"date"]) {
              x[1:16,names!="times"]=x[2:17,names != "times"]
              x[17,"date"]=x[17,"date"]+180
              x[17,"ageV"]=x[17,"ageV"]+.5
            }
          }
          
        }
      }
    }
    return(x)
  })
  
  
  C6 = vapply(unique(DF$ID),FUN= function(x) {
    # x=14
    df = subset(DF,ID==x)
    LL = ifelse(is.na(lvdate[lastday$id==x]),10e7,lvdate[lastday$id==x])
    C = rep(0,length(df$ID))
    if (!any(is.na(df[,paste0(exposure,6)])&(df$times%%6==0))) C = df$date>=LL else{
      C = df$date>=LL
      MM = min(which(is.na(df[,paste0(exposure,6)])&(df$times%%6==0)))
      if (all(C==FALSE)) {
        ind = MM
        C[ind:length(C)]=1}
      else {
        mm = min(which(C==TRUE))
        ind = min(mm,MM)
        C[ind:length(C)]=1}
      return(C)
    }},FUN.VALUE=rep(1,17))
  
  
  
  DF$C6= as.numeric(as.vector(C6))
  
  # Get minimum dates to use for baseline
  min.dateDF = ddply(DF[DF$times%%6==0,], .(ID), .fun = function(x) {
    if (any(x$C6==0)) m = min(x[x$C6==0,"date"],na.rm=TRUE) else m = x$date[1]
    d = data.frame(date=m,ID=x$ID[1]) 
    return(d)
  }) 
  
  df.baselinefinal6 = blprepare(outcome,exposure,peter, DF=min.dateDF, bigDF=DF)
  
  return(list(DF=DF,BL=df.baselinefinal,BL2 = df.baselinefinal2, BL3 = df.baselinefinal3,
              BL4 = df.baselinefinal4, BL6 = df.baselinefinal6))
}

#' @export
cutdf = function(T,DF,probs,halfyrs.per.pd){
  exposure = ifelse(halfyrs.per.pd==1,exposure,paste0(exposure,halfyrs.per.pd))
  DF = DF[DF$times%%halfyrs.per.pd==0,]
  head(DF)
  DF = DF[DF$times<=T,]
  DF = ddply(DF,.(ID),.fun = function(x) {
    x$times = 0:(length(x$times)-1)
    return(x)
  })
  
  cutoff = quantile(DF[DF$times<=(T/halfyrs.per.pd-1),exposure],na.rm=TRUE,probs = probs)
  
  DF$A = DF[,exposure]>cutoff 
  
  # shift everyone to cut off the baseline
  head(DF)
  Cind = which(names(DF)%in%"C")
  Cind
  DF = ddply(DF, .(ID),.fun=function(x){
    df = x
    df[1:(T/halfyrs.per.pd),1:(Cind-1)] = x[2:(T/halfyrs.per.pd+1),1:(Cind-1)]
    df = df[1:(T/halfyrs.per.pd),]
    if (all(is.na(df[,"A"]))) {
        for (a in 1:T/halfyrs.per.pd) {
          df[a,paste0("C",halfyrs.per.pd)]=1
          }
    }
    return(df)
  })
  
  # keep wanted variables
  DF$C = DF[,paste0("C",halfyrs.per.pd)]
  DF = DF[,c(1:Cind,length(names(DF)))]
  
  DF$id = NULL
  head(DF,20)
  
  # common = apply(DF[,c("cLheight","cLweight",paste0("cL",outcome))],1,FUN = function(row) !(sum(row)==3|sum(row)==0))
  # 
  # # since only 58 differ in when they were missing I will use the outcome imputation indicator
  # sum(common)
  
  names(DF)[grep(paste0("cL",outcome),names(DF))]="cL"
  DF$cLheight=DF$cLweight=DF$cLtwoweek  = NULL
  DF$Ltwoweek[is.na(DF$Ltwoweek)]=2
  DF$aRH24=DF$aT24=DF$aWS2=NULL
  DF$A[!is.na(DF$A)]=as.numeric(DF$A[!is.na(DF$A)])
  head(DF,30)
  
  names(DF)[grep(outcome,names(DF))]="L"
  DF$Y=DF$L

  DF[DF$times==T/halfyrs.per.pd,"C"]=DF[DF$times==T/halfyrs.per.pd,"cL"]==1|DF[DF$times==T/halfyrs.per.pd,"C"]==1
  # number of censored by time 6
  DF$ageV=DF$race_cat=DF$SEX=DF$date=NULL
  
  return(DF)
  
}

