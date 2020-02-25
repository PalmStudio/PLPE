# Packages ----------------------------------------------------------------
require(ggplot2)
require(stringr)
require(dplyr)
require(lubridate)
require(viridis)

# Import data -------------------------------------------------------------

###developement
dev=read.csv2(file = '1-data/raw_data/Development_PLPE_design.csv',header=T,sep=';',dec='.')
dev$Observation_Date=dmy(dev$Observation_Date)
dev$Transplanting_Date=dmy(dev$Transplanting_Date)
dev$TotalEmmitted=dev$MonthAfterPlanting*2.5

####leaf area
Area=read.csv2(file = '1-data/raw_data/Leaf_Area_Monitoring_PLPE_design.csv',header=T,sep=';',dec=',')
Area$obs_date=dmy(Area$obs_date)

###leaf angles (declinations)
dec=read.csv2(file = '1-data/raw_data/AnglesC&A_PLPE_design.csv',header=T,sep=';',dec='.')
dec$Observation_Date=dmy(dec$Observation_Date)
dec$Transplanting_Date=unique(dev$Transplanting_Date)
###get a MonthAfterPlanting per campain
dec$MonthAfterPlanting=lubridate::interval(dec$Transplanting_Date,dec$Observation_Date)%/%months(1)
dec$decliC= dec$BendingC+90
dec$decliA= dec$BendingA

# Estimation of Vpalm paramters -------------------------------------------
###select MAP
MAP=44


###rachis length
paramRachisL=dev %>%
  group_by(Design,Orientation) %>%
  dplyr::do(lm=list(lm(data=.,RachisLength~TotalEmmitted)))

paramRachisL$Lrac_intercept= sapply(paramRachisL$lm, function(x){stats::coef(x[[1]])[1]})
paramRachisL$Lrac_slp= sapply(paramRachisL$lm, function(x){stats::coef(x[[1]])[2]})

paramRachisL$MAP=MAP
paramRachisL$rachisLengthRank1=paramRachisL$Lrac_intercept+paramRachisL$Lrac_slp*(paramRachisL$MAP*2.5)

###leaflet dimensions at B point
paramDimLft=dev %>%
  group_by(Design,Orientation) %>%
  dplyr::do(lengthB.lm=list(lm(data=.,LeafletBLength~RachisLength)),widthB.lm=list(lm(data=.,LeafletBWidth~RachisLength)))

paramDimLft$LenghtB_intercept= sapply(paramDimLft$lengthB.lm, function(x){stats::coef(x[[1]])[1]})
paramDimLft$bLengthSlope= sapply(paramDimLft$lengthB.lm, function(x){stats::coef(x[[1]])[2]})
paramDimLft$WidthB_intercept= sapply(paramDimLft$widthB.lm, function(x){stats::coef(x[[1]])[1]})
paramDimLft$bWidthSlope= sapply(paramDimLft$widthB.lm, function(x){stats::coef(x[[1]])[2]})

paramDimLft$MAP=MAP
paramDimLft=merge(x=paramDimLft,y=paramRachisL)
paramDimLft$BwidthRank1=paramDimLft$WidthB_intercept+paramDimLft$rachisLengthRank1*paramDimLft$bWidthSlope
paramDimLft$BlengthRank1=paramDimLft$LenghtB_intercept+paramDimLft$rachisLengthRank1*paramDimLft$bLengthSlope

####ratio petiole on rachis length
paramRatioL=dev %>%
  filter(MonthAfterPlanting==MAP) %>%
  group_by(Design,Orientation) %>%
  summarize(ratioL=mean(PetioleLength/RachisLength,na.rm=T))

####declination at C point
paramDecli=dec%>%
  filter(MonthAfterPlanting>MAP-2& MonthAfterPlanting<MAP+2) %>%
  group_by(Design,Orientation)%>%
  dplyr::do(decli.lm=list(lm(data=.,decliC~Rank)))

paramDecli$decliC_intercept= sapply(paramDecli$decli.lm, function(x){stats::coef(x[[1]])[1]})
paramDecli$decliC_slope= sapply(paramDecli$decli.lm, function(x){stats::coef(x[[1]])[2]})

####declination at A point


dec%>%
  filter(MonthAfterPlanting>MAP-2& MonthAfterPlanting<MAP+2) %>%
  ggplot(aes(x= decliC, y= decliA))+
  facet_grid(vars(Design))+
  geom_point()

# decliA is measured only on leaf rank 17, so the data does not allow us to fit the allometry.
# paramDecli_A=
#   dec%>%
#   filter(MonthAfterPlanting>MAP-2& MonthAfterPlanting<MAP+2) %>%
#   group_by(Design,Orientation)%>%
#   dplyr::do(decli_nls=list(nls(data = .,
#                               formula =
#                                 decliA~sigmoid(X= decliC, max= decMaxA, slope= decSlopeA, infl= decInflA),
#                               start=list(decInflA= 41.58479, decMaxA= 180, decSlopeA= 0.01),
#                               upper=c(decInflA =120),
#                               lower= list(infl=0),algorithm='port')))

# /!\ Please adapt the following code to extract the three parameter values: 
# paramDecli$decInflA= sapply(paramDecli_A$decli_nls, function(x){stats::coef(x[[1]])[1]})
# paramDecli$decMaxA= sapply(paramDecli_A$decli_nls, function(x){stats::coef(x[[1]])[2]})
# paramDecli$decSlopeA= sapply(paramDecli_A$decli_nls, function(x){stats::coef(x[[1]])[3]})

####summarize parameters
PARAM=merge(x=paramDimLft[c("Design","Orientation","MAP","Lrac_slp","rachisLengthRank1","BlengthRank1","bLengthSlope","BwidthRank1","bWidthSlope")],y=paramRatioL)
PARAM=merge(PARAM,paramDecli[,c("Design","Orientation",'decliC_intercept','decliC_slope')])         

PARAM$Design= gsub("Doble","Double",PARAM$Design)

write.csv(x=PARAM,file='1-data/processed/param_Vpalm_PLPE_design.csv',row.names = F)


