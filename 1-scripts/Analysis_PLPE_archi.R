
# Packages ----------------------------------------------------------------
require(ggplot2)
require(stringr)
require(dplyr)
require(lubridate)
require(viridis)
require(RColorBrewer)
require(cowplot)

# myTheme <- theme(
#   panel.background=element_rect(fill="lightgrey", color=NA),  
#   plot.background = element_rect(fill = "transparent",colour = NA),
#   axis.line=element_line(colour="black"), 
#   axis.title=element_text(size=18),
#   axis.text.y=element_text(size=16, colour="black"), 
#   axis.text.x=element_text(size=16, colour="black", angle=0, hjust=0.5),
#   panel.grid.minor = element_blank(), 
#   panel.grid.major = element_line(colour="grey90", size=0.2), 
#   legend.text=element_text(size=14),
#   legend.title=element_text(size=14),
#   legend.position = c(0.1,0.8),
#   legend.background = element_rect(fill='transparent'),
#   strip.text.x = element_text(size = 14),
#   strip.text.y = element_text(size = 14)
# )

# palette=c('Control'='#ca0020','LargeInterRow'='#f4a582','DoubleRow'='#f7f7f7','DoubleInRow'='#92c5de','TripleInRow'='#0571b0')
# palette=c('Control'='#e41a1c','LargeInterRow'='#377eb8','DoubleRow'='#4daf4a','DoubleInRow'='#984ea3','TripleInRow'='#ff7f00')

colors_vec= brewer.pal(5, "BrBG")
colors_vec[3]= "grey30"
names(colors_vec)= c("Control", "LargeInterRow","DoubleRow","DoubleInRow","TripleInRow")

# Import data -------------------------------------------------------------



# # generate the data --------------------------------------------------------
# 
# 
# ###table to get correspondacne between design and density
# tableDensity=data.frame(Design=c('Control','DoubleInRow','TripleInRow','LargeInterRow','DoubleRow'),Density=c(136,272,250,200,200))
# 
# dev=read.csv2(file = 'Development_PLPE_design_v0.csv',header=T,sep=';',dec='.')
# dev$Observation_Date=dmy(dev$Observation_Date)
# dev$TreeNumber=paste0('T',dev$TreeNumber)
# dev$Transplanting_Date=dmy(dev$Transplanting_Date)
# 
# dev=unique(dev)
# ###get a MonthAfterPlanting per campain
# dev$MonthAfterPlanting=lubridate::interval(dev$Transplanting_Date,dev$Observation_Date)%/%months(1)
# 
# dev[dev$MonthAfterPlanting
# ==26,]$MonthAfterPlanting=27
# dev[dev$MonthAfterPlanting
# ==33,]$MonthAfterPlanting=32
# dev[dev$MonthAfterPlanting
# ==43,]$MonthAfterPlanting=44
# 
# dev$Progeny=paste0('P',dev$Progeny)
# dev$Orientation=str_sub(string = dev$Plot,start = 0,end = 2)
# dev$Design=as.factor(str_sub(string = dev$Plot,start = 3,end = 4))
# levels(dev$Design)=list(Control='01',LargeInterRow='04',DoubleRow='05',TripleInRow='03',DoubleInRow='02')
# 
# dev=merge(x=dev,tableDensity)
# 
# 
# ###add petiolLenght data and stem height
# pet=read.csv2(file = 'update_data_pet_PLPE.csv',header=T,sep=';',dec='.')
# pet$Observation_Date=dmy(pet$Observation_Date)
# pet$TreeNumber=paste0('T',pet$TreeNumber)
# 
# devComp=merge(x=dev,y=pet[,c('Observation_Date','TreeNumber','LeafIndex','PetioleLength','StemHeight17')],all.x=T)
# 
# 
# write.csv(x = devComp,file = 'Development_PLPE_design.csv',row.names = F)


dev=read.csv2(file = 'Development_PLPE_design.csv',header=T,sep=';',dec='.')
dev$Observation_Date=dmy(dev$Observation_Date)
dev$Transplanting_Date=dmy(dev$Transplanting_Date)

###ID of a tree
dev$Id=paste(dev$Orientation,dev$Design,dev$Progeny,dev$TreeNumber,sep='_')

###ordering by density
levels(dev$Design)=list(Control='Control',LargeInterRow='LargeInterRow',DoubleRow='DoubleRow',DoubleInRow='DoubleInRow',TripleInRow='TripleInRow')

###add variables
dev=dev %>%
  mutate(RelativeBposition=Bposition/RachisLength,FreqLft=Nb_leaflets/RachisLength,ratioL=PetioleLength/RachisLength,LeafLength=RachisLength+PetioleLength)

###leaf angles (declinations)
dec=read.csv2(file = 'AnglesC&A_PLPE_design.csv',header=T,sep=';',dec='.')
dec$Observation_Date=dmy(dec$Observation_Date)
dec$Transplanting_Date=unique(dev$Transplanting_Date)
###get a MonthAfterPlanting per campain
dec$MonthAfterPlanting=lubridate::interval(dec$Transplanting_Date,dec$Observation_Date)%/%months(1)
dec$decliC=dec$BendingC+90
levels(dec$Design)=list(Control='Control',LargeInterRow='LargeInterRow',DoubleRow='DoubleRow',DoubleInRow='DoubleInRow',TripleInRow='TripleInRow')

####leaf area
Area=read.csv2(file = 'Leaf_Area_Monitoring_PLPE_design.csv',header=T,sep=';',dec=',')
Area$obs_date=dmy(Area$obs_date)
Area$Observation_Date=ymd(Area$obs_date)
Area$Transplanting_Date=unique(dev$Transplanting_Date)
Area$MonthAfterPlanting=lubridate::interval(Area$Transplanting_Date,Area$Observation_Date)%/%months(1)
levels(Area$Design)=list(Control='Control',LargeInterRow='LargeInterRow',DoubleRow='DoubleRow',DoubleInRow='DoubleInRow',TripleInRow='TripleInRow')


# Stats -------------------------------------------------------------------
###sampling
table(dev$MonthAfterPlanting,dev$Design,dev$Progeny,dev$Orientation)


# graphics ----------------------------------------------------------------
#' Plot phenotypic value on PLPE design
#'
#' @param y = trait to describe
#'
#' @return
#' @export
#'
#' @examples
var_evo=function(y='RachisLength',ylab=NULL){
  
  dev.mean=dev%>%
    group_by(Design,Orientation,MonthAfterPlanting)%>%
    summarise(ymean=mean(get(y),na.rm=T))
  
    grph=ggplot()+
    # geom_smooth(aes(x=MonthAfterPlanting,y=get(y),col=Design),se=F,method='lm')+
    geom_point(data=dev,aes(x=MonthAfterPlanting,y=get(y),col=Design),alpha=0.5)+
    geom_line(data=dev.mean,aes(x=MonthAfterPlanting,y=ymean,col=Design),lwd=1.2)+
    facet_grid(~Orientation)+
    ylab(ifelse(is.null(ylab),y,ylab))+
    xlab(expression('Months after planting'))+
    scale_color_manual(values=colors_vec)
    # scale_color_viridis_d()+
    # myTheme
  print(grph)
}

var_evo(y = 'RachisLength',ylab='Rachis length (cm)')
var_evo(y = 'ratioL')
var_evo(y = 'Nb_leaflets')
var_evo(y = 'FreqLft')
var_evo(y = 'LeafletBLength')
var_evo(y = 'LeafletBWidth')
var_evo(y = 'RelativeBposition')


var_boxplot=function(y='RachisLength',MAP=44,ylab=NULL){
  sub=dev%>%
    filter(MonthAfterPlanting>MAP-2 &MonthAfterPlanting<MAP+2 )%>%
    arrange(Density)
  
  grph=ggplot(data=sub,aes(x=Design,y=get(y),fill=Design))+
   geom_boxplot()+
    # geom_jitter(pch=21)+
    facet_grid(~Orientation)+
    scale_fill_manual(values=colors_vec)+
    # scale_fill_viridis_d()+
    ylab(ifelse(is.null(ylab),y,ylab))+
    xlab(paste(MAP,'Months after planting'))+
    # myTheme+
    theme(axis.text.x = element_text(angle=0,size=8))
  print(grph)
}
  
# var_boxplot(y = 'RachisLength',MAP = 44)
var_boxplot(y = 'LeafLength',MAP = 44,ylab=expression('Leaf length'['Rank=17']~ (cm)))
var_boxplot(y = 'ratioL',MAP = 44)

var_boxplot(y = 'LeafletBLength',MAP = 44)
var_boxplot(y = 'LeafletBWidth',MAP = 44)
var_boxplot(y = 'StemHeight17',MAP = 44)


####declination

sub_dec=dec%>%
  filter(MonthAfterPlanting>MAP-2 &MonthAfterPlanting<MAP+2)%>%
  arrange(Density)

sub_dec%>%
  ggplot(aes(x=Rank,y=decliC,col=Design))+
  geom_point(alpha=0.5)+
  scale_color_manual(values=colors_vec)+
  geom_smooth(method='lm',se=F)+
  facet_grid(~Orientation)+
  # myTheme+
  ylab(expression('Declination at point C'~(degree)))

sub_dec17=dec%>%
  filter(MonthAfterPlanting>MAP-2 &MonthAfterPlanting<MAP+2 & Rank==17)%>%
  arrange(Density)

sub_dec17%>%
  ggplot(aes(x=Design,y=decliC,fill=Design))+
  geom_boxplot()+
  scale_fill_manual(values=colors_vec)+
  facet_grid(~Orientation)+
  # myTheme+
  theme(axis.text.x = element_text(angle=0,size=8))+
  ylab(expression('Declination at point C'['Rank=17']~(degree)))+
  xlab('')

# plot design -------------------------------------------------------------

source('1-scripts/Design_scheme.R')

design_files
colors_vec

d1_ns=plot_design_rep(plot_design =designs[[2]],voronois = 12,color=colors_vec[1])+
  ggtitle('Control',expression(136~palms~ha^-1))
d2_ns=plot_design_rep(plot_design =designs[[8]],voronois = 12,color=colors_vec[2])+
  ggtitle('LargeInterRow',expression(200~palms~ha^-1))
d3_ns=plot_design_rep(plot_design =designs[[6]],voronois = 12,color=colors_vec[3])+theme(panel.background=element_rect(fill=alpha(1,0.2), color=NA))+
  ggtitle('DoubleRow',expression(200~palms~ha^-1))
d4_ns=plot_design_rep(plot_design =designs[[4]],voronois = 12,color=colors_vec[4])+
  ggtitle('DoubleInRow',expression(272~palms~ha^-1))
d5_ns=plot_design_rep(plot_design =designs[[10]],voronois = 12,color=colors_vec[5])+
  ggtitle('TripleInRow',expression(272~palms~ha^-1))

d1_ew=plot_design_rep(plot_design =designs[[1]],voronois = 12,color=colors_vec[1])
d2_ew=plot_design_rep(plot_design =designs[[7]],voronois = 12,color=colors_vec[2])
d3_ew=plot_design_rep(plot_design =designs[[5]],voronois = 12,color=colors_vec[3])+
  theme(panel.background=element_rect(fill=alpha(1,0.2), color=NA))
d4_ew=plot_design_rep(plot_design =designs[[3]],voronois = 12,color=colors_vec[4])
d5_ew=plot_design_rep(plot_design =designs[[9]],voronois = 12,color=colors_vec[5])

NS=plot_grid(d1_ns,d2_ns,d3_ns,d4_ns,d5_ns,nrow=1)
EW=plot_grid(d1_ew,d2_ew,d3_ew,d4_ew,d5_ew,nrow=1)
plot_grid(NS,EW,labels = c('North - South orientation','East - West orientation'),align ='hv',nrow=2)

ggsave("Designs.png", device = "png",
       path = "2-outputs/plots",scale = 1, width = 30, height = 20, units = "cm",
       dpi = 150)
