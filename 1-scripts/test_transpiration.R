library(tidyverse)
library(data.table)
library(lubridate)
library(archimedR)
library(Vpalmr)
library(ggplot2)
library(ggimage)
library(viridis)
library(plotly)
source("helper-functions.R")
Sys.setenv(TZ="UTC")


outputDir= "output_test"

timestep= 30*60 # 30 minutes
clearness= 0.75
sim_date= data.frame(date= as.POSIXct("2019-03-20"), clearness= 0.5)
Hour= seq(from= as.POSIXct("2019-01-01 05:00:00"), by= timestep, 
          to = as.POSIXct("2019-01-01 18:30:00"))

# Dummy temperatures in the day:
Tair= archimedR::temperature_dynamic(Tmax = 30, Tmin = 18, per = (24*60*60)/timestep,
                                     shift = 3*pi/2)[10:37]

Meteo= 
  expand.grid(date= sim_date$date, hour_start= Hour)%>%
  arrange(date,hour_start)%>%
  mutate(hour_end= format(hour_start+timestep,"%H:%M:%S"),
         hour_start= format(hour_start,"%H:%M:%S"),
         temperature= Tair,
         relativeHumidity= 90,
         clearness= clearness)%>%
  mutate(date= format(date, "%Y/%m/%d"), wind= 3)

path_archimed= "../../ARCHIMED/Archimed_feb2019"

data.table::fwrite(
  Meteo,sep=";",
  file = file.path(path_archimed,'app_parameters/meteo/meteo_PLPE_02_2019.csv'))


## Making the scene using VPalmr (or VPALM-IDE)
params= 
  data.table::fread("1-data/processed/param_Vpalm_PLPE_design.csv", data.table = F)

designs_names= paste(params$Design,params$Orientation, sep="_")
design_files= list.files("1-data/PLPE_plot_design/",full.names = TRUE)

designs= 
  lapply(1:length(designs_names), function(x){
    params_new= update_param(params[x,])
    name= paste(params[x,]$Design,params[x,]$Orientation, sep="_")
    # Importing the right design:
    design_target= design_files[grep(name,design_files)]
    planting_design= data.table::fread(design_target, data.table = FALSE)
    # Making the scene:
    if(interactive()){
      make_scene_custom(x = params_new$custom,
                        path = file.path(path_archimed,"app_parameters","scene"),
                        AMAPStudio = "D:/OneDrive/Travail_AMAP/PalmStudio/VPalm_IDE",
                        planting_design = planting_design, name = name) 
    }
  })
designs= unlist(designs, recursive = FALSE)

# East-West:
path= file.path(path_archimed,"app_parameters","scene","scenes","TripleInRow_EW_MAP_44.ops")
ops= readLines(path)
target_opf= tail(grep("TripleInRow_EW_Average_MAP_44.opf",ops),1)
ops[target_opf]= gsub("TripleInRow",
                      "Control",ops[target_opf])
writeLines(text = ops, con = path)
# North-South: 
path= file.path(path_archimed,"app_parameters","scene","scenes","TripleInRow_NS_MAP_44.ops")
ops= readLines(path)
target_opf= tail(grep("TripleInRow_NS_Average_MAP_44.opf",ops),1)
ops[target_opf]= gsub("TripleInRow",
                      "Control",ops[target_opf])
writeLines(text = ops, con = path)



tmp= lapply(designs, function(x)plot_design_rep(plot_design = x, voronois = 3, title=x$plot))
tmp



PLPE= list(Latitude= 0.9261918, Altitude = 45)


archimedR::set_config(file = file.path(path_archimed,"app_parameters/ArchimedConfiguration.properties"),
                      parameter = "meteoFile", value = "app_parameters/meteo/meteo_PLPE_02_2019.csv")

archimedR::read_config(file= file.path(path_archimed, "app_parameters/ArchimedConfiguration.properties"),parameter = "meteoFile")

archimedR::set_config(file = file.path(path_archimed,"app_parameters/ArchimedConfiguration.properties"),
                      parameter = "skySectors", value = 46)
archimedR::set_config(file = file.path(path_archimed,"app_parameters/ArchimedConfiguration.properties"),
                      parameter = "numberOfPixels", value = 200000)

set_config(file = file.path(path_archimed,"app_parameters/ArchimedConfiguration.properties"),
           parameter = "latitude", value = PLPE$Latitude)
set_config(file = file.path(path_archimed,"app_parameters/ArchimedConfiguration.properties"),
           parameter = "altitude", value = PLPE$Altitude)
sim_folder= "output_test"
set_config(file = file.path(path_archimed,"app_parameters/ArchimedConfiguration.properties"),
           parameter = "outputDirectory", value = sim_folder)

### Running the simulations

parameters= list("xMin","yMin","xMax","yMax")
design_files= list.files("1-data/PLPE_plot_design/",full.names = TRUE)

#  Reading the model manager: 
model_manager= fread(file.path(path_archimed,"app_parameters/models_manager/models_palmtree.csv"), data.table = F)

# Extracting the MAP
MAP= params$MAP

# Cleaning the Output folder:
unlink(file.path(path_archimed,sim_folder), recursive = T)

# NB: if ARCHIMED did not complete the simulation and returned an error, open the OPS file and 
# change the OPF files for ones that worked previously, run a simulation, change back the OPFs
# to the right ones and it may work.
tmp=
  mapply(function(x,y){
    # Set the scene boundaries:
    planting_design= data.table::fread(x, data.table = FALSE)
    tmp=
      lapply(parameters, function(z){
        value= unique(planting_design[,grep(z,colnames(planting_design), ignore.case = TRUE)])
        set_config(file = file.path(path_archimed,"app_parameters/ArchimedConfiguration.properties"),
                   parameter = z, value = value)
      })
    # Set the ops name: 
    name= gsub("design_", "", basename(x))%>%gsub(".csv","",.)
    path= file.path('app_parameters', "scene", "scenes", paste0(name, "_MAP_",y, ".ops"))
    set_config(file = file.path(path_archimed,"app_parameters/ArchimedConfiguration.properties"),
               parameter = "file", value = path)
    # Updating the model manager:
    model_manager$Node_id[model_manager$Group=="pavement"]= 2 + nrow(designs[[name]])
    fwrite(model_manager, file.path(path_archimed,"app_parameters/models_manager/models_palmtree.csv"),
           sep = ";", quote = FALSE)
    # Run ARCHIMED:
    run_archimed(exe = file.path(path_archimed,"archimed-lib_florian-1.0.0.jar"), memory = 4096, config= "Archimed")
  }, design_files[1], MAP[1])
# Test if the simulations ran successfully:
if(any(!tmp)){
  warning(paste("Error during ARCHIMED simulation for:\n *",paste(names(tmp)[!tmp], collapse = "\n * ")))
}

files_sim= 
  file.path(path_archimed,sim_folder)%>%
  list.files(recursive = TRUE, full.names = TRUE)

path_nodes= files_sim[grep("nodes_values.csv",files_sim)]
path_meteo= files_sim[grep("meteo.csv",files_sim)]
path_configs= files_sim[grep("ArchimedConfiguration.properties",files_sim)]


meteo= 
  import_meteo(x = file.path(path_meteo[1]))%>%
  mutate(date= as.POSIXct(date)+ seconds(hour_start))

Area_plots= 
  lapply(path_configs, function(x){
    plot_dim= archimedR::read_config(file= x, parameter = c("xMin","yMin","xMax","yMax"))
    
    data.frame(
      Treatment= x%>%dirname()%>%strsplit(split = "/")%>%unlist()%>%
        head(-1)%>%tail(1)%>%strsplit(split = " ")%>%unlist()%>%head(1)%>%
        gsub("_MAP_44","",.),
      Area_plot= (plot_dim$xMax-plot_dim$xMin)*(plot_dim$yMax-plot_dim$yMin)
    )
  })%>%data.table::rbindlist()


nodes= 
  lapply(path_nodes, function(x,y){
    name= 
      x%>%dirname()%>%strsplit(split = "/")%>%unlist()%>%
      head(-1)%>%tail(1)%>%strsplit(split = " ")%>%unlist()%>%head(1)%>%
      gsub("_MAP_44","",.)
    read_out_node(path = x,
                  duration = data.frame(step= meteo$step,
                                        timestep= meteo$timestep))%>%
      mutate(Treatment= name)
  })%>%data.table::rbindlist()%>%tibble::as_tibble()

df_tree_step= 
  nodes%>%
  group_by(Treatment)%>%
  mutate(id_type= ifelse(plantId==max(plantId,na.rm = TRUE),"soil","plant"))%>%
  ungroup()%>%
  filter(frondRank>0&id_type=="plant")%>%
  dplyr::left_join(Area_plots, by= "Treatment")%>%
  dplyr::left_join(meteo%>%select(date,step), by= "step")%>%
  group_by(step,Treatment,plantId)%>%
  summarise(Date= unique(date),
            Global_Intercepted_leaves= sum(.data$Global_Intercepted,na.rm=T),
            # Global intercepted radiation in MJ tree-1 timestep-1
            An= sum(.data$An_leaflet*.data$timestep*10^-6,na.rm=T), 
            # umol m-2 leaf s-1 -> mol tree-1 timestep-1
            transpiration= sum(.data$transpiration,na.rm=T),
            # Total transpiration in the plot in mm tree-1 timestep-1
            Leaf_Area= sum(.data$area, na.rm=T), N= n(),
            # Leaf area of each tree in m-2 tree-1
            Area_plot= mean(.data$Area_plot))%>%
  separate(Treatment, c("design","orientation"), "_", remove= FALSE)%>%
  group_by(Treatment,design,orientation,step)%>%
  mutate(density= n()/.data$Area_plot*10000,
         LAI= sum(.data$Leaf_Area)/.data$Area_plot)%>%
  ungroup()


df_tree_step$design= as.factor(df_tree_step$design)
levels(df_tree_step$design)= list(Control= "Control", LargeInterRow= "LargeInterRow", DoubleRow= "DoubleRow", DoubleInRow= "DoubleInRow", TripleInRow= "TripleInRow")


df_plot_step=
  df_tree_step%>%
  group_by(Treatment,design,orientation,step)%>%
  summarise(Date= unique(Date),
            Global_Intercepted_leaves=
              sum(.data$Global_Intercepted_leaves/.data$Area_plot,na.rm=T),
            # Global intercepted radiation in MJ m-2 timestep-1
            An= sum(.data$An/.data$Area_plot,na.rm=T), 
            # umol m-2 leaf s-1 -> mol m-2[plot] timestep-1
            transpiration= sum(.data$transpiration/.data$Area_plot,na.rm=T),
            # Total transpiration in the plot in mm timestep-1
            Leaf_Area= sum(.data$Leaf_Area, na.rm=T), N= n(),
            Area_plot= mean(.data$Area_plot),
            ntrees= length(unique(.data$plantId)))%>%
  mutate(LAI= .data$Leaf_Area/.data$Area_plot,
         density= .data$ntrees/.data$Area_plot*10000)

df_plot_step$design= as.factor(df_plot_step$design)
levels(df_plot_step$design)= list(Control= "Control", LargeInterRow= "LargeInterRow", DoubleRow= "DoubleRow", DoubleInRow= "DoubleInRow", TripleInRow= "TripleInRow")


df_tree_day=
  df_tree_step%>%
  group_by(Treatment,design,orientation,plantId)%>%
  summarise(Global_Intercepted_leaves= sum(.data$Global_Intercepted_leaves,na.rm=T),
            # Global intercepted radiation in MJ tree-1 day-1
            An= sum(.data$An,na.rm=T), 
            # umol m-2 leaf s-1 -> mol tree-1 day-1
            transpiration= sum(.data$transpiration,na.rm=T),
            # Total transpiration in the plot in mm tree-1 day-1
            Leaf_Area= mean(.data$Leaf_Area, na.rm=T), N= n(),
            # Leaf area of each tree in m-2 tree-1
            Area_plot= mean(.data$Area_plot),
            density= unique(.data$density),
            LAI= unique(.data$LAI))

df_plot_day=
  df_tree_day%>%
  group_by(Treatment,design,orientation)%>%
  summarise(Global_Intercepted_leaves= 
              sum(.data$Global_Intercepted_leaves/.data$Area_plot,na.rm=T),
            # Global intercepted radiation in MJ m-2 timestep-1
            An= sum(.data$An/.data$Area_plot,na.rm=T), 
            # umol m-2 leaf s-1 -> mol m-2[plot] timestep-1
            transpiration= sum(.data$transpiration/.data$Area_plot,na.rm=T),
            # Total transpiration in the plot in mm timestep-1
            Leaf_Area= mean(.data$Leaf_Area, na.rm=T),
            Area_plot= mean(.data$Area_plot),
            density= unique(.data$density),
            LAI= unique(.data$LAI))%>%
  mutate(WUE= An/transpiration)

df_tree_day$design= as.factor(df_tree_day$design)
levels(df_tree_day$design)= list(Control= "Control", LargeInterRow= "LargeInterRow", DoubleRow= "DoubleRow", DoubleInRow= "DoubleInRow", TripleInRow= "TripleInRow")

# Write the results in csv files
mapply(
  function(x,y){
    data.table::fwrite(x = x, file = file.path("2-outputs",paste0(y,".csv")))
  },
  list(df_tree_step,df_plot_step,df_tree_day,df_plot_day),
  c("df_tree_step","df_plot_step","df_tree_day","df_plot_day"))



### Visualize the results

library(RColorBrewer)
colors_vec= brewer.pal(5, "BrBG")
colors_vec[3]= "grey30"
names(colors_vec)= c("Control", "LargeInterRow","DoubleRow","DoubleInRow","TripleInRow")

gg_plot_step= 
  df_plot_step%>%
  ggplot(aes(x= Date, y= Global_Intercepted_leaves))+
  facet_wrap(.~orientation)+
  geom_area(aes(fill= design, color= design), position = 'identity', alpha= 0.3)+
  geom_point(aes(color= design), size= 1.2)+
  scale_fill_manual(values= colors_vec)+
  scale_colour_manual(values= colors_vec)+
  xlab("Time (hour of the day)")+
  ylab("Intercepted radiation (MJ m-2 half-hour-1)")+
  labs(fill= "Design")+
  guides(color= FALSE)+
  ggtitle("Half-hourly intercepted global radiation by trees at plot scale")

gg_plot_step
# ggplotly(gg_plot_step)


df_tree_step%>%
  mutate(plantId= as.character(plantId))%>%
  ggplot(aes(x= Date, y= Global_Intercepted_leaves))+
  facet_wrap(vars(design, orientation), ncol = 2)+
  geom_line(aes(color= design, lty= plantId), size= 1)+
  geom_point(aes(color= design, lty= plantId), size= 1)+
  geom_area(aes(fill= design, group= plantId), position = 'identity', alpha= 0.3)+
  scale_fill_manual(values= colors_vec)+
  scale_colour_manual(values= colors_vec)+
  labs(colour= "Design", plantId= "Plant") +
  ylab("Intercepted Radiation (MJ tree-1 half-hour-1)")+
  xlab("Time (hour of the day)")+
  ggtitle("Half-hourly intercepted global radiation at tree-scale")+
  guides(color= FALSE)


#### Daily time-step


df_tree_day%>%
  filter(plantId==0)%>%
  ggplot(aes(x= design, y=Global_Intercepted_leaves))+
  geom_bar(aes(color= design, fill= design, alpha= orientation),
           stat="identity", show.legend = TRUE, position = 'dodge')+
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5), panel.background = element_rect(fill = "gray90"))+
  labs(fill= "Design") + 
  scale_fill_manual(values= colors_vec)+
  scale_color_manual(values= colors_vec)+
  guides(color = FALSE)+
  ylab("Intercepted Radiation (MJ tree-1 day-1)")+
  ggtitle("Daily intercepted radiation at tree scale")

gg_plot_day_1= 
  df_plot_day%>%
  ggplot(aes(x= design, y=Global_Intercepted_leaves))+
  geom_bar(aes(color= design, fill= design, alpha= orientation),
           stat="identity", show.legend = TRUE, position = 'dodge')+
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5))+
  labs(fill= "Design", alpha= "") +
  scale_fill_manual(values= colors_vec)+
  scale_color_manual(values= colors_vec)+
  guides(color = FALSE)+
  ylab("Intercepted Radiation (MJ m-2 day-1)")+
  ggtitle("Daily intercepted radiation at plot scale")

plotly::ggplotly(gg_plot_day_1, tooltip= c("y", "x", "alpha"))


gg_plot_day=
  df_plot_day%>%
  select(-.data$Leaf_Area, -.data$Area_plot)%>%
  # Using select to order the variables correctly:
  mutate(An= An*12.01)%>%
  select(Treatment,orientation, design, density, LAI,
         Global_Intercepted_leaves,An,transpiration,WUE)%>%
  # rename("Intercepted~Radiation~(MJ~m^-2~day^-1)"= Global_Intercepted_leaves,
  #        "C~Assimilation~(g[C]~m^-2~day^-1)"= An,
  #        "Transpiration~(mm~day^-1)"= transpiration,
  #        "Palm~tree~density~(tree~hectare^-1)"= density,
  #        "LAI~(m^2~m^-2)"= LAI,
  #        "WUE (g[C]~mm^-1)"= WUE)%>%
  rename("Intercepted Radiation (MJ m-2 day-1)"= Global_Intercepted_leaves,
         "C Assimilation (gC m-2 day-1)"= An,
         "Transpiration (mm day-1)"= transpiration,
         "Palm tree density (tree hectare-1)"= density,
         "LAI (m2 m-2)"= LAI,
         "WUE (gC mm-1)"= WUE)%>%
  reshape2::melt(id.vars= c("Treatment","design","orientation"))%>%
  ggplot(aes(x= design, color= design, fill= design))+
  # facet_wrap(.~variable, scales = "free_y", labeller= label_parsed, ncol = 2)+
  facet_wrap(.~variable, scales = "free_y", ncol = 1)+
  geom_bar(aes(y= value,alpha= orientation),
           stat="identity", show.legend = TRUE, position = 'dodge')+
  xlab("")+
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5))+
  labs(fill= "",alpha="") + guides(color = FALSE)+
  scale_fill_manual(values= colors_vec)+
  scale_color_manual(values= colors_vec)+
  ggtitle("Daily outputs at plot scale")

# gg_plot_day
# ggsave("Daily outputs at plot scale.png", device = "png",
#        path = "2-outputs/plots",scale = 1, width = 20, height = 15, units = "cm",
#        dpi = 150)
plotly::ggplotly(gg_plot_day, tooltip= c("y", "x", "alpha"))
