
#' Make a scene from pre-formatted VPalm inputs (Vpalmr::make_scene take
#' parameters as inputs)
make_scene_custom= function(x, path, AMAPStudio, planting_design= NULL, 
                            plant_dist= 9.2, name= NULL, progress= NULL){
  # As make_scene but for already-formated VPalm outputs for one tree only, comming
  # from user-input. 
  if(is.null(name)){
    name= "custom"
  }
  VPalm_in = format_tree(data = x)
  up_progress(progress, "format_tree")
  params = write_tree(data = VPalm_in, path = file.path(path,"VPalm_inputs"),
                      name= name, verbose = F, overwrite = TRUE)
  up_progress(progress, "write_tree")
  if(params) {
    message("VPalm parameters file was successfully written in: ", 
            file.path(path, "1-VPalm_inputs"))
  }else{
    stop("Error during VPalm parameter file writing")
  }
  
  MAP= VPalm_in$value[grep("Modelled Months After Planting", VPalm_in$name)]
  OPFs = make_opf(parameter = file.path(path, "VPalm_inputs", paste0(name,"_MAP_",MAP,".txt")), 
                  opf = file.path(path, "scenes","opf",paste0(name,"_Average_MAP_",MAP,".opf")),
                  AMAPStudio = AMAPStudio, overwrite = TRUE)
  up_progress(progress, "make_opf")
  
  if(is.null(planting_design)){
    planting_design = design_plot(rows = 1, cols = 1, x0 = 0, 
                                  x_dist = plant_dist)$design
  }
  up_progress(progress, "design_plot")
  
  format_ops(design = planting_design, Progeny = name, map = MAP,
             average = TRUE) %>% 
    write_ops(file.path(path, "scenes",paste0(name, "_MAP_",MAP, ".ops")), 
              overwrite = TRUE)
  up_progress(progress, "make_ops_all")
  
  out= list(plot_design= planting_design)
  setNames(out, name)
}

#' Imports a template of VPalm inputs
template= function(){
  readRDS("1-data/VPalm_template/vpalm_template.rds")
}

#' Update the template values
update_param= function(input){
  param_list= template()
  # param_list$nbFronds_M= input$nbleaves_custom
  param_list$MAP_requested= input$MAP
  param_list$nbLeafEmitted= round(param_list$MAP_requested*2.5)
  param_list$rachisLengthSlope= input$Lrac_slp
  param_list$rachisLengthIntercept= 
    input$rachisLengthRank1 - param_list$rachisLengthSlope * param_list$nbLeafEmitted
  
  # param_list$nbMax= input$nbMax
  # param_list$nbSlope= input$nbSlope
  # param_list$nbInfl= input$nbInfl
  param_list$leafletLengthAtBSlope= input$bLengthSlope
  param_list$lenfletLengthAtBIntercept= 
    input$BlengthRank1 - input$rachisLengthRank1 * param_list$leafletLengthAtBSlope
  
  param_list$bWidthSlope= input$bWidthSlope
  param_list$bWidthIntercept=
    input$BwidthRank1 - input$rachisLengthRank1 * param_list$bWidthSlope
  # param_list$xm_intercept= input$xm_intercept
  # param_list$xm_slope= input$xm_slope
  # param_list$ym_intercept= input$ym_intercept
  # param_list$ym_slope= input$ym_slope
  param_list$petioleRachisRatio_M= input$ratioL
  # param_list$decMaxA= input$decMaxA
  # param_list$decSlopeA= input$decSlopeA
  # param_list$decInflA= input$decInflA
  param_list$decliCintercept= input$decliC_intercept
  param_list$decliCslope= input$decliC_slope
  
  list(custom= param_list)
}

#' Plot the planting design with repetition
#'
#' @description Make a plot of the planting design with repetitions of the voronoï.
#' @param plot_design The design of the plot, as of the format from [`make_scene_custom()`]
#' @param xlim The boundaries in the x direction, e.g. `c(0,100)`
#' @param ylim The boundaries in the y direction, e.g. `c(0,100)`
#' @param voronois The number of voronois (how many time to repeat the design). Optional, only
#'                 needed if xlim and ylim are not set. 
#' @param border Number of edge voronois to add to the plots (will be cutted out by plot dimensions)
#' @param image The (optional) image of the plant 
#' @param plant_size The size of the image of the plant
#' @param title The plot title
#' 
#' @return
#' @export
#'
#' @examples
plot_design_rep= function(plot_design, xlim= NULL, ylim= NULL, voronois= NULL, 
                          border= 0, image= NULL, plant_size= 0.13, title= NULL){
  
  if(is.null(voronois)){
    voronoi_x= (xlim[2]-xlim[1]) %/% unique(plot_design$xmax-plot_design$xmin)
    voronoi_y= (ylim[2]-ylim[1]) %/% unique(plot_design$ymax-plot_design$ymin)
    n_vor= data.frame(x= voronoi_x,y= voronoi_y)
  }else{
    plot_range= c(x= max(plot_design$x),y= max(plot_design$y))
    wich_big= names(which.max(plot_range))
    wich_small= names(which.min(plot_range))
    # Taking the bigger side (either x or y):
    max_vor_big= max(plot_design[[wich_big]])*voronois+max(plot_design[[wich_big]])/2
    # How many smallest side to take to match the length of the big side * voronois:
    n_vor_small= round(max_vor_big/(max(plot_design[[wich_small]])*1.5))
    n_vor= data.frame(voronois,n_vor_small)
    names(n_vor)= c(wich_big,wich_small)
  }
  if(any(n_vor<1)){
    stop("No voronoï can be fitted in the scene, please correct the values of ",
         crayon::red("voronois"), " or ", crayon::red("xlim"), " and ", crayon::red("ylim"))
  }
   
  # Matrix of the design (each cell is a Voronoi):
  mat_plot= expand.grid(Row= 1:(n_vor$y+border), Col= 1:(n_vor$x+border))
  
  # Full design:
  design=
    mapply(function(Row,Col){
      plot_design%>%
        select(x,y,xmax,ymax,xmin,ymin)%>%
        mutate(xmin= xmax*(Col-1), ymin= ymax*(Row-1),
               x= x+xmin, y= y+ymin,
               xmax= xmax*Col, ymax= ymax*Row,
               Col= Col, Row= Row)
    }, Row= mat_plot$Row, Col= mat_plot$Col)%>%t()%>%as_tibble()%>%
    tidyr::unnest(cols = c(x, y, xmax, ymax, xmin, ymin, Col, Row))
  
  if(is.null(voronois)){
    ranges_full= c(xlim, ylim)
  }else{
    ranges_full= rep(range(design$xmax-(plot_design$xmax*border),
                           design$ymax-(plot_design$ymax*border),
                           design$xmin-(plot_design$xmin*border),
                           design$ymin-(plot_design$ymin*border)),2)
    # NB: remove some because we add some edge voronois to the plot (e.g. design$xmax-plot_design$xmax)
  }
  
  voronoi_stands=
    design%>%
    mutate(group= paste0('x:',Col,", y:",Row))%>%
    group_by(group)%>%
    summarise(coords= 
                list(expand.grid(x= unique(c(xmin,xmax)),
                                 y= unique(c(ymin,ymax))))
    )%>%
    mutate(v_id= as.factor(1:n()))%>%
    tidyr::unnest(cols = c(coords))%>%  
    group_by(v_id)%>%
    mutate(pos= ifelse(x==min(x)&y==min(y),1,
                       ifelse(x==min(x)&y==max(y),2,
                              ifelse(x==max(x)&y==min(y),4,3))))%>%
    arrange(v_id,pos)
  
  p=  design%>%
    mutate(image= image)%>%
    ggplot(aes(x= .data$x, y= .data$y))+
    # geom_image(aes(image= image), size= plant_size/3)+
    geom_point(aes(color= "Palm tree center"))+
    theme(legend.position="bottom")+
    labs(fill= "Voronoi index", x= "x coordinate (m)", y= "y coordinate (m)")+
    geom_polygon(data= voronoi_stands, aes(x= x, y= y, fill= v_id, color= v_id), alpha= 0.2,
                 show.legend = FALSE)+
    guides(color= FALSE)+
    coord_fixed(xlim= ranges_full[1:2], ylim= ranges_full[3:4])+
    ggtitle(title) + theme(plot.title = element_text(size=10))
  
  if(!is.null(image)){
    p + geom_image(data= design%>%mutate(image= image),
                   aes(image= image), size= plant_size/3)
  }else{
    p
  }
}
