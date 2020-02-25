params=data.table::fread("1-data/processed/param_Vpalm_PLPE_design.csv", data.table = F)

designs_names= paste(params$Design,params$Orientation, sep="_")
design_files= list.files("1-data/PLPE_plot_design/",full.names = TRUE)

designs=lapply(1:length(designs_names), function(x){
    name= paste(params[x,]$Design,params[x,]$Orientation, sep="_")
    # Importing the right design:
    design_target= design_files[grep(name,design_files)]
    planting_design= data.table::fread(design_target, data.table = FALSE)
  })
names(designs)= designs_names

