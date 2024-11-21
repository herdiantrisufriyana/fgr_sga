#' Plot ontoarray
#'
#' Plot the output of \code{viz.ontoarray}.
#'
#' @param x Input, the output of \code{viz.ontoarray}.
#' @param pal Color pallette, a character vector of two colors respectively 
#' for negative and positive.
#' @param label Feature label, a logical whether feature label is shown.
#' @param label.family Font family of feature label, a character of the font 
#' name.
#' @param label.size Font size of feature label, an integer of the font size.
#' @param label.color Font color of feature label, a character of the font 
#' color.
#' @param grid_col Grid column, an integer of the number of columns in the 
#' array grid.
#'
#' @details
#' None.
#' 
#' @return Ontoarray plot, a ggplot object of the ontoarray.
#' 

plot.viz.ontoarray=function(x
                            ,pal=c("red","green")
                            ,label=F
                            ,label.family="sans"
                            ,label.size=3
                            ,label.color="white"
                            ,grid_col=3){
  
  # Represent the array to a dataframe.
  p=x %>%
    lapply(X=names(.),Y=.,function(X,Y){
      ontoarray = Y[[X]]$output
      ontoarray %>%
        lapply(X=seq(dim(.)[3]),Y=.,Z=X,FUN=function(X,Y,Z){
          Y[, , X] %>% matrix()
        }) %>%
        do.call(rbind, .) %>%
        as.data.frame() %>% 
        setNames("fill") %>%
        mutate(
          x=rep(1:dim(ontoarray)[1],dim(ontoarray)[2]* dim(ontoarray)[3])
          ,y=
            rep(1:dim(ontoarray)[2],dim(ontoarray)[1]) %>%
            sort() %>%
            rep(dim(ontoarray)[3])
          ,z=
            rep(1:dim(ontoarray)[3], dim(ontoarray)[1]*dim(ontoarray)[2]) %>%
            sort()
        ) %>%
        left_join(
          Y[[X]]$ontotype
          ,by=c("x","y","z")
        ) %>% 
        mutate(ontology=X)
    }) %>%
    do.call(rbind,.) %>%
    mutate(z=paste0("z=",z))
  
  # If no label, create an empty column for the feature.
  if (!label) p=p %>% mutate(feature = NA)
  
  # Determine global maximum absolute value of any element in the array.
  max_range=
    p$fill %>%
    abs() %>%
    max(na.rm=T)
  
  # Plot the ontoarray.
  p %>%
    ggplot(aes(x=y,y=x,fill=fill)) +
    geom_tile() +
    geom_text(
      aes(label=feature)
      ,family=label.family
      ,size=label.size
      ,color=label.color
      ,na.rm=T
    ) + 
    facet_wrap(ontology~z,ncol=grid_col) +
    coord_equal() +
    scale_fill_gradientn(
      "- < output > +"
      ,colors=c(pal[1],"black",pal[2])
      ,na.value=NA,limit=c(-max_range,max_range)
    ) +
    theme_void() +
    theme(
      strip.text=
        element_text(
          family=label.family
          ,size=label.size*3
        )
      ,legend.title=
        element_text(
          family=label.family
          ,size=label.size*3
        )
      ,legend.text=
        element_text(
          family=label.family
          ,size=label.size*3
        )
      ,legend.position="bottom"
    ) + 
    guides(fill=guide_colorbar(title.position="top",title.hjust=0.5))
}