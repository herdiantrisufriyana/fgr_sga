#' Plot ontonet
#'
#' Plot the output of \code{viz.ontonet}.
#'
#' @param x Input, the output of \code{viz.ontonet}.
#' @param node.color Node color, a character of the node line color.
#' @param node.fill Node fill, a character of the node fill color.
#' @param node.shape Node shape, a character of the node shape. It can be 
#' circle or square.
#' @param node.size Node size, a numeric of the node size.
#' @param label Feature label, a logical whether feature label is shown.
#' @param label.family Font family of feature label, a character of the font 
#' name.
#' @param label.size Font size of feature label, an integer of the font size.
#' @param label.color Font color of feature label, a character of the font 
#' color.
#' @param grid_col Grid column, an integer of the number of columns in the 
#' array grid.
#' @param edge.color Edge color, a character of the edge line color.
#' @param seed_num Random seed, an integer for reproducible positioning of 
#' nodes (randomly positioned).
#' @param ... Any argument to \code{plot.igraph}.
#'
#' @details
#' None.
#' 
#' @return Ontonet plot, a igraph plot of the ontonet.
#' 

plot.viz.ontonet=function(x
                          ,node.color=NA
                          ,node.fill='black'
                          ,node.shape='circle'
                          ,node.size=20
                          ,label=F
                          ,label.family='sans'
                          ,label.cex=0.5
                          ,label.color='white'
                          ,edge.color='black'
                          ,seed_num=33
                          ,...){
  
  # Do node coloring.
  node=
    x$node %>%
    mutate(color=ifelse(is.na(color),node.fill,color))
  
  # Create igraph object from edge dataframe.
  set.seed(seed_num)
  ontograph=
    x$edge %>%
    graph_from_data_frame(directed=TRUE)
  
  # Do node coloring and shaping.
  V(ontograph)$color=node$color
  V(ontograph)$shape=node.shape
  
  # Make up the feature label if it is warranted.
  if(label){
    V(ontograph)$label=
      ifelse(
        is.na(x$node$avg)
        ,names(V(ontograph))
        ,paste0(names(V(ontograph)),'\n',round(x$node$avg,3))
      )
    V(ontograph)$label.family=label.family
    V(ontograph)$label.cex=label.cex
    V(ontograph)$label.color=label.color
  }else{
    V(ontograph)$label=NA
  }
  
  # Plot the ontonet.
  ontograph %>%
    plot.igraph(
      layout=layout_as_tree(.,mode='in')
      ,vertex.frame.color=node.color
      ,vertex.size=node.size
      ,edge.color=edge.color
      ,...
    )
  
}