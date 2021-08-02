plot_net <- function(n.net, net_name = "", current_cats = NULL, selected_category = NA) {
  
  text_labels <- c()

  # Create labels for the nodes
  for (i in 1:n.net$gal$n) { # iterates through the nodes
    row_data <- n.net$val[[i]]
    
    v_label <- c(paste0("<i>",(row_data$vertex.names),"</i>"))
    for (lab in names(row_data)[(names(row_data) != "na") & (names(row_data) != "vertex.names")]) {
      v_label <- c(v_label, paste0("<b>",lab,"</b>",": ",row_data[lab]))
    }
    
    text_labels <- c(text_labels, paste(v_label, collapse="\n"))
  }
  
  # convert \code{network} object to \code{igraph} object
  # in order to use igraph layout function
  set.seed(23) # reproducibility of plots
  ig.dataset <- asIgraph(n.net)
  L <- layout_nicely(ig.dataset) # let igraph determine the best layout
  
  # Create Vertices and Edges
  vs <- V(ig.dataset) # vertices
  Nv <- length(vs) # number of vertices
  
  es <- as.data.frame(get.edgelist(ig.dataset)) # edges
  Ne <- length(es[1]$V1) # number of edges
  
  # Create Nodes
  Xn <- L[,1] # x coordinate
  Yn <- L[,2] # y coordinate
  
  # if \code{selected_category} is provided, color of the nodes depends on it
  # otherwise use homogenous color
  if (!is.na(selected_category) & (length(current_cats) > 0)) {
    Cn <- as.factor(unlist(lapply(n.net$val, function(x) x[selected_category])))
    netplot <- plot_ly(x=Xn, y=Yn, color=Cn, colors="viridis", type = 'scatter', mode="markers", text=text_labels, hoverinfo="text")
  } else {
    netplot <- plot_ly(x=Xn, y=Yn, type = 'scatter', mode="markers", text=text_labels, hoverinfo="text") 
  }
  
  
  # Create Edges
  edge_shapes <- list() # unidrected
  arrow_shapes <- list() # directed
  
  dir_graph <- network::is.directed(n.net)
  
  for (i in 1:Ne) {
    v0 <- es[i,]$V1
    v1 <- es[i,]$V2
    
    if(dir_graph) {
      arrow_shape <- list(
        x = Xn[v1], ax = Xn[v0], axref="x", xref="x",
        y = Yn[v1], ay = Yn[v0], ayref="y", yref="y",
        showarrow = TRUE,
        arrowsize = 2, arrowwidth=.8, arrowhead=3, arrowcolor= "#03030344",
        text = ""
      )
      arrow_shapes[[i]] <- arrow_shape
    } else {
      edge_shape = list(
        type = "line",
        line = list(color = "#03030344", width=0.55),
        x0 = Xn[v0], y0 = Yn[v0],
        x1 = Xn[v1], y1 = Yn[v1]
      )
      edge_shapes[[i]] <- edge_shape
    }
    
  }
  
  axis <- list(title = "", showgrid = FALSE, showticklabels = FALSE, zeroline = FALSE)
  
  fig <- layout(netplot, shapes=edge_shapes, xaxis=axis, yaxis=axis) %>% 
    layout(legend=list(title=list(text=paste0('<b> ', selected_category ,' </b>'))), annotations=arrow_shapes)
  fig
}