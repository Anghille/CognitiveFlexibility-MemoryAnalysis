# PLot a parralel analysis with ggp
ggplot_pa = function(value, factor, vvalue){
  #Create data frame &amp;amp;amp;amp;amp;quot;obs&amp;amp;amp;amp;amp;quot; from observed eigenvalue data
  obs = data.frame(value)
  obs$type = c('Observed Data')
  obs$num = c(row.names(obs))
  obs$num = as.numeric(obs$num)
  colnames(obs) = c('eigenvalue', 'type', 'num')
  
  #Calculate quantiles for eigenvalues, but only store those from simulated CF model in percentile1
  percentile = apply(vvalue,2,function(x) quantile(x,.95))
  percentile1 = percentile[5:8]
  
  #Create data frame called &amp;amp;amp;amp;amp;quot;sim&amp;amp;amp;amp;amp;quot; with simulated eigenvalue data
  sim = data.frame(percentile1)
  sim$type = c('Simulated Data (95th %ile)')
  sim$num = c(row.names(obs))
  sim$num = as.numeric(sim$num)
  colnames(sim) = c('eigenvalue', 'type', 'num')
  
  #Merge the two data frames (obs and sim) together into data frame called eigendat
  eigendat = rbind(obs,sim)
  
  apatheme=theme_apa()+
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_blank(),
          panel.border = element_blank(),
          legend.title=element_blank(),
          legend.position=c(.7,.8),
          axis.line.x = element_line(color='black'),
          axis.line.y = element_line(color='black'))
  
  #Use data from eigendat. Map number of factors to x-axis, eigenvalue to y-axis, and give different data point shapes depending on whether eigenvalue is observed or simulated
  p = ggplot(eigendat, aes(x=num, y=eigenvalue, shape=type)) +
    #Add lines connecting data points
    geom_line()+
    #Add the data points.
    geom_point(size=4)+
    #Label the y-axis 'Eigenvalue'
    scale_y_continuous(name='Eigenvalue')+
    #Label the x-axis 'Factor Number', and ensure that it ranges from 1-max # of factors, increasing by one with each 'tick' mark.
    scale_x_continuous(name='Factor Number', breaks=min(eigendat$num):max(eigendat$num))+
    #Manually specify the different shapes to use for actual and simulated data, in this case, white and black circles.
    scale_shape_manual(values=c(16,1)) +
    #Add vertical line indicating parallel analysis suggested max # of factors to retain
    geom_vline(xintercept = factor, linetype = 'dashed')+
    #Apply our apa-formatting theme
    apatheme
  #Call the plot. Looks pretty!
  return(p)
}



# Plot a fitted lavaan object
ggsem <- function(fit, layout = "sugiyama") {
  
  # Extract standardized parameters
  params <- lavaan::standardizedSolution(fit)
  
  # Edge properties
  param_edges <- params %>% 
    filter(op %in% c("=~", "~", "~~"), lhs != rhs, pvalue < .10) %>%
    transmute(to = lhs,
              from = rhs,
              val = est.std,
              type = dplyr::case_when(
                op == "=~" ~ "loading",
                op == "~"  ~ "regression",
                op == "~~" ~ "correlation",
                TRUE ~ NA_character_))
  
  # Identify latent variables for nodes
  latent_nodes <- param_edges %>% 
    filter(type == "loading") %>% 
    distinct(to) %>% 
    transmute(metric = to, latent = TRUE)
  
  # Node properties
  param_nodes <- params %>% 
    filter(lhs == rhs) %>% 
    transmute(metric = lhs, e = est.std) %>% 
    left_join(latent_nodes) %>% 
    mutate(latent = if_else(is.na(latent), FALSE, latent))
  
  # Complete Graph Object
  param_graph <- tidygraph::tbl_graph(param_nodes, param_edges)
  
  # Plot
  ggraph(param_graph, layout = layout) +
    # Latent factor Nodes
    geom_node_point(aes(alpha = as.numeric(latent)),
                    shape = 16, size = 5) +
    geom_node_point(aes(alpha = as.numeric(latent)),
                    shape = 16, size = 4, color = "white") +
    # Observed Nodes
    geom_node_point(aes(alpha = as.numeric(!latent)),
                    shape = 15, size = 5) +
    geom_node_point(aes(alpha = as.numeric(!latent)),
                    shape = 15, size = 4, color = "white") +
    # Regression Paths (and text)
    geom_edge_link(aes(color = val, label = round(val, 2),
                       alpha = as.numeric(type == "regression")),
                   linetype = 1, angle_calc = "along", vjust = -.5,
                   arrow = arrow(20, unit(.3, "cm"), type = "closed")) +
    # Factor Loadings (no text)
    geom_edge_link(aes(color = val, alpha = as.numeric(type == "loading")),
                   linetype = 3, angle_calc = "along",
                   arrow = arrow(20, unit(.3, "cm"), ends = "first", type = "closed")) +
    # Correlation Paths (no text)
    geom_edge_link(aes(color = val, alpha = as.numeric(type == "correlation")),
                   linetype = 2, angle_calc = "along",
                   arrow = arrow(20, unit(.3, "cm"), type = "closed", ends = "both")) +
    # Node names
    geom_node_text(aes(label = metric),
                   nudge_y = .25, hjust = "inward") +
    # Node residual error
    geom_node_text(aes(label = sprintf("%.2f", e)),
                   nudge_y = -.1, size = 3) +
    # Scales and themes
    scale_alpha(guide = FALSE, range = c(0, 1)) +
    scale_edge_alpha(guide = FALSE, range = c(0, 1)) +
    scale_edge_colour_gradient2(guide = FALSE, low = "red", mid = "darkgray", high = "green") +
    scale_edge_linetype(guide = FALSE) +
    scale_size(guide = FALSE) +
    theme_graph()
}