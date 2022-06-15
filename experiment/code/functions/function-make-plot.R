#==========================================================================
# Plot Model Predictions (incl Density Distribution for the learning model)
#
#==========================================================================
# 
make_plot_design <- function(data, id) {
    data = data[id == id]
  
  the_name <- "Sample Size"
  the_labels <- c("Small (3 per option)", "Large (10 per option)")
  the_colors <- c("red", "grey25")
  ggplot(
    data = data,
    mapping = aes(x = trial, y = sum_belief, color = samplinggroup, fill = samplinggroup)) +
    stat_halfeye( # uses median and QI = quantile interval (also known as the percentile interval or equi-tailed interval)
      #.width = c(.66, 0.95), #use .66, .95 to show 66 and 96% HDI
      slab_alpha = 0.15,
      position = position_dodge(width = .01),
      aes(shape = samplinggroup), point_fill = "white") +
    geom_hline(data, mapping = aes(yintercept = prhv_rsft)) +
    theme_classic() +
    scale_y_continuous(limits = c(0,1), expand = c(0,0))+
    scale_fill_manual( values = the_colors, name = the_name, labels = the_labels) +
    scale_colour_manual( values = the_colors, name = the_name, labels = the_labels) +
    scale_shape_manual( values = c(16, 21), name = the_name, labels = the_labels) +
    facet_wrap(~trial + state + id, labeller = label_both, scale = "free_x") +
    ylab("Predicted Proportion of Risky Choices") +
    labs(title = "Environment",
         subtitle = paste0("Reach ", data$b[1], " in 3 trials: ", data$xh[1], " (",data$pxh[1],") or " , data$yh[1], "; ",
                           data$xl[1]," (", data$pxl[1], ") or ",data$yl[1])) +
    theme(axis.title.x = element_blank(),
          axis.text.x = element_blank(),
          axis.ticks.x = element_blank())
}

make_plot_critical <- function(data, id) {
  
  if(!missing(id)){
    data = data[id == id]
  }
  
  the_name <- "Sample Size"
  the_labels <- c("Large (10 per option)","Small (3 per option)")
  the_colors <- c("red", "grey25")
  ggplot(
    data = data,
    mapping = aes(x = trial, y = sum_belief, color = samplinggroup, fill = samplinggroup)) +
    stat_halfeye( # uses median and QI = quantile interval (also known as the percentile interval or equi-tailed interval)
      #.width = c(.66, 0.95), #use .66, .95 to show 66 and 96% HDI
      slab_alpha = 0.15,
      position = position_dodge(width = .01),
      aes(shape = samplinggroup), point_fill = "white") +
    geom_hline(data, mapping = aes(yintercept = prhv_rsft)) +
    theme_classic() +
    scale_y_continuous(limits = c(0,1), expand = c(0,0))+
    scale_fill_manual( values = the_colors, name = the_name, labels = c("3" = "Small (3 per option)", "10" = "Large (10 per option)")) +
    scale_colour_manual( values = the_colors, name = the_name, labels = c("3" = "Small (3 per option)", "10" = "Large (10 per option)")) +
    scale_shape_manual( values = c(16, 21), name = the_name, labels = c("3" = "Small (3 per option)", "10" = "Large (10 per option)")) +
    facet_wrap(~id + trial + state, labeller = label_both, scale = "free_x") +
    ylab("Predicted Proportion of Risky Choices") +
    theme(axis.title.x = element_blank(),
          axis.text.x = element_blank(),
          axis.ticks.x = element_blank())
}


maxdensity <- function(data,id){
  data = data[id == id]
  vmax <- NULL
  comb = unique(data[,.(trial,state,samplinggroup)])
  
  for(i in 1:nrow(comb)){
    samp <- comb$samplinggroup[i]
    t <- comb$trial[i]
    s = comb$state[i]
    sdb <- data[samplinggroup == samp & trial == t & state == s,]
    v = which.max(density(sdb$sum_belief)$y)
    max <- density(sdb$sum_belief)$x[v]
    vmax <- c(vmax, max)
    sdb <- NULL
  }
  
  densitypeak <- data.table(comb, peak = vmax)
  return(densitypeak)
}

densplot = function(data, id){
  the_name <- "Sample Size"
  the_labels <- c("Small (3 per option)", "Large (10 per option)")
  the_colors <- c("red", "grey25")
  p1 <- ggplot(data,
               aes(x=sum_belief, fill = samplinggroup, colour = samplinggroup))+
    theme_classic() +
    geom_density(alpha = 0.15) +
    scale_x_continuous(limits = c(0,1), expand = c(0,0))+
    scale_y_continuous(limits = c(0,20), expand = c(0.03,0))+
    geom_vline(data, mapping = aes(xintercept = prhv_rsft), linetype = 2, color = "grey37") +
    geom_point(maxdensity(data = data, id = id), mapping = aes(x = peak, y = 0, fill= samplinggroup), shape = 21, size = 3, alpha = 0.5) +
    #geom_vline(maxdensity(data = data, id = id), mapping = aes(xintercept = peak, colour= samplinggroup), linetype = 2, size = 0.5, alpha = 1)+
    coord_flip() +
    scale_fill_manual(values=the_colors, name = the_name ,labels = the_labels) +
    scale_colour_manual(values= the_colors, name = the_name ,labels = the_labels) +
    facet_wrap(~trial+state, labeller = label_both, scale = "free_x") +
    xlab("Predicted Proportion of Risky Choices") +
    labs(title = "Environment", subtitle = paste("Reach", data$b[1], "in 3 trials"))
  return(p1)
}
?geom_density
