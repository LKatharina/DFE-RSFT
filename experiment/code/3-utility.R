#==========================================================================
# Utility of Choice problem 
#
#==========================================================================

# Packages ----------------------------------------------------------------
pacman::p_load(data.table)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
sim <- readRDS("../stimuli/list-stimuli-t3-predictions.rds")

sim <- as.data.table(sim)
sim[, nr := 1:nrow(sim)]

w1 <- w2 <- w3 <- 1/3
sim[, u1 := as.numeric(( sum_belief > 0.5 & prhv_rsft < 0.5 ) | (sum_belief < 0.5 & prhv_rsft > 0.5))]
sim[, u3 := 2*abs(0.5 - prhv_rsft)]
agg = sim[, .(u1 = mean(u1), u2 = abs(mean(sum_belief) - mean(prhv_rsft)), u3 = mean(u3)), by = c("id", "samplinggroup", "trial", "state")]
agg[, utility := c(w1,w2,w3) %*% c(u1,u2,u3)*(u1 > 0), by = c("id", "samplinggroup", "trial", "state")][order(-utility)]


make_plot <- function(data) {
  the_name <- "Sample Size"
  the_labels <- c("Large (5 per option)", "Small (3 per option)")
  the_colors <- c("red", "grey25")
  ggplot(
    data = data,
    mapping = aes(x = trial, y = sum_belief, color = sampling, fill = sampling)) +
    stat_halfeye( # uses median and QI = quantile interval (also known as the percentile interval or equi-tailed interval)
      .width = c(.66, 0.95), #use .66, .95 to show 66 and 96% HDI
      slab_alpha = 0.15,
      position = position_dodge(width = .09),
      aes(shape = sampling), point_fill = "white") +
    theme_classic() +
    scale_fill_manual(
      values=the_colors,
      name = the_name,
      labels = the_labels) +
    scale_colour_manual(
      values=the_colors,
      name = the_name, labels = the_labels) +
    scale_shape_manual(
      values = c(16, 21),
      name = the_name, labels = the_labels) +
    facet_wrap(~trial+state, labeller = label_both, scale = "free_x") +
    ylab("Predicted Proportion of Risky Choices") +
    labs(title = "Environment",
         subtitle = paste("Reach", data$b[1], "in 3 trials")) +
    theme(axis.title.x = element_blank(),
          axis.text.x = element_blank(),
          axis.ticks.x = element_blank())
}





utility <- function(x,y){
  diff <- abs(x - y)
  dis <-  as.numeric(( x > 0.5 & y < 0.5 ) | (x < 0.5 & y > 0.5)) # 1 if true , 0 if false
  return( c(diff * dis))
}

utility_of_design <- function(x, w, m1, m2){
  colm1 <- grep(m1,names(x)) # Sucht nach einem Muster in einem Vektor, bei uns nach dem Namen rsft
  colm2 <- grep(m2,names(x)) # Sucht nach einem Muster in einem Vektor, bei uns nach dem Namen dcpt
  utvec <- utility(x = x[,..colm1], y = x[,..colm2]) # .. --> Ausserhalb der data table suchen
  return(utvec)
}

sim[ , ut := utility_of_design(.SD, w = 0.5, m1 = "sum_belief", m2 = "prhv_rsft"), by= "nr"]
sim[ , .(utility = 1- mean((1 - ut)^2)) , by = c("id", "samplinggroup", "trial", "state")]

