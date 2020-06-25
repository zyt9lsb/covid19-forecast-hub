get_g_ensemble_quantiles<- function(d){
  ggplot(d %>%
           mutate(team_model = paste(team,model,sep="-")),
         aes(x = quantile, y = team_model, fill = yes)) +
    geom_tile() +
    theme_bw() +
    theme(legend.position = "none",
          axis.text.x = element_text(angle = 90, hjust = 1))
}