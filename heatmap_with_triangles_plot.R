heatmap_with_triangles_plot <- function(d){

library(ggplot2)
library(RColorBrewer)

#plot a triangle fielded heatmap
d1 <- subset(d, variable=='Value.x')
d1$value <- 1-as.numeric(d1$value)
d2 <- subset(d, variable=='Value.y')
d2$value <- factor(d2$value, levels = c(NA, 3,0))

p1 <- ggplot()+ geom_polygon(data=d1, aes(x=x, y=y, group=group, fill=value), color=NA) +
  geom_polygon(data=d2, aes(x=x, y=y, group=group, alpha=value), color=NA) +
  coord_fixed(0.5)+
  theme_bw() + theme(panel.grid=element_blank()) +
scale_x_continuous(name='Normalisation',
                 breaks=0.5:(length(unique(d1$Normalisation))-0.5),
                 labels=unique(d$Normalisation)) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1,
                                   size = 12, hjust = 1))+
  scale_y_continuous(name='Regression',
                     breaks=0.5:(length(unique(d1$Regression))-0.5),
                     labels=unique(d$Regression)) +
  scale_fill_distiller(name= 'Acceptance rate',
                    limits=c(0,1),
                    direction = 1,
                    palette= "RdYlBu")+
  scale_alpha_discrete(name='Isolated cells\npresent',
                       breaks=c(0,3),
                       labels=c('FALSE','TRUE'),
                       na.value=0.65)
return(p1)
}
