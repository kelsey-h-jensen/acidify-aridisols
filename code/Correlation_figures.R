library(tidyverse)
library(ggpubr)
library(ggpmisc)

load(file = "../acidify-aridisols/data/acid.Rdata" )

plot_theme <- theme_bw()+
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
        axis.title = element_text(size = 16,face="bold"),
        axis.text.y = element_text(size=14),
        axis.text.x = element_text(size=14),
        axis.ticks = element_blank(), 
        legend.title = element_text(size = 16),
        legend.text = element_text(size = 14))



## Regression of washing vs fumigation

# SOC
lm1 <- lm(fum_gCm2 ~ DA_gCm2, data = acid )
lm1
summary(lm1)
anova(lm1)
# R^2 == 0.62,p < 0.001

# 13C
lm2 <- lm(fum_13C ~ DA_13C, data = acid )
lm2
summary(lm2)
anova(lm2)
# R^2 == 0.08,p == 0.03


# Correlation plot for gC m2
corr_soc <- acid %>%  
  ggplot(aes(x = fum_gCm2, y= DA_gCm2)) + geom_point(aes( shape = treatment), size = 2) + 
  geom_smooth(method = "lm") +
  geom_abline( intercept = 0, slope = 1) +
  coord_cartesian(xlim= c(0,1100), ylim= c(0, 1100))+
  annotate("text", x = 100, y = 900, size= 6,
           label = "atop(R^2 == 0.62,p < 0.001)", parse = TRUE) +
  plot_theme + 
  xlab(expression(paste(Fumigation~SOC~(g~C~"*"~m^{-2}))))+
  ylab(expression(paste(Acid~Wash~SOC~(g~C~"*"~m^{-2})))) +
  scale_shape_manual(name = "Treatment", 
                     values = c(19,17), 
                     labels=c(bquote(paste(Control~CO[2])),
                              bquote(paste(+CO[2]))))
  
corr_soc

ggsave(filename = "../acidify-aridisols/figures/corr_soc.png",
       plot= corr_soc, dpi = 300, width = 9, height = 6, units = "in")
  

# stats for correlations between 13C iso and 13C fum

# Relationship between 13C for each method
corr_isoC <- acid %>% 
  ggplot(aes(x = fum_isoC, y= DA_isoC, shape = treatment)) + geom_point(size = 2) + 
  geom_smooth(method = "lm") + 
  geom_abline(intercept = 0, slope = 1) +
  coord_cartesian(xlim=c(-32, -20), ylim= c(-32, -20)) +
  plot_theme +
  annotate("text", x = -22.5, y = -30, size= 6,
           label = "atop(R^2 == 0.08,p == 0.03)", parse = TRUE) + 
  scale_shape_manual(name = "Treatment", 
                     values = c(19,17), 
                     labels=c(bquote(paste(Control~CO[2])),
                              bquote(paste(+CO[2]))))+
  xlab(expression(paste(Fumigation~delta^{13},'C (‰)'))) +
  ylab(expression(paste(Acid~Wash~delta^{13},'C (‰)')))

corr_isoC

ggsave(filename = "../acidify-aridisols/figures/corr_isoC.png",
       plot= corr_isoC, dpi = 300, width = 9, height = 6, units = "in")

