# calculating ecosystem stocks
library(tidyverse)

isoC_means <- read.csv(file = "../acidify-aridisols/models/output/emmeans_trt_method_isoC.csv")
soc_means <- read.csv(file = "../acidify-aridisols/models/output/emmeans_trt_method.csv")
mean_diff <- read.csv(file = "../acidify-aridisols/models/output/emmeans_diff_gCm2.csv")

simple_theme_vertical <- theme(axis.line = element_line(colour = "black"),
                               axis.title.y = element_text(size = 16, face="bold"), 
                               axis.title.x =element_text(size = 16, face="bold"), 
                               axis.text.y = element_text(size=14),
                               axis.text.x = element_text(size=12),
                               axis.ticks = element_blank(),
                               legend.text = element_text(size = 14),
                               legend.title = element_text(size = 14, face= "bold"))

plant.labs <- c("A.dumosa", "Interspace", "L. tridentata", "L. Andersonii", "L. pallidum", "P. rigida")
names(plant.labs) <- c("AMDU", "INSP","LATR","LYAN","LYPA","PLRI")

method.labs <- c("Acid Washing", "Fumigation")
names(method.labs) <- c("DA", "fum")

################
# Difference between treatments (fum- wash)
################

mean_diff <- diff_methods %>% 
  ggplot(aes(x= reorder(plant, diff_gCm2), y = diff_gCm2, shape = treatment)) + 
  geom_point(size=4, position = position_dodge(.3)) +
  geom_linerange(aes(ymin = diff_gCm2 - diff_se, ymax =  diff_gCm2 + diff_se, 
                     linetype = treatment), size=1, position = position_dodge(.3)) +
  ylab(expression(paste(Fumigation-Washing~(g~SOC~"*"~m^{-2})))) +
  xlab(element_blank()) +
  scale_x_discrete(limits=c("INSP", "PLRI","AMDU","LYAN","LYPA","LATR"),
                   labels=c("Interspace", "P. rigida","A. dumosa",
                            "L. andersonii","L. pallidum",  "L. tridentata"))+
  scale_shape_manual(name = "Treatment", 
                     values = c(19,17), 
                     labels=c(bquote(paste(Control~CO[2])),
                              bquote(paste(+CO[2])))) +
  scale_color_discrete(guide = "none") +
  scale_linetype_discrete(guide = "none") +
  plot_theme +
  coord_cartesian(ylim = c(0, 400)) + 
  theme(axis.text.x = element_text(size=12, face = "italic",  angle = 30, vjust = 0.8, hjust = .7))

ggsave(filename = "../acidify-aridisols/figures/method_diff_plant.png",
       plot= mean_diff, dpi = 300, width = 9, height = 6, units = "in")


######################
# Method comp SOC gC m2
#######################


trt_means <- soc_means %>% 
  ggplot(aes(x = reorder(plant, response), y = response, shape = treatment)) + 
  geom_point(size = 2, position = position_dodge(.3))+
  facet_grid(~method, labeller = labeller(method = method.labs)) +
  geom_linerange(aes(ymin = response - SE, ymax =  response + SE), 
                 size=.6, position = position_dodge(.3)) +
  scale_shape_manual(name = "Treatment", 
                     values = c(19,17), 
                     labels=c(bquote(paste(Control~CO[2])),
                              bquote(paste(+CO[2])))) +
  scale_linetype_discrete(guide = "none") +
  scale_x_discrete(limits=c("INSP", "PLRI","AMDU","LYAN","LYPA","LATR"),
                   labels=c("Interspace", "P. rigida","A. dumosa",
                            "L. andersonii","L. pallidum",  "L. tridentata"))+
  ylab(expression(paste(SOC~(g~C~"*"~m^{-2})))) +
  xlab(element_blank()) + plot_theme +
  coord_cartesian(ylim = c(0, 900)) +
  theme(strip.text.x = element_text(size = 12, face = "bold"),
        axis.text.x = element_text(size=12, face = "italic", 
                                   angle = 30, vjust = 0.8, hjust = .7))

ggsave(filename = "../acidify-aridisols/figures/soc_method.png",
       plot= trt_means, dpi = 300, width = 9, height = 6, units = "in")

######################
# Method comp SOC isoC
#######################


isoC_data <- read.csv(file = "../Mojave_Carbonate/Data/emmeans_trt_method_isoC.csv", 
                      header = T)

isoC_diff <- isoC_means %>% 
  ggplot(aes(x = reorder(plant, emmean), y = emmean, shape = treatment)) + 
  geom_point(size = 2, position = position_dodge(.3))+
  facet_grid(~method, labeller = labeller(method = method.labs)) +
  geom_linerange(aes(ymin = emmean - SE, ymax =  emmean + SE), 
                 size=.6, position = position_dodge(.3)) +
  scale_shape_manual(name = "Treatment", 
                     values = c(19,17), 
                     labels=c(bquote(paste(Control~CO[2])),
                              bquote(paste(+CO[2])))) +
  scale_linetype_discrete(guide = "none") +
  scale_x_discrete(limits=c("INSP", "PLRI","AMDU","LYAN","LYPA","LATR"),
                   labels=c("Interspace", "P. rigida","A. dumosa",
                            "L. andersonii","L. pallidum",  "L. tridentata"))+
  ylab(expression(paste(SOC~delta^{13},'C (‰)'))) +
  xlab(element_blank()) + plot_theme +
  theme(strip.text.x = element_text(size = 12, face = "bold"),
        axis.text.x = element_text(size=12, face = "italic", 
                                   angle = 30, vjust = 0.8, hjust = .7))

ggsave(filename = "../acidify-aridisols/figures/isoC_method.png",
       plot= isoC_diff, dpi = 300, width = 9, height = 6, units = "in")
