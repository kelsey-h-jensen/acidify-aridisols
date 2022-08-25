library(tidyverse)
library(lme4)
library(lmerTest)
library(emmeans)


load(file = "../acidify-aridisols/data/acid.Rdata" )
load(file = "../acidify-aridisols/data/acid_long.Rdata" )


## Regression of washing vs fumigation

# SOC
lm1 <- lm(fum_gCm2 ~ DA_gCm2, data = acid )
lm1
summary(lm1)
anova(lm1)

# 13C
lm2 <- lm(fum_13C ~ DA_13C, data = acid )
lm2
summary(lm2)
anova(lm2)



# Difference in method by cover type and treatment
plot(sqrt(abs(acid$diff_gCm2)))
lmer1 <- lmer(sqrt((abs(diff_gCm2))) ~ (plant+treatment)^2 + (1|ring), data = na.omit(acid))

plot(predict(lmer1), residuals(lmer1)) 
qqnorm(residuals(lmer1)); qqline(residuals(lmer1))
hist(residuals(lmer1))

summary(lmer1)
av1 <- anova(lmer1)

paircomp1 <- summary((emmeans(lmer1, pairwise ~ treatment|plant)$contrasts))
emmeans1 <- summary(emmeans(lmer1,  ~treatment+plant), type= "response")

write.csv(av1, file = "../acidify-aridisols/models/output/anova_diff_gCm2.csv")
write.csv(paircomp1, file = "../acidify-aridisols/models/output/paircomp_diff_gCm2.csv")
write.csv(emmeans1, file = "../acidify-aridisols/models/output/emmeans_diff_gCm2.csv")


# Difference in CO2 effect by treatment 

# SOC
trt.data <- acid_long %>% filter(grepl("DA|fum", method)) %>% 
  filter(grepl("gCm2|isoC", response)) %>% 
  pivot_wider( values_from = value, names_from= response)

lmer2 <- lmer(log(gCm2) ~ (method + treatment + plant)^3 + 
                (1|ring) + (1|ID), data = trt.data)


plot(predict(lmer2), residuals(lmer2)) 
qqnorm(residuals(lmer2)); qqline(residuals(lmer2))
hist(residuals(lmer2))

summary(lmer2)
av2 <- anova(lmer2)

paircomp2 <- summary(emmeans(lmer2, pairwise ~ treatment|method|plant)$contrasts)
emmeans2 <- summary(emmeans(lmer2,  ~ method+treatment+plant), type= "response")

write.csv(av2, file = "../acidify-aridisols/models/output/anova_trt_method.csv")
write.csv(paircomp2, file = "../acidify-aridisols/models/output/paircomp_trt_method.csv")
write.csv(emmeans2, file = "../acidify-aridisols/models/output/emmeans_trt_method.csv")


# 13C
lmer3 <- lmer(isoC ~ (method + treatment + plant)^3 + 
                (1|ring) + (1|ID), data = trt.data)

plot(predict(lmer3), residuals(lmer3)) 
qqnorm(residuals(lmer3)); qqline(residuals(lmer3))
hist(residuals(lmer3))

summary(lmer3)
av3 <- anova(lmer3)

paircomp3 <- summary(emmeans(lmer3, pairwise ~ treatment|method|plant)$contrasts, type = "response")
emmeans3 <- summary(emmeans(lmer3,  ~ method+treatment+plant), type= "response")


write.csv(av3, file = "../acidify-aridisols/models/output/anova_trt_method_isoC.csv")
write.csv(paircomp3, file = "../acidify-aridisols/models/output/paircomp_trt_method_isoC.csv")
write.csv(emmeans3, file = "../acidify-aridisols/models/output/emmeans_trt_method_isoC.csv")

