# import packages ----
library(dplyr)
library(tidyr)
library(ggpubr)
library(lme4)
library(corrplot)
library(datawizard)
library(parameters)
library(effectsize)
library(performance)
library(report)
library(see)


# set proper contrast method ----
options(contrasts = c("contr.helmert", "contr.poly")) # SPSS contrast method
# options(contrasts = c("contr.treatment", "contr.poly"))
# options(contrasts = c("contr.sum", "contr.poly"))
options("contrasts")

# import data ----

data1 <- read.csv("data/cleanData.csv")

# set id as factor
data1$id <- as.factor(data1$id)

# set gender as factor
data1$gender <- as.factor(data1$gender)

data1$location <- as.factor(data1$location)

# standardize variables ----

## 3D PT ----

data1$PTOtherS <- scale(data1$PTOtotal, scale = T, center = F)
data1$PTSelfS <- scale(data1$PTStotal, scale = T, center = F)

## SO ----

data1$angDivMeanS <- scale(data1$angDivMean, scale = T, center = F)

## False Belief ----

data1$FBOtherS <- scale(data1$FB_Total, scale = T, center = F)
data1$FBSelfS <- scale(data1$FB_Control_Total, scale = T, center = F)

## Big-5 Personality ----

data1$BF_OS <- standardize(data1$bigFiveF_O)
data1$BF_CS <- standardize(data1$bigFiveF_C)
data1$BF_ES <- standardize(data1$bigFiveF_E)
data1$BF_AS <- standardize(data1$bigFiveF_A)
data1$BF_NS <- standardize(data1$bigFiveF_N)

## ASQ ----

data1$ASQS <- standardize(data1$ASQ_Total)

## SAQ ----

data1$SAQS <- standardize(data1$SAQ_Total)


# Remote Vs In-Person Scores ----

## 3D Perspective Task ----

### Other * ----
wilcox.test(data1$PTOtotal ~ data1$location)

# W = 15076, p-value = 0.001496 *

### Self ----
wilcox.test(data1$PTStotal ~ data1$location)

# W = 13427, p-value = 0.284

# plot

boxplot(PTOtotal ~ location, data = data1, frame = FALSE,
        ylab = "3D Perspective Task (Other)",
        xlab = "Location")

boxplot(PTStotal ~ location, data = data1, frame = FALSE,
        ylab = "3D Perspective Task (Self)",
        xlab = "Location")

### Figure B.16 ----
data1 %>%
    mutate(location = factor(location, labels = c("In-Person", "Remote"))) %>%
    ggplot(aes(x = location, y = PTOtotal / 30)) +
    geom_violin() +
    geom_jitter(width = 0.1, height = 0, alpha = 0.3, size = 3) +
    geom_hline(yintercept = 0.5, linetype = 'dashed', linewidth = 1) +
    theme_pubr() +
    ylab(label = "Score Proportion") +
    theme(text=element_text(size = 20), legend.position="none")

## SO Task * ----

wilcox.test(data1$angDivMean ~ data1$location)

# W = 10450, p-value = 0.009218 *

# plot
boxplot(angDivMean ~ location, data = data1, frame = FALSE,
        ylab = "Abs. Mean Angular Deviation (Degrees)",
        xlab = "Location")

### Figure B.17 ----
data1 %>%
    mutate(location = factor(location, labels = c("In-Person", "Remote"))) %>%
    ggplot(aes(x = location, y = angDivMean)) +
    geom_boxplot(outlier.colour = NA) +
    geom_jitter(width = 0.05, height = 0, alpha = 0.3, size = 3) +
    ylim(c(0,NA)) +
    scale_y_continuous(n.breaks = 10) +
    theme_pubr() +
    ylab(label = "Score Proportion") +
    theme(text=element_text(size = 20), legend.position="none")

## False Belief ----

### Other ----

wilcox.test(data1$FB_Total ~ data1$location)

# W = 13614, p-value = 0.166

### Self ----

wilcox.test(data1$FB_Control_Total ~ data1$location)

# W = 12709, p-value = 0.8686

# plot

boxplot(FB_Total ~ location, data = data1, frame = FALSE,
        ylab = "False Belie (Other)",
        xlab = "Location")

boxplot(FB_Control_Total ~ location, data = data1, frame = FALSE,
        ylab = "False Belie (Self)",
        xlab = "Location")


## Big 5 Personality * ----

data1 %>%
    select(bigFiveF_O, bigFiveF_C, bigFiveF_E, bigFiveF_A, bigFiveF_N) %>%
    apply(., 2, shapiro.test)

bfManova <- manova(cbind(bigFiveF_O, bigFiveF_C, bigFiveF_E, bigFiveF_A, bigFiveF_N) ~ location, data = data1)

summary(bfManova)

summary.aov(bfManova)

### Figure B.15 ----

indDat <- data1 %>%
    select('Openness' = bigFiveF_O, 'Conscientiousness' = bigFiveF_C, 
           'Extraversion' = bigFiveF_E, 'Agreeableness' = bigFiveF_A,
           'Neuroticism' = bigFiveF_N, 'Anxiety Symptoms' = ASQ_Total,
           'Spatial Anxiety' = SAQ_Total)

indDat %>%
    pivot_longer(cols = everything(), names_to = "Variable", values_to = "Score") %>%
    mutate(Variable = factor(Variable, levels = colnames(indDat)),
           Score = standardize(Score)) %>%
    ggplot(aes(sample = Score, color = Variable)) +
    stat_qq() +
    stat_qq_line(color = "black") +
    facet_wrap(~Variable, scales = "free") +
    theme_pubr() +
    scale_color_brewer(palette="Dark2") +
    ylab(label = "Standardized Sample Quantiles") +
    xlab(label = "Theoretical Quantiles") +
    theme(text=element_text(size = 20), legend.position="none")

# Agreeableness is the only personality different between location
# alpha Bonferroni adjusted for 5 tests (0.05/5 = 0.01)

# Response bigFiveF_A :
#     Df Sum Sq Mean Sq F value   Pr(>F)   
# location      1    292 292.038  7.3434 0.007098 **
# Residuals   316  12567  39.769 

t.test(data1$bigFiveF_A ~ data1$location, paired = F)

# t = 2.711, df = 310.49, p-value = 0.007081 **

## ASQ -----

t.test(data1$ASQ_Total ~ data1$location, paired = F)

# t = -1.8988, df = 284.74, p-value = 0.05861

## SAQ ----

t.test(data1$SAQ_Total ~ data1$location, paired = F)

# t = -1.8321, df = 303.24, p-value = 0.06792

# Other Vs Self ----

## 3D PT * ----

ptdata <- data1 %>%
    select(location, PTOtotal, PTStotal) %>%
    pivot_longer(cols = c(PTOtotal, PTStotal), names_to = "Perspective", values_to = "Score")

wilcox.test(Score ~ Perspective, data = ptdata)

# W = 92083, p-value < 2.2e-16 *

### plot ----

#### Figure B.13 ----
ptdata %>%
    mutate(Score = ifelse(Perspective == "PTOtotal", Score/30, Score/15)) %>%
    mutate(Perspective = factor(Perspective, labels = c("Other", "Self"))) %>%
    ggplot(aes(x = Perspective, y = Score, fill = Perspective)) +
    geom_violin() +
    geom_jitter(width = 0.1, alpha = 0.3, size = 3) +
    geom_hline(yintercept = 0.5, linetype = 'dashed', linewidth = 1) +
    theme_pubr() +
    ylab(label = "Score Proportion") +
    theme(text=element_text(size = 20), legend.position="none")

## False Belief * ----

fbdata <- data1 %>%
    select(location, FB_Total, FB_Control_Total) %>%
    pivot_longer(cols = c(FB_Total, FB_Control_Total), names_to = "Perspective", values_to = "Score")

wilcox.test(Score ~ Perspective, data = fbdata)

# W = 42299, p-value = 0.0001356 *

### plot ----

#### Figure B.14 ----
fbdata %>%
    mutate(Score = Score / 6) %>%
    mutate(Perspective = factor(Perspective, labels = c("Other", "Self"))) %>%
    ggplot(aes(x = Perspective, y = Score, fill = Perspective)) +
    geom_violin() +
    geom_jitter(width = 0.1, height = 0.02, alpha = 0.3, size = 3) +
    geom_hline(yintercept = 0.5, linetype = 'dashed', linewidth = 1) +
    ylim(c(0,NA)) +
    theme_pubr() +
    ylab(label = "Score Proportion") +
    theme(text=element_text(size = 20), legend.position="none")

# Correlations ----

## Combined ----

corrdf1 <- data1 %>%
    select("Angular\nDeviation" = angDivMean,
           "3D (Other)" = PTOtotal, "3D (Self)" = PTStotal,
           "Social (Other)" = FB_Total, "Social (Self)" = FB_Control_Total)

corMat1 <- cor(corrdf1, method = "kendall")
corRes1 <- cor.mtest(corrdf1)

### Figure B.18 ----

corrplot(corMat1,
         method = "color",
         type = "upper",
         order = "original",
         diag = F,
         p.mat = corRes1$p,
         sig.level = 0.05/10,
         insig = 'label_sig',
         tl.srt= 0,
         tl.offset= 0.6,
         tl.cex = 1.45,
         tl.col = "black")

### SO Vs 3D Other * ----

cor.test(corrdf1$`Angular\nDeviation`, corrdf1$`3D (Other)`, method = "kendall")
# -0.2410926, p-value = 4.816e-09 *

### SO Vs Social Other * ----

cor.test(corrdf1$`Angular\nDeviation`, corrdf1$`Social (Other)`, method = "kendall")
# -0.2063822, p-value = 1.969e-06 *

### 3D Other Vs Social Other * ----

cor.test(corrdf1$`3D (Other)`, corrdf1$`Social (Other)`, method = "kendall")
# 0.1972093, p-value = 3.37e-05 *

### 3D Other Vs Social Self ----

cor.test(corrdf1$`3D (Other)`, corrdf1$`Social (Self)`, method = "kendall")
# 0.1419464, p-value = 0.00227

### 3D Self Vs Social Other ----

cor.test(corrdf1$`3D (Self)`, corrdf1$`Social (Other)`, method = "kendall")
# 0.1351578, p-value = 0.004419

## In-Person ----

corrdf2 <- data1 %>%
    filter(location == "In-Person") %>%
    select("Angular\nDeviation" = angDivMean,
           "3D (Other)" = PTOtotal, "3D (Self)" = PTStotal,
           "Social (Other)" = FB_Total, "Social (Self)" = FB_Control_Total)

corMat2 <- cor(corrdf2, method = "kendall")
corRes2 <- cor.mtest(corrdf2)

### Figure B.19 ----

corrplot(corMat2,
         method = "color",
         type = "upper",
         order = "original",
         diag = F,
         p.mat = corRes2$p,
         sig.level = 0.05/10,
         insig = 'label_sig',
         tl.srt= 0,
         tl.offset= 0.6,
         tl.cex = 1.45,
         tl.col = "black")

## Remote ----

# Does NOT replicate...

corrdf3 <- data1 %>%
    filter(location == "Online") %>%
    select("Angular\nDeviation" = angDivMean,
           "3D (Other)" = PTOtotal, "3D (Self)" = PTStotal,
           "Social (Other)" = FB_Total, "Social (Self)" = FB_Control_Total)

corMat3 <- cor(corrdf3, method = "kendall")
corRes3 <- cor.mtest(corrdf3)

### Figure B.20 ----

corrplot(corMat3,
         method = "color",
         type = "upper",
         order = "original",
         diag = F,
         p.mat = corRes3$p,
         sig.level = 0.05/10,
         insig = 'label_sig',
         tl.srt= 0,
         tl.offset= 0.6,
         tl.cex = 1.45,
         tl.col = "black")

### Figure B.24 ----

corrdf4 <- data1 %>%
    select(location,
           "Angular\nDeviation" = angDivMean,
           "3D (Other)" = PTOtotal, "3D (Self)" = PTStotal,
           "Social (Other)" = FB_Total, "Social (Self)" = FB_Control_Total) %>%
    mutate(location = factor(location, labels = c("In-Person", "Remote")))

corrdf4 %>%
    ggplot(aes(y = `Social (Other)`, x = `3D (Other)`)) +
    geom_jitter(width = 0, height = .1, size = 3, alpha = 0.2) +
    geom_smooth(method = "lm", color = "blue") +
    stat_cor(method = "pearson", label.x = 12, label.y = 1.0, size = 6) +
    stat_cor(method = "kendall", label.x = 12, label.y = 0.5, size = 6,
             cor.coef.name = "tau") +
    expand_limits(y = c(0, 6)) +
    scale_y_continuous(n.breaks = 6) +
    scale_x_continuous(n.breaks = 16) +
    xlab("3D (Other) Perspective Score") +
    ylab("Social (Other) Perspective Score") +
    theme_pubr() +
    theme(plot.title = element_text(hjust = 0.5), text = element_text(size = 15)) +
    facet_grid(~location)

# Regression ----

## 3D PT ----

# General trend replicates but not exactly...

ptLong <- data1 %>%
    select(id, gender, location, starts_with("PT") & ends_with("C"), BF_OS , BF_CS , BF_ES , BF_AS , BF_NS , ASQS , SAQS) %>%
    pivot_longer(cols = starts_with("PT"), names_to = "trial", values_to = "score") %>%
    mutate(persp = ifelse(startsWith(trial, "PTS"), "Self", "Other") %>% as.factor(.),
           trial = trial %>% as.factor)

glimpse(ptLong)

ptMod <- ptLong %>%
    lmer(formula = score ~ gender + location + persp * (BF_OS + BF_CS + BF_ES + BF_AS + BF_NS + ASQS + SAQS) +
             (persp | id) + (1 | trial))

summary(ptMod)

ptfitTest <- allFit(ptMod)

### get R2 ----
r2_nakagawa(ptMod)

### get model estimates, CI, t, p-values ----
model_parameters(ptMod, effects = "fixed")

### get effect size ----
eta_squared(ptMod)

### generate model report ----
ptreport <- report(ptMod)

### Figure B.21 ----

ptmp <- model_parameters(ptMod, effects = "fixed")

ptmpPlot <- ptmp %>% plot() +
    scale_y_discrete(limits = rev(c("gender1","location1","persp1", "BF_OS","persp1:BF_OS",
                                    "BF_CS","persp1:BF_CS","BF_ES","persp1:BF_ES",
                                    "BF_AS","persp1:BF_AS","BF_NS","persp1:BF_NS",
                                    "ASQS","persp1:ASQS","SAQS","persp1:SAQS"))) +
    theme(axis.text=element_text(size = 15))

ggsave(filename = "plots/ptModelPlot.png", ptmpPlot, dpi = 142, units = "px", width = 800, height = 1000)

## SO Task ----

# General trend replicates but not exactly...

sodata <- data1 %>%
    select(id, gender, location, starts_with("SO") & ends_with("_AD"), BF_OS , BF_CS , BF_ES , BF_AS , BF_NS , ASQS , SAQS)

sodata <- sodata %>%
    pivot_longer(cols = ends_with("_AD"),
                 names_to = "trial",
                 values_to = "angle",
                 values_transform = list(angle = as.vector))

sogmod <- sodata %>%
    lmer(formula = angle ~ gender * location +
             (BF_OS + BF_CS + BF_ES + BF_AS + BF_NS + ASQS + SAQS) +
             (1 | id) + (1 | trial))

summary(sogmod)

sofitTest <- allFit(sogmod)

#### get R2 ----
r2_nakagawa(sogmod)

#### get model estimates, CI, t, p-values ----
model_parameters(sogmod, effects = "fixed")

### get effect size ----
eta_squared(sogmod)

### generate model report ----
soreport <- report(sogmod)

### Figure B.22 ----

somp <- model_parameters(sogmod, effects = "fixed")

sompPlot <- somp %>% plot() +
    scale_y_discrete(limits = rev(c("gender1","location1","BF_OS",
                                    "BF_CS","BF_ES","BF_AS","BF_NS","ASQS","SAQS"))) +
    scale_x_continuous(n.breaks = 10) +
    theme(axis.text=element_text(size = 15))

# ggsave(filename = "plots/soModelPlot.png", sompPlot, dpi = 142, units = "px", width = 800, height = 1000)

## False Belief ----

fbdata <- data1 %>%
    select(id, gender, location, starts_with("FB") & ends_with("C"), BF_OS , BF_CS , BF_ES , BF_AS , BF_NS , ASQS , SAQS) %>%
    pivot_longer(cols = starts_with("FB"), names_to = "trial", values_to = "score") %>%
    mutate(trial = as.factor(trial))

# assign perspective condition

fbdata$persp[fbdata$trial == "FB_01_RC"] <- "Other"
fbdata$persp[fbdata$trial == "FB_02_RC"] <- "Other"
fbdata$persp[fbdata$trial == "FB_03_RC"] <- "Other"
fbdata$persp[fbdata$trial == "FB_04_RC"] <- "Other"
fbdata$persp[fbdata$trial == "FB_05_RC"] <- "Other"
fbdata$persp[fbdata$trial == "FB_06_RC"] <- "Other"

fbdata$persp[fbdata$trial == "FB_07_RC"] <- "Self"
fbdata$persp[fbdata$trial == "FB_08_RC"] <- "Self"
fbdata$persp[fbdata$trial == "FB_09_RC"] <- "Self"
fbdata$persp[fbdata$trial == "FB_10_RC"] <- "Self"
fbdata$persp[fbdata$trial == "FB_11_RC"] <- "Self"
fbdata$persp[fbdata$trial == "FB_12_RC"] <- "Self"


fbmod <- fbdata %>%
    lmer(formula = score ~ persp * gender * location + persp *
             (BF_OS + BF_CS + BF_ES + BF_AS + BF_NS + ASQS + SAQS) +
             (1 | id) + (1 | trial))

summary(fbmod)

fbfitTest <- allFit(fbmod)

### get R2 ----
r2_nakagawa(fbmod)

### get model estimates, CI, t, p-values ----
model_parameters(fbmod, effects = "fixed")

### get effect size ----
eta_squared(fbmod)

### generate model report ----
fbreport <- report(fbmod)

### Figure B.23 ----

fbmp <- model_parameters(ptMod, effects = "fixed")

# General trend replicates but not exactly...

fbmpPlot <- fbmp %>% plot() +
    scale_y_discrete(limits = rev(c("gender1","location1","persp1", "BF_OS","persp1:BF_OS",
                                    "BF_CS","persp1:BF_CS","BF_ES","persp1:BF_ES",
                                    "BF_AS","persp1:BF_AS","BF_NS","persp1:BF_NS",
                                    "ASQS","persp1:ASQS","SAQS","persp1:SAQS"))) +
    scale_x_continuous(breaks = seq(-0.06, 0.06,0.02)) +
    theme(axis.text=element_text(size = 15))

# ggsave(filename = "plots/fbModelPlot.png", fbmpPlot, dpi = 142, units = "px", width = 800, height = 1000)
