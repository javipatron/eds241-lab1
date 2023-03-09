
library(ggplot2)
library(stargazer)
library(estimatr)



# IMPORT CSV DATA
CGL <- read.csv("cgl_2008_data_extract.csv")

summary(CGL)


# SUMMARY STATISTICS
stargazer(CGL, type="text", digits=2)

# This table tells you the min and the maximum so you can use it in your X, which is your covariance.

# EXAMINE BALANCE IN COVARIATES
# COVARIATE MEAN DIFFERENCES by DAPever
m1 <- lm(formula = LME ~ DAPever, data = CGL)
m2 <- lm(formula = genus ~ DAPever, data = CGL)
m3 <- lm(formula = species ~ DAPever, data = CGL)
se_models = starprep(m1, m2, m3, 
                     stat = c("std.error"), 
                     se_type = "HC2", 
                     alpha = 0.05)

stargazer(m1, m2, m3, 
          se = se_models, 
          type="text")


skimr::skim(CGL)
dplyr::glimpse(CGL)

# BOXPLOTS TO EXAMINE BALANCE IN COVARIATES
ggplot(CGL, aes(x=as.factor(DAPever), y=LME)) + 
  geom_boxplot(fill="cyan") + xlab("ITQ Status")

#LME = fisheries with ITQ. LME are the Large Marine Ecosystems.
#the covariance that we like to see is that the means overlap, so you can subtract the difference between the means and undertand the impact of ITQ.

ggplot(CGL, aes(x=as.factor(DAPever), y=genus)) + 
  geom_boxplot(fill="cyan") + xlab("ITQ Status")

ggplot(CGL, aes(x=as.factor(DAPever), y=species)) + 
  geom_boxplot(fill="cyan") + xlab("ITQ Status")


# BASIC OLS by DAPever -- THEN ADD INDICATORS FOR OTHER COVARIATES 
# NOTE DO NOT INCLUDE SPECIES IN MODELS TO KEEP RUNNING TIME FAST
mA <- lm(formula = collapse ~ DAPever, data=CGL) #Simple difference in the mean. Is the fishing an ITQ

mB <- lm(formula = collapse ~ DAPever + as.factor(LME), data=CGL)

mC <- lm(formula = collapse ~ DAPever + as.factor(LME) + as.factor(genus), data=CGL) #

se_models = starprep(mA, mB, mC, stat = c("std.error"), se_type = "HC1", alpha = 0.05)

# HC1 = Hectostatichity corrections
stargazer(mA, mB, mC, se = se_models, type="text", omit = "(LME)|(genus)|(species)")

#Interpretation
# -0.142 --> means that the probability for that area to collapse is -14%. A ITQ reduces the probabilities of collapse. In case you use ITQ as the treatment effect. But when you take the Genus into consideration which is the (3), that means that there are other factors affecting that decrease. 





#Questions: Because the assignment of the LME is randomly selected in goes in order with the world. When you see the graph and they are overlaping in the middle. How does that mean., 

