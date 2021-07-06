# _______
# TIP 1 - 
# if you don't know this already, you can click the horizontal staggered lines 
# in the top right corner of the script window to open the file 'outline', which
# you can then use to navigate more easily through 'sections' of a script

# TIP 2 -
# if you want to create your own 'sections' use the shortcut Cmd+Shif+R
# _______

# Setup -------------------------------------------------------------------
library(dplyr)
library(ggplot2)
library(lme4)

# Load the data - don't forget to update the path to wherever you saved this file!
status_df <- 
    read.csv("HMEI-GDrive/data/iucn_dat.csv") %>% 
    mutate(
        # Reclassify category change to 0/1
        cat_change_bi = as.logical(factor(cat_change_bi,
                                          levels = c("improve", "decline"),
                                          labels = c(TRUE, FALSE))),
        # Relevel Red List category
        redlistCategory = 
            factor(redlistCategory,
                   levels = c("Least Concern", "Near Threatened",
                              "Vulnerable", "Endangered", 
                              "Critically Endangered")),
        # Relevel old Red List category
        cat_old_cat = factor(cat_old_cat,
                             levels = c("LC", "NT", "VU",
                                        "EN", "CR"))) %>% 
    # Filter to amphibians, birds & mammals changing Red List status for a 'Genuine' reason
    filter(className %in% c("amphibia", "aves", "mammalia"),
           change_reason == "G",
           !(is.na(cat_change_bi)),
           !(is.na(redlistCategory)),
           !(is.na(cat_old_cat)))

# Look at the data --------------------------------------------------------

# The response variable, 'status_change', describes whether a species improved (1)
# or declined (0) in threat status

# The explanatory variables are taxonomic class and old Red List category
head(status_df)
ggplot(status_df, aes(x = cat_change_bi)) +
    geom_bar() +
    facet_grid(cat_old_cat ~ className) +
    xlab("Improve in Red List status")

# Generally more declining than improving
# - obviously Least Concern cannot improve further, and CR cannot decline
#   further because then would be excluded (i.e. Extinct)
# More birds and mammals changing status overall

# Model the data ----------------------------------------------------------

# Make a standard lm
lm1 <- lm(cat_change_bi ~ cat_old_cat * className, 
          data = status_df)

# Model validation plots
par(mfrow = c(2,2))
plot(lm1)

# Looks terrible, as expected! 
# You expect the residuals vs. fitted (top left) to show no pattern, and the
# Q-Q plot (top-right) to follow the dashed line


# GLM ---------------------------------------------------------------------

# Try the same thing with a binomial GLM
glm1 <- glm(cat_change_bi ~ cat_old_cat * className, 
            data = status_df,
            family = "binomial")

# Look at the model summary
summary(glm1)

# Let's test whether that interaction term is significant, by dropping it from
# the model & then comparing the reduced model to the full model
glm2 <- update(glm1, ~.- cat_old_cat:className)
anova(glm2, glm1, test = "LRT")
# There is no significant loss of explained deviance when we remove the 
# interaction, therefore it's not an important variable and we can leave it out

AIC(glm1, glm2)
# We also see that the model without the interaction has a much lower AIC score

# Let's now test the significance of the two individual variables, in the same way 
glm3 <- update(glm2, ~.- cat_old_cat)
anova(glm3, glm2, test = "LRT")
# Dropping old category *does* result in a significantly worse model

glm4 <- update(glm2, ~.- className)
anova(glm3, glm4, test = "LRT")
# Dropping taxonomic class *does* result in a significantly worse model

#### CONCLUSION
# glm2 is the minimal model, & demonstrates that both old Red List category
# and taxonomic class are predictors of the probability that a species will
# improve in Red List status


# Get model-predicted values --------------------------------------------------

# First we need to create a new dataframe with all possible values of old 
# Red List category and taxonomic class
newdat <-  
    expand.grid(cat_old_cat = unique(status_df$cat_old_cat),
                className = unique(status_df$className))

# Now we predict y values, using our minimal model
predvals <- predict(glm2, newdata = newdat, 
                    # Return y values on the scale of the link function (logistic)
                    type = "link", 
                    # Return standard errors of model-predicted y values
                    se.fit = TRUE)

# Function to calculate critical significance value, used to convert
# standard error to 95% confidence intervals
ci.critical <- 
    function(siglevel) qnorm((100 - siglevel) / 100 / 2, lower.tail = FALSE)
ci_const <- ci.critical(95)

# Assign to dataframe
newdat <-
    newdat %>% 
    mutate(status_improve_prob = predvals$fit, 
           se = predvals$se.fit,
           CI_lo = status_improve_prob - (ci_const * se),
           CI_hi = status_improve_prob + (ci_const * se))
head(newdat)

# You'll see that these values appear to be crazy at first - this is because
# we specified a binomial error family, meaning that the values by default are 
# on a logit scale and need to be back-transformed before plotting:

# Function to calculate inverse logit
inv.logit <- function(x){
    exp(x)/(1 + exp(x))
}

# Now we can back transform the y values
newdat <-
    newdat %>% 
    mutate(status_improve_prob = inv.logit(status_improve_prob),
           se = inv.logit(se),
           CI_lo = inv.logit(CI_lo),
           CI_hi = inv.logit(CI_hi))
head(newdat)
# Values now range between zero and one

# Plot model-predicted values ---------------------------------------------

ggplot(newdat, aes(x = className, y = status_improve_prob, 
                   colour = cat_old_cat)) +
    geom_point(position = position_dodge(width = 0.5)) +
    # 95% confidence intervals
    geom_errorbar(aes(ymin = CI_lo,
                      ymax = CI_hi),
                  position = position_dodge(width = 0.5),
                  width = 0) +
    ylab("Probability of status improving")

# Generally similar probabilities among taxa, but amphibians a little lower

# Increasing probability of improvement if old category was higher risk of extinction

# No errorbars for LC/CR because these categories can only go in one direction






