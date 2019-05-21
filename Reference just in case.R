


#*7* = need on 3/26 OR errors OR extraneous code
## Use *** to search for errors in the code or areas that need more work
## Use ***AIC to search for beginning of AIC code
## Use ***Simple Figures with Models to search for beginning of this figure code
## Use ***Simple Figures for Fish Richness to search for beginning of this figure code
## Use ***Simple Figures for Sponge Richness to search for beginning of this figure code
## Use ***Simple Figures for Coral Richness to search for beginning of this figure code
## Use ***Simple Figures for Combined Richness to search for beginning of this figure code


# For Poisson distribution:
#fish_year_p = glm(Fish_Richness ~ Year, data = variables, family = poisson)

# Compare log liklihoods between negative binomial (m1; df=2) and Poisson (m2; df=1)
m1 = glm.nb(Fish_Richness ~ 1, data = variables)
m2 = glm(Fish_Richness ~ 1, data = variables, family = poisson)
logLik(m1)
logLik(m2)
# Found this on 2 different websites, but I don't know how to interpret the result
#pchisq(2 * (logLik(m1) - logLik(m2)), df = 1, lower.tail = FALSE)

m1 = glm.nb(Coral_Richness ~ 1, data = variables)
m2 = glm(Coral_Richness ~ 1, data = variables, family = poisson)
logLik(m1)
logLik(m2)
m1 = glm.nb(Sponge_Richness ~ 1, data = variables)
m2 = glm(Sponge_Richness ~ 1, data = variables, family = poisson)
logLik(m1)
logLik(m2)
m1 = glm.nb(Sponge_and_Fish_Richness ~ 1, data = sponge_complete)
m2 = glm(Sponge_and_Fish_Richness ~ 1, data = variables, family = poisson)
logLik(m1)
logLik(m2)
m1 = glm.nb(Combined_Richness ~ 1, data = variables)
m2 = glm(Combined_Richness ~ 1, data = variables, family = poisson)
logLik(m1)
logLik(m2)



# ***When feel comfortable regarding models, review again with year as categorical for more support

# ***Simple Figures with Models

# Figure. Rugosity as a surrogate for fish richness with simple linear, logarithmic, and power models.
ggplot(variables, aes(x = Rugosity, y = Fish_Richness)) + 
  #geom_line(size = 1.1) +
  #geom_point(size = 5, aes(color = variables$Site))+
  geom_point(size = 3)+
  scale_x_continuous(name ="Rugosity") +
  scale_y_continuous(name ="Fish Richness") +
  geom_smooth(size = 1.2, method = "glm.nb", formula = y ~ x, aes(color = "Linear")) +
  geom_smooth(size = 1.2, method = "glm.nb", formula = y ~ log(x), aes(color = "Logarithmic")) +
  geom_smooth(size = 1.2, method = "glm.nb", formula = y ~ exp(1.3053 + 0.4944*log(x)), aes(color = "Power")) +
  scale_colour_manual(name = "Models", values = c("blue", "red", "green")) +
  theme(text=element_text(size=27), 
        #axis.text.x=element_text(angle = +90, hjust = 0),
        panel.grid.major = element_line(colour="light gray", size = (0.5)), 
        panel.grid.minor = element_line(colour="light gray", size = (0.5)),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"))

# Figure. Coral cover as a surrogate for fish richness with simple linear, logarithmic, and power models.
ggplot(variables, aes(x = Percent_Coral_Cover, y = Fish_Richness)) + 
  geom_point(size = 3)+
  scale_x_continuous(name ="Coral Cover (%)") +
  scale_y_continuous(name ="Fish Richness") +
  geom_smooth(size = 1.2, method = "glm.nb", formula = y ~ x, aes(color = "Linear")) +
  geom_smooth(size = 1.2, method = "glm.nb", formula = y ~ log(x), aes(color = "Logarithmic")) +
  geom_smooth(size = 1.2, method = "glm.nb", formula = y ~ exp(2.5059 + 0.2305*log(x)), aes(color = "Power")) +
  scale_colour_manual(name = "Models", values = c("blue", "red", "green")) +
  theme(text=element_text(size=27), 
        panel.grid.major = element_line(colour="light gray", size = (0.5)), 
        panel.grid.minor = element_line(colour="light gray", size = (0.5)),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"))

# Figure. Coral richness as a surrogate for fish richness with simple linear, logarithmic, and power models.
ggplot(variables, aes(x = Coral_Richness, y = Fish_Richness)) + 
  geom_point(size = 3)+
  scale_x_continuous(name ="Coral Richness") +
  scale_y_continuous(name ="Fish Richness") +
  geom_smooth(size = 1.2, method = "glm.nb", formula = y ~ x, aes(color = "Linear")) +
  #geom_smooth(size = 1.2, method = "glm.nb", formula = y ~ log(x), aes(color = "Logarithmic")) +
  #geom_smooth(size = 1.2, method = "glm.nb", formula = y ~ exp(2.1422 + 0.4041*log(x)), aes(color = "Power")) +
  scale_colour_manual(name = "Models", values = c("blue", "red", "green")) +
  theme(text=element_text(size=27), 
        panel.grid.major = element_line(colour="light gray", size = (0.5)), 
        panel.grid.minor = element_line(colour="light gray", size = (0.5)),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"))

# Figure. Rugosity as a surrogate for sponge richness with simple linear, logarithmic, and power models.
ggplot(variables, aes(x = Rugosity, y = Sponge_Richness)) + 
  geom_point(size = 3)+
  scale_x_continuous(name ="Rugosity") +
  scale_y_continuous(name ="Sponge Richness") +
  geom_smooth(size = 1.2, method = "glm.nb", formula = y ~ x, aes(color = "Linear")) +
  #geom_smooth(size = 1.2, method = "glm.nb", formula = y ~ log(x), aes(color = "Logarithmic")) +
  #geom_smooth(size = 1.2, method = "glm.nb", formula = y ~ exp(3.4258 + -0.1012*log(x)), aes(color = "Power")) +
  scale_colour_manual(name = "Models", values = c("blue", "red", "green")) +
  theme(text=element_text(size=27), 
        panel.grid.major = element_line(colour="light gray", size = (0.5)), 
        panel.grid.minor = element_line(colour="light gray", size = (0.5)),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"))

# Figure. Coral cover as a surrogate for sponge richness with simple linear, logarithmic, and power models.
ggplot(variables, aes(x = Percent_Coral_Cover, y = Sponge_Richness)) + 
  geom_point(size = 3)+
  scale_x_continuous(name ="Coral Cover (%)") +
  scale_y_continuous(name ="Sponge Richness") +
  geom_smooth(size = 1.2, method = "glm.nb", formula = y ~ x, aes(color = "Linear")) +
  #geom_smooth(size = 1.2, method = "glm.nb", formula = y ~ log(x), aes(color = "Logarithmic")) +
  #geom_smooth(size = 1.2, method = "glm.nb", formula = y ~ exp(3.4226 + -0.1344*log(x)), aes(color = "Power")) +
  scale_colour_manual(name = "Models", values = c("blue", "red", "green")) +
  theme(text=element_text(size=27), 
        panel.grid.major = element_line(colour="light gray", size = (0.5)), 
        panel.grid.minor = element_line(colour="light gray", size = (0.5)),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"))

# Figure. Coral richness as a surrogate for sponge richness with simple linear, logarithmic, and power models.
ggplot(variables, aes(x = Coral_Richness, y = Sponge_Richness)) + 
  geom_point(size = 3)+
  scale_x_continuous(name ="Coral Richness") +
  scale_y_continuous(name ="Sponge Richness") +
  geom_smooth(size = 1.2, method = "glm.nb", formula = y ~ x, aes(color = "Linear")) +
  #geom_smooth(size = 1.2, method = "glm.nb", formula = y ~ log(x), aes(color = "Logarithmic")) +
  #geom_smooth(size = 1.2, method = "glm.nb", formula = y ~ exp(3.5587 + -0.2029*log(x)), aes(color = "Power")) +
  scale_colour_manual(name = "Models", values = c("blue", "red", "green")) +
  theme(text=element_text(size=27), 
        panel.grid.major = element_line(colour="light gray", size = (0.5)), 
        panel.grid.minor = element_line(colour="light gray", size = (0.5)),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"))

# Figure. Rugosity as a surrogate for coral richness with linear model only.
ggplot(variables, aes(x = Rugosity, y = Coral_Richness)) + 
  geom_point(size = 3)+
  scale_x_continuous(name ="Rugosity") +
  scale_y_continuous(name ="Coral Richness") +
  geom_smooth(size = 1.2, method = "glm.nb", formula = y ~ x, aes(color = "Linear")) +
  geom_smooth(size = 1.2, method = "glm.nb", formula = y ~ log(x), aes(color = "Logarithmic")) +
  geom_smooth(size = 1.2, method = "glm.nb", formula = y ~ exp(0.8607 + 0.9674*log(x)), aes(color = "Power")) +
  scale_colour_manual(name = "Models", values = c("blue", "red", "green")) +
  theme(text=element_text(size=27), 
        panel.grid.major = element_line(colour="light gray", size = (0.5)), 
        panel.grid.minor = element_line(colour="light gray", size = (0.5)),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"))

# Figure. Coral cover as a surrogate for coral richness with simple linear, logarithmic, and power models.
ggplot(variables, aes(x = Percent_Coral_Cover, y = Coral_Richness)) + 
  geom_point(size = 3)+
  scale_x_continuous(name ="Coral Cover (%)") +
  scale_y_continuous(name ="Coral Richness") +
  geom_smooth(size = 1.2, method = "glm.nb", formula = y ~ x, aes(color = "Linear")) +
  geom_smooth(size = 1.2, method = "glm.nb", formula = y ~ log(x), aes(color = "Logarithmic")) +
  geom_smooth(size = 1.2, method = "glm.nb", formula = y ~ exp(1.6342 + 0.3108*log(x)), aes(color = "Power")) +
  scale_colour_manual(name = "Models", values = c("blue", "red", "green")) +
  theme(text=element_text(size=27), 
        panel.grid.major = element_line(colour="light gray", size = (0.5)), 
        panel.grid.minor = element_line(colour="light gray", size = (0.5)),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"))

# Figure. Rugosity as a surrogate for combined richness with simple linear, logarithmic, and power models.
ggplot(variables, aes(x = Rugosity, y = Combined_Richness)) + 
  geom_point(size = 3)+
  scale_x_continuous(name ="Rugosity") +
  scale_y_continuous(name ="Combined Richness") +
  geom_smooth(size = 1.2, method = "glm.nb", formula = y ~ x, aes(color = "Linear")) +
  geom_smooth(size = 1.2, method = "glm.nb", formula = y ~ log(x), aes(color = "Logarithmic")) +
  geom_smooth(size = 1.2, method = "glm.nb", formula = y ~ exp(3.2653 + 0.2149*log(x)), aes(color = "Power")) +
  scale_colour_manual(name = "Models", values = c("blue", "red", "green")) +
  theme(text=element_text(size=27), 
        panel.grid.major = element_line(colour="light gray", size = (0.5)), 
        panel.grid.minor = element_line(colour="light gray", size = (0.5)),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"))

# Figure. Coral cover as a surrogate for combined richness with simple linear, logarithmic, and power models.
ggplot(variables, aes(x = Percent_Coral_Cover, y = Combined_Richness)) + 
  geom_point(size = 3)+
  scale_x_continuous(name ="Coral Cover (%)") +
  scale_y_continuous(name ="Combined Richness") +
  geom_smooth(size = 1.2, method = "glm.nb", formula = y ~ x, aes(color = "Linear")) +
  geom_smooth(size = 1.2, method = "glm.nb", formula = y ~ log(x), aes(color = "Logarithmic")) +
  geom_smooth(size = 1.2, method = "glm.nb", formula = y ~ exp(3.79088 + 0.09888*log(x)), aes(color = "Power")) +
  scale_colour_manual(name = "Models", values = c("blue", "red", "green")) +
  theme(text=element_text(size=27), 
        panel.grid.major = element_line(colour="light gray", size = (0.5)), 
        panel.grid.minor = element_line(colour="light gray", size = (0.5)),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"))

# Figure. Coral richness as a surrogate for combined richness with simple linear, logarithmic, and power models.
ggplot(variables, aes(x = Coral_Richness, y = Combined_Richness)) + 
  geom_point(size = 3)+
  scale_x_continuous(name ="Coral Richness") +
  scale_y_continuous(name ="Combined Richness") +
  geom_smooth(size = 1.2, method = "glm.nb", formula = y ~ x, aes(color = "Linear")) +
  #geom_smooth(size = 1.2, method = "glm.nb", formula = y ~ log(x), aes(color = "Logarithmic")) +
  #geom_smooth(size = 1.2, method = "glm.nb", formula = y ~ exp(3.3032 + 0.3036*log(x)), aes(color = "Power")) +
  scale_colour_manual(name = "Models", values = c("blue", "red", "green")) +
  theme(text=element_text(size=27), 
        panel.grid.major = element_line(colour="light gray", size = (0.5)), 
        panel.grid.minor = element_line(colour="light gray", size = (0.5)),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"))



# Verify year is factor for x-axis labels
variables$True_Year <- as.factor(variables$True_Year)
# Rugosity over time
ggplot(variables, aes(x = True_Year, y = Rugosity, group = Site, color = Site)) + 
  geom_line(size = 1.1) +
  scale_x_discrete(name = "Time (year)") +
  scale_y_continuous(name = "Rugosity") +
  theme(text=element_text(size = 18), 
        axis.text.x=element_text(angle = +90, hjust = 0),
        panel.grid.major = element_line(colour = "light gray", size = (0.5)), 
        panel.grid.minor = element_line(colour = "light gray", size = (0.5)),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"))
# Coral cover over time
ggplot(variables, aes(x = True_Year, y = Percent_Coral_Cover, group = Site, color = Site)) + 
  geom_line(size = 1.1) +
  scale_x_discrete(name = "Time (year)") +
  scale_y_continuous(name = "Coral Cover (%)") +
  theme(text=element_text(size = 18), 
        axis.text.x=element_text(angle = +90, hjust = 0),
        panel.grid.major = element_line(colour = "light gray", size = (0.5)), 
        panel.grid.minor = element_line(colour = "light gray", size = (0.5)),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"))



## ***Simple Figures for Fish Richness

# Verify year is factor for x-axis labels
variables$True_Year <- as.factor(variables$True_Year)

# Over time
ggplot(variables, aes(x = True_Year, y = Fish_Richness, group = Site, color = Site)) + 
  geom_line(size = 1.1) +
  scale_x_discrete(name = "Time (year)") +
  scale_y_continuous(name = "Fish Richness") +
  theme(text=element_text(size = 18), 
        axis.text.x=element_text(angle = +90, hjust = 0),
        panel.grid.major = element_line(colour = "light gray", size = (0.5)), 
        panel.grid.minor = element_line(colour = "light gray", size = (0.5)),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"))

# Across sites when x = Rugosity
ggplot(variables, aes(x = Rugosity, y = Fish_Richness, group = Site, color = Site)) + 
  geom_point(size = 4)+
  scale_x_continuous(name = "Rugosity") +
  scale_y_continuous(name = "Fish Richness") +
  theme(text=element_text(size = 18), 
        panel.grid.major = element_line(colour = "light gray", size = (0.5)), 
        panel.grid.minor = element_line(colour = "light gray", size = (0.5)),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"))

# Across sites when x = Coral Cover
ggplot(variables, aes(x = Percent_Coral_Cover, y = Fish_Richness, group = Site, color = Site)) + 
  geom_point(size = 4)+
  scale_x_continuous(name = "Coral Cover (%)") +
  scale_y_continuous(name = "Fish Richness") +
  theme(text=element_text(size = 18), 
        panel.grid.major = element_line(colour = "light gray", size = (0.5)), 
        panel.grid.minor = element_line(colour = "light gray", size = (0.5)),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"))

# Across sites when x = Coral Richness
ggplot(variables, aes(x = Coral_Richness, y = Fish_Richness, group = Site, color = Site)) + 
  geom_point(size = 4)+
  scale_x_continuous(name = "Coral Richness") +
  scale_y_continuous(name = "Fish Richness") +
  theme(text=element_text(size = 18), 
        panel.grid.major = element_line(colour = "light gray", size = (0.5)), 
        panel.grid.minor = element_line(colour = "light gray", size = (0.5)),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"))

# Change year to numeric to allow for color gradient
variables$True_Year <- as.numeric(as.character(variables$True_Year))

# Across time when x = Rugosity
ggplot(variables, aes(x = Rugosity, y = Fish_Richness)) + 
  geom_point(aes(color = True_Year), size = 4) +
  scale_x_continuous(name = "Rugosity") +
  scale_y_continuous(name = "Fish Richness") +
  scale_color_gradient(low="lightblue", high="darkblue") +
  theme(text=element_text(size = 18), 
        panel.grid.major = element_line(colour = "light gray", size = (0.5)), 
        panel.grid.minor = element_line(colour = "light gray", size = (0.5)),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"))

# Across time when x = Coral Cover
ggplot(variables, aes(x = Percent_Coral_Cover, y = Fish_Richness)) + 
  geom_point(aes(color = True_Year), size = 4) +
  scale_x_continuous(name = "Coral Cover (%)") +
  scale_y_continuous(name = "Fish Richness") +
  scale_color_gradient(low="lightblue", high="darkblue") +
  theme(text=element_text(size = 18), 
        panel.grid.major = element_line(colour = "light gray", size = (0.5)), 
        panel.grid.minor = element_line(colour = "light gray", size = (0.5)),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"))

# Across time when x = Coral Richness
ggplot(variables, aes(x = Coral_Richness, y = Fish_Richness)) + 
  geom_point(aes(color = True_Year), size = 4) +
  scale_x_continuous(name = "Coral Richness") +
  scale_y_continuous(name = "Fish Richness") +
  scale_color_gradient(low="lightblue", high="darkblue") +
  theme(text=element_text(size = 18), 
        panel.grid.major = element_line(colour = "light gray", size = (0.5)), 
        panel.grid.minor = element_line(colour = "light gray", size = (0.5)),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"))

# Across time when x = Rugosity separated by site
ggplot(variables, aes(x = Rugosity, y = Fish_Richness)) + 
  geom_point(aes(color = True_Year), size = 3) +
  scale_x_continuous(name = "Rugosity") +
  scale_y_continuous(name = "Fish Richness") +
  scale_color_gradient(low="lightblue", high="darkblue") +
  theme(text=element_text(size = 18), 
        panel.grid.major = element_line(colour = "light gray", size = (0.5)), 
        panel.grid.minor = element_line(colour = "light gray", size = (0.5)),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black")) +
  facet_wrap(~ Site)

# Across time when x = Coral Cover separated by site
ggplot(variables, aes(x = Percent_Coral_Cover, y = Fish_Richness)) + 
  geom_point(aes(color = True_Year), size = 3) +
  scale_x_continuous(name = "Coral Cover (%)") +
  scale_y_continuous(name = "Fish Richness") +
  scale_color_gradient(low="lightblue", high="darkblue") +
  theme(text=element_text(size = 18), 
        panel.grid.major = element_line(colour = "light gray", size = (0.5)), 
        panel.grid.minor = element_line(colour = "light gray", size = (0.5)),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black")) +
  facet_wrap(~ Site)

# Across time when x = Coral Richness separated by site
ggplot(variables, aes(x = Coral_Richness, y = Fish_Richness)) + 
  geom_point(aes(color = True_Year), size = 3) +
  scale_x_continuous(name = "Coral Richness") +
  scale_y_continuous(name = "Fish Richness") +
  scale_color_gradient(low="lightblue", high="darkblue") +
  theme(text=element_text(size = 18), 
        panel.grid.major = element_line(colour = "light gray", size = (0.5)), 
        panel.grid.minor = element_line(colour = "light gray", size = (0.5)),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black")) +
  facet_wrap(~ Site)



## ***Simple Figures for Sponge Richness

# Verify year is factor for x-axis labels
variables$True_Year <- as.factor(variables$True_Year)

# Over time
ggplot(variables, aes(x = True_Year, y = Sponge_Richness, group = Site, color = Site)) + 
  geom_line(size = 1.1) +
  scale_x_discrete(name = "Time (year)") +
  scale_y_continuous(name = "Sponge Richness") +
  theme(text=element_text(size = 18), 
        axis.text.x=element_text(angle = +90, hjust = 0),
        panel.grid.major = element_line(colour = "light gray", size = (0.5)), 
        panel.grid.minor = element_line(colour = "light gray", size = (0.5)),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"))
# ***Notice that this figure has line breaks where the value for Sponge Richness in that given year is NA
# # The following code will remove the line breaks by deleting the years without values and merging the line segments 
# ggplot(variables[!is.na(variables$Sponge_Richness),], aes(x = True_Year, y = Sponge_Richness, group = Site, color = Site)) + 
#   geom_line(size = 1.1) +
#   scale_x_discrete(name ="Time (year)") +
#   scale_y_continuous(name ="Sponge Richness")

# Across sites when x = Rugosity
ggplot(variables, aes(x = Rugosity, y = Sponge_Richness, group = Site, color = Site)) + 
  geom_point(size = 4)+
  scale_x_continuous(name = "Rugosity") +
  scale_y_continuous(name = "Sponge Richness") +
  theme(text=element_text(size = 18), 
        panel.grid.major = element_line(colour = "light gray", size = (0.5)), 
        panel.grid.minor = element_line(colour = "light gray", size = (0.5)),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"))

# Across sites when x = Coral Cover
ggplot(variables, aes(x = Percent_Coral_Cover, y = Sponge_Richness, group = Site, color = Site)) + 
  geom_point(size = 4)+
  scale_x_continuous(name = "Coral Cover (%)") +
  scale_y_continuous(name = "Sponge Richness") +
  theme(text=element_text(size = 18), 
        panel.grid.major = element_line(colour = "light gray", size = (0.5)), 
        panel.grid.minor = element_line(colour = "light gray", size = (0.5)),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"))

# Across sites when x = Coral Richness
ggplot(variables, aes(x = Coral_Richness, y = Sponge_Richness, group = Site, color = Site)) + 
  geom_point(size = 4)+
  scale_x_continuous(name = "Coral Richness") +
  scale_y_continuous(name = "Sponge Richness") +
  theme(text=element_text(size = 18), 
        panel.grid.major = element_line(colour = "light gray", size = (0.5)), 
        panel.grid.minor = element_line(colour = "light gray", size = (0.5)),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"))

# Change year to numeric to allow for color gradient
variables$True_Year <- as.numeric(as.character(variables$True_Year))

# Across time when x = Rugosity
ggplot(variables, aes(x = Rugosity, y = Sponge_Richness)) + 
  geom_point(aes(color = True_Year), size = 4) +
  scale_x_continuous(name = "Rugosity") +
  scale_y_continuous(name = "Sponge Richness") +
  scale_color_gradient(low="lightblue", high="darkblue") +
  theme(text=element_text(size = 18), 
        panel.grid.major = element_line(colour = "light gray", size = (0.5)), 
        panel.grid.minor = element_line(colour = "light gray", size = (0.5)),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"))

# Across time when x = Coral Cover
ggplot(variables, aes(x = Percent_Coral_Cover, y = Sponge_Richness)) + 
  geom_point(aes(color = True_Year), size = 4) +
  scale_x_continuous(name = "Coral Cover (%)") +
  scale_y_continuous(name = "Sponge Richness") +
  scale_color_gradient(low="lightblue", high="darkblue") +
  theme(text=element_text(size = 18), 
        panel.grid.major = element_line(colour = "light gray", size = (0.5)), 
        panel.grid.minor = element_line(colour = "light gray", size = (0.5)),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"))

# Across time when x = Coral Richness
ggplot(variables, aes(x = Coral_Richness, y = Sponge_Richness)) + 
  geom_point(aes(color = True_Year), size = 4) +
  scale_x_continuous(name = "Coral Richness") +
  scale_y_continuous(name = "Sponge Richness") +
  scale_color_gradient(low="lightblue", high="darkblue") +
  theme(text=element_text(size = 18), 
        panel.grid.major = element_line(colour = "light gray", size = (0.5)), 
        panel.grid.minor = element_line(colour = "light gray", size = (0.5)),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"))

# Across time when x = Rugosity separated by site
ggplot(variables, aes(x = Rugosity, y = Sponge_Richness)) + 
  geom_point(aes(color = True_Year), size = 3) +
  scale_x_continuous(name = "Rugosity") +
  scale_y_continuous(name = "Sponge Richness") +
  scale_color_gradient(low="lightblue", high="darkblue") +
  theme(text=element_text(size = 18), 
        panel.grid.major = element_line(colour = "light gray", size = (0.5)), 
        panel.grid.minor = element_line(colour = "light gray", size = (0.5)),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black")) +
  facet_wrap(~ Site)

# Across time when x = Coral Cover separated by site
ggplot(variables, aes(x = Percent_Coral_Cover, y = Sponge_Richness)) + 
  geom_point(aes(color = True_Year), size = 3) +
  scale_x_continuous(name = "Coral Cover (%)") +
  scale_y_continuous(name = "Sponge Richness") +
  scale_color_gradient(low="lightblue", high="darkblue") +
  theme(text=element_text(size = 18), 
        panel.grid.major = element_line(colour = "light gray", size = (0.5)), 
        panel.grid.minor = element_line(colour = "light gray", size = (0.5)),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black")) +
  facet_wrap(~ Site)

# Across time when x = Coral Richness separated by site
ggplot(variables, aes(x = Coral_Richness, y = Sponge_Richness)) + 
  geom_point(aes(color = True_Year), size = 3) +
  scale_x_continuous(name = "Coral Richness") +
  scale_y_continuous(name = "Sponge Richness") +
  scale_color_gradient(low="lightblue", high="darkblue") +
  theme(text=element_text(size = 18), 
        panel.grid.major = element_line(colour = "light gray", size = (0.5)), 
        panel.grid.minor = element_line(colour = "light gray", size = (0.5)),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black")) +
  facet_wrap(~ Site)



## ***Simple Figures for Coral Richness

# Verify year is factor for x-axis labels
variables$True_Year <- as.factor(variables$True_Year)

# Over time
ggplot(variables, aes(x = True_Year, y = Coral_Richness, group = Site, color = Site)) + 
  geom_line(size = 1.1) +
  scale_x_discrete(name = "Time (year)") +
  scale_y_continuous(name = "Coral Richness") +
  theme(text=element_text(size = 18), 
        axis.text.x=element_text(angle = +90, hjust = 0),
        panel.grid.major = element_line(colour = "light gray", size = (0.5)), 
        panel.grid.minor = element_line(colour = "light gray", size = (0.5)),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"))

# Across sites when x = Rugosity
ggplot(variables, aes(x = Rugosity, y = Coral_Richness, group = Site, color = Site)) + 
  geom_point(size = 4)+
  scale_x_continuous(name = "Rugosity") +
  scale_y_continuous(name = "Coral Richness") +
  theme(text=element_text(size = 18), 
        panel.grid.major = element_line(colour = "light gray", size = (0.5)), 
        panel.grid.minor = element_line(colour = "light gray", size = (0.5)),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"))

# Across sites when x = Coral Cover
ggplot(variables, aes(x = Percent_Coral_Cover, y = Coral_Richness, group = Site, color = Site)) + 
  geom_point(size = 4)+
  scale_x_continuous(name = "Coral Cover (%)") +
  scale_y_continuous(name = "Coral Richness") +
  theme(text=element_text(size = 18), 
        panel.grid.major = element_line(colour = "light gray", size = (0.5)), 
        panel.grid.minor = element_line(colour = "light gray", size = (0.5)),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"))

# Change year to numeric to allow for color gradient
variables$True_Year <- as.numeric(as.character(variables$True_Year))

# Across time when x = Rugosity
ggplot(variables, aes(x = Rugosity, y = Coral_Richness)) + 
  geom_point(aes(color = True_Year), size = 4) +
  scale_x_continuous(name = "Rugosity") +
  scale_y_continuous(name = "Coral Richness") +
  scale_color_gradient(low="lightblue", high="darkblue") +
  theme(text=element_text(size = 18), 
        panel.grid.major = element_line(colour = "light gray", size = (0.5)), 
        panel.grid.minor = element_line(colour = "light gray", size = (0.5)),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"))

# Across time when x = Coral Cover
ggplot(variables, aes(x = Percent_Coral_Cover, y = Coral_Richness)) + 
  geom_point(aes(color = True_Year), size = 4) +
  scale_x_continuous(name = "Coral Cover (%)") +
  scale_y_continuous(name = "Coral Richness") +
  scale_color_gradient(low="lightblue", high="darkblue") +
  theme(text=element_text(size = 18), 
        panel.grid.major = element_line(colour = "light gray", size = (0.5)), 
        panel.grid.minor = element_line(colour = "light gray", size = (0.5)),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"))

# Across time when x = Rugosity separated by site
ggplot(variables, aes(x = Rugosity, y = Coral_Richness)) + 
  geom_point(aes(color = True_Year), size = 3) +
  scale_x_continuous(name = "Rugosity") +
  scale_y_continuous(name = "Coral Richness") +
  scale_color_gradient(low="lightblue", high="darkblue") +
  theme(text=element_text(size = 18), 
        panel.grid.major = element_line(colour = "light gray", size = (0.5)), 
        panel.grid.minor = element_line(colour = "light gray", size = (0.5)),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black")) +
  facet_wrap(~ Site)

# Across time when x = Coral Cover separated by site
ggplot(variables, aes(x = Percent_Coral_Cover, y = Coral_Richness)) + 
  geom_point(aes(color = True_Year), size = 3) +
  scale_x_continuous(name = "Coral Cover (%)") +
  scale_y_continuous(name = "Coral Richness") +
  scale_color_gradient(low="lightblue", high="darkblue") +
  theme(text=element_text(size = 18), 
        panel.grid.major = element_line(colour = "light gray", size = (0.5)), 
        panel.grid.minor = element_line(colour = "light gray", size = (0.5)),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black")) +
  facet_wrap(~ Site)



## ***Simple Figures for Combined Richness

# Verify year is factor for x-axis labels
variables$True_Year <- as.factor(variables$True_Year)

# Over time
ggplot(variables, aes(x = True_Year, y = Combined_Richness, group = Site, color = Site)) + 
  geom_line(size = 1.1) +
  scale_x_discrete(name = "Time (year)") +
  scale_y_continuous(name = "Combined Richness") +
  theme(text=element_text(size = 18), 
        axis.text.x=element_text(angle = +90, hjust = 0),
        panel.grid.major = element_line(colour = "light gray", size = (0.5)), 
        panel.grid.minor = element_line(colour = "light gray", size = (0.5)),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"))

# Across sites when x = Rugosity
ggplot(variables, aes(x = Rugosity, y = Combined_Richness, group = Site, color = Site)) + 
  geom_point(size = 4)+
  scale_x_continuous(name = "Rugosity") +
  scale_y_continuous(name = "Combined Richness") +
  theme(text=element_text(size = 18), 
        panel.grid.major = element_line(colour = "light gray", size = (0.5)), 
        panel.grid.minor = element_line(colour = "light gray", size = (0.5)),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"))

# Across sites when x = Coral Cover
ggplot(variables, aes(x = Percent_Coral_Cover, y = Combined_Richness, group = Site, color = Site)) + 
  geom_point(size = 4)+
  scale_x_continuous(name = "Coral Cover (%)") +
  scale_y_continuous(name = "Combined Richness") +
  theme(text=element_text(size = 18), 
        panel.grid.major = element_line(colour = "light gray", size = (0.5)), 
        panel.grid.minor = element_line(colour = "light gray", size = (0.5)),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"))

# Across sites when x = Coral Richness
ggplot(variables, aes(x = Coral_Richness, y = Combined_Richness, group = Site, color = Site)) + 
  geom_point(size = 4)+
  scale_x_continuous(name = "Coral Richness") +
  scale_y_continuous(name = "Combined Richness") +
  theme(text=element_text(size = 18), 
        panel.grid.major = element_line(colour = "light gray", size = (0.5)), 
        panel.grid.minor = element_line(colour = "light gray", size = (0.5)),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"))

# Change year to numeric to allow for color gradient
variables$True_Year <- as.numeric(as.character(variables$True_Year))

# Across time when x = Rugosity
ggplot(variables, aes(x = Rugosity, y = Combined_Richness)) + 
  geom_point(aes(color = True_Year), size = 4) +
  scale_x_continuous(name = "Rugosity") +
  scale_y_continuous(name = "Combined Richness") +
  scale_color_gradient(low="lightblue", high="darkblue") +
  theme(text=element_text(size = 18), 
        panel.grid.major = element_line(colour = "light gray", size = (0.5)), 
        panel.grid.minor = element_line(colour = "light gray", size = (0.5)),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"))

# Across time when x = Coral Cover
ggplot(variables, aes(x = Percent_Coral_Cover, y = Combined_Richness)) + 
  geom_point(aes(color = True_Year), size = 4) +
  scale_x_continuous(name = "Coral Cover (%)") +
  scale_y_continuous(name = "Combined Richness") +
  scale_color_gradient(low="lightblue", high="darkblue") +
  theme(text=element_text(size = 18), 
        panel.grid.major = element_line(colour = "light gray", size = (0.5)), 
        panel.grid.minor = element_line(colour = "light gray", size = (0.5)),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"))

# Across time when x = Coral Richness
ggplot(variables, aes(x = Coral_Richness, y = Combined_Richness)) + 
  geom_point(aes(color = True_Year), size = 4) +
  scale_x_continuous(name = "Coral Richness") +
  scale_y_continuous(name = "Combined Richness") +
  scale_color_gradient(low="lightblue", high="darkblue") +
  theme(text=element_text(size = 18), 
        panel.grid.major = element_line(colour = "light gray", size = (0.5)), 
        panel.grid.minor = element_line(colour = "light gray", size = (0.5)),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"))

# Across time when x = Rugosity separated by site
ggplot(variables, aes(x = Rugosity, y = Combined_Richness)) + 
  geom_point(aes(color = True_Year), size = 3) +
  scale_x_continuous(name = "Rugosity") +
  scale_y_continuous(name = "Combined Richness") +
  scale_color_gradient(low="lightblue", high="darkblue") +
  theme(text=element_text(size = 18), 
        panel.grid.major = element_line(colour = "light gray", size = (0.5)), 
        panel.grid.minor = element_line(colour = "light gray", size = (0.5)),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black")) +
  facet_wrap(~ Site)

# Across time when x = Coral Cover separated by site
ggplot(variables, aes(x = Percent_Coral_Cover, y = Combined_Richness)) + 
  geom_point(aes(color = True_Year), size = 3) +
  scale_x_continuous(name = "Coral Cover (%)") +
  scale_y_continuous(name = "Combined Richness") +
  scale_color_gradient(low="lightblue", high="darkblue") +
  theme(text=element_text(size = 18), 
        panel.grid.major = element_line(colour = "light gray", size = (0.5)), 
        panel.grid.minor = element_line(colour = "light gray", size = (0.5)),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black")) +
  facet_wrap(~ Site)

# Across time when x = Coral Richness separated by site
ggplot(variables, aes(x = Coral_Richness, y = Combined_Richness)) + 
  geom_point(aes(color = True_Year), size = 3) +
  scale_x_continuous(name = "Coral Richness") +
  scale_y_continuous(name = "Combined Richness") +
  scale_color_gradient(low="lightblue", high="darkblue") +
  theme(text=element_text(size = 18), 
        panel.grid.major = element_line(colour = "light gray", size = (0.5)), 
        panel.grid.minor = element_line(colour = "light gray", size = (0.5)),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black")) +
  facet_wrap(~ Site)

################################################################

# Create HTML files for most parsimonious models and models with one variable
tab_model(fish_site, fish_rugosity, fish_cover, fish_coralrichness, fish_year)
tab_model(sponge_rugosity_year_site_log, sponge_site, sponge_cover, sponge_coralrichness, sponge_year, sponge_rugosity)
tab_model(coral_cover_year_log, coral_cover, coral_site, coral_rugosity, coral_year)
tab_model(combined_cover_year_site_log, combined_site, combined_rugosity, combined_cover, combined_year)
tab_model(fishsponge_year_site, fishsponge_site, fishsponge_coralrichness, fishsponge_year)

##############################################################################################################################################################








## Figures for NEAFWA

# Coral richness across time when x = Coral Cover
ggplot(variables, aes(x = Percent_Coral_Cover, y = Coral_Richness)) + 
  geom_point(aes(color = True_Year), size = 4) +
  labs(color="Year") +
  scale_x_continuous(name = "Coral Cover (%)") +
  scale_y_continuous(name = "Coral Richness") +
  scale_color_gradient(low="lightblue", high="darkblue") +
  geom_smooth(size = 1.2, method = "glm.nb", formula = y ~ log(x), color = "black") +
  theme(text=element_text(size = 18), 
        panel.grid.major = element_line(colour = "light gray", size = (0.5)), 
        panel.grid.minor = element_line(colour = "light gray", size = (0.5)),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"))


# Sponge richness across time and with separate site lines when x = Rugosity
ggplot(variables, aes(x = Rugosity, y = Sponge_Richness)) + 
  geom_point(aes(color = True_Year), size = 4) +
  labs(color="Year") +
  scale_x_continuous(name = "Rugosity") +
  scale_y_continuous(name = "Sponge Richness") +
  scale_color_gradient(low="lightblue", high="darkblue") +
  geom_smooth(size = 1.2, method = "glm.nb", formula = y ~ log(x), color = "black") +
  theme(text=element_text(size = 18), 
        panel.grid.major = element_line(colour = "light gray", size = (0.5)), 
        panel.grid.minor = element_line(colour = "light gray", size = (0.5)),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"))+
  facet_wrap(~ Site)

# Fish richness with separate site lines when x = Cover
ggplot(variables, aes(x = Percent_Coral_Cover, y = Fish_Richness)) + 
  geom_point(color = "lightblue", size = 4) +
  scale_x_continuous(name = "Coral Cover (%)") +
  scale_y_continuous(name = "Fish Richness") +
  geom_smooth(size = 1.2, method = "glm.nb", formula = y ~ log(x), color = "black") +
  theme(text=element_text(size = 18), 
        panel.grid.major = element_line(colour = "light gray", size = (0.5)), 
        panel.grid.minor = element_line(colour = "light gray", size = (0.5)),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"))+
  facet_wrap(~ Site)

# Fish richness with separate site lines when x = Rugosity
ggplot(variables, aes(x = Rugosity, y = Fish_Richness)) + 
  geom_point(color = "lightblue", size = 4) +
  scale_x_continuous(name = "Rugosity") +
  scale_y_continuous(name = "Fish Richness") +
  geom_smooth(size = 1.2, method = "glm.nb", formula = y ~ x, color = "black") +
  theme(text=element_text(size = 18), 
        panel.grid.major = element_line(colour = "light gray", size = (0.5)), 
        panel.grid.minor = element_line(colour = "light gray", size = (0.5)),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"))+
  facet_wrap(~ Site)

# Sponge+fish richness across time and with separate site lines when x = Coral Richness
ggplot(variables, aes(x = Coral_Richness, y = Sponge_and_Fish_Richness)) + 
  geom_point(aes(color = True_Year), size = 4) +
  labs(color="Year") +
  scale_x_continuous(name = "Coral Richness") +
  scale_y_continuous(name = "Sponge + Fish Richness") +
  scale_color_gradient(low="lightblue", high="darkblue") +
  geom_smooth(size = 1.2, method = "glm.nb", formula = y ~ x, color = "black") +
  theme(text=element_text(size = 18), 
        panel.grid.major = element_line(colour = "light gray", size = (0.5)), 
        panel.grid.minor = element_line(colour = "light gray", size = (0.5)),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"))+
  facet_wrap(~ Site)

# Combined richness across time and with separate site lines when x = Cover
ggplot(variables, aes(x = Percent_Coral_Cover, y = Combined_Richness)) + 
  geom_point(aes(color = True_Year), size = 4) +
  labs(color="Year") +
  scale_x_continuous(name = "Coral Cover (%)") +
  scale_y_continuous(name = "Combined Richness") +
  scale_color_gradient(low="lightblue", high="darkblue") +
  geom_smooth(size = 1.2, method = "glm.nb", formula = y ~ log(x), color = "black") +
  theme(text=element_text(size = 18), 
        panel.grid.major = element_line(colour = "light gray", size = (0.5)), 
        panel.grid.minor = element_line(colour = "light gray", size = (0.5)),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"))+
  facet_wrap(~ Site)

# Sponge+fish richness with separate site lines when x = Time
ggplot(variables, aes(x = True_Year, y = Sponge_and_Fish_Richness)) + 
  geom_point(color = "lightblue", size = 4) +
  scale_x_continuous(name = "Year") +
  scale_y_continuous(name = "Sponge + Fish Richness") +
  geom_smooth(size = 1.2, method = "glm.nb", formula = y ~ x, color = "black") +
  theme(text=element_text(size = 18), 
        panel.grid.major = element_line(colour = "light gray", size = (0.5)), 
        panel.grid.minor = element_line(colour = "light gray", size = (0.5)),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"))+
  facet_wrap(~ Site)

# Fish richness with site
ggplot(variables, aes(x = Site, y = Fish_Richness, fill = Site)) + 
  scale_fill_viridis_d()+
  geom_boxplot() +
  scale_x_discrete(name = "Site") +
  scale_y_continuous(name = "Fish Richness") +
  theme(legend.position = "none",
        text=element_text(size = 18), 
        axis.text.x=element_text(angle = +90, hjust = 0),
        panel.grid.major = element_line(colour = "light gray", size = (0.5)), 
        panel.grid.minor = element_line(colour = "light gray", size = (0.5)),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"))
#######################################################################################################################
coral_cc = glm.nb(formula = Coral_Richness ~ Percent_Coral_Cover, data = variables)
coral_sc = glm.nb(formula = Coral_Richness ~ Percent_Sponge_Cover, data = variables)
coral_r = glm.nb(formula = Coral_Richness ~ Rugosity, data = variables)
coral_cc_sc = glm.nb(formula = Coral_Richness ~ Percent_Coral_Cover + Percent_Sponge_Cover, data = variables)
coral_cc_r = glm.nb(formula = Coral_Richness ~ Percent_Coral_Cover + Rugosity, data = variables)
coral_sc_r = glm.nb(formula = Coral_Richness ~ Percent_Sponge_Cover + Rugosity, data = variables)
coral_cc_sc_r = glm.nb(formula = Coral_Richness ~ Percent_Coral_Cover + Percent_Sponge_Cover + Rugosity, data = variables)
coral_surrogate <- aictab(cand.set = list(coral_cc, coral_sc, coral_r, coral_cc_sc, coral_cc_r, coral_sc_r, coral_cc_sc_r), 
                          modnames = c("coral_cc", "coral_sc", "coral_r", "coral_cc_sc", "coral_cc_r", "coral_sc_r", "coral_cc_sc_r"), 
                          digits = 4)
sponge_cc = glm.nb(formula = Sponge_Richness ~ Percent_Coral_Cover, data = variables)
sponge_sc = glm.nb(formula = Sponge_Richness ~ Percent_Sponge_Cover, data = variables)
sponge_r = glm.nb(formula = Sponge_Richness ~ Rugosity, data = variables)
sponge_cc_sc = glm.nb(formula = Sponge_Richness ~ Percent_Coral_Cover + Percent_Sponge_Cover, data = variables)
sponge_cc_r = glm.nb(formula = Sponge_Richness ~ Percent_Coral_Cover + Rugosity, data = variables)
sponge_sc_r = glm.nb(formula = Sponge_Richness ~ Percent_Sponge_Cover + Rugosity, data = variables)
sponge_cc_sc_r = glm.nb(formula = Sponge_Richness ~ Percent_Coral_Cover + Percent_Sponge_Cover + Rugosity, data = variables)
sponge_surrogate <- aictab(cand.set = list(sponge_cc, sponge_sc, sponge_r, sponge_cc_sc, sponge_cc_r, sponge_sc_r, sponge_cc_sc_r), 
                           modnames = c("sponge_cc", "sponge_sc", "sponge_r", "sponge_cc_sc", "sponge_cc_r", "sponge_sc_r", "sponge_cc_sc_r"), 
                           digits = 4)
fish_cc = glm.nb(formula = Fish_Richness ~ Percent_Coral_Cover, data = variables)
fish_sc = glm.nb(formula = Fish_Richness ~ Percent_Sponge_Cover, data = variables)
fish_r = glm.nb(formula = Fish_Richness ~ Rugosity, data = variables)
fish_cc_sc = glm.nb(formula = Fish_Richness ~ Percent_Coral_Cover + Percent_Sponge_Cover, data = variables)
fish_cc_r = glm.nb(formula = Fish_Richness ~ Percent_Coral_Cover + Rugosity, data = variables)
fish_sc_r = glm.nb(formula = Fish_Richness ~ Percent_Sponge_Cover + Rugosity, data = variables)
fish_cc_sc_r = glm.nb(formula = Fish_Richness ~ Percent_Coral_Cover + Percent_Sponge_Cover + Rugosity, data = variables)
fish_surrogate <- aictab(cand.set = list(fish_cc, fish_sc, fish_r, fish_cc_sc, fish_cc_r, fish_sc_r, fish_cc_sc_r), 
                         modnames = c("fish_cc", "fish_sc", "fish_r", "fish_cc_sc", "fish_cc_r", "fish_sc_r", "fish_cc_sc_r"), 
                         digits = 4)
combined_cc = glm.nb(formula = Combined_Richness ~ Percent_Coral_Cover, data = variables)
combined_sc = glm.nb(formula = Combined_Richness ~ Percent_Sponge_Cover, data = variables)
combined_r = glm.nb(formula = Combined_Richness ~ Rugosity, data = variables)
combined_cc_sc = glm.nb(formula = Combined_Richness ~ Percent_Coral_Cover + Percent_Sponge_Cover, data = variables)
combined_cc_r = glm.nb(formula = Combined_Richness ~ Percent_Coral_Cover + Rugosity, data = variables)
combined_sc_r = glm.nb(formula = Combined_Richness ~ Percent_Sponge_Cover + Rugosity, data = variables)
combined_cc_sc_r = glm.nb(formula = Combined_Richness ~ Percent_Coral_Cover + Percent_Sponge_Cover + Rugosity, data = variables)
combined_surrogate <- aictab(cand.set = list(combined_cc, combined_sc, combined_r, combined_cc_sc, combined_cc_r, combined_sc_r, combined_cc_sc_r), 
                             modnames = c("combined_cc", "combined_sc", "combined_r", "combined_cc_sc", "combined_cc_r", "combined_sc_r", "combined_cc_sc_r"), 
                             digits = 4)





# ***Simple Figures with Models



# Figure. Rugosity as a surrogate for fish richness with simple linear, logarithmic, and power models.
ggplot(variables, aes(x = Rugosity, y = Fish_Richness)) + 
  #geom_line(size = 1.1) +
  #geom_point(size = 5, aes(color = variables$Site))+
  geom_point(size = 3)+
  scale_x_continuous(name ="Rugosity") +
  scale_y_continuous(name ="Fish Richness") +
  geom_smooth(size = 1.2, method = "glm.nb", formula = y ~ x, aes(color = "Linear")) +
  geom_smooth(size = 1.2, method = "glm.nb", formula = y ~ log(x), aes(color = "Logarithmic")) +
  geom_smooth(size = 1.2, method = "glm.nb", formula = y ~ exp(1.3053 + 0.4944*log(x)), aes(color = "Power")) +
  scale_colour_manual(name = "Models", values = c("blue", "red", "green")) +
  theme(text=element_text(size=27), 
        #axis.text.x=element_text(angle = +90, hjust = 0),
        panel.grid.major = element_line(colour="light gray", size = (0.5)), 
        panel.grid.minor = element_line(colour="light gray", size = (0.5)),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"))

# Figure. Coral cover as a surrogate for fish richness with simple linear, logarithmic, and power models.
ggplot(variables, aes(x = Percent_Coral_Cover, y = Fish_Richness)) + 
  geom_point(size = 3)+
  scale_x_continuous(name ="Coral Cover (%)") +
  scale_y_continuous(name ="Fish Richness") +
  geom_smooth(size = 1.2, method = "glm.nb", formula = y ~ x, aes(color = "Linear")) +
  geom_smooth(size = 1.2, method = "glm.nb", formula = y ~ log(x), aes(color = "Logarithmic")) +
  geom_smooth(size = 1.2, method = "glm.nb", formula = y ~ exp(2.5059 + 0.2305*log(x)), aes(color = "Power")) +
  scale_colour_manual(name = "Models", values = c("blue", "red", "green")) +
  theme(text=element_text(size=27), 
        panel.grid.major = element_line(colour="light gray", size = (0.5)), 
        panel.grid.minor = element_line(colour="light gray", size = (0.5)),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"))

# Figure. Coral richness as a surrogate for fish richness with simple linear, logarithmic, and power models.
ggplot(variables, aes(x = Coral_Richness, y = Fish_Richness)) + 
  geom_point(size = 3)+
  scale_x_continuous(name ="Coral Richness") +
  scale_y_continuous(name ="Fish Richness") +
  geom_smooth(size = 1.2, method = "glm.nb", formula = y ~ x, aes(color = "Linear")) +
  #geom_smooth(size = 1.2, method = "glm.nb", formula = y ~ log(x), aes(color = "Logarithmic")) +
  #geom_smooth(size = 1.2, method = "glm.nb", formula = y ~ exp(2.1422 + 0.4041*log(x)), aes(color = "Power")) +
  scale_colour_manual(name = "Models", values = c("blue", "red", "green")) +
  theme(text=element_text(size=27), 
        panel.grid.major = element_line(colour="light gray", size = (0.5)), 
        panel.grid.minor = element_line(colour="light gray", size = (0.5)),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"))

# Figure. Rugosity as a surrogate for sponge richness with simple linear, logarithmic, and power models.
ggplot(variables, aes(x = Rugosity, y = Sponge_Richness)) + 
  geom_point(size = 3)+
  scale_x_continuous(name ="Rugosity") +
  scale_y_continuous(name ="Sponge Richness") +
  geom_smooth(size = 1.2, method = "glm.nb", formula = y ~ x, aes(color = "Linear")) +
  #geom_smooth(size = 1.2, method = "glm.nb", formula = y ~ log(x), aes(color = "Logarithmic")) +
  #geom_smooth(size = 1.2, method = "glm.nb", formula = y ~ exp(3.4258 + -0.1012*log(x)), aes(color = "Power")) +
  scale_colour_manual(name = "Models", values = c("blue", "red", "green")) +
  theme(text=element_text(size=27), 
        panel.grid.major = element_line(colour="light gray", size = (0.5)), 
        panel.grid.minor = element_line(colour="light gray", size = (0.5)),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"))

# Figure. Coral cover as a surrogate for sponge richness with simple linear, logarithmic, and power models.
ggplot(variables, aes(x = Percent_Coral_Cover, y = Sponge_Richness)) + 
  geom_point(size = 3)+
  scale_x_continuous(name ="Coral Cover (%)") +
  scale_y_continuous(name ="Sponge Richness") +
  geom_smooth(size = 1.2, method = "glm.nb", formula = y ~ x, aes(color = "Linear")) +
  #geom_smooth(size = 1.2, method = "glm.nb", formula = y ~ log(x), aes(color = "Logarithmic")) +
  #geom_smooth(size = 1.2, method = "glm.nb", formula = y ~ exp(3.4226 + -0.1344*log(x)), aes(color = "Power")) +
  scale_colour_manual(name = "Models", values = c("blue", "red", "green")) +
  theme(text=element_text(size=27), 
        panel.grid.major = element_line(colour="light gray", size = (0.5)), 
        panel.grid.minor = element_line(colour="light gray", size = (0.5)),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"))

# Figure. Coral richness as a surrogate for sponge richness with simple linear, logarithmic, and power models.
ggplot(variables, aes(x = Coral_Richness, y = Sponge_Richness)) + 
  geom_point(size = 3)+
  scale_x_continuous(name ="Coral Richness") +
  scale_y_continuous(name ="Sponge Richness") +
  geom_smooth(size = 1.2, method = "glm.nb", formula = y ~ x, aes(color = "Linear")) +
  #geom_smooth(size = 1.2, method = "glm.nb", formula = y ~ log(x), aes(color = "Logarithmic")) +
  #geom_smooth(size = 1.2, method = "glm.nb", formula = y ~ exp(3.5587 + -0.2029*log(x)), aes(color = "Power")) +
  scale_colour_manual(name = "Models", values = c("blue", "red", "green")) +
  theme(text=element_text(size=27), 
        panel.grid.major = element_line(colour="light gray", size = (0.5)), 
        panel.grid.minor = element_line(colour="light gray", size = (0.5)),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"))

# Figure. Rugosity as a surrogate for coral richness with linear model only.
ggplot(variables, aes(x = Rugosity, y = Coral_Richness)) + 
  geom_point(size = 3)+
  scale_x_continuous(name ="Rugosity") +
  scale_y_continuous(name ="Coral Richness") +
  geom_smooth(size = 1.2, method = "glm.nb", formula = y ~ x, aes(color = "Linear")) +
  geom_smooth(size = 1.2, method = "glm.nb", formula = y ~ log(x), aes(color = "Logarithmic")) +
  geom_smooth(size = 1.2, method = "glm.nb", formula = y ~ exp(0.8607 + 0.9674*log(x)), aes(color = "Power")) +
  scale_colour_manual(name = "Models", values = c("blue", "red", "green")) +
  theme(text=element_text(size=27), 
        panel.grid.major = element_line(colour="light gray", size = (0.5)), 
        panel.grid.minor = element_line(colour="light gray", size = (0.5)),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"))

# Figure. Coral cover as a surrogate for coral richness with simple linear, logarithmic, and power models.
ggplot(variables, aes(x = Percent_Coral_Cover, y = Coral_Richness)) + 
  geom_point(size = 3)+
  scale_x_continuous(name ="Coral Cover (%)") +
  scale_y_continuous(name ="Coral Richness") +
  geom_smooth(size = 1.2, method = "glm.nb", formula = y ~ x, aes(color = "Linear")) +
  geom_smooth(size = 1.2, method = "glm.nb", formula = y ~ log(x), aes(color = "Logarithmic")) +
  geom_smooth(size = 1.2, method = "glm.nb", formula = y ~ exp(1.6342 + 0.3108*log(x)), aes(color = "Power")) +
  scale_colour_manual(name = "Models", values = c("blue", "red", "green")) +
  theme(text=element_text(size=27), 
        panel.grid.major = element_line(colour="light gray", size = (0.5)), 
        panel.grid.minor = element_line(colour="light gray", size = (0.5)),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"))

# Figure. Rugosity as a surrogate for combined richness with simple linear, logarithmic, and power models.
ggplot(variables, aes(x = Rugosity, y = Combined_Richness)) + 
  geom_point(size = 3)+
  scale_x_continuous(name ="Rugosity") +
  scale_y_continuous(name ="Combined Richness") +
  geom_smooth(size = 1.2, method = "glm.nb", formula = y ~ x, aes(color = "Linear")) +
  geom_smooth(size = 1.2, method = "glm.nb", formula = y ~ log(x), aes(color = "Logarithmic")) +
  geom_smooth(size = 1.2, method = "glm.nb", formula = y ~ exp(3.2653 + 0.2149*log(x)), aes(color = "Power")) +
  scale_colour_manual(name = "Models", values = c("blue", "red", "green")) +
  theme(text=element_text(size=27), 
        panel.grid.major = element_line(colour="light gray", size = (0.5)), 
        panel.grid.minor = element_line(colour="light gray", size = (0.5)),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"))

# Figure. Coral cover as a surrogate for combined richness with simple linear, logarithmic, and power models.
ggplot(variables, aes(x = Percent_Coral_Cover, y = Combined_Richness)) + 
  geom_point(size = 3)+
  scale_x_continuous(name ="Coral Cover (%)") +
  scale_y_continuous(name ="Combined Richness") +
  geom_smooth(size = 1.2, method = "glm.nb", formula = y ~ x, aes(color = "Linear")) +
  geom_smooth(size = 1.2, method = "glm.nb", formula = y ~ log(x), aes(color = "Logarithmic")) +
  geom_smooth(size = 1.2, method = "glm.nb", formula = y ~ exp(3.79088 + 0.09888*log(x)), aes(color = "Power")) +
  scale_colour_manual(name = "Models", values = c("blue", "red", "green")) +
  theme(text=element_text(size=27), 
        panel.grid.major = element_line(colour="light gray", size = (0.5)), 
        panel.grid.minor = element_line(colour="light gray", size = (0.5)),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"))

# Figure. Coral richness as a surrogate for combined richness with simple linear, logarithmic, and power models.
ggplot(variables, aes(x = Coral_Richness, y = Combined_Richness)) + 
  geom_point(size = 3)+
  scale_x_continuous(name ="Coral Richness") +
  scale_y_continuous(name ="Combined Richness") +
  geom_smooth(size = 1.2, method = "glm.nb", formula = y ~ x, aes(color = "Linear")) +
  #geom_smooth(size = 1.2, method = "glm.nb", formula = y ~ log(x), aes(color = "Logarithmic")) +
  #geom_smooth(size = 1.2, method = "glm.nb", formula = y ~ exp(3.3032 + 0.3036*log(x)), aes(color = "Power")) +
  scale_colour_manual(name = "Models", values = c("blue", "red", "green")) +
  theme(text=element_text(size=27), 
        panel.grid.major = element_line(colour="light gray", size = (0.5)), 
        panel.grid.minor = element_line(colour="light gray", size = (0.5)),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"))



# Verify year is factor for x-axis labels
variables$True_Year <- as.factor(variables$True_Year)
# Rugosity over time
ggplot(variables, aes(x = True_Year, y = Rugosity, group = Site, color = Site)) + 
  geom_line(size = 1.1) +
  scale_x_discrete(name = "Time (year)") +
  scale_y_continuous(name = "Rugosity") +
  theme(text=element_text(size = 18), 
        axis.text.x=element_text(angle = +90, hjust = 0),
        panel.grid.major = element_line(colour = "light gray", size = (0.5)), 
        panel.grid.minor = element_line(colour = "light gray", size = (0.5)),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"))
# Coral cover over time
ggplot(variables, aes(x = True_Year, y = Percent_Coral_Cover, group = Site, color = Site)) + 
  geom_line(size = 1.1) +
  scale_x_discrete(name = "Time (year)") +
  scale_y_continuous(name = "Coral Cover (%)") +
  theme(text=element_text(size = 18), 
        axis.text.x=element_text(angle = +90, hjust = 0),
        panel.grid.major = element_line(colour = "light gray", size = (0.5)), 
        panel.grid.minor = element_line(colour = "light gray", size = (0.5)),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"))



## ***Simple Figures for Fish Richness

# Verify year is factor for x-axis labels
variables$True_Year <- as.factor(variables$True_Year)

# Over time
ggplot(variables, aes(x = True_Year, y = Fish_Richness, group = Site, color = Site)) + 
  geom_line(size = 1.1) +
  scale_x_discrete(name = "Time (year)") +
  scale_y_continuous(name = "Fish Richness") +
  theme(text=element_text(size = 18), 
        axis.text.x=element_text(angle = +90, hjust = 0),
        panel.grid.major = element_line(colour = "light gray", size = (0.5)), 
        panel.grid.minor = element_line(colour = "light gray", size = (0.5)),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"))

# Across sites when x = Rugosity
ggplot(variables, aes(x = Rugosity, y = Fish_Richness, group = Site, color = Site)) + 
  geom_point(size = 4)+
  scale_x_continuous(name = "Rugosity") +
  scale_y_continuous(name = "Fish Richness") +
  theme(text=element_text(size = 18), 
        panel.grid.major = element_line(colour = "light gray", size = (0.5)), 
        panel.grid.minor = element_line(colour = "light gray", size = (0.5)),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"))

# Across sites when x = Coral Cover
ggplot(variables, aes(x = Percent_Coral_Cover, y = Fish_Richness, group = Site, color = Site)) + 
  geom_point(size = 4)+
  scale_x_continuous(name = "Coral Cover (%)") +
  scale_y_continuous(name = "Fish Richness") +
  theme(text=element_text(size = 18), 
        panel.grid.major = element_line(colour = "light gray", size = (0.5)), 
        panel.grid.minor = element_line(colour = "light gray", size = (0.5)),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"))

# Across sites when x = Coral Richness
ggplot(variables, aes(x = Coral_Richness, y = Fish_Richness, group = Site, color = Site)) + 
  geom_point(size = 4)+
  scale_x_continuous(name = "Coral Richness") +
  scale_y_continuous(name = "Fish Richness") +
  theme(text=element_text(size = 18), 
        panel.grid.major = element_line(colour = "light gray", size = (0.5)), 
        panel.grid.minor = element_line(colour = "light gray", size = (0.5)),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"))

# Change year to numeric to allow for color gradient
variables$True_Year <- as.numeric(as.character(variables$True_Year))

# Across time when x = Rugosity
ggplot(variables, aes(x = Rugosity, y = Fish_Richness)) + 
  geom_point(aes(color = True_Year), size = 4) +
  scale_x_continuous(name = "Rugosity") +
  scale_y_continuous(name = "Fish Richness") +
  scale_color_gradient(low="lightblue", high="darkblue") +
  theme(text=element_text(size = 18), 
        panel.grid.major = element_line(colour = "light gray", size = (0.5)), 
        panel.grid.minor = element_line(colour = "light gray", size = (0.5)),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"))

# Across time when x = Coral Cover
ggplot(variables, aes(x = Percent_Coral_Cover, y = Fish_Richness)) + 
  geom_point(aes(color = True_Year), size = 4) +
  scale_x_continuous(name = "Coral Cover (%)") +
  scale_y_continuous(name = "Fish Richness") +
  scale_color_gradient(low="lightblue", high="darkblue") +
  theme(text=element_text(size = 18), 
        panel.grid.major = element_line(colour = "light gray", size = (0.5)), 
        panel.grid.minor = element_line(colour = "light gray", size = (0.5)),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"))

# Across time when x = Coral Richness
ggplot(variables, aes(x = Coral_Richness, y = Fish_Richness)) + 
  geom_point(aes(color = True_Year), size = 4) +
  scale_x_continuous(name = "Coral Richness") +
  scale_y_continuous(name = "Fish Richness") +
  scale_color_gradient(low="lightblue", high="darkblue") +
  theme(text=element_text(size = 18), 
        panel.grid.major = element_line(colour = "light gray", size = (0.5)), 
        panel.grid.minor = element_line(colour = "light gray", size = (0.5)),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"))

# Across time when x = Rugosity separated by site
ggplot(variables, aes(x = Rugosity, y = Fish_Richness)) + 
  geom_point(aes(color = True_Year), size = 3) +
  scale_x_continuous(name = "Rugosity") +
  scale_y_continuous(name = "Fish Richness") +
  scale_color_gradient(low="lightblue", high="darkblue") +
  theme(text=element_text(size = 18), 
        panel.grid.major = element_line(colour = "light gray", size = (0.5)), 
        panel.grid.minor = element_line(colour = "light gray", size = (0.5)),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black")) +
  facet_wrap(~ Site)

# Across time when x = Coral Cover separated by site
ggplot(variables, aes(x = Percent_Coral_Cover, y = Fish_Richness)) + 
  geom_point(aes(color = True_Year), size = 3) +
  scale_x_continuous(name = "Coral Cover (%)") +
  scale_y_continuous(name = "Fish Richness") +
  scale_color_gradient(low="lightblue", high="darkblue") +
  theme(text=element_text(size = 18), 
        panel.grid.major = element_line(colour = "light gray", size = (0.5)), 
        panel.grid.minor = element_line(colour = "light gray", size = (0.5)),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black")) +
  facet_wrap(~ Site)

# Across time when x = Coral Richness separated by site
ggplot(variables, aes(x = Coral_Richness, y = Fish_Richness)) + 
  geom_point(aes(color = True_Year), size = 3) +
  scale_x_continuous(name = "Coral Richness") +
  scale_y_continuous(name = "Fish Richness") +
  scale_color_gradient(low="lightblue", high="darkblue") +
  theme(text=element_text(size = 18), 
        panel.grid.major = element_line(colour = "light gray", size = (0.5)), 
        panel.grid.minor = element_line(colour = "light gray", size = (0.5)),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black")) +
  facet_wrap(~ Site)



## ***Simple Figures for Sponge Richness

# Verify year is factor for x-axis labels
variables$True_Year <- as.factor(variables$True_Year)

# Over time
ggplot(variables, aes(x = True_Year, y = Sponge_Richness, group = Site, color = Site)) + 
  geom_line(size = 1.1) +
  scale_x_discrete(name = "Time (year)") +
  scale_y_continuous(name = "Sponge Richness") +
  theme(text=element_text(size = 18), 
        axis.text.x=element_text(angle = +90, hjust = 0),
        panel.grid.major = element_line(colour = "light gray", size = (0.5)), 
        panel.grid.minor = element_line(colour = "light gray", size = (0.5)),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"))
# ***Notice that this figure has line breaks where the value for Sponge Richness in that given year is NA
# # The following code will remove the line breaks by deleting the years without values and merging the line segments 
# ggplot(variables[!is.na(variables$Sponge_Richness),], aes(x = True_Year, y = Sponge_Richness, group = Site, color = Site)) + 
#   geom_line(size = 1.1) +
#   scale_x_discrete(name ="Time (year)") +
#   scale_y_continuous(name ="Sponge Richness")

# Across sites when x = Rugosity
ggplot(variables, aes(x = Rugosity, y = Sponge_Richness, group = Site, color = Site)) + 
  geom_point(size = 4)+
  scale_x_continuous(name = "Rugosity") +
  scale_y_continuous(name = "Sponge Richness") +
  theme(text=element_text(size = 18), 
        panel.grid.major = element_line(colour = "light gray", size = (0.5)), 
        panel.grid.minor = element_line(colour = "light gray", size = (0.5)),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"))

# Across sites when x = Coral Cover
ggplot(variables, aes(x = Percent_Coral_Cover, y = Sponge_Richness, group = Site, color = Site)) + 
  geom_point(size = 4)+
  scale_x_continuous(name = "Coral Cover (%)") +
  scale_y_continuous(name = "Sponge Richness") +
  theme(text=element_text(size = 18), 
        panel.grid.major = element_line(colour = "light gray", size = (0.5)), 
        panel.grid.minor = element_line(colour = "light gray", size = (0.5)),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"))

# Across sites when x = Coral Richness
ggplot(variables, aes(x = Coral_Richness, y = Sponge_Richness, group = Site, color = Site)) + 
  geom_point(size = 4)+
  scale_x_continuous(name = "Coral Richness") +
  scale_y_continuous(name = "Sponge Richness") +
  theme(text=element_text(size = 18), 
        panel.grid.major = element_line(colour = "light gray", size = (0.5)), 
        panel.grid.minor = element_line(colour = "light gray", size = (0.5)),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"))

# Change year to numeric to allow for color gradient
variables$True_Year <- as.numeric(as.character(variables$True_Year))

# Across time when x = Rugosity
ggplot(variables, aes(x = Rugosity, y = Sponge_Richness)) + 
  geom_point(aes(color = True_Year), size = 4) +
  scale_x_continuous(name = "Rugosity") +
  scale_y_continuous(name = "Sponge Richness") +
  scale_color_gradient(low="lightblue", high="darkblue") +
  theme(text=element_text(size = 18), 
        panel.grid.major = element_line(colour = "light gray", size = (0.5)), 
        panel.grid.minor = element_line(colour = "light gray", size = (0.5)),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"))

# Across time when x = Coral Cover
ggplot(variables, aes(x = Percent_Coral_Cover, y = Sponge_Richness)) + 
  geom_point(aes(color = True_Year), size = 4) +
  scale_x_continuous(name = "Coral Cover (%)") +
  scale_y_continuous(name = "Sponge Richness") +
  scale_color_gradient(low="lightblue", high="darkblue") +
  theme(text=element_text(size = 18), 
        panel.grid.major = element_line(colour = "light gray", size = (0.5)), 
        panel.grid.minor = element_line(colour = "light gray", size = (0.5)),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"))

# Across time when x = Coral Richness
ggplot(variables, aes(x = Coral_Richness, y = Sponge_Richness)) + 
  geom_point(aes(color = True_Year), size = 4) +
  scale_x_continuous(name = "Coral Richness") +
  scale_y_continuous(name = "Sponge Richness") +
  scale_color_gradient(low="lightblue", high="darkblue") +
  theme(text=element_text(size = 18), 
        panel.grid.major = element_line(colour = "light gray", size = (0.5)), 
        panel.grid.minor = element_line(colour = "light gray", size = (0.5)),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"))

# Across time when x = Rugosity separated by site
ggplot(variables, aes(x = Rugosity, y = Sponge_Richness)) + 
  geom_point(aes(color = True_Year), size = 3) +
  scale_x_continuous(name = "Rugosity") +
  scale_y_continuous(name = "Sponge Richness") +
  scale_color_gradient(low="lightblue", high="darkblue") +
  theme(text=element_text(size = 18), 
        panel.grid.major = element_line(colour = "light gray", size = (0.5)), 
        panel.grid.minor = element_line(colour = "light gray", size = (0.5)),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black")) +
  facet_wrap(~ Site)

# Across time when x = Coral Cover separated by site
ggplot(variables, aes(x = Percent_Coral_Cover, y = Sponge_Richness)) + 
  geom_point(aes(color = True_Year), size = 3) +
  scale_x_continuous(name = "Coral Cover (%)") +
  scale_y_continuous(name = "Sponge Richness") +
  scale_color_gradient(low="lightblue", high="darkblue") +
  theme(text=element_text(size = 18), 
        panel.grid.major = element_line(colour = "light gray", size = (0.5)), 
        panel.grid.minor = element_line(colour = "light gray", size = (0.5)),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black")) +
  facet_wrap(~ Site)

# Across time when x = Coral Richness separated by site
ggplot(variables, aes(x = Coral_Richness, y = Sponge_Richness)) + 
  geom_point(aes(color = True_Year), size = 3) +
  scale_x_continuous(name = "Coral Richness") +
  scale_y_continuous(name = "Sponge Richness") +
  scale_color_gradient(low="lightblue", high="darkblue") +
  theme(text=element_text(size = 18), 
        panel.grid.major = element_line(colour = "light gray", size = (0.5)), 
        panel.grid.minor = element_line(colour = "light gray", size = (0.5)),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black")) +
  facet_wrap(~ Site)



## ***Simple Figures for Coral Richness

# Verify year is factor for x-axis labels
variables$True_Year <- as.factor(variables$True_Year)

# Over time
ggplot(variables, aes(x = True_Year, y = Coral_Richness, group = Site, color = Site)) + 
  geom_line(size = 1.1) +
  scale_x_discrete(name = "Time (year)") +
  scale_y_continuous(name = "Coral Richness") +
  theme(text=element_text(size = 18), 
        axis.text.x=element_text(angle = +90, hjust = 0),
        panel.grid.major = element_line(colour = "light gray", size = (0.5)), 
        panel.grid.minor = element_line(colour = "light gray", size = (0.5)),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"))

# Across sites when x = Rugosity
ggplot(variables, aes(x = Rugosity, y = Coral_Richness, group = Site, color = Site)) + 
  geom_point(size = 4)+
  scale_x_continuous(name = "Rugosity") +
  scale_y_continuous(name = "Coral Richness") +
  theme(text=element_text(size = 18), 
        panel.grid.major = element_line(colour = "light gray", size = (0.5)), 
        panel.grid.minor = element_line(colour = "light gray", size = (0.5)),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"))

# Across sites when x = Coral Cover
ggplot(variables, aes(x = Percent_Coral_Cover, y = Coral_Richness, group = Site, color = Site)) + 
  geom_point(size = 4)+
  scale_x_continuous(name = "Coral Cover (%)") +
  scale_y_continuous(name = "Coral Richness") +
  theme(text=element_text(size = 18), 
        panel.grid.major = element_line(colour = "light gray", size = (0.5)), 
        panel.grid.minor = element_line(colour = "light gray", size = (0.5)),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"))

# Change year to numeric to allow for color gradient
variables$True_Year <- as.numeric(as.character(variables$True_Year))

# Across time when x = Rugosity
ggplot(variables, aes(x = Rugosity, y = Coral_Richness)) + 
  geom_point(aes(color = True_Year), size = 4) +
  scale_x_continuous(name = "Rugosity") +
  scale_y_continuous(name = "Coral Richness") +
  scale_color_gradient(low="lightblue", high="darkblue") +
  theme(text=element_text(size = 18), 
        panel.grid.major = element_line(colour = "light gray", size = (0.5)), 
        panel.grid.minor = element_line(colour = "light gray", size = (0.5)),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"))

# Across time when x = Coral Cover
ggplot(variables, aes(x = Percent_Coral_Cover, y = Coral_Richness)) + 
  geom_point(aes(color = True_Year), size = 4) +
  scale_x_continuous(name = "Coral Cover (%)") +
  scale_y_continuous(name = "Coral Richness") +
  scale_color_gradient(low="lightblue", high="darkblue") +
  theme(text=element_text(size = 18), 
        panel.grid.major = element_line(colour = "light gray", size = (0.5)), 
        panel.grid.minor = element_line(colour = "light gray", size = (0.5)),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"))

# Across time when x = Rugosity separated by site
ggplot(variables, aes(x = Rugosity, y = Coral_Richness)) + 
  geom_point(aes(color = True_Year), size = 3) +
  scale_x_continuous(name = "Rugosity") +
  scale_y_continuous(name = "Coral Richness") +
  scale_color_gradient(low="lightblue", high="darkblue") +
  theme(text=element_text(size = 18), 
        panel.grid.major = element_line(colour = "light gray", size = (0.5)), 
        panel.grid.minor = element_line(colour = "light gray", size = (0.5)),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black")) +
  facet_wrap(~ Site)

# Across time when x = Coral Cover separated by site
ggplot(variables, aes(x = Percent_Coral_Cover, y = Coral_Richness)) + 
  geom_point(aes(color = True_Year), size = 3) +
  scale_x_continuous(name = "Coral Cover (%)") +
  scale_y_continuous(name = "Coral Richness") +
  scale_color_gradient(low="lightblue", high="darkblue") +
  theme(text=element_text(size = 18), 
        panel.grid.major = element_line(colour = "light gray", size = (0.5)), 
        panel.grid.minor = element_line(colour = "light gray", size = (0.5)),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black")) +
  facet_wrap(~ Site)



## ***Simple Figures for Combined Richness

# Verify year is factor for x-axis labels
variables$True_Year <- as.factor(variables$True_Year)

# Over time
ggplot(variables, aes(x = True_Year, y = Combined_Richness, group = Site, color = Site)) + 
  geom_line(size = 1.1) +
  scale_x_discrete(name = "Time (year)") +
  scale_y_continuous(name = "Combined Richness") +
  theme(text=element_text(size = 18), 
        axis.text.x=element_text(angle = +90, hjust = 0),
        panel.grid.major = element_line(colour = "light gray", size = (0.5)), 
        panel.grid.minor = element_line(colour = "light gray", size = (0.5)),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"))

# Across sites when x = Rugosity
ggplot(variables, aes(x = Rugosity, y = Combined_Richness, group = Site, color = Site)) + 
  geom_point(size = 4)+
  scale_x_continuous(name = "Rugosity") +
  scale_y_continuous(name = "Combined Richness") +
  theme(text=element_text(size = 18), 
        panel.grid.major = element_line(colour = "light gray", size = (0.5)), 
        panel.grid.minor = element_line(colour = "light gray", size = (0.5)),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"))

# Across sites when x = Coral Cover
ggplot(variables, aes(x = Percent_Coral_Cover, y = Combined_Richness, group = Site, color = Site)) + 
  geom_point(size = 4)+
  scale_x_continuous(name = "Coral Cover (%)") +
  scale_y_continuous(name = "Combined Richness") +
  theme(text=element_text(size = 18), 
        panel.grid.major = element_line(colour = "light gray", size = (0.5)), 
        panel.grid.minor = element_line(colour = "light gray", size = (0.5)),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"))

# Across sites when x = Coral Richness
ggplot(variables, aes(x = Coral_Richness, y = Combined_Richness, group = Site, color = Site)) + 
  geom_point(size = 4)+
  scale_x_continuous(name = "Coral Richness") +
  scale_y_continuous(name = "Combined Richness") +
  theme(text=element_text(size = 18), 
        panel.grid.major = element_line(colour = "light gray", size = (0.5)), 
        panel.grid.minor = element_line(colour = "light gray", size = (0.5)),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"))

# Change year to numeric to allow for color gradient
variables$True_Year <- as.numeric(as.character(variables$True_Year))

# Across time when x = Rugosity
ggplot(variables, aes(x = Rugosity, y = Combined_Richness)) + 
  geom_point(aes(color = True_Year), size = 4) +
  scale_x_continuous(name = "Rugosity") +
  scale_y_continuous(name = "Combined Richness") +
  scale_color_gradient(low="lightblue", high="darkblue") +
  theme(text=element_text(size = 18), 
        panel.grid.major = element_line(colour = "light gray", size = (0.5)), 
        panel.grid.minor = element_line(colour = "light gray", size = (0.5)),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"))

# Across time when x = Coral Cover
ggplot(variables, aes(x = Percent_Coral_Cover, y = Combined_Richness)) + 
  geom_point(aes(color = True_Year), size = 4) +
  scale_x_continuous(name = "Coral Cover (%)") +
  scale_y_continuous(name = "Combined Richness") +
  scale_color_gradient(low="lightblue", high="darkblue") +
  theme(text=element_text(size = 18), 
        panel.grid.major = element_line(colour = "light gray", size = (0.5)), 
        panel.grid.minor = element_line(colour = "light gray", size = (0.5)),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"))

# Across time when x = Coral Richness
ggplot(variables, aes(x = Coral_Richness, y = Combined_Richness)) + 
  geom_point(aes(color = True_Year), size = 4) +
  scale_x_continuous(name = "Coral Richness") +
  scale_y_continuous(name = "Combined Richness") +
  scale_color_gradient(low="lightblue", high="darkblue") +
  theme(text=element_text(size = 18), 
        panel.grid.major = element_line(colour = "light gray", size = (0.5)), 
        panel.grid.minor = element_line(colour = "light gray", size = (0.5)),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"))

# Across time when x = Rugosity separated by site
ggplot(variables, aes(x = Rugosity, y = Combined_Richness)) + 
  geom_point(aes(color = True_Year), size = 3) +
  scale_x_continuous(name = "Rugosity") +
  scale_y_continuous(name = "Combined Richness") +
  scale_color_gradient(low="lightblue", high="darkblue") +
  theme(text=element_text(size = 18), 
        panel.grid.major = element_line(colour = "light gray", size = (0.5)), 
        panel.grid.minor = element_line(colour = "light gray", size = (0.5)),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black")) +
  facet_wrap(~ Site)

# Across time when x = Coral Cover separated by site
ggplot(variables, aes(x = Percent_Coral_Cover, y = Combined_Richness)) + 
  geom_point(aes(color = True_Year), size = 3) +
  scale_x_continuous(name = "Coral Cover (%)") +
  scale_y_continuous(name = "Combined Richness") +
  scale_color_gradient(low="lightblue", high="darkblue") +
  theme(text=element_text(size = 18), 
        panel.grid.major = element_line(colour = "light gray", size = (0.5)), 
        panel.grid.minor = element_line(colour = "light gray", size = (0.5)),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black")) +
  facet_wrap(~ Site)

# Across time when x = Coral Richness separated by site
ggplot(variables, aes(x = Coral_Richness, y = Combined_Richness)) + 
  geom_point(aes(color = True_Year), size = 3) +
  scale_x_continuous(name = "Coral Richness") +
  scale_y_continuous(name = "Combined Richness") +
  scale_color_gradient(low="lightblue", high="darkblue") +
  theme(text=element_text(size = 18), 
        panel.grid.major = element_line(colour = "light gray", size = (0.5)), 
        panel.grid.minor = element_line(colour = "light gray", size = (0.5)),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black")) +
  facet_wrap(~ Site)

################################################################

# Create HTML files for most parsimonious models and models with one variable
tab_model(fish_site, fish_rugosity, fish_cover, fish_coralrichness, fish_year)
tab_model(sponge_rugosity_year_site_log, sponge_site, sponge_cover, sponge_coralrichness, sponge_year, sponge_rugosity)
tab_model(coral_cover_year_log, coral_cover, coral_site, coral_rugosity, coral_year)
tab_model(combined_cover_year_site_log, combined_site, combined_rugosity, combined_cover, combined_year)
tab_model(fishsponge_year_site, fishsponge_site, fishsponge_coralrichness, fishsponge_year)

##############################################################################################################################################################








## Figures for NEAFWA

# Coral richness across time when x = Coral Cover
ggplot(variables, aes(x = Percent_Coral_Cover, y = Coral_Richness)) + 
  geom_point(aes(color = True_Year), size = 4) +
  labs(color="Year") +
  scale_x_continuous(name = "Coral Cover (%)") +
  scale_y_continuous(name = "Coral Richness") +
  scale_color_gradient(low="lightblue", high="darkblue") +
  geom_smooth(size = 1.2, method = "glm.nb", formula = y ~ log(x), color = "black") +
  theme(text=element_text(size = 18), 
        panel.grid.major = element_line(colour = "light gray", size = (0.5)), 
        panel.grid.minor = element_line(colour = "light gray", size = (0.5)),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"))


# Sponge richness across time and with separate site lines when x = Rugosity
ggplot(variables, aes(x = Rugosity, y = Sponge_Richness)) + 
  geom_point(aes(color = True_Year), size = 4) +
  labs(color="Year") +
  scale_x_continuous(name = "Rugosity") +
  scale_y_continuous(name = "Sponge Richness") +
  scale_color_gradient(low="lightblue", high="darkblue") +
  geom_smooth(size = 1.2, method = "glm.nb", formula = y ~ log(x), color = "black") +
  theme(text=element_text(size = 18), 
        panel.grid.major = element_line(colour = "light gray", size = (0.5)), 
        panel.grid.minor = element_line(colour = "light gray", size = (0.5)),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"))+
  facet_wrap(~ Site)

# Fish richness with separate site lines when x = Cover
ggplot(variables, aes(x = Percent_Coral_Cover, y = Fish_Richness)) + 
  geom_point(color = "lightblue", size = 4) +
  scale_x_continuous(name = "Coral Cover (%)") +
  scale_y_continuous(name = "Fish Richness") +
  geom_smooth(size = 1.2, method = "glm.nb", formula = y ~ log(x), color = "black") +
  theme(text=element_text(size = 18), 
        panel.grid.major = element_line(colour = "light gray", size = (0.5)), 
        panel.grid.minor = element_line(colour = "light gray", size = (0.5)),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"))+
  facet_wrap(~ Site)

# Fish richness with separate site lines when x = Rugosity
ggplot(variables, aes(x = Rugosity, y = Fish_Richness)) + 
  geom_point(color = "lightblue", size = 4) +
  scale_x_continuous(name = "Rugosity") +
  scale_y_continuous(name = "Fish Richness") +
  geom_smooth(size = 1.2, method = "glm.nb", formula = y ~ x, color = "black") +
  theme(text=element_text(size = 18), 
        panel.grid.major = element_line(colour = "light gray", size = (0.5)), 
        panel.grid.minor = element_line(colour = "light gray", size = (0.5)),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"))+
  facet_wrap(~ Site)

# Sponge+fish richness across time and with separate site lines when x = Coral Richness
ggplot(variables, aes(x = Coral_Richness, y = Sponge_and_Fish_Richness)) + 
  geom_point(aes(color = True_Year), size = 4) +
  labs(color="Year") +
  scale_x_continuous(name = "Coral Richness") +
  scale_y_continuous(name = "Sponge + Fish Richness") +
  scale_color_gradient(low="lightblue", high="darkblue") +
  geom_smooth(size = 1.2, method = "glm.nb", formula = y ~ x, color = "black") +
  theme(text=element_text(size = 18), 
        panel.grid.major = element_line(colour = "light gray", size = (0.5)), 
        panel.grid.minor = element_line(colour = "light gray", size = (0.5)),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"))+
  facet_wrap(~ Site)

# Combined richness across time and with separate site lines when x = Cover
ggplot(variables, aes(x = Percent_Coral_Cover, y = Combined_Richness)) + 
  geom_point(aes(color = True_Year), size = 4) +
  labs(color="Year") +
  scale_x_continuous(name = "Coral Cover (%)") +
  scale_y_continuous(name = "Combined Richness") +
  scale_color_gradient(low="lightblue", high="darkblue") +
  geom_smooth(size = 1.2, method = "glm.nb", formula = y ~ log(x), color = "black") +
  theme(text=element_text(size = 18), 
        panel.grid.major = element_line(colour = "light gray", size = (0.5)), 
        panel.grid.minor = element_line(colour = "light gray", size = (0.5)),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"))+
  facet_wrap(~ Site)

# Sponge+fish richness with separate site lines when x = Time
ggplot(variables, aes(x = True_Year, y = Sponge_and_Fish_Richness)) + 
  geom_point(color = "lightblue", size = 4) +
  scale_x_continuous(name = "Year") +
  scale_y_continuous(name = "Sponge + Fish Richness") +
  geom_smooth(size = 1.2, method = "glm.nb", formula = y ~ x, color = "black") +
  theme(text=element_text(size = 18), 
        panel.grid.major = element_line(colour = "light gray", size = (0.5)), 
        panel.grid.minor = element_line(colour = "light gray", size = (0.5)),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"))+
  facet_wrap(~ Site)

# Fish richness with site
ggplot(variables, aes(x = Site, y = Fish_Richness, fill = Site)) + 
  scale_fill_viridis_d()+
  geom_boxplot() +
  scale_x_discrete(name = "Site") +
  scale_y_continuous(name = "Fish Richness") +
  theme(legend.position = "none",
        text=element_text(size = 18), 
        axis.text.x=element_text(angle = +90, hjust = 0),
        panel.grid.major = element_line(colour = "light gray", size = (0.5)), 
        panel.grid.minor = element_line(colour = "light gray", size = (0.5)),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"))
#######################################################################################################################
coral_cc = glm.nb(formula = Coral_Richness ~ Percent_Coral_Cover, data = variables)
coral_sc = glm.nb(formula = Coral_Richness ~ Percent_Sponge_Cover, data = variables)
coral_r = glm.nb(formula = Coral_Richness ~ Rugosity, data = variables)
coral_cc_sc = glm.nb(formula = Coral_Richness ~ Percent_Coral_Cover + Percent_Sponge_Cover, data = variables)
coral_cc_r = glm.nb(formula = Coral_Richness ~ Percent_Coral_Cover + Rugosity, data = variables)
coral_sc_r = glm.nb(formula = Coral_Richness ~ Percent_Sponge_Cover + Rugosity, data = variables)
coral_cc_sc_r = glm.nb(formula = Coral_Richness ~ Percent_Coral_Cover + Percent_Sponge_Cover + Rugosity, data = variables)
coral_surrogate <- aictab(cand.set = list(coral_cc, coral_sc, coral_r, coral_cc_sc, coral_cc_r, coral_sc_r, coral_cc_sc_r), 
                          modnames = c("coral_cc", "coral_sc", "coral_r", "coral_cc_sc", "coral_cc_r", "coral_sc_r", "coral_cc_sc_r"), 
                          digits = 4)
sponge_cc = glm.nb(formula = Sponge_Richness ~ Percent_Coral_Cover, data = variables)
sponge_sc = glm.nb(formula = Sponge_Richness ~ Percent_Sponge_Cover, data = variables)
sponge_r = glm.nb(formula = Sponge_Richness ~ Rugosity, data = variables)
sponge_cc_sc = glm.nb(formula = Sponge_Richness ~ Percent_Coral_Cover + Percent_Sponge_Cover, data = variables)
sponge_cc_r = glm.nb(formula = Sponge_Richness ~ Percent_Coral_Cover + Rugosity, data = variables)
sponge_sc_r = glm.nb(formula = Sponge_Richness ~ Percent_Sponge_Cover + Rugosity, data = variables)
sponge_cc_sc_r = glm.nb(formula = Sponge_Richness ~ Percent_Coral_Cover + Percent_Sponge_Cover + Rugosity, data = variables)
sponge_surrogate <- aictab(cand.set = list(sponge_cc, sponge_sc, sponge_r, sponge_cc_sc, sponge_cc_r, sponge_sc_r, sponge_cc_sc_r), 
                           modnames = c("sponge_cc", "sponge_sc", "sponge_r", "sponge_cc_sc", "sponge_cc_r", "sponge_sc_r", "sponge_cc_sc_r"), 
                           digits = 4)
fish_cc = glm.nb(formula = Fish_Richness ~ Percent_Coral_Cover, data = variables)
fish_sc = glm.nb(formula = Fish_Richness ~ Percent_Sponge_Cover, data = variables)
fish_r = glm.nb(formula = Fish_Richness ~ Rugosity, data = variables)
fish_cc_sc = glm.nb(formula = Fish_Richness ~ Percent_Coral_Cover + Percent_Sponge_Cover, data = variables)
fish_cc_r = glm.nb(formula = Fish_Richness ~ Percent_Coral_Cover + Rugosity, data = variables)
fish_sc_r = glm.nb(formula = Fish_Richness ~ Percent_Sponge_Cover + Rugosity, data = variables)
fish_cc_sc_r = glm.nb(formula = Fish_Richness ~ Percent_Coral_Cover + Percent_Sponge_Cover + Rugosity, data = variables)
fish_surrogate <- aictab(cand.set = list(fish_cc, fish_sc, fish_r, fish_cc_sc, fish_cc_r, fish_sc_r, fish_cc_sc_r), 
                         modnames = c("fish_cc", "fish_sc", "fish_r", "fish_cc_sc", "fish_cc_r", "fish_sc_r", "fish_cc_sc_r"), 
                         digits = 4)
combined_cc = glm.nb(formula = Combined_Richness ~ Percent_Coral_Cover, data = variables)
combined_sc = glm.nb(formula = Combined_Richness ~ Percent_Sponge_Cover, data = variables)
combined_r = glm.nb(formula = Combined_Richness ~ Rugosity, data = variables)
combined_cc_sc = glm.nb(formula = Combined_Richness ~ Percent_Coral_Cover + Percent_Sponge_Cover, data = variables)
combined_cc_r = glm.nb(formula = Combined_Richness ~ Percent_Coral_Cover + Rugosity, data = variables)
combined_sc_r = glm.nb(formula = Combined_Richness ~ Percent_Sponge_Cover + Rugosity, data = variables)
combined_cc_sc_r = glm.nb(formula = Combined_Richness ~ Percent_Coral_Cover + Percent_Sponge_Cover + Rugosity, data = variables)
combined_surrogate <- aictab(cand.set = list(combined_cc, combined_sc, combined_r, combined_cc_sc, combined_cc_r, combined_sc_r, combined_cc_sc_r), 
                             modnames = c("combined_cc", "combined_sc", "combined_r", "combined_cc_sc", "combined_cc_r", "combined_sc_r", "combined_cc_sc_r"), 
                             digits = 4)
