install.packages("Synth")
library("Synth")
install.packages("reporttools")
library(reporttools)
install.packages("xtable")
library(xtable)
install.packages("dplyr")
library(dplyr)
install.packages("tidyr")
library(tidyr)
install.packages("foreign")
library(foreign)
install.packages("ggplot2")
library(ggplot2)



GMU <- read.csv("/Users/RekaFujii4/Desktop/Replication Presentation/cleaned_data.csv")
str(GMU)
names(GMU)
summary(GMU)
View(GMU)


GMU_avg_predictors <- GMU %>%
  filter(
    UNITID == 232186,
    year %in% 2004:2006
  ) %>%
  summarise(
    First_Year_Cohort = mean(firstyearclass),
    Total_Price = mean(price),
    Appropriations_per_FTE = mean(appropriations.per.FTE),
    Average_SAT = mean(SAT),
    Percent_Admitted = mean(admitted),
    Percent_Yield = mean(yield),
    Average_Institutional_Grant = mean(avg..instgrant),
    Percent_UR_Minority = mean(perums)
  )

print(GMU_avg_predictors)


Donors_34_avg_predictors <- GMU %>%
  filter(
    UNITID != 232186,
    year %in% 2004:2006
  ) %>%
  
  # Remove rows with NA in any of the eight predictors
  drop_na(
    firstyearclass,
    price,
    appropriations.per.FTE,
    SAT,
    admitted,
    yield,
    avg..instgrant,
    perums
  ) %>%
  
  summarise(
    First_Year_Cohort = mean(firstyearclass),
    Total_Price = mean(price),
    Appropriations_per_FTE = mean(appropriations.per.FTE),
    Average_SAT = mean(SAT),
    Percent_Admitted = mean(admitted),
    Percent_Yield = mean(yield),
    Average_Institutional_Grant = mean(avg..instgrant),
    Percent_UR_Minority = mean(perums)
  )

print(Donors_34_avg_predictors)


#adds number representation for each state
GMU$number <- as.numeric(as.factor(GMU$institution))
GMU$institution <- as.character(GMU$institution)

#to find missing numbers
#install.packages("reporttools")
#attach connects dataset variable name to the dataset (rather than having to list it each time)


####################################################################
######## Pell Enrollment
dataprep.out <- dataprep(
  foo = GMU,
  
  predictors = c(
    "price",
    "avg..instgrant",
    "admitted",
    "yield",
    "SAT",
    "firstyearclass",
    "perums"
  ),
  
  predictors.op = "mean",
  
  time.predictors.prior = 2004:2006,
  
  special.predictors = list(
    list("appropriations.per.FTE", 2004:2006, "mean")
  ),
  
  dependent = "pell",
  
  unit.variable = "UNITID",
  unit.names.variable = "institution",
  time.variable = "year",
  
  treatment.identifier = 232186,
  
  controls.identifier = setdiff(
    unique(GMU$UNITID), 232186
  ),
  
  time.optimize.ssr = 2004:2014,
  time.plot = 2004:2014
)

ls()

synth.out <- synth(
  data.prep.obj = dataprep.out,
  optimxmethod = "BFGS"
)

class(synth.out)
names(synth.out)
str(synth.out)

synth.tables <- synth.tab(
  dataprep.res = dataprep.out,
  synth.res = synth.out
)

synth.tables


 
#########################figure in R
scm_ggplot <- function(dataprep.out,
                       synth.out,
                       actual_name = "George Mason University",
                       synthetic_name = "Synthetic GMU",
                       ylab,
                       percentage = TRUE) {
  # Extract actual and synthetic outcomes
  Ydata <- data.frame(
    year = as.numeric(rownames(dataprep.out$Y1plot)),
    Actual = as.numeric(dataprep.out$Y1plot),
    Synthetic = as.numeric(
      dataprep.out$Y0plot %*% synth.out$solution.w
    )
  )
  
  # Convert to long format
  Ydata_long <- Ydata %>%
    pivot_longer(
      cols = c(Actual, Synthetic),
      names_to = "Institution",
      values_to = "Outcome"
    )
  
  # Create plot
  p <- ggplot(
    Ydata_long,
    aes(x = year, y = Outcome,
        color = Institution,
        linetype = Institution)
  ) +
    geom_line(linewidth = 1) +
    
    # Policy implementation
    geom_vline(
      xintercept = 2007,
      color = "darkgreen",
      linetype = "dotted",
      linewidth = 0.8
    ) +
    
    scale_color_manual(
      values = c(
        "Actual" = "magenta4",
        "Synthetic" = "gray40"
      ),
      breaks = c("Actual", "Synthetic"),
      labels = c(actual_name, synthetic_name)
    ) +
    
    scale_linetype_manual(
      values = c(
        "Actual" = "solid",
        "Synthetic" = "dashed"
      ),
      breaks = c("Actual", "Synthetic"),
      labels = c(actual_name, synthetic_name)
    ) +
    
    scale_x_continuous(
      breaks = seq(
        min(Ydata$year),
        max(Ydata$year),
        by = 1
      )
    ) +
    
    labs(
      x = "Year",
      y = ylab,
      color = NULL,
      linetype = NULL
    ) +
    
    theme_classic(base_size = 13) +
    
    theme(
      text = element_text(family = "serif"),
      legend.position = "top",
      legend.background = element_rect(
        fill = "white",
        color = "black"
      ),
      legend.key.width = grid::unit(1.2, "cm"),
      axis.text = element_text(color = "black"),
      panel.border = element_rect(
        color = "black",
        fill = NA,
        linewidth = 0.5
      )
    )
  
  if (percentage) {
    p <- p +
      scale_y_continuous(
        breaks = seq(0, 0.40, by = 0.05),
        labels = scales::label_percent(accuracy = 1)
      ) +
      coord_cartesian(ylim = c(0, 0.40))
  } else {
    p <- p +
      scale_y_continuous(
        labels = scales::label_dollar()
      )
  }
  
  return(p)
}
p_pell <- scm_ggplot(
  dataprep.out,
  synth.out,
  ylab = "% Pell Students Enrolled"
)

print(p_pell)

######## Pell Placebo Test
 dataprep.out <-
 dataprep(foo = GMU,
          predictors = c("price", "avg..instgrant", "admitted", "yield", "SAT", "firstyearclass", "perums") ,
          predictors.op = "mean" ,
          time.predictors.prior = 2004:2006 ,
          special.predictors = list(
            #list("pell" , 2004:2006 , "mean"), 
            list("appropriations.per.FTE" , 2004:2006, "mean")),
          dependent = "pell",
  #################################################        
          unit.variable = "UNITID",
          unit.names.variable = "institution",
          time.variable = "year",
          
          treatment.identifier = 217484,
          
          controls.identifier = setdiff(
            unique(GMU$UNITID), c(217484, 232186)
          ),
     ################################################     
          time.optimize.ssr = 2004:2014,
          time.plot = 2004:2014)
ls()	

synth.out <- synth(data.prep.obj = dataprep.out,
                    optimxmethod = "BFGS")
class(synth.out)
names(synth.out)
str(synth.out)	

pdf("/Users/RekaFujii4/Desktop/Replication files/Figures_2/pellplacebo1.pdf",width=7.5,height=5, bg="white")
path.plot(synth.res = synth.out,
           dataprep.res = dataprep.out,
           Ylab = "% Pell Students Enrolled",
           Xlab = "Year",
           Ylim = c(0,.4), 
          abline (v=2007,col=3,lty=1),
           Legend = c("University of South Carolina","Synthetic USC"),
           Legend.position = "topright"
           )

dev.off()	

###################Figure R
scm_ggplot <- function(dataprep.out,
                       synth.out,
                       actual_name = "University of Rhode Island",
                       synthetic_name = "Synthetic URI",
                       ylab,
                       percentage = TRUE) {
  # Extract actual and synthetic outcomes
  Ydata <- data.frame(
    year = as.numeric(rownames(dataprep.out$Y1plot)),
    Actual = as.numeric(dataprep.out$Y1plot),
    Synthetic = as.numeric(
      dataprep.out$Y0plot %*% synth.out$solution.w
    )
  )
  
  # Convert to long format
  Ydata_long <- Ydata %>%
    pivot_longer(
      cols = c(Actual, Synthetic),
      names_to = "Institution",
      values_to = "Outcome"
    )
  
  # Create plot
  p <- ggplot(
    Ydata_long,
    aes(x = year, y = Outcome,
        color = Institution,
        linetype = Institution)
  ) +
    geom_line(linewidth = 1) +
    
    # Policy implementation
    geom_vline(
      xintercept = 2007,
      color = "darkgreen",
      linetype = "dotted",
      linewidth = 0.8
    ) +
    
    scale_color_manual(
      values = c(
        "Actual" = "magenta4",
        "Synthetic" = "gray40"
      ),
      breaks = c("Actual", "Synthetic"),
      labels = c(actual_name, synthetic_name)
    ) +
    
    scale_linetype_manual(
      values = c(
        "Actual" = "solid",
        "Synthetic" = "dashed"
      ),
      breaks = c("Actual", "Synthetic"),
      labels = c(actual_name, synthetic_name)
    ) +
    
    scale_x_continuous(
      breaks = seq(
        min(Ydata$year),
        max(Ydata$year),
        by = 1
      )
    ) +
    
    labs(
      x = "Year",
      y = ylab,
      color = NULL,
      linetype = NULL
    ) +
    
    theme_classic(base_size = 13) +
    
    theme(
      text = element_text(family = "serif"),
      legend.position = "top",
      legend.background = element_rect(
        fill = "white",
        color = "black"
      ),
      legend.key.width = grid::unit(1.2, "cm"),
      axis.text = element_text(color = "black"),
      panel.border = element_rect(
        color = "black",
        fill = NA,
        linewidth = 0.5
      )
    )
  
  if (percentage) {
    p <- p +
      scale_y_continuous(
        breaks = seq(0, 0.40, by = 0.05),
        labels = scales::label_percent(accuracy = 1)
      ) +
      coord_cartesian(ylim = c(0, 0.40))
  } else {
    p <- p +
      scale_y_continuous(
        labels = scales::label_dollar()
      )
  }
  
  return(p)
}
p_pell_placebo <- scm_ggplot(
  dataprep.out,
  synth.out,
  ylab = "% Pell Students Enrolled"
)

print(p_pell_placebo)




####################################################################
######## PerUms Enrollment
dataprep.out <-
 dataprep(foo = GMU,
          predictors = c("price", "avg..instgrant", "admitted", "yield", "SAT", "firstyearclass") ,
          predictors.op = "mean" ,
          time.predictors.prior = 2004:2006 ,
          special.predictors = list(
            list("pell" , 2004:2006 , "mean"), 
            list("appropriations.per.FTE" , 2004:2006, "mean")),
          dependent = "perums",
          #######################
          unit.variable = "UNITID",
          unit.names.variable = "institution",
          time.variable = "year",
          
          treatment.identifier = 232186,
          
          controls.identifier = setdiff(
            unique(GMU$UNITID), 232186
          ),
          
          ########################
          time.optimize.ssr = c(2004:2014),
          time.plot = c(2004:2014))
ls()	

synth.out <- synth(data.prep.obj = dataprep.out,
                    optimxmethod = "BFGS")
class(synth.out)
names(synth.out)
str(synth.out)	

synth.tables <- synth.tab(dataprep.res = dataprep.out,
                           synth.res = synth.out
                           )
synth.tables




######## Figure
pdf("/Users/RekaFujii4/Desktop/Replication files/Figures_2/perums.pdf",width=7.5,height=5, bg="white")
path.plot(synth.res = synth.out,
           dataprep.res = dataprep.out,
           Ylab = "% Underrepresented Students Enrolled",
           Xlab = "Year",
           #Ylim = c(0,12), (remove Ylim b/c it is too narrow)
          abline (v=2007,col=3,lty=1),
           Legend = c("George Mason University","Synthetic GMU"),
           Legend.position = "topright"
           )

dev.off()	

######## PerUms Placebo Test
dataprep.out <-
 dataprep(foo = GMU,
          predictors = c("price", "avg..instgrant", "admitted", "yield", "SAT", "firstyearclass") ,
          predictors.op = "mean" ,
          time.predictors.prior = 2004:2006 ,
          special.predictors = list(
            list("pell" , 2004:2006 , "mean"), 
            list("appropriations.per.FTE" , 2004:2006, "mean")),
          dependent = "perums",
          #################################################        
          unit.variable = "UNITID",
          unit.names.variable = "institution",
          time.variable = "year",
          
          treatment.identifier = 155317 ,
          
          controls.identifier = setdiff(
            unique(GMU$UNITID), c(232186, 155317)
          ),
          ################################################ ,
          time.optimize.ssr = c(2004:2014),
          time.plot = c(2004:2014))
ls()	

synth.out <- synth(data.prep.obj = dataprep.out,
                    optimxmethod = "BFGS")
class(synth.out)
names(synth.out)
str(synth.out)	
View(GMU)
pdf("/Users/RekaFujii4/Desktop/Replication files/Figures_2/perumsplacebo.pdf",width=7.5,height=5, bg="white")
path.plot(synth.res = synth.out,
           dataprep.res = dataprep.out,
           Ylab = "% Underrepresented Students Enrolled",
           Xlab = "Year",
           #Ylim = c(0,12), 
          abline (v=2007,col=3,lty=1),
           Legend = c("Western Michigan University","Synthetic WMU"),
           Legend.position = "topright"
           )

dev.off()	

####################################################################
######## PerBlack Enrollment
 dataprep.out <-
 dataprep(foo = GMU,
          predictors = c("price", "avg..instgrant", "admitted", "yield", "SAT", "firstyearclass") ,
          predictors.op = "mean" ,
          time.predictors.prior = 2004:2006 ,
          special.predictors = list(
            list("pell" , 2004:2006 , "mean"), 
            list("appropriations.per.FTE" , 2004:2006, "mean")),
          dependent = "perblack",
          #######################
          unit.variable = "UNITID",
          unit.names.variable = "institution",
          time.variable = "year",
          
          treatment.identifier = 232186,
          
          controls.identifier = setdiff(
            unique(GMU$UNITID), 232186
          ),
          
          ########################,
          time.optimize.ssr = c(2004:2014),
          time.plot = c(2004:2014))
ls()	

synth.out <- synth(data.prep.obj = dataprep.out,
                    optimxmethod = "BFGS")
class(synth.out)
names(synth.out)
str(synth.out)	

synth.tables <- synth.tab(dataprep.res = dataprep.out,
                           synth.res = synth.out
                           )
synth.tables

######## Figure
pdf("/Users/RekaFujii4/Desktop/Replication files/Figures_2/perblack.pdf",width=7.5,height=5, bg="white")
path.plot(synth.res = synth.out,
           dataprep.res = dataprep.out,
           Ylab = "% Black Students Enrolled",
           Xlab = "Year",
           #Ylim = c(0,12), (remove Ylim b/c it is too narrow)
          abline (v=2007,col=3,lty=1),
           Legend = c("George Mason University","Synthetic GMU"),
           Legend.position = "topright"
           )

dev.off()	

######## PerBlack Placebo Test
 dataprep.out <-
 dataprep(foo = GMU,
          predictors = c("price", "avg..instgrant", "admitted", "yield", "SAT", "firstyearclass") ,
          predictors.op = "mean" ,
          time.predictors.prior = 2004:2006 ,
          special.predictors = list(
            list("pell" , 2004:2006 , "mean"), 
            list("appropriations.per.FTE" , 2004:2006, "mean")),
          dependent = "perblack",
          #################################################        
          unit.variable = "UNITID",
          unit.names.variable = "institution",
          time.variable = "year",
          
          treatment.identifier = 155317,
          
          controls.identifier = setdiff(
            unique(GMU$UNITID), c(232186, 155317)
          ),
          ################################################ ,,
          time.optimize.ssr = c(2004:2007,2010:2014),
          time.plot = c(2004:2007,2010:2014))
ls()	

synth.out <- synth(data.prep.obj = dataprep.out,
                    optimxmethod = "BFGS")
class(synth.out)
names(synth.out)
str(synth.out)	

pdf("/Users/RekaFujii4/Desktop/Replication files/Figures_2/perblackplacebo.pdf",width=7.5,height=5, bg="white")
path.plot(synth.res = synth.out,
           dataprep.res = dataprep.out,
           Ylab = "% Black Students Enrolled",
           Xlab = "Year",
           #Ylim = c(0,12), (remove Ylim b/c it is too narrow)
          abline (v=2007,col=3,lty=1),
           Legend = c("University of Rhode Island","Synthetic URI"),
           Legend.position = "topright"
           )

dev.off()	


####################################################################
######## PerHisp Enrollment
 dataprep.out <-
 dataprep(foo = GMU,
          predictors = c("price", "avg..instgrant", "admitted", "yield", "SAT", "firstyearclass") ,
          predictors.op = "mean" ,
          time.predictors.prior = 2004:2006 ,
          special.predictors = list(
            list("pell" , 2004:2006 , "mean"), 
            list("appropriations.per.FTE" , 2004:2006, "mean")),
          dependent = "perhisp",
          #######################
          unit.variable = "UNITID",
          unit.names.variable = "institution",
          time.variable = "year",
          
          treatment.identifier = 232186,
          
          controls.identifier = setdiff(
            unique(GMU$UNITID), 232186
          ),
          
          ########################
          time.optimize.ssr = c(2004:2014),
          time.plot = c(2004:2014))
ls()	

synth.out <- synth(data.prep.obj = dataprep.out,
                    optimxmethod = "BFGS")
class(synth.out)
names(synth.out)
str(synth.out)	

synth.tables <- synth.tab(dataprep.res = dataprep.out,
                           synth.res = synth.out
                           )
synth.tables

######## Figure
pdf("/Users/RekaFujii4/Desktop/Replication files/Figures_2/perhisp.pdf",width=7.5,height=5, bg="white")
path.plot(synth.res = synth.out,
           dataprep.res = dataprep.out,
           Ylab = "% Hispanic Students Enrolled",
           Xlab = "Year",
           #Ylim = c(0,12), (remove Ylim b/c it is too narrow)
          abline (v=2007,col=3,lty=1),
           Legend = c("George Mason University","Synthetic GMU"),
           Legend.position = "topright"
           )

dev.off()	

######## PerHisp Placebo Test
 dataprep.out <-
 dataprep(foo = GMU,
          predictors = c("price", "avg..instgrant", "admitted", "yield", "SAT", "firstyearclass") ,
          predictors.op = "mean" ,
          time.predictors.prior = 2004:2006 ,
          special.predictors = list(
            list("pell" , 2004:2006 , "mean"), 
            list("appropriations.per.FTE" , 2004:2006, "mean")),
          dependent = "perhisp",
      
          #################################################        
          unit.variable = "UNITID",
          unit.names.variable = "institution",
          time.variable = "year",
          
          treatment.identifier = 217484,
          
          controls.identifier = setdiff(
            unique(GMU$UNITID), c(232186, 217484)
          ),
          
          ###################################################
          
          
          time.optimize.ssr = c(2004:2014),
          time.plot =c(2004:2014))
ls()	

synth.out <- synth(data.prep.obj = dataprep.out,
                    optimxmethod = "BFGS")
class(synth.out)
names(synth.out)
str(synth.out)	

pdf("/Users/RekaFujii4/Desktop/Replication files/Figures_2/perhispplacebo.pdf",width=7.5,height=5, bg="white")
path.plot(synth.res = synth.out,
           dataprep.res = dataprep.out,
           Ylab = "% Hispanic Students Enrolled",
           Xlab = "Year",
           #Ylim = c(0,12), (remove Ylim b/c it is too narrow)
          abline (v=2007,col=3,lty=1),
           Legend = c("University of Connecticut","Synthetic UConn"),
           Legend.position = "topright"
           )

dev.off()

####################################################################
######## Institutional Grant Aid per FTE
 dataprep.out <-
 dataprep(foo = GMU,
          predictors = c("price", "admitted", "yield", "SAT", "firstyearclass", "perums") ,
          predictors.op = "mean" ,
          time.predictors.prior = 2004:2006 ,
          special.predictors = list(
            list("pell" , 2004:2006 , "mean"),  
            list("appropriations.per.FTE" , 2004:2006, "mean")),
          dependent = "avg..instgrant",
          #######################
          unit.variable = "UNITID",
          unit.names.variable = "institution",
          time.variable = "year",
          
          treatment.identifier = 232186,
          
          controls.identifier = setdiff(
            unique(GMU$UNITID), 232186
          ),
          
          ########################,
          time.optimize.ssr = c(2004:2008,2010:2014),
          time.plot = c(2007:2008,2010:2014))
ls()	

synth.out <- synth(data.prep.obj = dataprep.out,
                    optimxmethod = "BFGS")
class(synth.out)
names(synth.out)
str(synth.out)	

synth.tables <- synth.tab(dataprep.res = dataprep.out,
                           synth.res = synth.out
                           )
synth.tables

######## Figure
pdf("/Users/RekaFujii4/Desktop/Replication files/Figures_2/grant.pdf",width=7.5,height=5, bg="white")
path.plot(synth.res = synth.out,
           dataprep.res = dataprep.out,
           Ylab = "Avg. Institutional Grant per FTE Student",
           Xlab = "Year",
           #Ylim = c(0,12), (remove Ylim b/c it is too narrow)
          abline (v=2007,col=3,lty=1),
           Legend = c("George Mason University","Synthetic GMU"),
           Legend.position = "topright"
           )

dev.off()	

######## Institutional Grant Aid per FTE Placebo Test
dataprep.out <-
 dataprep(foo = GMU,
          predictors = c("price", "admitted", "yield", "SAT", "firstyearclass", "perums") ,
          predictors.op = "mean" ,
          time.predictors.prior = 2004:2006 ,
          special.predictors = list(
            list("pell" , 2004:2006 , "mean"),  
            list("appropriations.per.FTE" , 2004:2006, "mean")),
          dependent = "avg..instgrant",
          
          #################################################        
          unit.variable = "UNITID",
          unit.names.variable = "institution",
          time.variable = "year",
          
          treatment.identifier = 157289,
          
          controls.identifier = setdiff(
            unique(GMU$UNITID), c(232186, 157289)
          ),
          
          ###################################################

          time.optimize.ssr = c(2004:2008, 2010:2014),
          time.plot = c(2004:2008, 2010:2014))
ls()		

synth.out <- synth(data.prep.obj = dataprep.out,
                    optimxmethod = "BFGS")
class(synth.out)
names(synth.out)
str(synth.out)	

pdf("/Users/RekaFujii4/Desktop/Replication files/Figures_2/grantplacebo.pdf",width=7.5,height=5, bg="white")
path.plot(synth.res = synth.out,
           dataprep.res = dataprep.out,
           Ylab = "Avg. Institutional Grant per FTE Student",
           Xlab = "Year",
           #Ylim = c(0,12), (remove Ylim b/c it is too narrow)
          abline (v=2007,col=3,lty=1),
           Legend = c("University of Louisville","Synthetic ULouisville"),
           Legend.position = "topright"
           )

dev.off()	


############################################# Significance Testing
dim(dataprep.out$Y0plot)
dim(synth.out$solution.w )


Ydata <- cbind(dataprep.out$Y1plot, (dataprep.out$Y0plot %*% synth.out$solution.w))
Ydata <- as.data.frame(Ydata)
Ydata
#outcomes for Tennessee and synth Tennessee by year

class(Ydata)
Ydata$year <- rownames(Ydata)
Ydata
colnames(Ydata)[1:2] <- c("Actual", "Synthetic")
Ydata

# just need to change pre and post years from here on
# below computes the difference in means for pre implementation -- stored as diff.observed
# Time zero
diff.observed = mean(Ydata$Actual[Ydata$year<=2006]) - mean(Ydata$Synthetic[Ydata$year<=2006], na.rm = TRUE)
diff.observed


number_of_permutations = 50000
diff.random = NULL
for (i in 1 : number_of_permutations) {
# Sample from the combined dataset without replacement
shuffled = sample (c(Ydata$Actual[Ydata$year<=2006], Ydata$Synthetic[Ydata$year<=2006]), length(c(Ydata$Actual[Ydata$year<=2006], Ydata$Synthetic[Ydata$year<=2006])))
a.random = shuffled[1 : length(Ydata$Actual[Ydata$year<=2006])]
b.random = shuffled[(length(Ydata$Actual[Ydata$year<=2006]) + 1) : length(c(Ydata$Actual[Ydata$year<=2006], Ydata$Synthetic[Ydata$year<=2006]))]
diff.random[i] = mean(b.random) - mean(a.random)
}

NumberTimesDiffhappenbychange <- length(diff.random[abs(diff.random)>=abs(diff.observed)]) 

NumberTimesDiffhappenbychange

## example: average differnce pre treatment = -0.06
## the number of times this was observed randomly: 49297 out of 50K : prob is 98.5% (calulated below)
## so the result happened by random change, no structure, they are not different
## this is what we expected to see--no difference before policy implementation 

pvalue = length(diff.random[abs(diff.random)>=abs(diff.observed)]) / number_of_permutations
print (pvalue)#Pre 
diff.observed#Pre md

smd_pre <- (mean(Ydata$Actual[Ydata$year<=2006]) - mean(Ydata$Synthetic[Ydata$year<=2006], na.rm = TRUE))/(sd(c((Ydata$Actual[Ydata$year<=2006]), (Ydata$Synthetic[Ydata$year<=2006])))) #pre smd
smd_pre#pre smd


mean(diff.random)

hist(diff.random)
abline(v=diff.observed,col="red")

t.test(Ydata$Actual[Ydata$year<=2006], Ydata$Synthetic[Ydata$year<=2006])

## difference between treated and control groups post-treatment:
# Time one--only need to change years from here
diff.observed = mean(Ydata$Actual[Ydata$year>2008]) - mean(Ydata$Synthetic[Ydata$year>2008], na.rm = TRUE)
diff.observed
## -20.19 on average, there is a difference of 20 between synth cali and cali
number_of_permutations = 10000
diff.random = NULL
for (i in 1 : number_of_permutations) {
# Sample from the combined dataset without replacement
shuffled = sample (c(Ydata$Actual[Ydata$year>2008], Ydata$Synthetic[Ydata$year>2008]), length(c(Ydata$Actual[Ydata$year>2008], Ydata$Synthetic[Ydata$year>2008])))
a.random = shuffled[1 : length(Ydata$Actual[Ydata$year>2008])]
b.random = shuffled[(length(Ydata$Actual[Ydata$year>2008]) + 1) : length(c(Ydata$Actual[Ydata$year>2008], Ydata$Synthetic[Ydata$year>2008]))]
diff.random[i] = mean(b.random) - mean(a.random)
}

NumberTimesDiffhappenbychange <- length(diff.random[abs(diff.random)>=abs(diff.observed)]) 

NumberTimesDiffhappenbychange

pvalue = length(diff.random[abs(diff.random)>=abs(diff.observed)]) / number_of_permutations
print (pvalue)#Post p-value - number of times diff.random was greater magnitude than observed
diff.observed#Post mean difference (observed and synthetics)

mean(diff.random)

hist(diff.random)
abline(v=diff.observed,col="red")
## if p-value is significant, then this difference observed is based on structure, not on random chance

# Assumes normality
t.test(Ydata$Actual[Ydata$year>2008], Ydata$Synthetic[Ydata$year>2008])
#non-normally distributed or ordinal variables
wilcox.test(Ydata$Actual[Ydata$year>2008], Ydata$Synthetic[Ydata$year>2008])

#standardized difference
smd_post <- (mean(Ydata$Actual[Ydata$year>2008]) - mean(Ydata$Synthetic[Ydata$year>2008], na.rm = TRUE))/(sd(c((Ydata$Actual[Ydata$year>2008]), (Ydata$Synthetic[Ydata$year>2008]))))

smd_post 
