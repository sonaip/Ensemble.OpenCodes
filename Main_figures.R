# Codes for generating figures in 
# the Manuscript "Ensemble perception in the time domain: 
# evidence in favor of logarithmic encoding of time intervals"
# 

# ---- Install necessary packages and import data ----
source('loadPackages.R')
dat_interval <- readRDS('rawData.rds')


# -------- Figure 1 Experimental design -----
# Illustrations for the mean distributions of tested 3 sequences
# First attempt of illustration
levels(dat_interval$conds$sequence) <- c("Set 1","Set 2","Set 3")
fig_sets <- dat_interval$conds %>% 
  ggplot(aes(levels, duration)) +
  geom_bar(stat = 'identity') + facet_wrap(~sequence,nrow = 3) + 
  xlab('Intervals') + ylab('Durations (ms)')+theme_minimal()+ 
  theme(legend.position= "null",#c(0.8,0.1),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line.y = element_line(color="black", size = 0.2),
        axis.line.x = element_line(color="black", size = 0.2),
        axis.ticks = element_line(color = "grey")
        #strip.text.x = element_blank()                       
  )

fig_design <- dat_interval$conds %>% group_by(sequence) %>% 
  summarise(Arithmetic = mean(duration), Geometric = geometric.mean(duration)) %>%
  pivot_longer(2:3, names_to = 'Coding', values_to = 'Mean') %>%
  ggplot(., aes(x = sequence, y = Mean, color = Coding,  group = Coding)) + 
  geom_point( size = 3) + geom_line() + 
  coord_cartesian(ylim = c(650,900))+ theme_classic()+
  theme(legend.position = c(0.6, 0.8)) + 
  xlab('') + ylab('Mean (ms)') 

# interval design
fig1 <- plot_grid(fig_sets,fig_design, ncol = 2, labels = c("a","b"))
fig1 # show figure
# save figure 1
ggsave('fig1.pdf', fig1, width = 7, height = 3)


# ----- Figure 2 Experimental procedure ------
# PNG file

# ----- Figure 3 --------
# UML trial sequences and parameter estimates
# matlab code: fig3_trialseq.m

# ----- Figure 4 -------
# calculate mean
m_exp1 <- dat_interval$exp1 %>% 
  group_by(condition) %>% 
  summarise(pse = mean(alpha), n = n(), se = sd(alpha)/sqrt(n-1),
            jnd = mean(beta), se_jnd = sd(beta)/sqrt(n-1))

m_exp2 <- dat_interval$exp2 %>% group_by(condition) %>% 
  summarise(pse = mean(alpha), n = n(), se = sd(alpha)/sqrt(n-1),
            jnd = mean(beta), se_jnd = sd(beta)/sqrt(n-1))


violinplot1 <- dat_interval$exp1 %>% 
  ggplot(., aes(x = condition, y = alpha))+
  geom_violin(trim=FALSE, fill = 'grey',color = 'grey') + 
  #stat_summary(fun.y = mean, geom="point")+
  geom_point(data = m_exp1, aes(x = condition, y = pse))+
  geom_errorbar(data= m_exp1,aes(y = pse,ymin = pse - se, ymax = pse + se),
                width = 0.2,color = "black") + 
  geom_jitter(shape=16,color = 'grey43',alpha = 0.3, position=position_jitter(0.15))+
  geom_line(data= m_exp1, aes(x = as.numeric(condition), y = pse), 
            color = 'black', size = 0.8)+
  theme_classic()+
  ggtitle('Auditory')+
  theme(plot.title = element_text(hjust = 0.5))+
  xlab("Set")+
  ylab('PSE (ms)') +
 # ylim(c(300,1030)) +
  geom_signif(comparisons = list(c(1, 2),c(2,3)),
              y = c(930,960),
              annotations=c("*","***"))

violinplot2 <- dat_interval$exp2 %>% filter(subject!=12) %>%
  ggplot(., aes(x = condition, y = alpha))+
  geom_violin(trim=FALSE, fill = 'grey',color = 'grey') + 
  #stat_summary(fun.y = mean, geom="point")+
  geom_point(data = m_exp2, aes(x = condition, y = pse))+
  geom_errorbar(data= m_exp2,aes(y = pse,ymin = pse - se, ymax = pse + se),
                width = 0.2,color = "black") + 
  geom_jitter(shape=16,color = 'grey43',alpha = 0.3, position=position_jitter(0.15))+
  geom_line(data= m_exp2, aes(x = as.numeric(condition), y = pse), 
            color = 'black', size = 0.8)+
  theme_classic()+
  ggtitle('Visual')+
  theme(plot.title = element_text(hjust = 0.5))+
  xlab("Set")+
  ylab('PSE (ms)') +
 # ylim(c(300,1030))+
  geom_signif(comparisons = list(c(1, 2),c(2,3)),
              y = c(930,960),
              annotations=c("*","**"))

Figure4 <- plot_grid(violinplot1,violinplot2, ncol=2,labels = c('a','b'))
Figure4
ggsave('fig4.pdf', Figure4, width = 7, height = 3)

## ---- Figure 5 ----
# Exp 1
dat_scatter <- dat_interval$exp1 %>% 
  dplyr::select(., subject, condition, alpha) %>%
  spread(condition,alpha)
dat_scatter$s12<- dat_scatter$`1`- dat_scatter$`2`
dat_scatter$s13 <- dat_scatter$`1`- dat_scatter$`3`

# mean point
dat_scatter_mean <- dat_scatter %>% 
  dplyr::select(., s12, s13, a_dis, g_dis)

# dat_scatter Amean/Gmean distance
dat_scatter$a_dis <- (dat_scatter$s12 ^2 + (dat_scatter$s13-73)^2)^(1/2)
dat_scatter$g_dis <- ((dat_scatter$s12+77) ^2 + dat_scatter$s13^2)^(1/2)
# add distance factors, for further color categorization in the scatter plot
dat_scatter$com <- factor(dat_scatter$a_dis <- dat_scatter$g_dis)

mean(dat_scatter$g_dis)
# scatter plot
fig_scatter1 <- ggplot (dat_scatter, aes(x= s13, y = s12,colour = com))+
  
  geom_vline(xintercept = 0, color = 'grey43')+
  geom_hline(yintercept = 0, color = 'grey43')+
  geom_point(size = 4, alpha = 0.3, shape = 16, color = 'grey43') +
  #add comparison points
  geom_point(aes(x=73, y=0), shape = 17,size = 3,color = 'blue')+
  annotate("text", x = 73, y = -10, label = "Arith.M.")+
  geom_point(aes(x=0, y=-77), shape = 17,size = 3, color = 'red')+
  annotate("text", x = -29, y = -77, label = "Geo.M.")+
  #add harmonic mean point
  geom_point(aes(x=-74, y=-148), shape = 17,size = 3,color = 'yellow')+
  annotate("text", x = -74, y = -158, label = "Harmonic.M.")+
  
  # add comparison boundry
  geom_abline(intercept = (-300/77), slope = (-73/77),color = 'darkgrey', linetype = 'dashed')+
  #add mean point
  geom_point(aes(x= 26.33, y =-52.16), color = 'black',size = 2)+
  geom_errorbar(aes(x= 26.33, ymin = (-52.16-17.14), ymax= (-52.16+17.14)), width = 5, size = 0.3, color = 'black')+
  geom_errorbarh(aes(y= -52.16, xmin = (26.33-21.62), xmax= (26.33+21.62)), height = 5, size = 0.3, color = 'black')+
  ggtitle('Auditory')+
  ylim(c(-175,150))+
  xlim(c(-150,250))+
  ylab('Interval Difference (Set1-Set2) (ms)')+
  xlab('Interval Difference (Set1-Set3) (ms)')+
  theme_minimal() +
  theme(legend.position= "null",
        plot.title = element_text(hjust = 0.5))

#Exp2
dat_scatter2 <- dat_interval$exp2 %>% filter(subject !=12) %>%
  dplyr::select(., subject, condition, alpha) %>%
  spread(condition,alpha)
dat_scatter2$s12<- dat_scatter2$`1`- dat_scatter2$`2`
dat_scatter2$s13 <- dat_scatter2$`1`- dat_scatter2$`3`

# mean point
dat_scatter_mean2 <- dat_scatter2 %>% 
  dplyr::select(., s12, s13, a_dis, g_dis)

# dat_scatter Amean/Gmean distance
dat_scatter2$a_dis <- (dat_scatter2$s12 ^2 + (dat_scatter2$s13-73)^2)^(1/2)
dat_scatter2$g_dis <- ((dat_scatter2$s12+77) ^2 + dat_scatter2$s13^2)^(1/2)
# add distance factors, for further color categorization in the scatter plot
dat_scatter2$com <- factor(dat_scatter2$a_dis <- dat_scatter2$g_dis)

#> [1] alpha two   three alpha two  
# statistics on distance
t.test(dat_scatter2$a_dis, dat_scatter2$g_dis)

mean(dat_scatter2$g_dis)
# scatter plot
fig_scatter2 <- ggplot (dat_scatter2, aes(x= s13, y = s12,colour = com))+
  
  geom_vline(xintercept = 0, color = 'grey43')+
  geom_hline(yintercept = 0, color = 'grey43')+
  geom_point(size = 4, alpha = 0.3, shape = 16, color = 'grey43') +
  #add comparison points
  geom_point(aes(x=73, y=0), shape = 17,size = 3,color = 'blue')+
  annotate("text", x = 73, y = -10, label = "Arith.M.")+
  geom_point(aes(x=0, y=-77), shape = 17,size = 3, color = 'red')+
  annotate("text", x = -29, y = -77, label = "Geo.M.")+
  #add harmonic mean point
  geom_point(aes(x=-74, y=-148), shape = 17,size = 3,color = 'yellow')+
  annotate("text", x = -74, y = -158, label = "Harmonic.M.")+
  
  # add comparison boundary
  geom_abline(intercept = (-300/77), slope = (-73/77),color = 'darkgrey', linetype = 'dashed')+
  #add mean point
  geom_point(aes(x= -33.80, y =-13.96), color = 'black',size = 2)+
  geom_errorbar(aes(x= -33.80, ymin = (-13.96-12.51), ymax= (-13.96+12.51)), width = 5, size = 0.3, color = 'black')+
  geom_errorbarh(aes(y= -13.96, xmin = (-33.80-11.19), xmax= (-33.80+11.19)), height = 5, size = 0.3, color = 'black')+
  ylim(c(-175,150))+
  xlim(c(-150,250))+
  ggtitle('Visual')+
  ylab('Interval Difference (Set1-Set2) (ms)')+
  xlab('Interval Difference (Set1-Set3) (ms)')+
  theme_minimal() +
  theme(legend.position= "null",
        plot.title = element_text(hjust = 0.5))

Figure5 <- plot_grid(fig_scatter1,NULL,fig_scatter2,ncol = 3,rel_widths = c(1,0.1,1), labels = c('a','b'))
Figure5
ggsave('fig5.pdf', Figure5, width = 7, height = 3)


## ---- Figure 6 ----
# estimated sensitivity from beta- the slope to the estimated psychometric function
# jnd = ln3/beta *1000 
beta_exp1 <- dat_interval$exp1 %>% 
  group_by(condition) %>% 
  summarise(pse = mean(log(3)/beta)*1000, n = n(), se = sd(log(3)/beta)*1000/sqrt(n-1))
beta_exp2 <- dat_interval$exp2 %>% group_by(condition) %>% 
  summarise(pse = mean(log(3)/beta*1000), n = n(), se = sd(log(3)/beta*1000)/sqrt(n-1))

violinplot1_jnd <- dat_interval$exp1 %>% filter(subject!=8) %>%
  ggplot(., aes(x = condition, y = log(3)/beta*1000))+
  geom_violin(trim=FALSE, fill = 'grey',color = 'grey') + 
  #stat_summary(fun.y = mean, geom="point")+
  geom_point(data = beta_exp1, aes(x = condition, y = pse))+
  geom_errorbar(data= beta_exp1,aes(y = pse,ymin = pse - se, ymax = pse + se),
                width = 0.2,color = "black") + 
  geom_jitter(shape=16,color = 'grey43',alpha = 0.3, position=position_jitter(0.15))+
  geom_line(data= beta_exp1, aes(x = as.numeric(condition), y = pse), 
            color = 'black', size = 0.8)+
  theme_classic()+
  ggtitle('Auditory')+
  theme(plot.title = element_text(hjust = 0.5))+
  xlab("Set")+
  ylab('JND (ms)') +
  ylim(c(0,500)) 

violinplot2_jnd <- dat_interval$exp2 %>% filter(subject!=12) %>%
  ggplot(., aes(x = condition, y = log(3)/beta*1000))+
  geom_violin(trim=FALSE, fill = 'grey',color = 'grey') + 
  #stat_summary(fun.y = mean, geom="point")+
  geom_point(data = beta_exp2, aes(x = condition, y = pse))+
  geom_errorbar(data= beta_exp2,aes(y = pse,ymin = pse - se, ymax = pse + se),
                width = 0.2,color = "black") + 
  geom_jitter(shape=16,color = 'grey43',alpha = 0.3, position=position_jitter(0.15))+
  geom_line(data= beta_exp2, aes(x = as.numeric(condition), y = pse), 
            color = 'black', size = 0.8)+
  theme_classic()+
  ggtitle('Visual')+
  ylim(c(0,500)) +
  
  theme(plot.title = element_text(hjust = 0.5))+
  xlab("Set")+
  ylab('JND (ms)')

Figure6 <- plot_grid(violinplot1_jnd,violinplot2_jnd, ncol=2,labels = c('a','b'))
Figure6
ggsave('fig6.pdf', Figure6, width = 7, height = 3)

# ----- Figure 7 -----
# Figure 7 is generated in model_simulation.R 

