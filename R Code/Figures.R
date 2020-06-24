## Figures and Tables ##
## corresponds to figures in report
## data from "VDem.R", "Asian_Barometer.R", and "Pandem.R"

## load packages ##
library(wesanderson)
library(RColorBrewer)
library(extrafont)
library(tidyverse)

#### System-level Figures ####

## Regime types over time ##
regime_type_plot <- ggplot(regime_counts, aes(x = year, y = n, colour=v2x_regime_f)) + 
  theme_minimal() +
  scale_y_continuous(minor_breaks = seq(0,10,1), breaks = seq(0,10,1)) +
  scale_x_continuous(breaks=c(1989,1994,1999,2004,2009,2014,2019), minor_breaks=seq(1989,2019,1), limits=c(1989,2019), expand=c(0,0) ) +
  geom_line(size=1) +
  scale_color_manual(values=wes_palette(n=4, name="IsleofDogs1")) +
  labs(title="Figure 2.1 - Regime Types Since Thrid Wave of Democracy", x="", y="counts",
       caption="Source: Varieties of Democracy, Ver. 10",
       subtitle="Select Asia Pacific Countries, 1989-2019") +
  guides(colour = guide_legend(reverse=F, title="")) +
  theme(text=element_text(family="Times New Roman"),
        legend.position="right",
        axis.ticks.x = element_line(),
        axis.ticks.y = element_line(),
        axis.title.x = element_text(size=10),
        axis.text.y = element_text(size=10),
        axis.text.x.bottom=element_text(size=10),
        strip.text=element_text(size=10),
        plot.title=element_text(size=12, face="bold.italic"),
        plot.caption=element_text(size=10, face="italic"),
        legend.text=element_text(size=10))

ggsave("fig1.1 regime_type_plot.pdf", regime_type_plot,
       device=cairo_pdf, width = 7, height=5)

## Civil society and libdem over time
libdem_civil_year_plot <- ggplot(libdem_civil_year, aes(x = year, y = value, colour=variable)) + 
  geom_line(size=1) +
  theme_minimal() +
  scale_y_continuous(breaks=c(.2,.3,.4,.5,.6,.7), limits=c(.2,.7) ) +
  scale_x_continuous(breaks=c(1989,1994,1999,2004,2009,2014,2019),  minor_breaks=seq(1989,2019,1), limits=c(1989,2019) ) +
  scale_colour_manual(name = element_blank(),
                      values=wes_palette(n=4, name="GrandBudapest1"),
                      labels = c("Liberal Democracy Index", "Core Civil Society Index"),
                      guide = guide_legend(reverse = TRUE)) + 
  labs(title="Figure 2.2 - Extent of Liberal Democracy and Robust Civil Society", x="", y="index scores",
       caption="Source: Varieties of Democracy, Ver. 10",
       subtitle="Select Asia Pacific Countries, 1989-2019") +
  annotate("text", x=2016, y=.70, label="Cor.=0.82") +
  theme(text=element_text(family="Times New Roman"),
        legend.position="top",
        axis.ticks.x = element_line(),
        axis.ticks.y = element_line(),
        axis.title.x = element_text(size=10),
        axis.text.y = element_text(size=10),
        axis.text.x.bottom=element_text(size=10),
        strip.text=element_text(size=10),
        plot.title=element_text(size=12, face="bold.italic"),
        plot.caption=element_text(size=10, face="italic"),
        legend.text=element_text(size=10))

ggsave("fig1.2 libdem_civil_year_plot.pdf", libdem_civil_year_plot,
       device=cairo_pdf, width = 7, height=5)

## Libdem x 3 years ##
libdem_year3_plot <- ggplot(libdem_year3, aes(fill=year, x=reorder(country_name, -value2019), y=v2x_libdem)) +
  geom_bar(position="dodge", stat="identity") +
  #geom_errorbar(position=move_it, aes(ymin=v2x_libdem_codelow, ymax=v2x_libdem_codehigh), width=.4, colour="grey") +
  coord_flip() +
  labs(title="Figure 2.3 - Liberal Democracy Scores, 1989-2019", x="", y="liberal democracy index\n<< less liberal democratic, more liberal democratic >>",
       caption="Source: Varieties of Democracy, Ver. 10",
       subtitle="Select Asia Pacific Countries, by 2019 Regime Type") +
  scale_y_continuous(breaks=c(0,.25,.50,.75,1), limits=c(0,1),expand = c(0, 0) ) +
  scale_x_discrete(expand = c(0, 0)) +
  theme_light() +
  scale_fill_manual(values=wes_palette(n=4, name="IsleofDogs2")) +
  facet_grid(v2x_regime_f~., switch = "y", scales = "free_y", space = "free_y") +
  guides(fill = guide_legend(reverse=T, title="")) +
  theme(text=element_text(family="Times New Roman"),
        legend.position="right",
        axis.ticks.x = element_line(),
        axis.ticks.y = element_line(),
        axis.title.x = element_text(size=10),
        axis.text.y = element_text(size=10),
        axis.text.x.bottom=element_text(size=10),
        strip.text=element_text(size=8),
        plot.title=element_text(size=12, face="bold.italic"),
        plot.caption=element_text(size=10, face="italic"),
        legend.text=element_text(size=10))

ggsave("fig1.3 libdem_year3_plot.pdf", libdem_year3_plot,
       device=cairo_pdf, width = 7, height=7)

## libdem change x country over time ##
libdem_change_plot <- ggplot(libdem_change, aes(fill=demobi, x=reorder(country_name, libdem_diff30 ), y=libdem_diff30 )) +
  geom_bar(position="dodge", stat="identity", colour="white") +
  labs(title="Figure 2.4 - Difference in Average Liberal Democracy Scores", x="",
       y="liberal democracy score average (2005-2019) minus average (1989-2004)\n<< less liberal democratic, more liberal democratic >>",
       caption="Source: Varieties of Democracy, Ver. 10",
       subtitle="Select Asia Pacific Countries, 1989-2019") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust=1),
        axis.ticks.x = element_line(),
        axis.ticks.y = element_line()) +
  scale_x_discrete(expand = c(0, 0)) +
  geom_abline(slope=0, intercept=0, colour = "black", lty=1) +
  scale_fill_manual(values=wes_palette(n=2, name="IsleofDogs2")) +
  guides(fill = guide_legend(reverse=F, title="Liberal/Electoral Democracy in 2004?")) +
  theme(legend.position = "top",
        text=element_text(family="Times New Roman"),
        strip.text.x = element_blank(),
        axis.ticks.x = element_line(),
        axis.ticks.y = element_line(),
        axis.title.x = element_text(size=10),
        axis.title.y = element_text(size=9),
        axis.text.y = element_text(size=8),
        axis.text.x.bottom=element_text(size=10),
        strip.text=element_text(size=10),
        plot.title=element_text(size=12, face="bold.italic"),
        plot.caption=element_text(size=10, face="italic"),
        legend.text=element_text(size=10),
        legend.title=element_text(size=10))

ggsave("fig1.4 libdem_change_plot.pdf", libdem_change_plot,
       device=cairo_pdf, width = 7, height=5)

## Civil society x 3 years ##
civil_year3_plot <- ggplot(civil_year3, aes(fill=year, x=reorder(country_name, -value2019), y=v2xcs_ccsi)) +
  geom_bar(position="dodge", stat="identity") +
  #geom_errorbar(position=move_it, aes(ymin=v2xcs_ccsi_codelow, ymax=v2xcs_ccsi_codehigh), width=.4, colour="grey90") +
  coord_flip() +
  labs(title="Figure 2.5 - Core Civil Society Scores, 1989-2019", x="", y="core civil society index\n<<less robust civil society, more robust civil society>>",
       caption="Source: Varieties of Democracy, Ver. 10",
       subtitle="Select Asia Pacific Countries, by 2019 Regime Type") +
  scale_y_continuous(breaks=c(0,.25,.50,.75,1), limits=c(0,1),expand = c(0,0) ) +
  scale_x_discrete(expand = c(0,0)) +
  theme_light() +
  scale_fill_manual(values=wes_palette(n=4, name="IsleofDogs2")) +
  facet_grid(v2x_regime_f~., switch = "y", scales = "free_y", space = "free_y") +
  guides(fill = guide_legend(reverse=T, title="")) +
  theme(text=element_text(family="Times New Roman"),
        legend.position="right",
        axis.ticks.x = element_line(),
        axis.ticks.y = element_line(),
        axis.title.x = element_text(size=10),
        axis.text.y = element_text(size=10),
        axis.text.x.bottom=element_text(size=10),
        strip.text=element_text(size=8),
        plot.title=element_text(size=12, face="bold.italic"),
        plot.caption=element_text(size=10, face="italic"),
        legend.text=element_text(size=10))

ggsave("fig1.5 civil_year3_plot.pdf", civil_year3_plot,
       device=cairo_pdf, width = 7, height=7)

## Civil society change x country over time ##  
civil_change_plot <- ggplot(civil_change, aes(fill=demobi, x=reorder(country_name, civil_diff30 ), y=civil_diff30 )) +
  geom_bar(position="dodge", stat="identity", colour="white") +
  labs(title="Figure 2.6 - Difference in Average Core Civil Society Scores", x="",
       y="civil soceity score average (2005-2019) minus average (1989-2004)\n<< less robust civil society, more robust civil society >>",
       caption="Source: Varieties of Democracy, Ver. 10",
       subtitle="Select Asia Pacific Countries, 1989-2019") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust=1),
        axis.ticks.x = element_line(),
        axis.ticks.y = element_line()) +
  #scale_y_continuous(breaks=c(-.2,-.1, 0,.1,.2,.3,.4,.5), limits=c(-.15,.45), expand = c(0, 0)  ) +
  scale_x_discrete(expand = c(0, 0)) +
  geom_abline(slope=0, intercept=0, colour = "black", lty=1) +
  scale_fill_manual(values=wes_palette(n=2, name="IsleofDogs2")) +
  guides(fill = guide_legend(reverse=F, title="Liberal/Electoral Democracy in 2004?")) +
  theme(legend.position = "top",
        text=element_text(family="Times New Roman"),
        strip.text.x = element_blank(),
        axis.ticks.x = element_line(),
        axis.ticks.y = element_line(),
        axis.title.x = element_text(size=10),
        axis.title.y = element_text(size=9),
        axis.text.y = element_text(size=8),
        axis.text.x.bottom=element_text(size=10),
        strip.text=element_text(size=10),
        plot.title=element_text(size=12, face="bold.italic"),
        plot.caption=element_text(size=10, face="italic"),
        legend.text=element_text(size=10),
        legend.title=element_text(size=10))

ggsave("fig1.6 civil_change_plot.pdf", civil_change_plot,
       device=cairo_pdf, width = 7, height=5)



#### Individual-level Figures ####

## Trust ##
trust_plot <- w.mean.all.vars %>% select(c(country_f, trust, v2x_regime_f)) %>%
  ggplot(aes(fill=v2x_regime_f,x=reorder(country_f, -trust), y=trust)) +
  geom_bar(position="dodge", stat="identity", colour="white") +
  geom_text(aes(label = trust), hjust = 1.1, color="white", size=3) +
  coord_flip() +
  labs(title="Figure 3.1 - Social Capital", x="", y="social capital index\n<< less trusting, more trusting >>",
       caption="Sources: Asia Barometer, Fourth Wave (2014-2016) & Varieties of Democracy, Ver. 10",
       subtitle="How much do people trust family, neighbors & others?") +
  scale_y_continuous(breaks=c(0,.25,.50,.75,1), limits=c(0,1.04),expand = c(0, 0) ) +
  scale_x_discrete(expand = c(0, 0)) +
  theme_light() +
  scale_fill_manual(values=wes_palette(n=4, name="IsleofDogs2")) +
  facet_grid(v2x_regime_f~., switch = "y", scales = "free_y", space = "free_y") +
  guides(fill = guide_legend(reverse=F, title="Regime Type")) +
  theme(legend.position = "top",
        text=element_text(family="Times New Roman"),
        strip.text.y = element_blank(),
        axis.ticks.x = element_line(),
        axis.ticks.y = element_line(),
        axis.title.x = element_text(size=10),
        axis.text.y = element_text(size=10, family = "Times New Roman"),
        axis.text.x.bottom=element_text(size=10),
        strip.text=element_text(size=10),
        plot.title=element_text(size=12, face="bold.italic"),
        plot.caption=element_text(size=10, face="italic"),
        legend.text=element_text(size=8),
        legend.title=element_text(size=8))

ggsave("fig2.1 trust_plot.pdf", trust_plot,
       device=cairo_pdf, width = 7, height=5)


## Trust in Institutions ##
trust_inst_plot <- w.mean.all.vars %>% select(c(country_f, trust_inst, v2x_regime_f)) %>%
  ggplot(aes(fill=v2x_regime_f, x=reorder(country_f, -trust_inst), y=trust_inst)) +
  geom_bar(position="dodge", stat="identity", colour="white") +
  geom_text(aes(label = trust_inst), hjust = 1.1, colour="white", size=3) +
  coord_flip() +
  labs(title="Figure 3.2 - Trust in Institutions", x="", y="institutional trust index\n<< less trust, more trust >>",
       caption="Sources: Asia Barometer, Fourth Wave (2014-2016) & Varieties of Democracy, Ver. 10",
       subtitle="How trusted are political parties, local government, media, and related institutions?") +
  scale_y_continuous(breaks=c(0,.25,.50,.75,1), limits=c(0,1.04),expand = c(0, 0) ) +
  scale_x_discrete(expand = c(0, 0)) +
  theme_light() +
  scale_fill_manual(values=wes_palette(n=4, name="IsleofDogs2")) +
  facet_grid(v2x_regime_f~., switch = "y", scales = "free_y", space = "free_y") +
  guides(fill = guide_legend(reverse=F, title="Regime Type")) +
  theme(legend.position = "top",
        text=element_text(family="Times New Roman"),
        strip.text.y = element_blank(),
        axis.ticks.x = element_line(),
        axis.ticks.y = element_line(),
        axis.title.x = element_text(size=10),
        axis.text.y = element_text(size=10, family = "Times New Roman"),
        axis.text.x.bottom=element_text(size=10),
        strip.text=element_text(size=10),
        plot.title=element_text(size=12, face="bold.italic"),
        plot.caption=element_text(size=10, face="italic"),
        legend.text=element_text(size=8),
        legend.title=element_text(size=8))

ggsave("fig2.2 trust_inst_plot.pdf", trust_inst_plot,
       device=cairo_pdf, width = 7, height=5)


## Political Values ##
demo_value_plot <- w.mean.all.vars %>% select(c(country_f, demo_value , v2x_regime_f)) %>%
  ggplot(aes(fill=v2x_regime_f, x=reorder(country_f, -demo_value ), y=demo_value )) +
  geom_bar(position="dodge", stat="identity", colour="white") +
  geom_text(aes(label = demo_value ), hjust = 1.1, color="white", size=3) +
  labs(title="Figure 3.5 - Democratic/Authoritarian Values", x="", y="values index\n<< more authoritarian, more democratic >>",
       caption="Sources: Asia Barometer, Fourth Wave (2014-2016) & Varieties of Democracy, Ver. 10",
       subtitle="How democratic or authoritarian are people's beliefs?") +
  scale_y_continuous(breaks=c(0,.25,.50,.75,1), limits=c(0,1.04),expand = c(0, 0) ) +
  scale_x_discrete(expand = c(0, 0)) +
  coord_flip() +
  theme_light() +
  scale_fill_manual(values=wes_palette(n=4, name="IsleofDogs2")) +
  facet_grid(v2x_regime_f~., switch = "y", scales = "free_y", space = "free_y") +
  guides(fill = guide_legend(reverse=F, title="Regime Type")) +
  theme(legend.position = "top",
        text=element_text(family="Times New Roman"),
        strip.text.y = element_blank(),
        axis.ticks.x = element_line(),
        axis.ticks.y = element_line(),
        axis.title.x = element_text(size=10),
        axis.text.y = element_text(size=10, family = "Times New Roman"),
        axis.text.x.bottom=element_text(size=10),
        strip.text=element_text(size=10),
        plot.title=element_text(size=12, face="bold.italic"),
        plot.caption=element_text(size=10, face="italic"),
        legend.text=element_text(size=8),
        legend.title=element_text(size=8))

ggsave("fig2.3 demo_value_plot.pdf", demo_value_plot,
       device=cairo_pdf, width = 7, height=5)



## Regime Preference ##
regime_pref_plot <- na.omit(w.mean.all.vars) %>% select(c(country_f, regime_pref, v2x_regime_f)) %>%
  ggplot(aes(x=reorder(country_f, -regime_pref), y=regime_pref, fill=v2x_regime_f)) +
  geom_bar(stat="identity", colour="white") +
  geom_text(aes(label = regime_pref), hjust = 1.1, color="white", size=3) +
  labs(title="Figure 3.6 - Democratic Legitimacy/Preference for Democracy", x="", y="regime preference index\n<< prefers autocracy, prefers democracy >>",
       caption="Sources: Asia Barometer, Fourth Wave (2014-2016) & Varieties of Democracy, Ver. 10",
       subtitle="What kind of political system is preferred?") +
  scale_y_continuous(breaks=c(0,.25,.50,.75,1), limits=c(0,1.04),expand = c(0, 0) ) +
  scale_x_discrete(expand = c(0, 0)) +
  coord_flip() +
  theme_light() +
  scale_fill_manual(values=wes_palette(n=4, name="IsleofDogs2")) +
  facet_grid(v2x_regime_f~., switch = "y", scales = "free_y", space = "free_y") +
  guides(fill = guide_legend(reverse=F, title="Regime Type")) +
  theme(legend.position = "top",
        text=element_text(family="Times New Roman"),
        strip.text.y = element_blank(),
        axis.ticks.x = element_line(),
        axis.ticks.y = element_line(),
        axis.title.x = element_text(size=10),
        axis.text.y = element_text(size=10, family = "Times New Roman"),
        axis.text.x.bottom=element_text(size=10),
        strip.text=element_text(size=10),
        plot.title=element_text(size=12, face="bold.italic"),
        plot.caption=element_text(size=10, face="italic"),
        legend.text=element_text(size=8),
        legend.title=element_text(size=8))

ggsave("fig2.4 regime_pref_plot.pdf", regime_pref_plot,
       device=cairo_pdf, width = 7, height=5)


## Quality Government ##
qual_gov2_plot <- w.mean.all.vars %>% select(c(country_f, qual_gov2, v2x_regime_f)) %>%
  ggplot(aes(fill=v2x_regime_f, x=reorder(country_f, -qual_gov2), y=qual_gov2)) +
  geom_bar(position="dodge", stat="identity", colour="white") +
  geom_text(aes(label = qual_gov2), hjust = 1.1, color="white", size=3) +
  coord_flip() +
  labs(title="Figure 3.3 - Quality of Governance", x="", y="government quality index\n<< quality low, quality high >>",
       caption="Sources: Asia Barometer, Fourth Wave (2014-2016) & Varieties of Democracy, Ver. 10",
       subtitle="Do government leaders act responsibly?") +
  scale_y_continuous(breaks=c(0,.25,.50,.75,1), limits=c(0,1.04),expand = c(0,0) ) +
  scale_x_discrete(expand = c(0, 0)) +
  theme_light() +
  scale_fill_manual(values=wes_palette(n=4, name="IsleofDogs2")) +
  facet_grid(v2x_regime_f~., switch = "y", scales = "free_y", space = "free_y") +
  guides(fill = guide_legend(reverse=F, title="Regime Type")) +
  theme(legend.position = "top",
        text=element_text(family="Times New Roman"),
        strip.text.y = element_blank(),
        axis.ticks.x = element_line(),
        axis.ticks.y = element_line(),
        axis.title.x = element_text(size=10),
        axis.text.y = element_text(size=10, family = "Times New Roman"),
        axis.text.x.bottom=element_text(size=10),
        strip.text=element_text(size=10),
        plot.title=element_text(size=12, face="bold.italic"),
        plot.caption=element_text(size=10, face="italic"),
        legend.text=element_text(size=8),
        legend.title=element_text(size=8))

ggsave("fig2.5 qual_gov2p.pdf", qual_gov2_plot,
       device=cairo_pdf, width = 7, height=5)

## Political Participation ##
pol_part_plot <- subset(w.mean.all.vars, country_f != "Malaysia") %>% select(c(country_f, pol_part , v2x_regime_f)) %>%
  ggplot(aes(fill=v2x_regime_f, x=reorder(country_f, -pol_part ), y=pol_part )) +
  geom_bar(position="dodge", stat="identity", colour="white") +
  geom_text(aes(label = pol_part), hjust = 1.1, color="white", size=3) +
  coord_flip() +
  labs(title="Figure 3.4 - Political Participation", x="", y="political participation index\n<< less active, more active >>",
       caption="Sources: Asia Barometer, Fourth Wave (2014-2016) & Varieties of Democracy, Ver. 10",
       subtitle="How politically active are people?") +
  scale_y_continuous(breaks=c(0,.25,.50,.75,1), limits=c(0,1.04),expand = c(0,0) ) +
  scale_x_discrete(expand = c(0, 0)) +
  theme_light() +
  scale_fill_manual(values=wes_palette(n=4, name="IsleofDogs2")) +
  facet_grid(v2x_regime_f~., switch = "y", scales = "free_y", space = "free_y") +
  guides(fill = guide_legend(reverse=F, title="Regime Type")) +
  theme(legend.position = "top",
        text=element_text(family="Times New Roman"),
        strip.text.y = element_blank(),
        axis.ticks.x = element_line(),
        axis.ticks.y = element_line(),
        axis.title.x = element_text(size=10),
        axis.text.y = element_text(size=10, family = "Times New Roman"),
        axis.text.x.bottom=element_text(size=10),
        strip.text=element_text(size=10),
        plot.title=element_text(size=12, face="bold.italic"),
        plot.caption=element_text(size=10, face="italic"),
        legend.text=element_text(size=8),
        legend.title=element_text(size=8))

ggsave("fig2.6 pol_part_plot.pdf", pol_part_plot,
       device=cairo_pdf, width = 7, height=5)


#### Models - pol values + social, all/youth ####

## political values
pr_political_values_plot <- ggplot(pr_political_values_both, aes(x=reorder(country_f, predicted), y=predicted, colour=group)) +
  geom_hline(yintercept = 0, color="#707D85") +
  geom_point(position=move_it_pol, size=1.8, aes(shape=group, fill=group)) +
  geom_errorbar(position=move_it_pol, aes(ymin=conf.low, ymax=conf.high, linetype=group), width=0) + 
  theme_light() +
  labs(title="Figure 3.7 - Political Norms and Values Scale", x="", y="<< more authoritarian, more democratic >>\nstandardized score",
       subtitle="Select Asia Pacific Countries, 2014-2016",
       caption="Sources: Asia Barometer, Fourth Wave (2014-2016) & Varieties of Democracy, Ver. 10\n Youth is counted as respondents 18-29 years-old. Confidence intervals at 95%.") +
  scale_y_continuous(breaks=c(-1,-.50,0,.50,1), limits=c(-1.07,1.05)) +
  scale_colour_manual(values=wes_palette(n=2, name="BottleRocket1")) +
  facet_grid(~v2x_regime_f, switch = "x", scales = "free_x", space = "free_x") +
  theme(legend.position = "right",
        text=element_text(family="Times New Roman"),
        axis.text.x = element_text(angle = 45, hjust=1),
        strip.text.x = element_text(size=7.5),
        axis.ticks.x = element_line(),
        axis.ticks.y = element_line(),
        axis.title.x = element_text(size=10),
        axis.text.y = element_text(size=10),
        axis.text.x.bottom=element_text(size=10),
        strip.text=element_text(size=10),
        plot.title=element_text(size=12, face="bold.italic"),
        plot.caption=element_text(size=10, face="italic"),
        legend.text=element_text(size=10),
        legend.title=element_blank())

ggsave("fig2.7 pr_political_values_plot.pdf", pr_political_values_plot,
       device=cairo_pdf, width = 7, height=6)

## society plot
pr_political_society_plot <- ggplot(data=pr_political_society_both, aes(x=reorder(country_f, predicted), y=predicted, colour=group)) +
  geom_hline(yintercept = 0, color="#707D85") +
  geom_point(position=move_it_pol, size=1.8, aes(shape=group, fill=group)) +
  geom_errorbar(position=move_it_pol, aes(ymin=conf.low, ymax=conf.high, linetype=group), width=0) + 
  theme_light()+
  labs(title="Figure 3.8 - Trust and Good Governance Scale", x="", y="<< less trusthworthy, more trustworthy >>\nstandardized score",
       subtitle="Select Asia Pacific Countries, 2014-2016",
       caption="Sources: Asia Barometer, Fourth Wave (2014-2016) & Varieties of Democracy, Ver. 10\n Youth is counted as respondents 18-29 years-old. Confidence intervals at 95%.") +
  scale_colour_manual(values=wes_palette(n=2, name="BottleRocket1")) +
  facet_grid(~v2x_regime_f, switch = "x", scales = "free_x", space = "free_x") +
  scale_y_continuous(breaks=c(-1,-.50,0,.50,1), limits=c(-1,1)) +
  theme(legend.position = "right",
        text=element_text(family="Times New Roman"),
        axis.text.x = element_text(angle = 45, hjust=1),
        strip.text.x = element_text(size=7.5),
        axis.ticks.x = element_line(),
        axis.ticks.y = element_line(),
        axis.title.x = element_text(size=10),
        axis.text.y = element_text(size=10),
        axis.text.x.bottom=element_text(size=10),
        strip.text=element_text(size=10),
        plot.title=element_text(size=12, face="bold.italic"),
        plot.caption=element_text(size=10, face="italic"),
        legend.text=element_text(size=10),
        legend.title=element_blank())

ggsave("fig2.8 pr_political_society_plot.pdf", pr_political_society_plot,
       device=cairo_pdf, width = 7, height=6)


## Identifying institutional-public opinion congruence

# political values convergence
convergence_plot <- ggplot(pr_political_values_joined, aes(x=predicted, y=libdem.avg10_s, colour=congruence)) +
  geom_hline(yintercept=0) +
  geom_vline(xintercept = 0) +
  geom_point()+
  geom_label_repel(aes(label = country_f),
                   box.padding   = 0.35, 
                   point.padding = 0.5,
                   segment.color = 'grey50') +
  annotate("text", x = c(.5,.5,-.5,-.5), y = c(-1.2,.5,1.4,-.25),
           label = c("Democratic Values\nAuthoritarian System", "Democratic Congruence", 
                     "Authoritarian Values\nDemocratic System", "Authoritarian Congruence"), 
           fontface="bold.italic") +
  theme_light() +
  scale_colour_manual(values=wes_palette(n=4, name="Moonrise2")) +
  labs(title="Figure 3.9 - Identifying Institutional-Public Opinion Congruence",
       x="scaled political values index",
       y="scaled liberal democracy index (10-year average)",
       caption="Sources: Asia Barometer, Fourth Wave (2014-2016) & Varieties of Democracy, Ver. 10",
       subtitle="Association Between Political Values & Liberal Democracy\nSelect Asia Pacific Countries") +
  theme(legend.position = "none",
        text=element_text(family="Times New Roman"),
        axis.text.y = element_text(size=10),
        axis.text.x = element_text(size=10),
        plot.title=element_text(size=12, face="bold.italic"),
        plot.caption=element_text(size=10, face="italic"))

ggsave("fig2.9 convergence_plot.pdf", convergence_plot,
       device=cairo_pdf, width = 7, height=6)

# social values convergence
convergence_plot_2 <- ggplot(pr_political_society_joined, aes(x=predicted, y=libdem.avg10_s, colour=congruence)) +
  geom_hline(yintercept=0) +
  geom_vline(xintercept = 0) +
  geom_point()+
  geom_label_repel(aes(label = country_f),
                   box.padding   = 0.35, 
                   point.padding = 0.2,
                   segment.color = 'grey50') +
  annotate("text", x = c(.5,.5,-.4,-.4), y = c(-1.2,.5,.5,-1.2),
           label = c("Socially Cohesive\nAuthoritarian System", "Socially Cohesive\nDemocratic System", 
                     "Socially Contentious\nDemocratic System", "Socially Contentious\nAuthoritarian System"), 
           fontface="bold.italic") +
  theme_light() +
  scale_colour_manual(values=wes_palette(n=4, name="Moonrise2")) +
  labs(title="Figure 3.10 - Identifying Institutional-Public Opinion Congruence",
       x="scaled trust and social values index",
       y="scaled liberal democracy index (10-year average)",
       caption="Sources: Asia Barometer, Fourth Wave (2014-2016) & Varieties of Democracy, Ver. 10",
       subtitle="Association Between Trust and Good Governance & Liberal Democracy\nSelect Asia Pacific Countries") +
  theme(legend.position = "none",
        text=element_text(family="Times New Roman"),
        axis.text.y = element_text(size=10),
        axis.text.x = element_text(size=10),
        plot.title=element_text(size=12, face="bold.italic"),
        plot.caption=element_text(size=10, face="italic"))

ggsave("fig2.10 convergence_plot_2.pdf", convergence_plot_2,
       device=cairo_pdf, width = 7, height=6)

#### Cohort Analysis ####

#### __ Political values ####
## cohort 1 / japan, taiwan, korea, hong kong, cambodia

japanp1 <- ggplot(data=japan_pr1, aes(x=x, y=predicted, group=group)) +
  geom_errorbar(position=move_it, aes(ymin=conf.low, ymax=conf.high), width=0) + geom_point(position=move_it, size=1.8) +
  geom_smooth(method = "lm", formula = y ~ poly(x, 3), se=TRUE, fullrange=FALSE, level=0.95, colour="red") +
  theme_minimal() +
  labs(title="", x="", y="",
       subtitle="Japan",
       caption="") +
  theme(axis.ticks.x = element_line(),
        axis.text.x = element_text(angle = 45, hjust=1, size=8),
        axis.text.y = element_text(size=8)) + #### work w/ this.
  scale_y_continuous(breaks=c(.40,.50,.60,.70,.80), limits=c(.40,.85) )

taiwanp1 <- ggplot(data=taiwan_pr1, aes(x=x, y=predicted, group=group)) +
  geom_errorbar(position=move_it, aes(ymin=conf.low, ymax=conf.high), width=0) + geom_point(position=move_it, size=1.8) +
  geom_smooth(method = "lm", formula = y ~ poly(x, 3), se=TRUE, fullrange=FALSE, level=0.95, colour="red") +
  theme_minimal() +
  labs(title="", x="", y="",
       subtitle="Taiwan",
       caption="") +
  theme(axis.ticks.x = element_line(),
        axis.text.x = element_text(angle = 45, hjust=1, size=8),
        axis.text.y = element_text(size=8)) + #### work w/ this.
  scale_y_continuous(breaks=c(.40,.50,.60,.70,.80), limits=c(.40,.85) )

koreap1 <- ggplot(data=korea_pr1, aes(x=x, y=predicted, group=group)) +
  geom_errorbar(position=move_it, aes(ymin=conf.low, ymax=conf.high), width=0) + geom_point(position=move_it, size=1.8) +
  geom_smooth(method = "lm", formula = y ~ poly(x, 3), se=TRUE, fullrange=FALSE, level=0.95, colour="red") +
  theme_minimal() +
  labs(title="", x="", y="",
       subtitle="South Korea",
       caption="") +
  theme(axis.ticks.x = element_line(),
        axis.text.x = element_text(angle = 45, hjust=1, size=8),
        axis.text.y = element_text(size=8)) + #### work w/ this.
  scale_y_continuous(breaks=c(.40,.50,.60,.70,.80), limits=c(.40,.85) )

hkp1 <- ggplot(data=hk_pr1, aes(x=x, y=predicted, group=group)) +
  geom_errorbar(position=move_it, aes(ymin=conf.low, ymax=conf.high), width=0) + geom_point(position=move_it, size=1.8) +
  geom_smooth(method = "lm", formula = y ~ poly(x, 3), se=TRUE, fullrange=FALSE, level=0.95, colour="red") +
  theme_minimal() +
  labs(title="", x="", y="",
       subtitle="Hong Kong",
       caption="") +
  theme(axis.ticks.x = element_line(),
        axis.text.x = element_text(angle = 45, hjust=1, size=8),
        axis.text.y = element_text(size=8)) + #### work w/ this.
  scale_y_continuous(breaks=c(.40,.50,.60,.70,.80), limits=c(.40,.85) )

camp1 <- ggplot(data=cambodia_pr1, aes(x=x, y=predicted, group=group)) +
  geom_errorbar(position=move_it, aes(ymin=conf.low, ymax=conf.high), width=0) + geom_point(position=move_it, size=1.8) +
  geom_smooth(method = "lm", formula = y ~ poly(x, 3), se=TRUE, fullrange=FALSE, level=0.95, colour="red") +
  theme_minimal() +
  labs(title="", x="", y="",
       subtitle="Cambodia",
       caption="") +
  theme(axis.ticks.x = element_line(),
        axis.text.x = element_text(angle = 45, hjust=1, size=8),
        axis.text.y = element_text(size=8)) + #### work w/ this.
  scale_y_continuous(breaks=c(.40,.50,.60,.70,.80), limits=c(.40,.85) )

viet1 <- ggplot(data=vietnam_pr1, aes(x=x, y=predicted, group=group)) +
  geom_errorbar(position=move_it, aes(ymin=conf.low, ymax=conf.high), width=0) + geom_point(position=move_it, size=1.8) +
  geom_smooth(method = "lm", formula = y ~ poly(x, 3), se=TRUE, fullrange=FALSE, level=0.95, colour="red") +
  theme_minimal() +
  labs(title="", x="", y="",
       subtitle="Vietnam",
       caption="") +
  theme(axis.ticks.x = element_line(),
        axis.text.x = element_text(angle = 45, hjust=1, size=8),
        axis.text.y = element_text(size=8)) + #### work w/ this.
  scale_y_continuous(breaks=c(.40,.50,.60,.70,.80), limits=c(.40,.80) )


#grid.arrange(japanp1, taiwanp1, koreap1, camp1, hkp1, ncol=3)


## cohort 2 / philippines, singapore, indonesia, malaysia, china

philp1 <- ggplot(data=phil_pr1, aes(x=x, y=predicted, group=group)) +
  geom_errorbar(position=move_it, aes(ymin=conf.low, ymax=conf.high), width=0) + geom_point(position=move_it, size=1.8) +
  geom_smooth(method = "lm", formula = y ~ poly(x, 3), se=TRUE, fullrange=FALSE, level=0.95, colour="red") +
  theme_minimal() +
  labs(title="", x="", y="",
       subtitle="Philippines",
       caption="") +
  theme(axis.ticks.x = element_line(),
        axis.text.x = element_text(angle = 45, hjust=1, size=8),
        axis.text.y = element_text(size=8)) + #### work w/ this.
  scale_y_continuous(breaks=c(.40,.50,.60,.70,.80), limits=c(.40,.85) )

singp1 <- ggplot(data=sing_pr1, aes(x=x, y=predicted, group=group)) +
  geom_errorbar(position=move_it, aes(ymin=conf.low, ymax=conf.high), width=0) + geom_point(position=move_it, size=1.8) +
  geom_smooth(method = "lm", formula = y ~ poly(x, 3), se=TRUE, fullrange=FALSE, level=0.95, colour="red") +
  theme_minimal() +
  labs(title="", x="", y="",
       subtitle="Singapore",
       caption="") +
  theme(axis.ticks.x = element_line(),
        axis.text.x = element_text(angle = 45, hjust=1, size=8),
        axis.text.y = element_text(size=8)) + #### work w/ this.
  scale_y_continuous(breaks=c(.40,.50,.60,.70,.80), limits=c(.40,.85) )

indop1 <- ggplot(data=indo_pr1, aes(x=x, y=predicted, group=group)) +
  geom_errorbar(position=move_it, aes(ymin=conf.low, ymax=conf.high), width=0) + geom_point(position=move_it, size=1.8) +
  geom_smooth(method = "lm", formula = y ~ poly(x, 3), se=TRUE, fullrange=FALSE, level=0.95, colour="red") +
  theme_minimal() +
  labs(title="", x="", y="",
       subtitle="Indonesia",
       caption="") +
  theme(axis.ticks.x = element_line(),
        axis.text.x = element_text(angle = 45, hjust=1, size=8),
        axis.text.y = element_text(size=8)) + #### work w/ this.
  scale_y_continuous(breaks=c(.40,.50,.60,.70,.80), limits=c(.40,.85) )

malayp1 <- ggplot(data=malay_pr1, aes(x=x, y=predicted, group=group)) +
  geom_errorbar(position=move_it, aes(ymin=conf.low, ymax=conf.high), width=0) + geom_point(position=move_it, size=1.8) +
  geom_smooth(method = "lm", formula = y ~ poly(x, 3), se=TRUE, fullrange=FALSE, level=0.95, colour="red") +
  theme_minimal() +
  labs(title="", x="", y="",
       subtitle="Malaysia",
       caption="") +
  theme(axis.ticks.x = element_line(),
        axis.text.x = element_text(angle = 45, hjust=1, size=8),
        axis.text.y = element_text(size=8)) + #### work w/ this.
  scale_y_continuous(breaks=c(.40,.50,.60,.70,.80), limits=c(.40,.85) )

chp1 <- ggplot(data=ch_pr1, aes(x=x, y=predicted, group=group)) +
  geom_errorbar(position=move_it, aes(ymin=conf.low, ymax=conf.high), width=0) + geom_point(position=move_it, size=1.8) +
  geom_smooth(method = "lm", formula = y ~ poly(x, 3), se=TRUE, fullrange=FALSE, level=0.95, colour="red") +
  theme_minimal() +
  labs(title="", x="", y="",
       subtitle="China",
       caption="") +
  theme(axis.ticks.x = element_line(),
        axis.text.x = element_text(angle = 45, hjust=1, size=8),
        axis.text.y = element_text(size=8)) + #### work w/ this.
  scale_y_continuous(breaks=c(.40,.50,.60,.70,.80), limits=c(.40,.85) )


#grid.arrange(philp1,singp1,indop1,malayp1,chp1, ncol=3)
#grid.arrange(japanp1, taiwanp1, koreap1, camp1, hkp1, ncol=3)

## cohort 3 / mongolia, thailand, myanmar/burma

mongp1 <- ggplot(data=mong_pr1, aes(x=x, y=predicted, group=group)) +
  geom_errorbar(position=move_it, aes(ymin=conf.low, ymax=conf.high), width=0) + geom_point(position=move_it, size=1.8) +
  geom_smooth(method = "lm", formula = y ~ poly(x, 3), se=TRUE, fullrange=FALSE, level=0.95, colour="red") +
  theme_minimal() +
  labs(title="", x="", y="",
       subtitle="Mongolia",
       caption="") +
  theme(axis.ticks.x = element_line(),
        axis.text.x = element_text(angle = 45, hjust=1, size=8),
        axis.text.y = element_text(size=8)) + #### work w/ this.
  scale_y_continuous(breaks=c(.40,.50,.60,.70,.80), limits=c(.40,.85) )

thaip1 <- ggplot(data=thai_pr1, aes(x=x, y=predicted, group=group)) +
  geom_errorbar(position=move_it, aes(ymin=conf.low, ymax=conf.high), width=0) + geom_point(position=move_it, size=1.8) +
  geom_smooth(method = "lm", formula = y ~ poly(x, 3), se=TRUE, fullrange=FALSE, level=0.95, colour="red") +
  theme_minimal() +
  labs(title="", x="", y="",
       subtitle="Thailand",
       caption="") +
  theme(axis.ticks.x = element_line(),
        axis.text.x = element_text(angle = 45, hjust=1, size=8),
        axis.text.y = element_text(size=8)) + #### work w/ this.
  scale_y_continuous(breaks=c(.40,.50,.60,.70,.80), limits=c(.37,.85) )

myanp1 <- ggplot(data=myan_pr1, aes(x=x, y=predicted, group=group)) +
  geom_errorbar(position=move_it, aes(ymin=conf.low, ymax=conf.high), width=0) + geom_point(position=move_it, size=1.8) +
  geom_smooth(method = "lm", formula = y ~ poly(x, 3), se=TRUE, fullrange=FALSE, level=0.95, colour="red") +
  theme_minimal() +
  labs(title="", x="", y="",
       subtitle="Myanmar",
       caption="") +
  theme(axis.ticks.x = element_line(),
        axis.text.x = element_text(angle = 45, hjust=1, size=8),
        axis.text.y = element_text(size=8)) + #### work w/ this.
  scale_y_continuous(breaks=c(.40,.50,.60,.70,.80), limits=c(.37,.85) )


#grid.arrange(mongp1,thaip1,myanp1, ncol=3)


grid.arrange(japanp1, taiwanp1, koreap1, camp1, hkp1,
             philp1,singp1,indop1,malayp1,viet1,chp1,
             mongp1,thaip1,myanp1, ncol=5,
             top=textGrob("Figure 3.11 - Political Values By Age Cohorts\nSelect Asia Pacific Countries"))

#grid.arrange(p1,p3,p2,p4, ncol=2, nrow=2,
# top = textGrob("Daily QC: Blue",gp=gpar(fontsize=20,font=3)))

#### Social trust ####

## cohort 1 / japan, taiwan, korea, hong kong, cambodia

japanp2 <- ggplot(data=japan_pr2, aes(x=x, y=predicted, group=group)) +
  geom_errorbar(position=move_it, aes(ymin=conf.low, ymax=conf.high), width=0) + geom_point(position=move_it, size=1.8) +
  geom_smooth(method = "lm", formula = y ~ poly(x, 3), se=TRUE, fullrange=FALSE, level=0.95, colour="red") +
  theme_minimal() +
  labs(title="", x="", y="",
       subtitle="Japan",
       caption="") +
  theme(axis.ticks.x = element_line(),
        axis.text.x = element_text(angle = 45, hjust=1, size=8),
        axis.text.y = element_text(size=8)) + #### work w/ this.
  scale_y_continuous(breaks=c(.40,.50,.60,.70), limits=c(.40,.75) )

taiwanp2 <- ggplot(data=taiwan_pr2, aes(x=x, y=predicted, group=group)) +
  geom_errorbar(position=move_it, aes(ymin=conf.low, ymax=conf.high), width=0) + geom_point(position=move_it, size=1.8) +
  geom_smooth(method = "lm", formula = y ~ poly(x, 3), se=TRUE, fullrange=FALSE, level=0.95, colour="red") +
  theme_minimal() +
  labs(title="", x="", y="",
       subtitle="Taiwan",
       caption="") +
  theme(axis.ticks.x = element_line(),
        axis.text.x = element_text(angle = 45, hjust=1, size=8),
        axis.text.y = element_text(size=8)) + #### work w/ this.
  scale_y_continuous(breaks=c(.40,.50,.60,.70), limits=c(.40,.75) )


koreap2 <- ggplot(data=korea_pr2, aes(x=x, y=predicted, group=group)) +
  geom_errorbar(position=move_it, aes(ymin=conf.low, ymax=conf.high), width=0) + geom_point(position=move_it, size=1.8) +
  geom_smooth(method = "lm", formula = y ~ poly(x, 3), se=TRUE, fullrange=FALSE, level=0.95, colour="red") +
  theme_minimal() +
  labs(title="", x="", y="",
       subtitle="South Korea",
       caption="") +
  theme(axis.ticks.x = element_line(),
        axis.text.x = element_text(angle = 45, hjust=1, size=8),
        axis.text.y = element_text(size=8)) + #### work w/ this.
  scale_y_continuous(breaks=c(.40,.50,.60,.70), limits=c(.40,.75) )

hkp2 <- ggplot(data=hk_pr2, aes(x=x, y=predicted, group=group)) +
  geom_errorbar(position=move_it, aes(ymin=conf.low, ymax=conf.high), width=0) + geom_point(position=move_it, size=1.8) +
  geom_smooth(method = "lm", formula = y ~ poly(x, 3), se=TRUE, fullrange=FALSE, level=0.95, colour="red") +
  theme_minimal() +
  labs(title="", x="", y="",
       subtitle="Hong Kong",
       caption="") +
  theme(axis.ticks.x = element_line(),
        axis.text.x = element_text(angle = 45, hjust=1, size=8),
        axis.text.y = element_text(size=8)) + #### work w/ this.
  scale_y_continuous(breaks=c(.40,.50,.60,.70), limits=c(.40,.75) )

camp2 <- ggplot(data=cambodia_pr2, aes(x=x, y=predicted, group=group)) +
  geom_errorbar(position=move_it, aes(ymin=conf.low, ymax=conf.high), width=0) + geom_point(position=move_it, size=1.8) +
  geom_smooth(method = "lm", formula = y ~ poly(x, 3), se=TRUE, fullrange=FALSE, level=0.95, colour="red") +
  theme_minimal() +
  labs(title="", x="", y="",
       subtitle="Cambodia",
       caption="") +
  theme(axis.ticks.x = element_line(),
        axis.text.x = element_text(angle = 45, hjust=1, size=8),
        axis.text.y = element_text(size=8)) + #### work w/ this.
  scale_y_continuous(breaks=c(.40,.50,.60,.70), limits=c(.40,.75) )


#grid.arrange(japanp2, taiwanp2, koreap2, camp2, hkp2, ncol=3)


## cohort 2 / philippines, singapore, indonesia, malaysia, china

philp2 <- ggplot(data=phil_pr2, aes(x=x, y=predicted, group=group)) +
  geom_errorbar(position=move_it, aes(ymin=conf.low, ymax=conf.high), width=0) + geom_point(position=move_it, size=1.8) +
  geom_smooth(method = "lm", formula = y ~ poly(x, 3), se=TRUE, fullrange=FALSE, level=0.95, colour="red") +
  theme_minimal() +
  labs(title="", x="", y="",
       subtitle="Philippines",
       caption="") +
  theme(axis.ticks.x = element_line(),
        axis.text.x = element_text(angle = 45, hjust=1, size=8),
        axis.text.y = element_text(size=8)) + #### work w/ this.
  scale_y_continuous(breaks=c(.40,.50,.60,.70), limits=c(.40,.75) )

singp2 <- ggplot(data=sing_pr2, aes(x=x, y=predicted, group=group)) +
  geom_errorbar(position=move_it, aes(ymin=conf.low, ymax=conf.high), width=0) + geom_point(position=move_it, size=1.8) +
  geom_smooth(method = "lm", formula = y ~ poly(x, 3), se=TRUE, fullrange=FALSE, level=0.95, colour="red") +
  theme_minimal() +
  labs(title="", x="", y="",
       subtitle="Singapore",
       caption="") +
  theme(axis.ticks.x = element_line(),
        axis.text.x = element_text(angle = 45, hjust=1, size=8),
        axis.text.y = element_text(size=8)) + #### work w/ this.
  scale_y_continuous(breaks=c(.40,.50,.60,.70), limits=c(.40,.75) )

indop2 <- ggplot(data=indo_pr2, aes(x=x, y=predicted, group=group)) +
  geom_errorbar(position=move_it, aes(ymin=conf.low, ymax=conf.high), width=0) + geom_point(position=move_it, size=1.8) +
  geom_smooth(method = "lm", formula = y ~ poly(x, 3), se=TRUE, fullrange=FALSE, level=0.95, colour="red") +
  theme_minimal() +
  labs(title="", x="", y="",
       subtitle="Indonesia",
       caption="") +
  theme(axis.ticks.x = element_line(),
        axis.text.x = element_text(angle = 45, hjust=1, size=8),
        axis.text.y = element_text(size=8)) + #### work w/ this.
  scale_y_continuous(breaks=c(.40,.50,.60,.70), limits=c(.40,.75) )

malayp2 <- ggplot(data=malay_pr2, aes(x=x, y=predicted, group=group)) +
  geom_errorbar(position=move_it, aes(ymin=conf.low, ymax=conf.high), width=0) + geom_point(position=move_it, size=1.8) +
  geom_smooth(method = "lm", formula = y ~ poly(x, 3), se=TRUE, fullrange=FALSE, level=0.95, colour="red") +
  theme_minimal() +
  labs(title="", x="", y="",
       subtitle="Malaysia",
       caption="") +
  theme(axis.ticks.x = element_line(),
        axis.text.x = element_text(angle = 45, hjust=1, size=8),
        axis.text.y = element_text(size=8)) + #### work w/ this.
  scale_y_continuous(breaks=c(.40,.50,.60,.70), limits=c(.40,.75) )

chp2 <- ggplot(data=ch_pr2, aes(x=x, y=predicted, group=group)) +
  geom_errorbar(position=move_it, aes(ymin=conf.low, ymax=conf.high), width=0) + geom_point(position=move_it, size=1.8) +
  geom_smooth(method = "lm", formula = y ~ poly(x, 3), se=TRUE, fullrange=FALSE, level=0.95, colour="red") +
  theme_minimal() +
  labs(title="", x="", y="",
       subtitle="China",
       caption="") +
  theme(axis.ticks.x = element_line(),
        axis.text.x = element_text(angle = 45, hjust=1, size=8),
        axis.text.y = element_text(size=8)) + #### work w/ this.
  scale_y_continuous(breaks=c(.40,.50,.60,.70), limits=c(.40,.75) )


#grid.arrange(philp2,singp2,indop2,malayp2,chp2, ncol=3)


## cohort 3 / mongolia, thailand, myanmar/burma

mongp2 <- ggplot(data=mong_pr2, aes(x=x, y=predicted, group=group)) +
  geom_errorbar(position=move_it, aes(ymin=conf.low, ymax=conf.high), width=0) + geom_point(position=move_it, size=1.8) +
  geom_smooth(method = "lm", formula = y ~ poly(x, 3), se=TRUE, fullrange=FALSE, level=0.95, colour="red") +
  theme_minimal() +
  labs(title="", x="", y="",
       subtitle="Mongolia",
       caption="") +
  theme(axis.ticks.x = element_line(),
        axis.text.x = element_text(angle = 45, hjust=1, size=8),
        axis.text.y = element_text(size=8)) + #### work w/ this.
  scale_y_continuous(breaks=c(.40,.50,.60,.70), limits=c(.40,.75) )

thaip2 <- ggplot(data=thai_pr2, aes(x=x, y=predicted, group=group)) +
  geom_errorbar(position=move_it, aes(ymin=conf.low, ymax=conf.high), width=0) + geom_point(position=move_it, size=1.8) +
  geom_smooth(method = "lm", formula = y ~ poly(x, 3), se=TRUE, fullrange=FALSE, level=0.95, colour="red") +
  theme_minimal() +
  labs(title="", x="", y="",
       subtitle="Thailand",
       caption="") +
  theme(axis.ticks.x = element_line(),
        axis.text.x = element_text(angle = 45, hjust=1, size=8),
        axis.text.y = element_text(size=8)) + #### work w/ this.
  scale_y_continuous(breaks=c(.40,.50,.60,.70), limits=c(.40,.75) )

myanp2 <- ggplot(data=myan_pr2, aes(x=x, y=predicted, group=group)) +
  geom_errorbar(position=move_it, aes(ymin=conf.low, ymax=conf.high), width=0) + geom_point(position=move_it, size=1.8) +
  geom_smooth(method = "lm", formula = y ~ poly(x, 3), se=TRUE, fullrange=FALSE, level=0.95, colour="red") +
  theme_minimal() +
  labs(title="", x="", y="",
       subtitle="Myanmar",
       caption="") +
  theme(axis.ticks.x = element_line(),
        axis.text.x = element_text(angle = 45, hjust=1, size=8),
        axis.text.y = element_text(size=8)) + #### work w/ this.
  scale_y_continuous(breaks=c(.40,.50,.60,.70), limits=c(.40,.75) )


## vietnam
vietp2 <- ggplot(data=viet_pr2, aes(x=x, y=predicted, group=group)) +
  geom_errorbar(position=move_it, aes(ymin=conf.low, ymax=conf.high), width=0) + geom_point(position=move_it, size=1.8) +
  geom_smooth(method = "lm", formula = y ~ poly(x, 3), se=TRUE, fullrange=FALSE, level=0.95, colour="red") +
  theme_minimal() +
  labs(title="", x="", y="",
       subtitle="Myanmar",
       caption="") +
  theme(axis.ticks.x = element_line(),
        axis.text.x = element_text(angle = 45, hjust=1, size=8),
        axis.text.y = element_text(size=8)) + #### work w/ this.
  scale_y_continuous(breaks=c(.40,.50,.60,.70), limits=c(.40,.75) )


#grid.arrange(mongp2,thaip2,myanp2, ncol=3)

###


grid.arrange(singp2,vietp2,hkp2,chp2,thaip2,
             japanp2,indop2,malayp2,camp2,
             philp2,mongp2,myanp2,taiwanp2,koreap2,
             ncol=5,top=textGrob("Figure 3.12 - Trust and Social Values By Age Cohorts\nSelect Asia Pacific Countries"))


#### Pandemic backsliding ####
pandemic_plot <- ggplot(pandemic_back, aes(x = reorder(country_f, -pandem), y = pandem, fill=v2x_regime_f)) + 
  geom_bar(position="dodge", stat="identity", colour="white") +
  geom_text(aes(label = pandem), size=3, hjust = 1.1, color="white") +
  coord_flip() +
  labs(title="Figure 4.1 - COVID-19 Response and Pandemic Backsliding", x="", y="pandemic response index\n<<less freedom-restricting, more freedom-restricting>>",
       caption="Source: Varieties of Democracy, Pandemic Backsliding, Ver. 2 (May 2020)",
       subtitle="Have democratic standards been violated?") +
  scale_y_continuous(breaks=c(0,.25,.50,.75,1), limits=c(0,1.04),expand = c(0,0) ) +
  scale_x_discrete(expand = c(0, 0)) +
  theme_light() +
  scale_fill_manual(values=wes_palette(n=4, name="IsleofDogs2")) +
  facet_grid(v2x_regime_f~., switch = "y", scales = "free_y", space = "free_y") +
  guides(fill = guide_legend(reverse=F, title="Regime Type")) +
  theme(legend.position = "top",
        text=element_text(family="Times New Roman"),
        strip.text.y = element_blank(),
        axis.ticks.x = element_line(),
        axis.ticks.y = element_line(),
        axis.title.x = element_text(size=10),
        axis.text.y = element_text(size=10, family = "Times New Roman"),
        axis.text.x.bottom=element_text(size=10),
        strip.text=element_text(size=10),
        plot.title=element_text(size=12, face="bold.italic"),
        plot.caption=element_text(size=10, face="italic"),
        legend.text=element_text(size=8.5),
        legend.title=element_text(size=8.5))

ggsave("fig3.1 pandemic_plot.pdf",pandemic_plot,
       device=cairo_pdf,width = 7,height=6)

## libdem index <> pandemic index association
pandemic_line_plot <- ggplot(pandemic_back, aes(x=pandem, y=libdem10avg)) + 
  geom_point() +
  stat_smooth(method = "lm", formula = y ~ poly(x, 2), level=0.95, colour="red") +
  geom_label_repel(aes(label = country_f),
                   box.padding   = 0.35, 
                   point.padding = 0.5,
                   segment.color = 'grey50') +
  theme_light() +
  labs(title="Figure 4.2 - Association Between Pandemic Response & Liberal Democracy Indices",
       x="pandemic response index\n<< less freedom-restricting, more freedom-restricting >>",
       y="liberal democracy index (10-year average, 2010-2019)\n<<less liberal democratic, more liberal democratic>>",
       caption="Source: Varieties of Democracy, Pandemic Backsliding, Ver. 2 (May 2020)\nSecond order (quadratic) polynomial fit",
       subtitle="Select Asia Pacific Countries, 2019") +
  theme(text=element_text(family="Times New Roman"),
        axis.text.x = element_text(size=10, angle = 45, hjust=1),
        strip.text.x = element_text(size=7.5),
        axis.ticks.x = element_line(),
        axis.ticks.y = element_line(),
        axis.title.x = element_text(size=10),
        axis.title.y = element_text(size=10),
        axis.text.y = element_text(size=10),
        axis.text.x.bottom=element_text(size=10),
        strip.text=element_text(size=10),
        plot.title=element_text(size=12, face="bold.italic"),
        plot.caption=element_text(size=10, face="italic"),
        legend.text=element_text(size=10),
        legend.title=element_blank())

ggsave("fig3.2 pandemic_line_plot.pdf",pandemic_line_plot,
       device=cairo_pdf,width = 7,height=6)

pandemic_line_plot_2 <- ggplot(pandemic_back, aes(x=pandem, y=civilsociety10avg)) + 
  geom_point() +
  stat_smooth(method = "lm", formula = y ~ poly(x, 2), level=0.95, colour="red") +
  geom_label_repel(aes(label = country_f),
                   box.padding   = 0.35, 
                   point.padding = 0.5,
                   segment.color = 'grey50') +
  theme_light() +
  labs(title="Figure 4.3 - Association Between Pandemic Response & Core Civil Society Index",
       x="pandemic response index\n<< less freedom-restricting, more freedom-restricting >>",
       y="core civil soceity index (10-year average, 2010-2019)\n<<less robust, more robust>>",
       caption="Source: Varieties of Democracy, Pandemic Backsliding, Ver. 2 (May 2020)\nSecond order (quadratic) polynomial fit",
       subtitle="Select Asia Pacific Countries, 2019") +
  theme(text=element_text(family="Times New Roman"),
        axis.text.x = element_text(size=10, angle = 45, hjust=1),
        strip.text.x = element_text(size=7.5),
        axis.ticks.x = element_line(),
        axis.ticks.y = element_line(),
        axis.title.x = element_text(size=10),
        axis.title.y = element_text(size=10),
        axis.text.y = element_text(size=10),
        axis.text.x.bottom=element_text(size=10),
        strip.text=element_text(size=10),
        plot.title=element_text(size=12, face="bold.italic"),
        plot.caption=element_text(size=10, face="italic"),
        legend.text=element_text(size=10),
        legend.title=element_blank())

ggsave("fig3.3 pandemic_line_plot_2.pdf",pandemic_line_plot_2,
       device=cairo_pdf,width = 7,height=6)
