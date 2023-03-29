#######################################
# strength and conservatism
# study 4
# muscularity, fat, and conservatism
#######################################

pckgs <- c('lme4', 'lmerTest', 'rstatix', 'haven', 'psych', 'car', 'tidyverse', 
           'ggcorrplot', 'effectsize', 'ggpubr', 'afex', 'emmeans')

for (i in 1:length(pckgs)) {
  if (!(pckgs[[i]] %in% installed.packages())) {
    install.packages(pckgs[[i]])
  }
  lapply(pckgs[[i]], library, character.only = T)
}


df <- read_sav('study 4/Body Composition Politics.sav')

colnames(df)

# demographics
df %>%
  get_summary_stats(Age, type = 'mean_sd')

df %>%
  count(Race) %>%
  mutate(prop = 100* (n / sum(n)))

df %>%
  count(Sex) %>%
  mutate(prop = 100 * (n / sum(n)))

df %>%
  get_summary_stats(Politics, type = 'mean_sd')


df <- df %>% filter(`filter_$` == 1)


# select columns and reshape
df_long <- df %>%
  select(
    MTurkCode,
    HFSMFiscal:LFLMSTR
  ) %>%
  pivot_longer(
    cols = HFSMFiscal:LFLMSTR,
    names_to = 'stims',
    values_to = 'rating'
  ) %>%
  separate(col = 'stims', into = c('stims', 'item'), sep = 4) %>%
  separate(col = 'stims', into = c('fat', 'muscle'), sep = 2) %>%
  mutate(fat = ifelse(fat == 'HF', 'high', 'low'),
         muscle = ifelse(muscle == 'SM', 'small', 'large'))
         # fat_c = ifelse(fat == 'high', 1, -1),
         # mus_c = ifelse(muscle == 'large', 1, -1),
         # con_c = case_when(
         #   item == 'Fiscal' ~ 1,
         #   item == 'Social' ~ -1,
         #   item == 'STR' ~ 0
         # ))

str_mod <- aov_4(rating ~ fat * muscle + (fat * muscle|MTurkCode), data = filter(df_long, item == 'STR'))
str_mod


df_main <- df_long %>% filter(item != 'STR')

mod_1 <- aov_4(rating ~ fat * muscle * item + (fat * muscle * item|MTurkCode), data = df_main, anova_table = list(es = 'pes'))
mod_1


plot_summ <- df_main %>%
  group_by(fat, muscle, item) %>%
  get_summary_stats(rating, type = 'mean_ci')


item_labs <- c('Fiscal Conservatism', 'Social Conservatism')
names(item_labs) <- c('Fiscal', 'Social')


df_main %>%
  ggplot(aes(fat, rating, fill = muscle)) +
  geom_point(alpha = .25, size = .5, position = position_jitterdodge(.15, .15, .9), shape = 4) +
  geom_violin(alpha = .75, color = 'black', position = position_dodge(.9)) +
  geom_point(data = plot_summ, aes(fat, mean),
             alpha = .8, shape = 7, size = 3, position = position_dodge(.9), color = '#ffffff') +
  geom_errorbar(data = plot_summ, aes(x = fat, y = mean,
                                      ymin = mean - ci, ymax = mean + ci),
                alpha = .7, width = .15, position = position_dodge(.9), color = '#ffffff') +
  theme_classic() +
  facet_wrap(~ item, labeller = labeller(item = item_labs)) +
  labs(x = '',
       y = 'Conservatism Perception',
       fill = '',
       color = '') +
  scale_color_manual(values = c('#0b2265', '#a71930'),
                     labels = c('Large\nMuscle', 'Small\nMuscle')) +
  scale_fill_manual(values = c('#0b2265', '#a71930'),
                    labels = c('Large\nMuscle', 'Small\nMuscle')) +
  scale_x_discrete(labels = c('High Body\nFat', 'Low Body\nFat')) +
  scale_y_continuous(breaks = seq(1, 7, 1)) +
  theme(legend.position = 'bottom')


ggsave('study 4 conservatism plot.jpg', device = 'jpeg', path = 'study 4', units = 'cm')

