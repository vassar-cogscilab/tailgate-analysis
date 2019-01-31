library(dplyr)
library(readr)
library(ggplot2)

all.data <- read_csv('data/tailgate-data.csv')
all.data$block <- as.numeric(as.character(all.data$block))
all.data$rt <- as.numeric(as.character(all.data$rt))
all.data$srt_trial_index <- as.numeric(as.character(all.data$srt_trial_index))
all.data$srt_sequence_index <- as.numeric(as.character(all.data$srt_sequence_index))

# number of subjects in the dataset

length(unique(all.data$prolific_pid))

# extract testing data

test.data <- all.data %>% filter(trial_type == 'unity-loader')

# first attempt on each item ONLY

first.attempts <- test.data %>% group_by(prolific_pid, block) %>% distinct(srt_trial_index, .keep_all = TRUE)

# list of subjects

unique(test.data$prolific_pid)

# import Roboto

library(extrafont)
font_import(pattern="Roboto")
loadfonts(device="win")

# plot average response time by repetition

first.attempts %>% filter(was_input_correct == 1) %>% group_by(prolific_pid, block) %>% summarize(rt = mean(rt)) %>% ungroup() %>%
  group_by(block) %>% summarize(rt.mean = mean(rt), sd = sd(rt), n = n(),  se = sd(rt)/sqrt(n())) %>%
  ggplot(aes(x=block, y=rt.mean, ymax= rt.mean + se, ymin = rt.mean - se)) +
  geom_smooth(color = "#F49661", size = 2, se = FALSE, method = 'loess') +
  geom_errorbar(width=0.5, size=1) +
  geom_point(size=3) + 
  scale_x_continuous(breaks = c(1,10,20,30)) + 
  labs(x = "\nAttempt Number", y = "Response Time (ms)\n")+
  theme_bw(base_size = 30, base_family = "Roboto")

# look at sequence by subject

first.attempts %>% filter(was_input_correct == 1) %>%
  ggplot(aes(x=block, y=rt, color = prolific_pid, group=prolific_pid))+
  facet_wrap(~srt_sequence_index) +
  geom_point()+
  geom_smooth(se=FALSE)

# look at attempt by subject

which.subject <- "5b96b5df57dc8100012bef63" # "5b9581133b90af00014e3cb2" # good subject -> "5b31c0811d2f710001fb83db"

first.attempts %>% filter(was_input_correct == 1, prolific_pid == which.subject) %>%
  ggplot(aes(x=srt_sequence_index, y=rt))+
  scale_x_continuous(breaks=c(3,6,9,12))+
  coord_cartesian(ylim=c(0,1000))+
  facet_wrap(~block)+
  geom_line()+
  geom_point()+
  geom_vline(data = first.attempts %>% filter(was_input_correct == 0, prolific_pid == which.subject), mapping = aes(xintercept=srt_sequence_index), color = "#012BFF")+
  labs(x="\nSequence Location", y="Response Time (ms)\n")+
  theme_minimal(base_size=18, base_family = "Roboto")+
  theme(axis.title = element_text(size=30))

