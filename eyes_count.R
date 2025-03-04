#count pupil size and saccade parameters

'here is the exaplme to count saccade and pupil parameters. Data "_pushes" from DataViewer EyeLink contains:
- Recording sesion label - perticipant id;
- trial index;
- average saccade duration - average per trial;
- average saccade amplitude - average per trial;
- fixation count;
- pupil size mean;
- duration_fix_1 (_2, _3) - the duration of fixation stimuli presentation for 3 blocks;
- type_1 (_2, _3) - stimuli valence type for 3 blocks'

install.packages("stats")
install.packages("reshape2")

'sorting data to one column'
n <- nrow(data)

for (i in 1:n)
  if (data[i,2] > 200) {data[i,16] <- data[i,18]
  data[i,13] <- data[i,15]} else 
    if(data[i,2] > 100) {data[i,16] <- data[i,17]
    data[i,13] <- data[i,14]}

#Count relative duration: we count ratio between Fixation duration and the duration of fixation stimuli presentation'

data$ratio <- data$AVERAGE_FIXATION_DURATION / data$duration_fix_1

'create new table with mean ratio value for each participant for each stimuli type'
ms_RelDuration <- aggregate(ratio ~ RECORDING_SESSION_LABEL + type,
                            data = data, mean)

'single group table - create tables for research groups 1,2...'
RelDuration_group1 <- dcast(ms_RelDuration, RECORDING_SESSION_LABEL ~ type_1)
n <- nrow(RelDuration_group1)
group <- rep("group1", n)
RelDuration_group1 <- cbind(RelDuration_group1, group)

'combined table - create table for the whole sample'
Res_RelDuration <- rbind(RelDuration_group1, RelDuration_group2)
colnames(Res_RelDuration) <- c("Label","type1", "type2", "type3", "Group")

#count pupil size delta - difference between trial pupil size and pupil size baseline

base <- subset(data, select=c(RECORDING_SESSION_LABEL, INDEX, PUPIL_SIZE_MEAN, type))
pup <- subset(data, select=c(RECORDING_SESSION_LABEL, INDEX, PUPIL_SIZE_MEAN, type))
delta <- subset(data, select=c(RECORDING_SESSION_LABEL, INDEX, type))

delta$diff <- pup$PUPIL_SIZE_MEAN - base$PUPIL_SIZE_MEAN

'create new table with mean delta value for each participant for each stimuli type'
ms_Delta <- aggregate(diff ~ RECORDING_SESSION_LABEL + type,
                      data = delta, mean)

'single group table'
Delta_group1 <- dcast(ms_Delta, RECORDING_SESSION_LABEL ~ type)
n <- nrow(Delta_group1)
group <- rep("group1", n)
Delta_group1 <- cbind(Delta_group1, group)

'combined table'
Res_Delta<- rbind(Delta_group1, Delta_group2)
colnames(Res_Delta) <- c("Label","type1", "type2", "type3", "Group")

#расчет среднего значения количества фиксаций для каждого человека #

'count mean velocities for evry participant'
ms_Fix <- aggregate(FIXATION_COUNT ~ RECORDING_SESSION_LABEL + type,
                     data = data, mean)

'create a final table for one group'
Fixation_group1 <- dcast(ms_Fix, RECORDING_SESSION_LABEL ~ type)

'create table for research groups'
colnames(Fixation_group1) <- c("Label","type1", "type2", "type3")
n <- nrow(Fixation_group1)
c <- rep("group1", n)
Fixation_group1 <- cbind(Fixation_group1, c)

Res_Fixation_count <- rbind(Fixation_group1, Fixation_group2)

#count average saccade velocity
'file data from DataViewer EyeLink contains:
- Recording sesion label - perticipant id;
- trial index;
- current saccade average velocity - per each saccade in a trial;
- type_1 (_2, _3) - stimuli valence type for 3 blocks'

'trial average - count mean velocities for each trial'
ms_Velocity <- aggregate(CURRENT_SAC_AVG_VELOCITY ~ RECORDING_SESSION_LABEL+ TRIAL_INDEX + type,
                   data = data, mean)

'label average - count mean velocities for evry participant'
ms_Velocity <- aggregate(CURRENT_SAC_AVG_VELOCITY ~ RECORDING_SESSION_LABEL + type,
                   data = ms_Velocity, mean)

'create a final table for one group'
Velocity_group1 <- dcast(ms_Velocity, RECORDING_SESSION_LABEL ~ type)

'create table for research groups'
colnames(Velocity_group1) <- c("Label","type1", "type2", "type3")
n <- nrow(Velocity_group1)
c <- rep("group1", n)
Velocity_group1 <- cbind(Velocity_group1, c)

Res_Velocity_number <- rbind(Velocity_group1, Velocity_group2)
