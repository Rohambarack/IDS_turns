# Project content overview
## cleaning
The raw data is not commited

### clean_v2
The cleaning script merges different participant data files and extracts relevant columns. 
Participants with missing data are removed. Participant turns are numbered for each visit.

The data is used for step_0 and step_2 scripts
### prepare turn data
Data preparation for step_3. Turns with possible noise related 0s in pitch variance and articulation rate are turned to NA.
This is done separately in each step_0 and step_2 model script, but for conviniance was done directly on the data for step_3.

The data is separated by participant and visit. Articulation data for each turn is marked as t,
while the components of the following turn are added to the dataframe, marked with t1.

For example:

Ar_t_same: Articlation rate for Interlocutor "A" at a given turn.

Ar_t_other: Articulation rate for Interlocutor "B", following and probably in response to Ar_t_same

Ar_t1_same: Articulation rate for Interlocutor "A", following and probably in response to Ar_t_other

The abbreviations in the data are: Ar = Articulation rate, Median = pitch median, Iqr = pitch interquantile range, Pc = pause counts, Pd = pause durations

Data is normalized for children and parent interlocutor separately for each visit and each participant

## data
Output folder for the cleaning scripts input for the analysis scripts

## step_0_NO_IND_DIFF
Modelling articulation differences without adding individually measured motor skills, linguistic ability or socialisation.
Articulation rate, pitch median and pitch iqr are modelled as lognormally distributed, pause count as a hurdle poisson, and pause duration as a hurdle
gamma distribuiton.

Only child directed speech is modeled, so only Parent interlocutors are kept in the data.
### model
script outputs resulting in vocal component models. 
### scripts
scripts for creating the models.
### plots
result visualization help.

## step_2
The same as step_0, but with added predictors of individual motor, linguistic and social measures.

## step_3 child and parent
step_3 separates the child and parent interlocutors. Turn based, vocal components are modeled at t1, base on the previous turn by the other interlocutor and the same interlocutor
at turn t.
