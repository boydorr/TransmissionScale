# Rabies simulator in Java

The code in this folder is that used to simulate rabies dynamics in the context of the manuscript.

The code is provided for replication purposes only. It was written to establish the most important 
processes of rabies transmission and dynamics, and thus explicitly simulates processes that have
been found to be unnecessary.

Individuals wishing to simulate rabies dynamics should contact the corresponding author, Katie Hampson, for more 
efficient code that has been developed on the basis of the findings from this work.

# Overview of workflow for Java code
The Java code works using a folder structure akin to that in Rabies\_Sim\_Output\_Test, which is provided
as a prototype for replication purposes. 

## Folder structure
The folder Rabies\_Sim\_Output\_Test contains several subfolders.

1. static\_files, which holds information to be read in by the simulator on, for each spatial scale 
(indexed as the length of the side of the grid cell in metres): 
(1) the simulation grids in Well Known Text format (in order of popID); 
(2) population information, incremented monthly (numbers in the top row indicate days from the start of the simulation);
(3) vaccination data by population id at the given scale, date and number of dogs vaccinated (ignore auName, which is deprecated).
The folder also contains the vaccination data at 1/3 the measured levels at the 1km^2 scale, used in the experiments under
reduced vaccination coverage. It also contains an initial cases file (d\_initial\_cases\_java.csv)
(intentionally empty because not used for the version of the code for the paper for which all exogenous cases 
are included as "incursions") and the file d\_inc\_inf\_with\_zeros.csv which contains the serial intervals 
(mislabelled as generation interval) and infectious periods from the data (see Supplementary Materials) 
which are drawn from during simulation.

2. matching\_data, which contains d\_cases\_java.csv, the caselist from the data. This file is used 
by Postprocessor, which runs the postprocessing required prior to the ABC step (which is conducted in R code).

3. Round0\_1000. This folder is the main folder into which the code runs. The name of this folder
gives the round number of the ABC step (Round0 originally used to generate information on the density at which cases occurred, 
Round1 for the 50k runs under uniform distributions, Round2 for the "reliability" runs, 
and Round3 for the experiments under the posterior distribution). 
To run the Java code, it needs to contain the following subfolders (some of which are not picked up by GitHub because 
they are initially empty, and may need to be generated manually before running the code):
incursions, input\_files, output\_files, postprocessing. The folder incursions is pre-populated with the 
de-identified version of the incursions; the other folders (if they exist) are empty. The other file,
Round0\_Priors\_1000.csv contains the priors for the particular set of runs, and needs to be named
using the information from the folder name (here, this is the round number and spatial scale). 
These consist of the shape and scale of the handling time (Th) and the mean of the search 
time (here called "discovery time", Td).

## Run sequence

The Java code should be run in the following sequence.

1. Execute main in Initialise. This generates input files in
the folder input\_files, consisting .properties files containing the parameters (one per run). 
2. Execute the main method in Main. This runs the epidemics (one per run), initialised with the incursions in static\_files, and
writes one caselist file per run into output\_files, as well as a summary of all runs into a file
called epi\_outcomes\_001\_XXX.csv (where XXX is the run number of the last run). 
3. Execute main in Postprocessor. This generates summary files, including those used to 
conduct the ABC step. 

Parameters which are not expected to change between runs are contained in EpidemicProperties.java,
while other parameters can be edited in Initialise, Main and Postprocessor (see comments in code).

Note that some of the grids are large and the code will only run provided sufficient
memory is available on the machine being used.


