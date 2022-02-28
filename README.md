# Rabies shows how scale of transmission can enable acute infections to persist at low prevalence
Authors: Rebecca Mancy, Malavika Rajeev, Ahmed Lugelo, Kirstyn Brunker, Sarah Cleaveland, Elaine A. Ferguson, Karen Hotopp, Rudovick Kazwala, Matthias Magoto, Kristyna Rysava, Daniel T. Haydon, Katie Hampson
 
This repository contains all the code and de-identified data in the paper by Mancy et al. 2022.

Geographic masking was used to de-identify the spatial data on rabies transmission (which is mostly localized to households of either dog owners or persons bitten by rabid animals), by jittering XY locations within a 1km radius. Running the transmission tree code on these jittered data will therefore not generate precisely the same inferred progenitors as from the raw data because the very localized spatial structure is masked. The resulting consensus trees are provided from the original raw data as are the distances between cases and to contacts and the inferred step lengths of rabid animals.

For more information: katie.hampson@glasgow.ac.uk
