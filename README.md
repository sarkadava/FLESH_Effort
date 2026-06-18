# Github repository to project When communication fails, physical effort increases but not to greater effect

![Multimodal animation](assets/multimodal_anim.gif)

This repository stores coding pipeline to process and analyze data associated with project "PWhen communication fails, physical effort increases but not to greater effect". This project investigates how people modulate their effort when they encounter communicative breakdowns in a referential game. The project is part of the [FLESH project](https://vicom.info/projects/on-the-flexibility-and-stability-of-gesture-speech-coordination-flesh-evidence-from-production-comprehension-and-imitation/).

This project has been preregistered as a two-phase preregistration. In [Phase I](https://osf.io/3nygq), we preregistered the data collection. In [Phase II](https://osf.io/8ajsg), we have preregistered the analysis plan, including the processing steps.

## Updates

[✅] Preregistration of data collection  <br>
[✅] Data collection completed  <br>
[✅] Preregistration of analysis and processing steps  <br>
[✅] Preprint published  <br>
[] Manuscript published  <br>
[] Data available at open access repository  <br>


## Overview

The pipeline consists of several processing and analysis steps, whereby each step works on the output of the previous step. However, they are build in modular way such that one can implement individual scripts for their own purposes.

You can browse through the pipeline as a [website](https://sarkadava.github.io/FLESH_Effort/).

The pipeline is divided into the following steps:

- Pre-processing: Processing recorded XDF files into workable trial formats
- Motion tracking: 3D pose and joint estimation using OpenPose, Pose2sim and OpenSim 
- Processing: Processing of motion, balance and acoustics into merged, workable csv files
- Computing concept similarity using ConceptNet word embeddings
- Extraction of effort-related features
- Statistical analysis: Modelling the effect of communicative attempt (H1) and answer similarity (H2) on effort

## Prerequisites

If you wish to use only some steps of the pipeline, you will find the prerequisites and installation guide in the respective folder.

If you wish to run the entire pipeline, you can follow the steps below. Note that this project mostly in Python, but implements also some steps in R. Note that, for example, Visual Studio Code allows one to run both Python and R scripts. Additionally, the workflow also depends on some external softwares such as [Praat](https://www.fon.hum.uva.nl/praat/). Refer to the software' documentations for installation.

To prevent any conflicts in dependencies, we recommend to follow our workflow of creating three virtual environments, one for general processing steps, and one for motion tracking. The following installation sets up both environments.

```bash
# 1 - Clone the Repository
git clone https://github.com/sarkadava/FLESH_Effort.git
cd FLESH_ContinuousBodilyEffort

# 2.1 - Create a FLESH_TSPROCESS Conda Environment (Recommended)
conda env create -f environment.yml

# 2.2 - Create FLESH_MTRACK Conda Environment (Recommended)
conda env create -f mt-environment.yml

# 3 - Add Both Conda Environments to Jupyter Notebook
conda activate FLESH_TSPROCESS
python -m ipykernel install --user --name=FLESH_TSPROCESS --display-name "Python (FLESH_TSPROCESS)"

conda activate FLESH_MTRACK
python -m ipykernel install --user --name=FLESH_MTRACK --display-name "Python (FLESH_MTRACK)"

# 4 - Run the Jupyter Notebook (Optional - You can also open the scripts in Visual Studio Code)
jupyter notebook
```

## How to cite

If you want to use and cite and part of the **coding pipeline**, cite:

[xxx]

If you want to cite the **paper**, cite:

[xxx]

## Contact

kadava[at]leibniz-zas[dot]de (Šárka Kadavá)
