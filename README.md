# Github repository to project Putting in the Effort: Modulation of Multimodal Effort in Communicative Breakdowns during a Gestural-Vocal Referential Game

![Multimodal animation](multimodal_anim.gif)

## Overview

This repository stores coding pipeline to process and analyze data associated with project "Putting in the Effort: Modulation of Multimodal Effort in Communicative Breakdowns during a Gestural-Vocal Referential Game" (FLESH). This project investigates how people modulate their effort when they encounter communicative breakdowns in a referential game. The project is part of the FLESH project (ViCom, DFG).

This project has been preregistered as a two-phase preregistration. In [Phase I](), we preregistered the data collection. In [Phase II](), we have preregistered the analysis plan, including the processing steps.

### Updates

[✅] Preregistration of data collection  <br>
[✅] Data collection completed  <br>
[] Preregistration of analysis and processing steps  <br>
[] Preprint published  <br>
[] Manuscript published  <br>
[] Data available at open access repository  <br>

---

The pipeline consists of several processing and analysis steps, whereby each step works on the output of the previous step. However, they are build in modular way such that one can implement individual scripts for their own purposes.

You can browse through the pipeline as a [website](https://sarkadava.github.io/FLESH_ContinuousBodilyEffort/).

The pipeline is divided into the following steps:

- Pre-processing I: From XDF to raw files

- Motion tracking I: Preparation of videos
- Motion tracking II: 2D pose estimation via OpenPose
- Motion tracking III: Triangulation via Pose2sim
- Motion tracking IV: Modeling inverse kinematics and dynamics

- Processing I: Motion tracking and balance
- Processing II: Acoustics
- Processing III: Merging multimodal data

- Movement annotation I: Preparing training data and data for classifier
- Movement annotation II: Training movement classifier, and annotating timeseries data
- Movement annotation III: Computing interrater agreement between manual and automatic annotation

- Final merge: Merging timeseries with annotations

- Computing concept similarity using ConceptNet word embeddings
- Extraction of effort-related features

- Exploratory Analysis I: Using PCA to identify effort dimensions
- Exploratory Analysis II: Identifying effort-related features contributing to misunderstanding resolution

- Statistical analysis: Modelling the effect of communicative attempt (H1) and answer similarity (H2) on effort

## Prerequisites

If you wish to use only some steps of the pipeline, you will find the prerequisites and installation guide in the respective folder.

If you wish to run the entire pipeline, you can follow the steps below. Note that this project mostly in Python, but implements also some steps in R. Note that, for example, Visual Studio Code allows one to run both Python and R scripts. Additionally, the workflow also depends on some external softwares such as [Praat](https://www.fon.hum.uva.nl/praat/), [ELAN](https://archive.mpi.nl/tla/elan) and [EasyDIAG](https://sourceforge.net/projects/easydiag/). Refer to the softwares' documentations for installation.

To prevent any conflicts in dependencies, we recommend to follow our workflow of creating three virtual environments, one for general processing steps, one for pose2sim and one for OpenSim scripting. In the following installation, we will setup environment for general processing steps, but you can find the installation instructions for the other two environments in their respective folders (02_MotionTracking_processing).

```bash
# 1 - Clone the Repository
git clone https://github.com/sarkadava/FLESH_ContinuousBodilyEffort.git
cd FLESH_ContinuousBodilyEffort

# 2 - Create a FLESH_TSPROCESS Conda Environment (Recommended)
conda create --name FLESH_TSPROCESS python=3.12.2
conda activate FLESH_TSPROCESS

# 3 - Install Dependencies
pip install -r requirements_tsprocess.txt

# 4 - Add Conda Environment to Jupyter Notebook
pip install ipykernel
python -m ipykernel install --user --name=FLESH_TSPROCESS --display-name "Python (FLESH_TSPROCESS)"

# 5 - Run the Jupyter Notebook (Optional - You can also open the scripts in Visual Studio Code)
jupyter notebook
```

## How to cite

Kadavá, Š., Pouw, W., Fuchs, S., Holler, J., & Aleksandra , Ć. (2025). Putting in the Effort: Modulation of Multimodal Effort in Communicative Breakdowns during a Gestural-Vocal Referential Game (Version 1.0.0) [Computer software]. https://github.com/sarkadava/FLESH_ContinuousBodilyEffort

## Contact

kadava@leibniz-zas.de (Šárka Kadavá)