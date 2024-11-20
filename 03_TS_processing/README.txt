In folder *TS_PROCESSING* can be found all scripts and files that you need to extracting features

Folders:

- Animations: some examples of animations containing videos+timeseries
- answer_data: datasets with accuracy info, expressibility, similarity etc.
- Audio: 48kHz audio files for dyad0
- Datasets: datasets that result from extracting script (TS_extraction.ipynb)
- InterAg: files needed for Interrater Agreement (done in EasyDIAG)
- ManualAnno: manual ELAN annotations from Ola and Gillian
- MT_annotated: automatic annotations (done by TS_movementDetection.ipynb)
- Plots: plots
- TS_acoustics: timeseries for envelope, f0, soundgen features (file per trial)
- TS_formants: timeseries for f1-f5 from Praat script made by Ch. Carignan (file per trial)
- TS_merged: merged timeseries for motion, acoustics, etc. (file per trial) - if you download dyad0_merged from SharePoint, this is the folder
- TS_motiontracking: timeseries for movement (file per trial)
- TS_processing_files: some quarto stuff
- Videos: openpose videos for each trial (middle camera)

Scripts:

- BERT_similarity.ipynb: computes similarity between 2 words/concepts, process the similarity survey
- dashboard.py: plot features across corrections + correlation between 2 features
- Get_Speakers_register.praat: computes register for wav file
- MotorComplexity.ipynb: computes PCA to get motor complexity from inverse kinematics (joint angles)
- MT_animation.ipynb: creates animations (into folder Animations, using videos from Videos)
- Soundgen_analysis.Rmd: extract features from wav files using soundgen package
- TS_extraction.ipynb: extract features of effort
- TS_formants_comparsion.ipynb: compares Carignan's formants with formants from soundgen
- TS_movementDetection.ipynb: detects movement based on openpose markers and create ELAN annotations from it (into MT_annotated)
- TS_processing_ntb.ipynb: process all raw timeseries and finally merge them into one (into TS_merged)

