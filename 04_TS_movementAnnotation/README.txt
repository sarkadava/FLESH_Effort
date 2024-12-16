SETUP

If you continue from folders 1-3, you can skip step 1) 

1) create virtual environment FLESH_TSPROCESS (we will use this environment for most of the processing steps)

conda create --name FLESH_TSPROCESS python=3.12.2

2) install necessary packages

conda activate FLESH_TSPROCESS

cd '\xxx\04_TS_movementAnnotation'

pip install -r requirements_general.txt

3) For interrater agreement, you will also need the EasyDIAG software

Download link: https://sourceforge.net/projects/easydiag/

4) Ready to work with 01_Classify_preparation.ipynb, 02_MovementClassifier.ipynb and 03_InterAgreement.ipynb




