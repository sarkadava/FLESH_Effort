SETUP

If you continue from folders 1-3, you can skip step 1-2

1) create virtual environment FLESH_TSPROCESS (we will use this environment for most of the processing steps)

conda create --name FLESH_TSPROCESS python=3.12.2

2) - Add Conda Environment to Jupyter Notebook
pip install ipykernel
python -m ipykernel install --user --name=FLESH_TSPROCESS --display-name "Python (FLESH_TSPROCESS)"

3) install necessary packages

conda activate FLESH_TSPROCESS

cd '\xxx\04_TS_movementAnnotation'

pip install -r requirements_general.txt

4) For interrater agreement, you will also need the EasyDIAG software

Download link: https://sourceforge.net/projects/easydiag/

5) Ready to work with 01_Classify_preparation.ipynb, 02_MovementClassifier.ipynb and 03_InterAgreement.ipynb




