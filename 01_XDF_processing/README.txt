SETUP

1) create virtual environment FLESH_TSPROCESS (we will use this environment for most of the processing steps)

conda create --name FLESH_TSPROCESS python=3.12.2

conda activate FLESH_TSPROCESS

2) install necessary packages

cd '\xxx\01_XDF_PROCESSING'

pip install -r requirements_general.txt

3) We need to also install package `shign`, but directly from the source

git clone https://github.com/KnurpsBram/shign
cd shign
pip install .

3) open xdf_workflow.ipynb






