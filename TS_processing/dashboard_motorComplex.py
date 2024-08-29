import dash
from dash import dcc, html, Input, Output, State
import dash_bootstrap_components as dbc
import pandas as pd
import numpy as np
import plotly.express as px
import plotly.graph_objs as go
from sklearn.decomposition import PCA
from sklearn.preprocessing import StandardScaler
import matplotlib.pyplot as plt
from io import BytesIO
import base64

# Initialize the Dash app
app = dash.Dash(__name__, external_stylesheets=[dbc.themes.BOOTSTRAP])

# Layout of the app
app.layout = dbc.Container([
    dbc.Row([
        dbc.Col(html.H1("Motor Complexity Dashboard"), width=12)
    ]),
    dbc.Row([
        dbc.Col([
            dcc.Upload(
                id='upload-data',
                children=html.Div([
                    'Drag and Drop or ',
                    html.A('Select CSV File')
                ]),
                style={
                    'width': '100%',
                    'height': '60px',
                    'lineHeight': '60px',
                    'borderWidth': '1px',
                    'borderStyle': 'dashed',
                    'borderRadius': '5px',
                    'textAlign': 'center',
                    'margin': '10px'
                },
                multiple=False
            ),
            html.Div(id='output-data-upload')
        ], width=6)
    ]),
    dbc.Row([
        dbc.Col([
            html.H5("Body Complexity PCA"),
            dcc.Graph(id='body-complexity-graph')
        ], width=6),
        dbc.Col([
            html.H5("Arm Complexity PCA"),
            dcc.Graph(id='arm-complexity-graph')
        ], width=6)
    ]),
    dbc.Row([
        dbc.Col([
            html.H5("Video Display"),
            html.Video(id='video-player', controls=True, width="100%")
        ], width=12)
    ])
])

# Helper function to perform PCA
def get_PCA(df):

    # Step 1: Standardize the Data
    scaler = StandardScaler()
    standardized_data = scaler.fit_transform(df)
    
    # Step 2: Apply PCA
    pca = PCA()
    pca.fit(standardized_data)
    
    # Step 3: Explained Variance
    explained_variance = pca.explained_variance_ratio_
    cumulative_explained_variance = explained_variance.cumsum()

    # Step 4: Find the number of components that explain 80% and 95% variance
    n_components_for_80_variance = np.argmax(cumulative_explained_variance >= 0.8) + 1
    n_components_for_95_variance = np.argmax(cumulative_explained_variance >= 0.95) + 1
    
    # Step 5: Compute the slopes
    if n_components_for_80_variance > 1:
        slope_80 = (cumulative_explained_variance[n_components_for_80_variance-1] - cumulative_explained_variance[0]) / (n_components_for_80_variance - 1)
    else:
        slope_80 = cumulative_explained_variance[0]

    if n_components_for_95_variance > 1:
        slope_95 = (cumulative_explained_variance[n_components_for_95_variance-1] - cumulative_explained_variance[0]) / (n_components_for_95_variance - 1)
    else:
        slope_95 = cumulative_explained_variance[0]

    return n_components_for_80_variance, slope_80, n_components_for_95_variance, slope_95, cumulative_explained_variance

# Helper function to create a plot and encode it as a base64 string
def plot_cumulative_variance(cumulative_explained_variance, n_components_for_80_variance, n_components_for_95_variance):
    plt.figure(figsize=(8, 6))
    plt.plot(cumulative_explained_variance, marker='o', label='Cumulative Explained Variance')
    plt.axvline(x=n_components_for_80_variance-1, color='red', linestyle='--', label=f'80% Variance at {n_components_for_80_variance} components')
    plt.axvline(x=n_components_for_95_variance-1, color='green', linestyle='--', label=f'95% Variance at {n_components_for_95_variance} components')
    plt.xlabel('Number of Principal Components')
    plt.ylabel('Cumulative Explained Variance')
    plt.title('Cumulative Explained Variance by PCA')
    plt.grid(True)
    plt.legend()
    
    # Save plot to a BytesIO object and encode it to base64
    buffer = BytesIO()
    plt.savefig(buffer, format="png")
    buffer.seek(0)
    image_base64 = base64.b64encode(buffer.read()).decode('utf-8')
    plt.close()
    
    return f"data:image/png;base64,{image_base64}"

# Step 3: Parse Uploaded File and Update Graphs and Video
@app.callback(
    Output('body-complexity-graph', 'figure'),
    Output('arm-complexity-graph', 'figure'),
    Output('video-player', 'src'),
    Input('upload-data', 'contents'),
    State('upload-data', 'filename')
)
def update_graphs(contents, filename):
    if contents is None:
        return {}, {}, ""

    # Parse the uploaded file
    content_type, content_string = contents.split(',')
    df = pd.read_csv(BytesIO(base64.b64decode(content_string)))
    
    subdf = df.drop(columns=['time', 'TrialID'])

    # get rid of all cols that includes speed, acc or jerk
    subdf = subdf.loc[:,~subdf.columns.str.contains('speed|acc|jerk|angAcc|angJerk|angSpeed')]

    # list all the columns
    allcols = subdf.columns
    allcols

    # get only armcols
    armkeywords = ['arm', 'elbow', 'pro_sup', 'wrist']

    # get all columns that include the keywords
    armdf = subdf.loc[:, subdf.columns.str.contains('|'.join(armkeywords))]

    # list all the cols
    armcols = armdf.columns
    armcols

    # Assuming the file contains only one trial
    trialid = df['TrialID'].iloc[0]
    
    # Separate data for body and arms
    bodydf = df.loc[:, allcols]  # Replace `allcols` with actual body columns
    armdf = df.loc[:, armcols]   # Replace `armcols` with actual arm columns

    # Perform PCA on body data
    body_nComp_80, body_slope_80, body_nComp_95, body_slope_95, body_cum_variance = get_PCA(bodydf)
    body_image_src = plot_cumulative_variance(body_cum_variance, body_nComp_80, body_nComp_95)
    
    # Perform PCA on arm data
    arm_nComp_80, arm_slope_80, arm_nComp_95, arm_slope_95, arm_cum_variance = get_PCA(armdf)
    arm_image_src = plot_cumulative_variance(arm_cum_variance, arm_nComp_80, arm_nComp_95)
    
    # Placeholder for the video file URL - you should replace this with the correct path logic
    video_src = f"/Videos/{trialid}.avi"  # Ensure videos are stored in the correct path

    # Return the plots and video source
    body_fig = px.imshow(plt.imread(BytesIO(base64.b64decode(body_image_src.split(",")[1]))))
    arm_fig = px.imshow(plt.imread(BytesIO(base64.b64decode(arm_image_src.split(",")[1]))))
    
    return body_fig, arm_fig, video_src

# Run the app
if __name__ == '__main__':
    app.run_server(debug=True)
