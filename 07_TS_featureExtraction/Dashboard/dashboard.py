import base64
import io
import dash
from dash import dcc
from dash import html
from dash.dependencies import Input, Output
import plotly.express as px
import pandas as pd
import seaborn as sns
import matplotlib.pyplot as plt
import os

curfolder = os.getcwd()

# Create the Dash app
app = dash.Dash(__name__)

# Initialize with an empty DataFrame
df = pd.DataFrame()

# Directory to save the images
image_dir = "ImagesDash"
os.makedirs(image_dir, exist_ok=True)

# Layout of the dashboard
app.layout = html.Div([
    html.H1("Interactive Dashboard"),
    
    html.Div([
        html.Label("Upload CSV File:"),
        dcc.Upload(
            id='upload-data',
            children=html.Button('Upload CSV'),
            multiple=False
        ),
        html.Div(id='file-info'),
    ]),
    
    html.Div([
        html.Label("Select Feature 1:"),
        dcc.Dropdown(
            id='feature1-dropdown',
            options=[],  # options will be updated after file upload
            value=None
        ),
    ]),
    
    html.Div([
        html.Label("Select Feature 2:"),
        dcc.Dropdown(
            id='feature2-dropdown',
            options=[],  # options will be updated after file upload
            value=None
        ),
    ]),
    
    dcc.Graph(id='correlation-plot'),
    dcc.Graph(id='boxplot1'),
    dcc.Graph(id='boxplot2'),
])

# Function to remove outliers using Tukey's rule
def remove_outliers(df, feature):
    Q1 = df[feature].quantile(0.25)
    Q3 = df[feature].quantile(0.75)
    IQR = Q3 - Q1
    lower_bound = Q1 - 1.5 * IQR
    upper_bound = Q3 + 1.5 * IQR
    return df[(df[feature] >= lower_bound) & (df[feature] <= upper_bound)]

# Callback to handle file upload
@app.callback(
    [Output('file-info', 'children'),
     Output('feature1-dropdown', 'options'),
     Output('feature2-dropdown', 'options')],
    Input('upload-data', 'contents')
)
def update_data(contents):
    global df
    if contents is not None:
        content_type, content_string = contents.split(',')
        decoded = base64.b64decode(content_string)
        df = pd.read_csv(io.StringIO(decoded.decode('utf-8')))
        
        # Update dropdown options
        columns = df.columns
        options = [{'label': col, 'value': col} for col in columns if col not in ['modality', 'correction_info']]
        
        return (
            f'File uploaded: {df.shape[0]} rows, {df.shape[1]} columns',
            options,
            options
        )
    return 'Upload a CSV file', [], []

# Callback to update the correlation plot
@app.callback(
    Output('correlation-plot', 'figure'),
    Input('feature1-dropdown', 'value'),
    Input('feature2-dropdown', 'value')
)
def update_correlation_plot(feature1, feature2):
    if feature1 is None or feature2 is None or df.empty:
        return px.scatter()  # Return an empty plot if no features are selected or no data
    
    # Remove outliers
    df_filtered_feature1 = remove_outliers(df, feature1)
    # get rid of feature 2 from df_filtered_feature1
    df_filtered_feature1 = df_filtered_feature1.drop(feature2, axis=1)

    df_filtered_feature2 = remove_outliers(df, feature2)
    # get rid of feature 1 from df_filtered_feature2
    df_filtered_feature2 = df_filtered_feature2.drop(feature1, axis=1)

    # concatenate the two dataframes
    df_filtered = pd.concat([df_filtered_feature1, df_filtered_feature2], axis=1)

    # Debug: Print the columns after filtering
    print(f"Columns in DataFrame after filtering: {df_filtered.columns.tolist()}")

    print(df_filtered[feature1])
    print(df_filtered[feature2])

    # Check if features exist in the DataFrame
    if feature1 not in df_filtered.columns or feature2 not in df_filtered.columns:
        raise ValueError(f"One or both features '{feature1}' and '{feature2}' not found in DataFrame columns.")

    # Ensure that the features are not empty
    if df_filtered[feature1].empty or df_filtered[feature2].empty:
        raise ValueError(f"One or both features '{feature1}' and '{feature2}' are empty.")
    
    
    # Calculate the correlation
    correlation = df_filtered[feature1].corr(df_filtered[feature2])
    print(f"Correlation between {feature1} and {feature2}: {correlation:.2f}")
    
    # Plot
    fig = px.scatter(df_filtered, x=feature1, y=feature2, trendline="ols", marginal_y="histogram", marginal_x="histogram")
    fig.update_layout(title=f'Correlation between {feature1} and {feature2}')
    
    #Add correlation as an annotation
    fig.add_annotation(
        x=1, y=-0.15, xref='paper', yref='paper', showarrow=False,
        text=f'Correlation: {correlation:.2f}', font=dict(size=14)
    )
    
    return fig

# Callback to update the boxplot 1
@app.callback(
    Output('boxplot1', 'figure'),
    Input('feature1-dropdown', 'value')
)
def update_boxplot1(feature):
    if feature is None or df.empty:
        return px.violin()  # Return an empty plot if no feature is selected or no data
    
    # Remove outliers
    df_filtered = remove_outliers(df, feature)

    # order the df by column correction_info, in order c0_only, c0, c1, c2
    order = ['c0_only', 'c0', 'c1', 'c2']
    #df_filtered['correction_info'] = pd.Categorical(df_filtered['correction_info'], categories=order, ordered=True)
    
    fig = px.violin(df_filtered, x='modality', y=feature, color='correction_info', box=True, points="all")
    # order the boxes by the order list
    fig.update_xaxes(categoryorder='array', categoryarray=order)
    fig.update_layout(title=f'Boxplot of {feature} by Modality and Correction Info')
    return fig

# Callback to update the boxplot 2
@app.callback(
    Output('boxplot2', 'figure'),
    Input('feature2-dropdown', 'value')
)
def update_boxplot2(feature):
    if feature is None or df.empty:
        return px.violin()  # Return an empty plot if no feature is selected or no data
    
    # Remove outliers
    df_filtered = remove_outliers(df, feature)

    # order the df by column correction_info, in order c0_only, c0, c1, c2
    order = ['c0_only', 'c0', 'c1', 'c2']
    #df_filtered['correction_info'] = pd.Categorical(df_filtered['correction_info'], categories=order, ordered=True)
    
    fig = px.violin(df_filtered, x='modality', y=feature, color='correction_info', box=True, points="all")
    # order the boxes by the order list
    fig.update_xaxes(categoryorder='array', categoryarray=order)
    fig.update_layout(title=f'Boxplot of {feature} by Modality and Correction Info')
    return fig

# Run the app
if __name__ == '__main__':
    app.run_server(debug=True)
