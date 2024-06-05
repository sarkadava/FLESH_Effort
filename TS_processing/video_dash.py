import tempfile
import shutil
import glob
import base64
import tqdm
import cv2
import numpy as np
import pandas as pd
from scipy.signal import savgol_filter
import dash
from dash import dcc, html
from dash.dependencies import Input, Output, State
import dash_bootstrap_components as dbc
import plotly.graph_objects as go

# Define the window size in seconds
window = 4

# Function to plot pose and speed
def plot_pose_speed(body, speed, midpoint):
    fig = go.Figure()

    start = midpoint - window / 2
    end = midpoint + window / 2
    if start < 0:
        start = 0

    body = body[(body['Time'] >= start) & (body['Time'] <= end)]
    speed = speed[(speed['Time'] >= start) & (speed['Time'] <= end)]

    body['Time'] = body['Time'] - midpoint
    speed['Time'] = speed['Time'] - midpoint

    fig.add_trace(go.Scatter(x=speed['Time'], y=speed['RWrist_speed'], mode='lines', name='RWrist_speed', line=dict(width=8, color='green')))
    fig.add_trace(go.Scatter(x=speed['Time'], y=speed['LWrist_speed'], mode='lines', name='LWrist_speed', line=dict(width=8, color='blue')))

    fig.add_trace(go.Scatter(x=body['Time'], y=body['RWrist_z'], mode='lines', name='RWrist (vertical)', line=dict(width=4, color='magenta', dash='dot')))
    fig.add_trace(go.Scatter(x=body['Time'], y=body['LWrist_z'], mode='lines', name='LWrist (vertical)', line=dict(width=4, color='darkblue', dash='dot')))

    fig.add_vline(x=0, line=dict(color='red', width=4, dash='dash'))
    fig.add_hline(y=15, line=dict(color='red', width=4, dash='dash'))

    fig.update_layout(
        title="Pose and Speed Time Series",
        xaxis_title="Time (s)",
        yaxis_title="Value",
        font=dict(size=18)
    )

    return fig

app = dash.Dash(__name__, external_stylesheets=[dbc.themes.BOOTSTRAP])

app.layout = dbc.Container([
    dbc.Row([
        dbc.Col(html.H1("Motion Tracking Dashboard"), className="mb-2")
    ]),
    dbc.Row([
        dbc.Col(html.H6("Upload a video and corresponding CSV file for motion tracking."), className="mb-4")
    ]),
    dbc.Row([
        dbc.Col(dcc.Upload(id='upload-video', children=html.Div(['Drag and Drop or ', html.A('Select Video File')]), 
                          style={'width': '100%', 'height': '60px', 'lineHeight': '60px', 'borderWidth': '1px', 'borderStyle': 'dashed', 'borderRadius': '5px', 'textAlign': 'center'}, 
                          multiple=False), width=6),
        dbc.Col(dcc.Upload(id='upload-csv', children=html.Div(['Drag and Drop or ', html.A('Select CSV File')]), 
                          style={'width': '100%', 'height': '60px', 'lineHeight': '60px', 'borderWidth': '1px', 'borderStyle': 'dashed', 'borderRadius': '5px', 'textAlign': 'center'}, 
                          multiple=False), width=6)
    ]),
    dbc.Row([
        dbc.Col(html.Div(id='output-video-upload'), className="mb-4")
    ]),
    dbc.Row([
        dbc.Col(dcc.Graph(id='pose-speed-graph'), width=12)
    ])
])

def parse_contents(contents, filename):
    content_type, content_string = contents.split(',')
    decoded = base64.b64decode(content_string)
    return decoded

@app.callback(
    [Output('pose-speed-graph', 'figure'), Output('output-video-upload', 'children')],
    [Input('upload-video', 'contents'), Input('upload-csv', 'contents')],
    [State('upload-video', 'filename'), State('upload-csv', 'filename')]
)
def update_output(video_content, csv_content, video_filename, csv_filename):
    if video_content and csv_content:
        video_data = parse_contents(video_content, video_filename)
        csv_data = parse_contents(csv_content, csv_filename)

        temp_video = tempfile.NamedTemporaryFile(delete=False, suffix='.avi')
        temp_video.write(video_data)

        temp_csv = tempfile.NamedTemporaryFile(delete=False, suffix='.csv')
        temp_csv.write(csv_data)

        mt = pd.read_csv(temp_csv.name)
        cols = mt.columns[:-1]
        cols = [x.split('_')[0] for x in cols]
        colsforspeed = list(set(cols))
        colstosmooth = mt.columns[:-1]

        mt_smooth = pd.DataFrame()
        for col in colstosmooth:
            mt_smooth[col] = savgol_filter(mt[col], 15, 3)
            mt_smooth[col] = mt_smooth[col] * 100
        mt_smooth['Time'] = mt['Time']
        sr = 1 / np.mean(np.diff(mt['Time']))

        for col in colsforspeed:
            x = mt_smooth[col + '_x']
            y = mt_smooth[col + '_y']
            z = mt_smooth[col + '_z']
            mt_smooth[col + '_speed'] = np.insert(np.sqrt(np.diff(x) ** 2 + np.diff(y) ** 2 + np.diff(z) ** 2), 0, 0)
            mt_smooth[col + '_speed'] = mt_smooth[col + '_speed'] * sr
            mt_smooth[col + '_speed'] = savgol_filter(mt_smooth[col + '_speed'], 15, 3)

        speed = mt_smooth
        body = mt_smooth
        capture = cv2.VideoCapture(temp_video.name)
        frameWidth = int(capture.get(cv2.CAP_PROP_FRAME_WIDTH))
        frameHeight = int(capture.get(cv2.CAP_PROP_FRAME_HEIGHT))
        fps = capture.get(cv2.CAP_PROP_FPS)

        output_video = tempfile.NamedTemporaryFile(delete=False, suffix='.mp4')
        fourcc = cv2.VideoWriter_fourcc(*'mp4v')
        out = cv2.VideoWriter(output_video.name, fourcc, fps, (frameWidth * 4, frameHeight))
        frame_number = 0

        for i in tqdm.tqdm(range(int(capture.get(cv2.CAP_PROP_FRAME_COUNT)))):
            ret, frame = capture.read()
            if ret:
                img = plot_pose_speed(body, speed, frame_number / fps)
                img = cv2.resize(img, (frameWidth * 3, frameHeight))
                frame = np.concatenate([frame, img], axis=1)
                out.write(frame)
                frame_number += 1
            else:
                break

        capture.release()
        out.release()

        video_player = html.Video(src=f"data:video/mp4;base64,{base64.b64encode(open(output_video.name, 'rb').read()).decode()}", controls=True)

        return plot_pose_speed(body, speed, frame_number / fps), video_player

    return go.Figure(), html.Div("Please upload both video and CSV files.")

if __name__ == '__main__':
    app.run_server(debug=True)
