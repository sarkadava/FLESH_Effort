
# Imports
import matplotlib.pyplot as plt
import numpy as np

import nidaqmx
from nidaqmx.stream_readers import AnalogMultiChannelReader
from nidaqmx import constants

from pylsl import StreamInfo, StreamOutlet, local_clock

import threading
import time
import pylsl


#check info about the different settings
#print(help(constants.TerminalConfiguration))
#data and other attributes defined here:
# |
# |  DEFAULT = <TerminalConfiguration.DEFAULT: -1>
# |
# |  DIFF = <TerminalConfiguration.DIFF: 10106>
# |
# |  NRSE = <TerminalConfiguration.NRSE: 10078>
# |
# |  PSEUDO_DIFF = <TerminalConfiguration.PSEUDO_DIFF: 12529>
# |
# |  RSE = <TerminalConfiguration.RSE: 10083> 

print(nidaqmx.__version__)
# Parameters
sampling_freq_in = 500  # in Hz
buffer_in_size = 50
bufsize_callback = buffer_in_size
buffer_in_size_cfg = round(buffer_in_size * 1)  # clock configuration
chans_in = 4  # set to number of active OPMs (x2 if By and Bz are used, but that is not recommended)

# Initialize data placeholders
buffer_out = np.zeros((chans_in, buffer_in_size))
buffer_in = np.zeros((chans_in, buffer_in_size))
buffer_in_list = [[0]*buffer_in_size for e in range(chans_in)]
seconds = 0
leftDown    = 0
leftUp      = 1
rightDown   = 2
rightUp     = 3


# Definitions of basic functions
def ask_user():
    global running
    input("Press ENTER/RETURN to stop acquisition and coil drivers.")
    running = False


def cfg_read_task(acquisition):  # uses above parameters
    acquisition.ai_channels.add_ai_voltage_chan("Dev2/ai0:%i" % (chans_in - 1),
                                                min_val=-5.0, max_val=5.0, terminal_config=constants.TerminalConfiguration.RSE, 
                                                units=constants.VoltageUnits.VOLTS)  # has to match with chans_in
    acquisition.timing.cfg_samp_clk_timing(rate=sampling_freq_in, sample_mode=constants.AcquisitionType.CONTINUOUS,
                                            samps_per_chan=buffer_in_size_cfg)


def reading_task_callback(task_idx, event_type, num_samples, callback_data):  # bufsize_callback is passed to num_samples
    global seconds
    global buffer_in
    global seconds
    global send_data
    
    if running:
        #stream_in.read_many_sample(buffer_in, num_samples, timeout=constants.WAIT_INFINITELY)
        stream_in.read_many_sample(buffer_in, buffer_in_size, timeout=constants.WAIT_INFINITELY)
        seconds = local_clock()
        send_data = True
        time.sleep(0.001)
        #print(1/(time.time()-seconds))
        #print(time.time() - seconds)
        # for i in range(len(buffer_in[0])):
        #     outlet.push_sample([np.round(buffer_in[leftDown][i],4), 
        #                         np.round(buffer_in[leftUp][i],4), 
        #                         np.round(buffer_in[rightDown][i],4), 
        #                         np.round(buffer_in[rightUp][i],4)])
        #data = np.append(data, buffer_in, axis=1)  # appends buffered data to total variable data

    return 0  # Absolutely needed for this callback to be well defined (see nidaqmx doc).


# Configure and setup the tasks
task_in = nidaqmx.Task()
cfg_read_task(task_in)
stream_in = AnalogMultiChannelReader(task_in.in_stream)
task_in.register_every_n_samples_acquired_into_buffer_event(bufsize_callback, reading_task_callback)

info = StreamInfo(name='BalanceBoard_stream', 
    type='COP', 
    channel_count=chans_in, 
    nominal_srate = sampling_freq_in,
    channel_format='float32', 
    source_id='BalanceBoard_stream')
# add info to channels
channels = info.desc().append_child("channels")
for c in ["left_up","left_down","right_up","right_down"]:
    chan = channels.append_child("channel")
    chan.append_child_value("name", c)
    chan.append_child_value("unit", "volts")
    chan.append_child_value("type", "DAQmx_Val_RSE")
outlet = StreamOutlet(info)  # Broadcast the stream. 

# Start threading to prompt user to stop
thread_user = threading.Thread(target=ask_user)
thread_user.start()

# Main loop
running = True
send_data = False
task_in.start()

while running:  # make this adapt to number of channels automatically
    if send_data:
        #outlet.push_chunk(buffer_in)
        #for i in range(len(buffer_in)):
        #    buffer_in[i] = np.round(buffer_in[i],4)

        buffer_in_transpose = np.transpose(buffer_in)
        buffer_in_transpose = buffer_in_transpose.tolist()
        #print(buffer_in_transpose)
        outlet.push_chunk(buffer_in_transpose,seconds)
        #outlet.push_chunk(buffer_in, timestamp = seconds)
        send_data = False
        time.sleep(0.001)
        #print(np.shape(buffer_in))
        #print(buffer_in.tolist())


# Close task to clear connection once done
task_in.close()
