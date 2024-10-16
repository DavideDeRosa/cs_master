import sys 
import os 
import time
import daemon
import logging
from datetime import datetime

"""
Function that, after setting up the logger, runs in a loop that every 10 seconds checks the directory for new files
If found, a line is written inside the log file
"""
def dirwatcher(path):
    files = set() 

    logging.basicConfig(
        filename=path + '_logs.log',
        encoding='utf-8', 
        level=logging.DEBUG,
        format='%(message)s'
    )
    
    while True: 
        for file in os.listdir(path):
            file_path = os.path.join(path, file)

            if os.path.isfile(file_path):
                if file_path not in files: # if the file was not in the set, it is added and logged
                    files.add(file_path)
                    time_added = datetime.fromtimestamp(os.path.getctime(file_path)).strftime('%d/%m/%Y %H:%M:%S')
                    logging.debug(f'[{time_added}] File: "{file}" added to the folder!')
        
        time.sleep(10)

"""
Execution of the main function
The directory to watch is passed as an argument when executing the script
The whole script is launched as a Daemon in the background
"""
if __name__ == '__main__':
    path = sys.argv[1]

    with daemon.DaemonContext() as context:
        dirwatcher(path)