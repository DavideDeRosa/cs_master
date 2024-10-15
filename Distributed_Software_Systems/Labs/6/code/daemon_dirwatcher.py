import sys 
import os 
import time
import daemon
import logging
from datetime import datetime

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
                if file_path not in files:
                    files.add(file_path)
                    time_added = datetime.fromtimestamp(os.path.getctime(file_path)).strftime('%d/%m/%Y %H:%M:%S')
                    logging.debug(f'[{time_added}] File: "{file}" added to the folder!')
        
        time.sleep(10)

if __name__ == '__main__':
    path = sys.argv[1]

    with daemon.DaemonContext() as context:
        dirwatcher(path)