import os
import requests

LOCAL_FILE_DIRECTORY = './files'
API_URL = 'http://127.0.0.1:8000/files'

"""
Check if path exists, or create it
"""
if not os.path.exists(LOCAL_FILE_DIRECTORY):
    os.makedirs(LOCAL_FILE_DIRECTORY)

"""
filename is the name of the file which has to be shown on screen

The function chekcs if the file already exists on the local machine,
otherwise it will ask the server for it

The file will be printed on the console using the printFile function
"""
def searchFile(filename):
    local_file_path = os.path.join(LOCAL_FILE_DIRECTORY, filename)

    if os.path.exists(local_file_path):
        printFile(local_file_path, filename)
    else:
        try:
            response = requests.get(API_URL, params={'filename': filename})

            if response.status_code == 200:
                with open(local_file_path, 'wb') as file:
                    file.write(response.content)
                
                printFile(local_file_path, filename)
            elif response.status_code == 404:
                print(f"\nFile '{filename}' not found.\n")
        except requests.exceptions.ConnectionError:
            print(f"\nFile '{filename}' not found.\n")

"""
The function prints the file on the console
If it doesn't work, an exception is printed on the console
"""
def printFile(local_file_path, filename):
    try:
        with open(local_file_path, 'r') as file:
            print("\n" + file.read() + "\n")
    except Exception as e:
        print(f"\nError reading file '{filename}': {e}\n")

"""
Asks the user if it wants to see a file or quit
"""
if __name__ == "__main__":
    while(True):
        filename = input("Enter the filename (or 0 to cancel): ").strip()

        if filename == "0":
            break
        elif filename == "":
            print("\nYou have to write a filename!\n")
        else: 
            searchFile(filename)