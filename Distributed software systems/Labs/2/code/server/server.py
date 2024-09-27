import os
from flask import Flask, request, send_from_directory, abort

app = Flask(__name__)

FILE_DIRECTORY = './files'

"""
Using Flask, we expose a GET HTTP method, which asks for a filename in the query and gives back a file, if it exists
"""
@app.route('/files', methods=['GET'])
def files():
    filename = request.args.get('filename')
    
    if not filename:
        return {"error": "Filename is required."}, 400
    
    file_path = os.path.join(FILE_DIRECTORY, filename)
    
    if os.path.exists(file_path):
        return send_from_directory(FILE_DIRECTORY, filename, as_attachment=True)
    else:
        return {"error": "File not found."}, 404

if __name__ == '__main__':
    if not os.path.exists(FILE_DIRECTORY):
        os.makedirs(FILE_DIRECTORY)
    
    app.run(port=8000, debug=True)