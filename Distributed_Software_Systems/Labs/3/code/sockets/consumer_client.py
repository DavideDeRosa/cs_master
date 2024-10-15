import socket
import json
import time
import random

# Server connection settings
HOST = 'localhost'
PORT = 12345

"""
Class that defines the Consumer client
"""
class ConsumerClient:
    def __init__(self, id: int) -> None:
        self.id = id
        self.socket = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
        self.socket.connect((HOST, PORT))

    def log(self, msg: str) -> None:
        print(f"\033[0;3{self.id + 1}m[Consumer {self.id}] {msg}\033[0m")

    """
    Function that handles the consuming of the resource from the shared queue
    """
    def consume(self):
        while True:
            data = {"type": "consumer"}
            self.socket.send(json.dumps(data).encode('utf-8'))

            response = self.socket.recv(1024).decode('utf-8')

            if response == 'queue_empty':
                self.log("Queue is empty. Waiting for new items...")
            else:
                item = json.loads(response)
                self.log(f"Consumed: {item}")

            time.sleep(random.uniform(1, 3))

if __name__ == "__main__":
    consumer_id = int(input("Enter Consumer ID: "))
    consumer = ConsumerClient(consumer_id)
    consumer.consume()