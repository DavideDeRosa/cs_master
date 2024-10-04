import socket
import json
import random
import time

# Server connection settings
HOST = 'localhost'
PORT = 12345

"""
Class that defines the Producer client
"""
class ProducerClient:
    def __init__(self, id: int) -> None:
        self.id = id
        self.socket = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
        self.socket.connect((HOST, PORT))

    def log(self, msg: str) -> None:
        print(f"\u001b[3{3 + self.id}m[Producer {self.id}] {msg}\033[0m")

    """
    Function that handles the creating of the resource in JSON format
    """
    def produce(self):
        while True:
            data = {
                "type": "producer",
                "producer_id": self.id,
                "value": random.randint(1, 100),
                "timestamp": time.time()
            }
            json_data = json.dumps(data)

            self.log(f"Sending data: {json_data}")
            self.socket.send(json_data.encode('utf-8'))

            time.sleep(random.uniform(0.5, 2))

if __name__ == "__main__":
    producer_id = int(input("Enter Producer ID: "))
    producer = ProducerClient(producer_id)
    producer.produce()