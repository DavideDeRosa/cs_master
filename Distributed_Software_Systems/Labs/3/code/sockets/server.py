import socket
import threading
import json
from queue import Queue, Full, Empty

# Server configurations
HOST = 'localhost'
PORT = 12345

#Shared queue configurations
MAX_QUEUE_SIZE = 5
queue = Queue(MAX_QUEUE_SIZE)

"""
Class that defines the Server, which hosts the shared queue. We used sockets for the connection between the server and clients
"""
class Server:
    def __init__(self) -> None:
        self.server_socket = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
        self.server_socket.bind((HOST, PORT))
        self.server_socket.listen(5)
        print("Server started and listening on port", PORT)

    """
    Function that handles the connection with clients
    It recognizes which type of client is connecting (Producer, Consumer) and it handles that case
    When the connection is lost, or there is a JSON Error, an exception is being handled
    """
    def handle_client(self, client_socket, client_address) -> None:
        print(f"New connection from {client_address}")
        while True:
            try:
                message = client_socket.recv(1024).decode('utf-8')
                if not message:
                    break
                data = json.loads(message)
                client_type = data.get('type')

                if client_type == 'producer':
                    self.handle_producer(data)
                elif client_type == 'consumer':
                    self.handle_consumer(client_socket)

            except (ConnectionResetError, json.JSONDecodeError) as e:
                print(f"Connection lost from {client_address}: {e}")
                break

        client_socket.close()

    """
    Function that handles the case if a client is a Producer
    """
    def handle_producer(self, data) -> None:
        try:
            queue.put(data, timeout=1)  # Block for 1 second if full
            print(f"Produced: {data}")
        except Full:
            print(f"Queue is full. Producer {data['producer_id']} is waiting.")

    """
    Function that handles the case if a client is a Consumer
    """
    def handle_consumer(self, client_socket) -> None:
        try:
            item = queue.get(timeout=1)  # Block for 1 second if empty
            client_socket.send(json.dumps(item).encode('utf-8'))
            print(f"Consumed: {item}")
        except Empty:
            client_socket.send(b'queue_empty')
            print("Queue is empty. Consumer is waiting.")

    def start(self) -> None:
        while True:
            client_socket, client_address = self.server_socket.accept()
            client_handler = threading.Thread(target=self.handle_client, args=(client_socket, client_address))
            client_handler.start()

if __name__ == "__main__":
    server = Server()
    server.start()