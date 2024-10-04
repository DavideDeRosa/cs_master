from queue import Queue
import json
import threading
import random
import time

#Declaring a lock which will allow us to manage concurrency between threads
mutex = threading.Lock()

#Declaring a shared queue for producers and consumers
MAX_QUEUE_SIZE = 5
queue = Queue(MAX_QUEUE_SIZE)

"""
Class which defines the Producer
"""
class Producer():
    def __init__(self, id: int) -> None:
        self.id = id

    """
    Functions which makes the logging of this producer different from others
    """
    def log(self, msg: str) -> None:
        print(f"\u001b[3{3 + self.id}m[Producer {self.id}] " + msg + "\033[0m")
    
    """
    Functions which waits for the lock before accessing the shared queue. When the lock is obtained, it puts a random generated object inside the queue if it is not full
    """
    def produce(self):
        while True:
            data = {
                "producer_id": self.id,
                "value": random.randint(1, 100),
                "timestamp": time.time()
            }

            json_data = json.dumps(data)

            time.sleep(random.uniform(0.5, 2))

            self.log(f"Waiting on Lock.")

            with mutex:
                self.log(f"Lock acquired.")

                if not queue.full():
                    queue.put(json_data)
                    self.log(f"Producer {self.id} produced: {json_data}")
                else:
                    self.log(f"Queue is full. Producer {self.id} is waiting.")

"""
Class which defines the Consumer
"""
class Consumer():
    def __init__(self, id: int) -> None:
        self.id = id
    
    """
    Functions which makes the logging of this consumer different from others
    """
    def log(self, msg: str) -> None:
        print(f"\033[0;3{self.id + 1}m[Consumer {self.id}] " + msg + "\033[0m")

    """
    Functions which waits for the lock before accessing the shared queue. When the lock is obtained, it looks for an object and if present it consumes it
    """
    def consume(self):
        while True:
            self.log(f"Waiting on Lock.")

            with mutex:
                self.log("Lock acquired.")

                if not queue.empty():
                    json_data = queue.get()

                    data = json.loads(json_data)

                    self.log(f"Consumer {self.id} consumed: {data}")

                    queue.task_done()
                else:
                    self.log(f"Queue is empty. Consumer {self.id} is waiting.")

            time.sleep(random.uniform(1, 3))

"""
First mode of execution, where 2 producers and 1 consumer are running concurrently
"""
def start1():
    producer1 = Producer(1)
    producer2 = Producer(2)

    consumer1 = Consumer(1)

    producer_thread1 = threading.Thread(target=producer1.produce)
    producer_thread2 = threading.Thread(target=producer2.produce)

    consumer_thread1 = threading.Thread(target=consumer1.consume)

    producer_thread1.start()
    producer_thread2.start()
    consumer_thread1.start()

    time.sleep(10)

"""
Second mode of execution, where 3 producers and 2 consumers are running concurrently
"""
def start2():
    producer1 = Producer(1)
    producer2 = Producer(2)
    producer3 = Producer(3)

    consumer1 = Consumer(1)
    consumer2 = Consumer(2)

    producer_thread1 = threading.Thread(target=producer1.produce)
    producer_thread2 = threading.Thread(target=producer2.produce)
    producer_thread3 = threading.Thread(target=producer3.produce)

    consumer_thread1 = threading.Thread(target=consumer1.consume)
    consumer_thread2 = threading.Thread(target=consumer2.consume)

    producer_thread1.start()
    producer_thread2.start()
    producer_thread3.start()
    consumer_thread1.start()
    consumer_thread2.start()

    time.sleep(10)

"""
Starting point of the program, where you can choose which mode you want to execute
"""
if __name__ == "__main__":
    x = input("Mode 1 or 2: ")
    
    if x == "1":
        start1()
    elif x == "2":
        start2()
    else:
        print("You need to choose a correct mode!")