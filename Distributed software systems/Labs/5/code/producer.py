import json
from confluent_kafka import Producer
from time import sleep
from datetime import datetime
import random

"""
Configuration variables for our Kafka ENV
"""
bootstrap_servers = 'localhost:9092'
topic_name = 'test-topic'

"""
Creation of the Producer object
"""
producer = Producer({'bootstrap.servers': bootstrap_servers})

"""
Function that reports if the message is delivered or not
"""
def delivery_report(err, msg):
    if err is not None:
        print(f"Message delivery failed: {err}")
    else:
        print(f"Message delivered to {msg.topic()} [{msg.partition()}]")

"""
Function that produces a message every 5 seconds. The message is sent to the Kafka cluster
"""
def produce_messages():
    print("Starting Kafka producer...\n")

    while True:
        message = {
            'timestamp': datetime.now().strftime('%H:%M:%S'),
            'content': random.randint(0, 100)
        }

        producer.produce(topic_name, value=json.dumps(message), callback=delivery_report)
        print(f'[{message["timestamp"]}] Message sent: {message["content"]}')
        
        producer.poll(0)
        sleep(5)

"""
Execution of the main function. If the user presses "CTRL-C", the execution is interrupted
"""
if __name__ == '__main__':
    try:
        produce_messages()
    except KeyboardInterrupt:
        print("Kafka Producer interrupted")
    finally:
        producer.flush()