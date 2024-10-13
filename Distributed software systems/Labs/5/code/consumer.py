import json
from confluent_kafka import Consumer, KafkaException

"""
Configuration variables for our Kafka ENV
"""
bootstrap_servers = 'localhost:9092'
topic_name = 'test-topic'

"""
Creation of the Consumer object
"""
consumer = Consumer({
    'bootstrap.servers': bootstrap_servers,
    'group.id': 'test-group',
    'auto.offset.reset': 'earliest'
})

consumer.subscribe([topic_name])

"""
Function that consumes the messages from the Kafka cluster, and then prints it
"""
def consume_messages():
    print("Starting Kafka consumer...\n")

    while True:
        msg = consumer.poll(1.0)

        if msg is None:
            continue

        if msg.error():
            raise KafkaException(msg.error())

        value = json.loads(msg.value().decode('utf-8')) if msg.value() is not None else None

        print(f"Consumed: {value}")

"""
Execution of the main function. If the user presses "CTRL-C", the execution is interrupted
"""
if __name__ == '__main__':
    try:
        consume_messages()
    except KeyboardInterrupt:
        print("Kafka Consumer interrupted")
    finally:
        consumer.close()