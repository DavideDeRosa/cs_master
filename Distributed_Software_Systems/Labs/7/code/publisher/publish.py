from dapr.clients import DaprClient
import json
import time
import logging

# command: dapr run --app-id checkout-sdk --resources-path ../components/ -- python publish.py

logging.basicConfig(level=logging.INFO)

"""
The client is created as a Dapr client. 
It creates and publishes the order, using the publish_event method from Dapr PubSub system.
"""
with DaprClient() as client:
    for i in range(1, 20):
        order = {
            'orderId': i,
            'customerName': f'Customer {i}',
            'amount': i * 10.0,
            'status': 'Pending'
        }
        try:
            result = client.publish_event(
                pubsub_name='orderpubsub',
                topic_name='orders',
                data=json.dumps(order),
                data_content_type='application/json',
            )
            logging.info('Published data: %s', json.dumps(order))
        except Exception as e:
            logging.error('Error publishing data: %s', str(e))
        
        time.sleep(1)