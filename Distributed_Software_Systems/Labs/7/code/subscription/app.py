from flask import Flask, request, jsonify
from cloudevents.http import from_http
import json
import os

# command: dapr run --app-id order-processor-sdk --resources-path ../components/ --app-port 6001 -- uvicorn app:app --port 6002 

app = Flask(__name__)

app_port = os.getenv('APP_PORT', '6002')

"""
Registers Dapr pub/sub subscriptions. 
"""
@app.route('/dapr/subscribe', methods=['GET'])
def subscribe():
    subscriptions = [{
        'pubsubname': 'orderpubsub',
        'topic': 'orders',
        'route': 'orders'
    }]
    print('Dapr pub/sub is subscribed to: %s' % json.dumps(subscriptions))
    return jsonify(subscriptions)

"""
Dapr system receives the subscription and prints it on the console.
"""
@app.route('/orders', methods=['POST'])
def orders_subscriber():
    event = from_http(request.headers, request.get_data())
    order_id = event.data.get('orderId')
    
    if order_id is None:
        return json.dumps({'success': False, 'error': 'orderId missing'}), 400, {'ContentType': 'application/json'}

    print('Subscriber received: orderId=%s, customerName=%s, amount=%s, status=%s' % (
        order_id,
        event.data.get('customerName'),
        event.data.get('amount'),
        event.data.get('status'),
    ), flush=True)

    return json.dumps({'success': True}), 200, {'ContentType': 'application/json'}

app.run(port=app_port)