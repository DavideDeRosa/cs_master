from flask import Flask, request, jsonify, make_response
from flask_sqlalchemy import SQLAlchemy
from os import environ
from sqlalchemy import text

app = Flask(__name__)
app.config['SQLALCHEMY_DATABASE_URI'] = environ.get('DATABASE_URL')
db = SQLAlchemy(app)

class Order(db.Model):
    __tablename__ = 'orders'

    id = db.Column(db.Integer, primary_key=True)
    product_name = db.Column(db.String(120), nullable=False)
    quantity = db.Column(db.Integer, nullable=False)
    user_id = db.Column(db.Integer, nullable=False)
    
    def json(self):
        return {
            'id': self.id,
            'product_name': self.product_name,
            'quantity': self.quantity,
            'user_id': self.user_id
        }

with app.app_context():
    db.create_all()

def user_exists(user_id):
    query = text("SELECT 1 FROM users WHERE id = :user_id LIMIT 1")
    result = db.session.execute(query, {'user_id': user_id})
    return result.fetchone() is not None

@app.route('/orders', methods=['POST'])
def create_order():
    try:
        data = request.get_json()

        if not user_exists(data['user_id']):
            return make_response(jsonify({'message': 'user not found'}), 404)

        new_order = Order(
            product_name=data['product_name'],
            quantity=data['quantity'],
            user_id=data['user_id']
        )
        db.session.add(new_order)
        db.session.commit()
        return make_response(jsonify({'message': 'order created'}), 200)
    except Exception as e:
        return make_response(jsonify({'message': 'error creating order', 'error': str(e)}), 500)

@app.route('/orders/<int:id>', methods=['GET'])
def get_order(id):
    try:
        order = Order.query.filter_by(id=id).first()
        if order:
            return make_response(jsonify({'order': order.json()}), 200)
        return make_response(jsonify({'message': 'order not found'}), 404)
    except Exception as e:
        return make_response(jsonify({'message': 'error getting order', 'error': str(e)}), 500)

@app.route('/orders', methods=['GET'])
def list_orders():
    try:
        orders = Order.query.all()
        if orders:
            return make_response(jsonify({'orders': [order.json() for order in orders]}), 200)
        return make_response(jsonify({'message': 'no orders found'}), 404)
    except Exception as e:
        return make_response(jsonify({'message': 'error fetching orders', 'error': str(e)}), 500)

if __name__ == '__main__':
    app.run(host='0.0.0.0', port=5001)