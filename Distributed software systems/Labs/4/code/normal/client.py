import grpc
import greet_pb2
import greet_pb2_grpc

"""
Function that connects to the gRPC Server, sends a Greet request and prints the response
"""
def run(x):
    channel = grpc.insecure_channel('localhost:50051')
    stub = greet_pb2_grpc.GreetServiceStub(channel)
    response = stub.Greet(greet_pb2.GreetRequest(name=x))
    print("Server response:", response.message)

if __name__ == "__main__":
    x = input("What's your name? ")
    run(x)
