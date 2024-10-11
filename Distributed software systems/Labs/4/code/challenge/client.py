import grpc
import greet_pb2
import greet_pb2_grpc

"""
Function that creates a loop to chat where the User can send messages to the Server
"""
def chat(stub, x):
    while(True):
        msg = input("Type your message here (0 to stop): ")

        if(msg == "0"):
            break
        
        msg_list = [greet_pb2.ChatMessage(sender=x, message=msg)]

        for response in stub.Chat(iter(msg_list)):
            print(f"Server: {response.message}")

"""
Function that defines the channel and the stub, before starting the chatting session
"""
def run(x):
    channel = grpc.insecure_channel('localhost:50051')
    stub = greet_pb2_grpc.GreetServiceStub(channel)
    chat(stub, x)

if __name__ == "__main__":
    x = input("What's your name? ")
    run(x)