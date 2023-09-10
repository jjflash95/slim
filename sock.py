import socket

# Define the server address and port
server_address = ('127.0.0.1', 8080)

# Create a socket object
client_socket = socket.socket(socket.AF_INET, socket.SOCK_STREAM)

try:
    # Connect to the server
    client_socket.connect(server_address)
    
    # Send data to the server
    message = "hello"
    client_socket.sendall(message.encode())
    
    # Receive and print the server's response
    response = client_socket.recv(1024)
    print("Received:", response.decode())
    
except ConnectionRefusedError:
    print("Connection refused. Make sure the server is running.")
except Exception as e:
    print("Error:", e)

finally:
    # Close the socket
    client_socket.close()
