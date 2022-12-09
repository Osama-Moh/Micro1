b = b''

with open("Brown2.bin", "rb") as file:
    while a := file.read(1):
        if a == b'\x01':
            b += b'\x06'
        else :
            b+=b'\x5A'            

with open("chess.bin", "wb") as file:
    file.write(b)