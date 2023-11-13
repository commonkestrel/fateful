#: Control words for each possible control state.
#: Should be a max of 24 bits.
CONTENT = [0] * (1 << 11)

def main():
    wb("src/ctrl1.rom", map(lambda x: x & 0xFF, CONTENT))
    wb("src/ctrl2.rom", map(lambda x: (x >> 8) & 0xFF, CONTENT))
    wb("src/ctrl3.rom", map(lambda x: (x >> 16) & 0xFF, CONTENT))
        
def wb(file: str, content: [int]):
    with open(file, 'wb') as f:
        f.write(bytes(content))

if __name__ == "__main__":
    main()