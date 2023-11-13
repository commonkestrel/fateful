BLANK = bytes([0] * (1 << 11))

def main():
    blank("src/ctrl1.rom")
    blank("src/ctrl2.rom")
    blank("src/ctrl3.rom")
        
def blank(file: str):
    with open(file, 'wb') as f:
        f.write(BLANK)

if __name__ == "__main__":
    main()