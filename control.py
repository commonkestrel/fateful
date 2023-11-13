def main():
    blank = bytes([0] * (1 << 11))
    with open("src/ctrl1.rom", 'wb') as f:
        f.write(blank)
        
    with open("src/ctrl2.rom", 'wb') as f:
        f.write(blank)

if __name__ == "__main__":
    main()