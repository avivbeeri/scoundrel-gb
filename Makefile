.PHONY: clean

scoundrel.gb: scoundrel.o
	@rgblink -n scoundrel.sym -o scoundrel.gb scoundrel.o
	@rgbfix -v --title="Scoundrel" -p 0xFF scoundrel.gb

scoundrel.o: scoundrel.asm tiles.bin *.inc
	@rgbasm -o scoundrel.o scoundrel.asm

tiles.bin: assets/tiles.png
	@rgbgfx -o tiles.bin assets/tiles.png

clean:
	rm scoundrel.sym scoundrel.o tiles.bin
