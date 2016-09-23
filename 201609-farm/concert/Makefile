all: short.mp4 track.m4a

short.mp4: src/*mp4 src/*mkv src/*webm short.rkt
	racket short.rkt

track.bin: track.rkt
	racket track.rkt

%.wav: %.bin
	ffmpeg -y -f s8 -ar 44100 -ac 2 -i $^ -ac 1 $@

%.m4a: %.wav
	ffmpeg -y -i $^ -c:a libfdk_aac -b:a 64k $@

clean:
	rm -f short/*mp4
