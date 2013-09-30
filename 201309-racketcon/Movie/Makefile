all: clean imgs

Clip.mp4: Original.mp4
	ffmpeg -ss 00:00:28.0 -i $^ -acodec copy -vcodec copy $@

Crop.mp4: Clip.mp4
	# 256x224
	ffmpeg -i $^ -acodec copy -filter:v "crop=256:140:0:84" $@

clean:
	rm -fr Clip.mp4 Crop.mp4 imgs

fsdisplay: Crop.mp4
	mplayer -fs $^

pdisplay: Crop.mp4
	mplayer -geometry x50:40+50+50 $^

imgs: Crop.mp4
	mkdir imgs
	ffmpeg -i $^ -r 1 -f image2 imgs/%05d.png
	rm imgs/00001.png imgs/00002.png imgs/00003.png imgs/00004.png imgs/00005.png imgs/00121.png imgs/00122.png
