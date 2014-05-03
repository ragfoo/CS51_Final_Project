SOURCES = \
Draw.ml \
Event51.ml \
Helpers.ml \
Main.ml \
UI.ml \
World.ml \
WorldObject.ml \
WorldObjectI.ml

all: $(SOURCES)
	corebuild -quiet -lib graphics Main.native

check: $(SOURCES)
	@chmod u+x ../check_width
	@
	../check_width Draw.ml; \
	../check_width Event51.ml; \
	../check_width Helpers.ml; \
	../check_width Main.ml; \
	../check_width UI.ml; \
	../check_width World.ml; \
	../check_width WorldObject.ml; \
	../check_width WorldObjectI.ml

clean:
	rm -rf _build Main.native

