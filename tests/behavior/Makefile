SRCS = $(wildcard *.fm)
IRS = $(patsubst %.fm,%.ll,$(SRCS))
ASMS = $(patsubst %.fm,%.s,$(SRCS))
OBJS = $(patsubst %.fm,%,$(SRCS))

CC = clang
CFLAGS = -g -c
LLC = llc
OPT = opt
OPTFLAGS = -S -passes='default<O3>'
ZIGTEST = zig test
FEMTOC = ../../zig-out/bin/femtoc
FMFLAGS = --emit-llvm

ifndef VERBOSE
.SILENT:
endif

build: femtoc $(OBJS)

femtoc:
	zig build

%: %.fm
	$(FEMTOC) $< $(FMFLAGS) -o $(patsubst %.fm,%.ll,$<)
	echo "$(patsubst %.fm,%,$<): unoptimized"
	# unoptimized
	$(LLC) $(patsubst %.fm,%.ll,$<) -o $(patsubst %.fm,%.s,$<)
	$(CC) $(CFLAGS) $(patsubst %.fm,%.s,$<) -o $(patsubst %.fm,%.o,$<)
	zig test $(patsubst %.fm,%.zig,$<) $(patsubst %.fm,%.o,$<)
	# optimized
	echo "$(patsubst %.fm,%,$<): optimized"
	$(OPT) $(OPTFLAGS) $(patsubst %.fm,%.ll,$<) -o $(patsubst %.fm,%.ll,$<)
	$(LLC) $(patsubst %.fm,%.ll,$<) -o $(patsubst %.fm,%.s,$<)
	$(CC) $(CFLAGS) $(patsubst %.fm,%.s,$<) -o $(patsubst %.fm,%.o,$<)
	zig test $(patsubst %.fm,%.zig,$<) $(patsubst %.fm,%.o,$<)

clean:
	rm -f $(IRS) $(ASMS) $(OBJS)
