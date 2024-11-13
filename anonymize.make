.PHONY: clean all

all: anonymous-submission.tar

private/faster-minikanren:
	git submodule init
	git submodule update

anonymous-submission: docs/README.anonymous.md private/faster-minikanren
	mkdir -p $@
	cp docs/README.anonymous.md $@/README.md

	cp *.scm *.py *.sh *.rkt $@
	cp -r private tests $@
	rm -r $@/private/faster-minikanren/.git

anonymous-submission.tar: anonymous-submission
	{ \
	matches=$$(grep -n -H -E -R -i --exclude "README.md" --exclude-dir="faster-minikanren" "michael|ballantyne|jason|hemann|nada|\bamin\b|william|byrd|raffi|sanna|harvard|indiana|northeastern|seton|alabama|UAB|birmingham" $^) ;\
	if [ -n "$$matches" ] ;\
	then echo "FAILING, found references to personal information:" ;\
		echo "$$matches" ;\
		exit 1 ;\
	fi ;\
	tar --create --file $@ $^ ;\
	}

clean:
	rm -rf ./anonymous-submission ./anonymous-submission.tar

