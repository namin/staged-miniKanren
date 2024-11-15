.PHONY: clean all

all: anonymous-submission.tar

private/faster-minikanren:
	git submodule init
	git submodule update

anonymous-submission: private/faster-minikanren
	mkdir -p $@
	cp README.anonymous.md $@/README.md

	cp *.scm *.py *.sh *.rkt $@
	cp -r private tests $@
	rm -r $@/private/faster-minikanren/.git

anonymous-submission.tar: anonymous-submission
	{ \
	matches=$$(grep -n -H -E -R -i --exclude "README.md" --exclude "README.anonymous.md" --exclude-dir="faster-minikanren" "michael|ballantyne|jason|hemann|nada|\bamin\b|william|byrd|raffi|sanna|harvard|indiana|northeastern|seton|alabama|UAB|birmingham" $^) ;\
	if [ -n "$$matches" ] ;\
	then echo "WARNING, found references to personal information:" ;\
		echo "$$matches" ;\
		echo "These can be edited in the 'anonymous-submission' directory";\
		read -p "Hit enter to continue or C-c to quit: " response;\
	fi ;\
	tar --create --file $@ $^ ;\
	}

clean:
	rm -rf ./anonymous-submission ./anonymous-submission.tar

