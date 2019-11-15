# -*- mode: makefile-gmake; -*-
POSTS_MD := $(wildcard posts/*.md)
POSTS_HTML := $(POSTS_MD:%.md=%.html)

AUTHOR := "Alan Dipert"
BLOG_URL := https://tailrecursion.com/jlt
FEED_ID := urn:uuid:489f7ea4-0699-11ea-b588-843a4b7c6000
POST_BASE_URL := $(BLOG_URL)/posts
TITLE := "Just Lisp Things"

CF_DIST := E6GOTXLS9MCZF
S3_PATH := s3://tailrecursion.com/jlt
CACHE_CONTROL_DAYS := 7
SECONDS_IN_DAY := 86400
CACHE_CONTROL := max-age=$(shell expr $(CACHE_CONTROL_DAYS) \* $(SECONDS_IN_DAY))

GEN := emacs --quick --script scripts/gen.el

all: public

atom.xml: $(POSTS_MD) scripts/gen.el
	$(GEN) atom "posts" $(TITLE) $(AUTHOR) $(BLOG_URL) $(FEED_ID) $(POST_BASE_URL) > $@

index.html: templates/index.html templates/listing.html $(POSTS_HTML) scripts/gen.el
	$(GEN) index "posts" templates/index.html templates/listing.html > $@

%.html: %.md templates/article.html scripts/gen.el
	$(GEN) post $< > $@

public: atom.xml index.html style.css $(POSTS_HTML)
	$(shell mkdir -p public/posts)
	cp atom.xml public
	cp index.html public
	cp style.css public
	$(GEN) syntax-highlight-css > public/code_highlight.css
	$(foreach html,$(POSTS_HTML),$(shell cp $(html) public/$(html)))

deploy: public
	aws s3 sync public $(S3_PATH) --cache-control $(CACHE_CONTROL)
	aws cloudfront create-invalidation --distribution-id $(CF_DIST) --paths '/*'

clean:
	rm -f atom.xml index.html $(POSTS_HTML)
	rm -rf public

print-%:
	@echo '$*=$($*)'

.PHONY: clean print-% deploy
