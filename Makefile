format:
	find chapter*/src chapter*/test -name "*.purs" -exec purty format {} --write \;

.PHONY: format
