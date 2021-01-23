build:
	elm make src/Main.elm --output dist/main.js

typecheck:
	find src/ -name '*.elm' | entr -s 'clear; elm make src/**/*.elm --output /dev/null'

test:
	find src/ tests/ -name '*.elm' | entr -s 'clear; elm-test'
