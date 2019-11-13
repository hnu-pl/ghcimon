enscript test.ghci --highlight=haskell --color --language=html -o test-in-raw.html
ghci -ghci-script test.ghci < /dev/null | enscript --highlight=haskell --color --language=html -o test-out-raw.html
