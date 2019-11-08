enscript hello.ghci --highlight=haskell --color --language=html -o hello-in-raw.html
ghci -ghci-script hello.ghci < /dev/null | enscript --highlight=haskell --color --language=html -o hello-out-raw.html
