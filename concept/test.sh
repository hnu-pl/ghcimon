runhaskell AddEcho.hs < hello.ghci > test.ghci
enscript test.ghci --highlight=haskell --color --language=html -o hello-in-raw.html
runhaskell TagInput.hs < hello-in-raw.html > test-in-raw.html

ghci -ghci-script test.ghci < /dev/null | enscript --highlight=haskell --color --language=html -o test-out-raw.html
